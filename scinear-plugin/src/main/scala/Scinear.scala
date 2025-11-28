package scinear

import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.ast.Trees.Tree
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.Names.TermName
import dotty.tools.dotc.core.StdNames
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.TypeUtils
import dotty.tools.dotc.core.Types
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.plugins.PluginPhase
import dotty.tools.dotc.plugins.StandardPlugin
import dotty.tools.dotc.report
import dotty.tools.dotc.transform.Staging
import dotty.tools.dotc.transform.init.Util
import dotty.tools.dotc.util.SimpleIdentitySet.empty

import scala.collection.mutable

class Scinear extends StandardPlugin:
  val name: String = "Scinear"
  override val description: String = "scinear"
  override def init(options: List[String]): List[PluginPhase] =
    ScinearPhase() :: Nil
end Scinear

class LinearTypesUsageTracker:
  val linearTypesInternalUsedFields: mutable.Map[Symbol, Set[Name]] =
    mutable.Map.empty[Symbol, Set[Name]]
  val linearTypesExternalUsedFields: mutable.Map[Symbol, Set[Name]] =
    mutable.Map.empty[Symbol, Set[Name]]

  def addUsage(linearType: Symbol, field: Name, isInternal: Boolean): Unit =
    val usedFieldsMap =
      if isInternal then linearTypesInternalUsedFields else linearTypesExternalUsedFields
    val usedFields = usedFieldsMap.getOrElse(linearType, Set.empty[Name])
    usedFieldsMap.update(linearType, usedFields + field)

  def validate()(using ctx: Context): Unit =
    linearTypesInternalUsedFields.foreach { (linearType, internalFields) =>
      val externalFields = linearTypesExternalUsedFields.getOrElse(linearType, Set.empty)
      val commonFields = internalFields.intersect(externalFields)
      if commonFields.nonEmpty then
        report.error(
          s"Linear type ${linearType.name} has fields ${commonFields.mkString(", ")} used both internally and externally",
          linearType.sourcePos
        )
    }

end LinearTypesUsageTracker

class ScinearPhase() extends PluginPhase:
  override def phaseName: String = ScinearPhase.name
  override val runsAfter = Set("refchecks")
  override def runsBefore: Set[String] = Set("protectedAccessors")

  val usageTracker = new LinearTypesUsageTracker()

  def checkLinearPolymorphicTypeArgument(expr: tpd.Tree)(using ctx: Context): Unit =
    val traverser = new TreeTraverser:
      def traverse(tree: Tree[Type])(using Context) =
        tree match
          case tpd.DefDef(name, _, _, _) =>
            if name.toString != "unapply" then traverseChildren(tree)
          case tpd.TypeApply(fun, args) =>
            if isFunctionOkToHaveUnsafeAccess(fun) then
              report.inform(
                "Granting unsafe access of linear values to a function",
                fun.sourcePos
              )
            else
              def checkArgWithParamLinearity(
                  isParamsLinear: List[Boolean],
                  args: List[tpd.Tree]
              ): Unit =
                assert(isParamsLinear.length >= args.length)
                isParamsLinear
                  .zip(args)
                  .foreach((isParamLinear, arg) =>
                    if arg.tpe != null && isLinear(arg.tpe) && !isParamLinear then
                      report.error(
                        "Passing linear types as polymorphic type arguments are not allowed here",
                        arg.sourcePos
                      )
                  )

              val isParamsLinear = fun.symbol.info match
                case pt: Types.PolyType =>
                  val paramSymbols = fun.symbol.paramSymss.flatten
                  pt.paramInfos
                    .zip(paramSymbols)
                    .map((paramInfo, paramSymbol) =>
                      isLinear(paramInfo) || doesHideLinearity(paramSymbol)
                    )
                case _ => List.empty[Boolean]

              checkArgWithParamLinearity(
                if isParamsLinear.length < args.length then
                  isParamsLinear ++ List.fill(args.length - isParamsLinear.length)(false)
                else isParamsLinear,
                args
              )
          case _ => traverseChildren(tree)
    traverser.traverse(expr)

  def checkStatement(stat: tpd.Tree, assumptions: Assumptions)(using Context): StatementResult =
    logger.debug("Checking statement: " + stat.show + " // " + stat) {
      stat match
        case valDef: tpd.ValDef =>
          if checkValDef(valDef) && isLinear(valDef.tpt.tpe) then
            if assumptions.contains(valDef.name) then
              // TODO: make sure it is ok to redefine a linear value
              // throw new Exception(s"Somehow redefining a linear value ${valDef.name} for the second time")
              ()
            StatementResult(
              checkExpr(valDef.rhs, assumptions),
              Map[Name, Symbol](valDef.name -> valDef.symbol)
            )
          else StatementResult(checkExpr(valDef.rhs, assumptions), emptyAssumptions)
        case stat =>
          // Only valDef can create a new linear value, so in other cases we treat the statement like an expression.
          StatementResult(checkExpr(stat, assumptions), emptyAssumptions)
    }

  def checkStatements(stats: List[tpd.Tree], assumptions: Assumptions)(using
      Context
  ): StatementResult =
    logger.debug("Checking statements: " + stats) {
      val (overallCreated, overallUsed) =
        stats.foldLeft((emptyAssumptions, emptyAssumptions))((assumptionPair, stat) => {
          val (created, used) = assumptionPair
          val nextResult = checkStatement(stat, assumptions ++ created -- used)
          (created ++ nextResult.assumptionsCreated, used ++ nextResult.assumptionsUsed)
        })

      StatementResult(overallUsed -- overallCreated, overallCreated -- overallUsed)
    }

  def checkCase(caseDef: tpd.CaseDef, assumptions: Assumptions)(using Context): Assumptions =
    // TODO: case pattern may use some linear values, this should be taken care of because although all bodies are not executed but all cases are.
    val pat = caseDef.pat
    val guard = caseDef.guard
    val body = caseDef.body

    val linearIdents = new TreeAccumulator[Assumptions]:
      def apply(assumptions: Assumptions, tree: Tree[Type])(using Context) =
        tree match
          case ident: tpd.Ident if isLinear(ident.tpe) =>
            assumptions ++ Map(ident.name -> ident.symbol)
          case bind: tpd.Bind if isLinear(bind.body.tpe) =>
            assumptions ++ Map(bind.name -> bind.symbol)
          case _ => foldOver(assumptions, tree)
    val createdAssumptions = linearIdents.apply(emptyAssumptions, pat)
    val afterBody =
      AssumptionBag(assumptions ++ createdAssumptions)
        .after(checkExpr(guard, _))
        .after(checkExpr(body, _))

    if (createdAssumptions -- afterBody.usedAssumptions).nonEmpty then
      val notUsed = createdAssumptions -- afterBody.usedAssumptions
      notUsed.foreach((name, symbol) =>
        report.error(
          s"Linear value ${name} is not used (being defined in the case)",
          symbol.sourcePos
        )
      )

    afterBody.usedAssumptions -- createdAssumptions

  // TODO: refactor it so the checkExpr does not get assumptions as a parameter
  def checkExpr(expr: tpd.Tree, assumptions: Assumptions, ignoreCompilerGenerated: Boolean = false)(
      using Context
  ): Assumptions =
    logger.debug("Checking expr: " + expr.show + " // " + expr) {
      expr match
        case ident: tpd.Ident =>
          val linearTermsInType = getLinearTermsMentionedInTypeParameters(ident.symbol)
          linearTermsInType.foreach(sym =>
            if !assumptions.contains(sym.name) then
              report.error(
                s"Linear value ${sym.name} is mentioned in the type of ${ident.name} but is either not in scope or used already.",
                ident.sourcePos
              )
          )
          if isLinear(ident.symbol) then
            if assumptions.contains(ident.name) then Map[Name, Symbol](ident.name -> ident.symbol)
            else if ignoreCompilerGenerated && isCompilerGeneratedVariable(ident.symbol) then
              /** Compiler might generate use cases of a compiler generated variable (e.g., field
                * accessors), that does not follow linearity rules.
                */
              report.warning(
                s"Ignoring double use of linear value ${ident.name} here, assuming it is compiler generated.",
                ident.sourcePos
              )
              emptyAssumptions
            else
              report.error(
                s"Linear value ${ident.name} is being used twice, or is not accessed directly.",
                ident.sourcePos
              )
              // TODO: this is not a proper way of showing empty assumptions:
              emptyAssumptions
          else emptyAssumptions

        case Util.NewExpr(tref, New(tpt), ctor, argss) =>
          val args = argss.flatten
          args
            .foldLeft(AssumptionBag(assumptions))((assumptionsBag, arg) =>
              assumptionsBag.after(checkExpr(arg.tree, _))
            )
            .usedAssumptions

        case select @ tpd.Select(qualifier, name) =>
          if isLinear(select.tpe) && isLinear(qualifier.tpe) then
            usageTracker.addUsage(
              qualifier.typeOpt.typeSymbol,
              name,
              isInternal = false
            )
          checkExpr(
            qualifier,
            assumptions,
            /* if it's field accessor ignore it if it's compiler generated */ isFieldAccessorMethod(
              name.toString
            )
          )

        case Util.Call(ref, argss) =>
          val afterFn = AssumptionBag(assumptions).after(checkExpr(ref, _))

          val args = argss.flatten
          args
            .foldLeft(afterFn)((assumptionsBag, arg) =>
              assumptionsBag.after(checkExpr(arg.tree, _))
            )
            .usedAssumptions

        case tthis: tpd.This =>
          // TODO: make sure it is ok
          if isLinear(tthis.symbol) then
            report.error(
              "Referring linear types as `this` is not allowed",
              tthis.sourcePos
            )
          emptyAssumptions

        case tpd.Literal(_) =>
          // TODO: make sure it is ok
          emptyAssumptions

        case tpd.Typed(expr, tpt) =>
          // TODO: be aware of type casts (from or to linear types)
          checkExpr(expr, assumptions)

        case tpd.NamedArg(_, arg) =>
          // TODO: why can the arg be empty? what should I do for that case
          checkExpr(arg, assumptions)

        case tpd.Assign(lhs, rhs) =>
          // TODO: maybe can implement it by just doing isLinear(lhs.symbol)
          lhs match
            case tpd.Select(qual, _) =>
              if isLinear(qual.symbol) then
                report.error("Assigning to a linear value is not allowed", lhs.sourcePos)
            case id: Ident =>
              if isLinear(id.symbol) then
                report.error("Assigning to a linear value is not allowed", lhs.sourcePos)
          checkExpr(rhs, assumptions)

        case tpd.closureDef(ddef) =>
          checkDefDef(ddef)
          emptyAssumptions

        case Util.PolyFun(ddef) =>
          checkDefDef(ddef)
          emptyAssumptions

        case tpd.Block(stats, expr) =>
          val statsResult = checkStatements(stats, assumptions)
          val usedAfterExpr =
            checkExpr(
              expr,
              assumptions ++ statsResult.assumptionsCreated -- statsResult.assumptionsUsed
            )
          (statsResult.assumptionsCreated -- usedAfterExpr).foreach((name, symbol) =>
            report.error(s"Linear value ${name} is not used", symbol.sourcePos)
          )
          usedAfterExpr ++ statsResult.assumptionsUsed -- statsResult.assumptionsCreated

        case tpd.If(cond, thenp, elsep) =>
          /** TODO: Instead of enforcing all branches to use the same linear values, union them and
            * consider that as used.
            */
          val afterCond = AssumptionBag(assumptions).after(checkExpr(cond, _))
          val thenUsed = checkExpr(thenp, afterCond.notUsedAssumptions)
          val elseUsed = checkExpr(elsep, afterCond.notUsedAssumptions)
          if thenUsed != elseUsed then
            report.error("If branches should use the same linear values", expr.sourcePos)
          thenUsed ++ afterCond.usedAssumptions

        case tpd.Annotated(arg, annot) =>
          // TODO: make sure it is ok to ignore arg
          checkExpr(arg, assumptions)

        case tpd.Match(selector, cases) =>
          /** TODO: Instead of enforcing all cases to use the same linear values, union them and
            * consider that as used.
            */
          val afterSelector = AssumptionBag(assumptions).after(checkExpr(selector, _))
          val casesAssumptions =
            cases.map(caseDef => checkCase(caseDef, afterSelector.notUsedAssumptions))
          if !casesAssumptions.forall(_ == casesAssumptions.head) then
            report.error("All match cases should use the same linear values", expr.sourcePos)
          afterSelector.usedAssumptions ++ casesAssumptions.head

        case tpd.Return(expr, from) =>
          // TODO: make sure it is ok to ignore from
          checkExpr(expr, assumptions)

        case tpd.WhileDo(cond, body) =>
          checkExpr(cond, emptyAssumptions)
          checkExpr(body, emptyAssumptions)
          emptyAssumptions

        case tpd.Labeled(_, expr) =>
          // TODO: take care of the binding
          checkExpr(expr, assumptions)

        case tpd.Try(block, cases, finalizer) =>
          val afterBlock = AssumptionBag(assumptions).after(checkExpr(block, _))
          val casesAssumptions =
            cases.map(caseDef => checkExpr(caseDef.body, afterBlock.notUsedAssumptions))
          if !casesAssumptions.forall(_ == casesAssumptions.head) then
            report.error("All catch blocks should use the same linear values", expr.sourcePos)
          val usedInCases = casesAssumptions.foldLeft(emptyAssumptions)(_ ++ _)
          val usedInFinally = checkExpr(
            finalizer,
            afterBlock.notUsedAssumptions -- usedInCases
          )
          afterBlock.usedAssumptions ++ usedInCases ++ usedInFinally

        case tpd.SeqLiteral(elems, elemtpt) =>
          elems
            .foldLeft(AssumptionBag(assumptions))((assumptionsBag, elem) =>
              assumptionsBag.after(checkExpr(elem, _))
            )
            .usedAssumptions

        case tpd.Inlined(call, bindings, expansion) =>
          /** For now, we assume that the inline does not allow in linear values to be passed
            * through the bindings.
            *
            * TODO: Transfer the linear values through the bindings.
            */
          checkExpr(expansion, emptyAssumptions)

        case tpd.Thicket(List()) =>
          // TODO: make sure it is ok
          // possible in try/catch/finally, see tests/crash/i6914.scala
          emptyAssumptions

        case valDef: tpd.ValDef =>
          if checkValDef(valDef) && isLinear(valDef.tpt.tpe) then
            // TODO: check if it maybe useful
            report.warning("found a linear value definition but gonna ignore it", valDef.sourcePos)
          checkExpr(valDef.rhs, assumptions)

        case defDef: tpd.DefDef =>
          checkDefDef(defDef)
          emptyAssumptions

        case typeDef: tpd.TypeDef =>
          if typeDef.isClassDef then
            if isLinear(typeDef.symbol) then
              // TODO: check if I should report an error here
              checkLinearTypeDef(typeDef)
            else checkNonLinearTypeDef(typeDef)
          emptyAssumptions

        case tpl: Template =>
          // TODO: support it
          throw new Exception("Template is not supported yet")

        case _: Import | _: Export | _: Quote | _: Splice | _: QuotePattern | _: SplicePattern =>
          // TODO: make sure it is ok
          emptyAssumptions

        case e =>
          report.error("Unexpected expression: " + e.show, e.sourcePos)
          emptyAssumptions
    }

  def checkNoLinearValueExistsIn(expr: tpd.Tree)(using Context): Unit =
    // TODO: implement it
    throw new Exception("Not implemented yet")
    ()

  def checkNotLinear(tree: tpd.Tree)(using Context): Unit =
    if isLinear(tree.tpe) then
      report.error("Linear values are not allowed to be here", tree.sourcePos)

  def checkValDef(valDef: tpd.ValDef)(using Context): Boolean =
    // TODO: check if valDef is a linear type:
    // TODO:     - It should not be mutable
    // TODO:     - It should not be lazy
    true

  // TODO: add logging which makes it easier to understand the error
  def checkLinearTypeDef(typeDef: tpd.TypeDef)(using Context): Unit =
    // TODO: check if the typedef is not an object
    // TODO: should a linear type derive from another non-linear type? what will happen to the methods?
    // TODO: Don't allow nonlinear types inherit from linear types.
    typeDef.rhs
      .asInstanceOf[tpd.Template]
      .body
      .foldLeft(emptyAssumptions) { (assumptions, stat) =>
        stat match
          case valDef: tpd.ValDef =>
            val usedAndDefinedInExpr = checkExpr(valDef.rhs, assumptions)
            (usedAndDefinedInExpr -- assumptions).foreach((name, symbol) =>
              report.error(s"Linear value ${name} is not used", symbol.sourcePos)
            )

            val remainingAssumptions = assumptions -- usedAndDefinedInExpr
            val usedAssumptions = assumptions -- remainingAssumptions
            usedAssumptions
              .foreach((name, _) => usageTracker.addUsage(typeDef.symbol, name, isInternal = true))

            valDef match
              case vdef if isLinear(vdef.tpt.tpe) =>
                remainingAssumptions ++ Map[Name, Symbol](vdef.name -> vdef.symbol)
              case _ => remainingAssumptions

          case defDef: tpd.DefDef =>
            if !defDef.symbol.isAnyOverride then
              if !isCompilerGeneratedMethod(defDef) then
                checkDefDef(defDef)
                // TODO: Make a decision to allow methods or not
                report.warning("Only default methods are allowed in linear types", defDef.sourcePos)
              else
                // This methods are generated by the compiler, so just informing the user about ignoring them by a warning.
                report.warning(
                  s"Ignoring compiler generated method ${defDef.name.toString}, if it is not generated, delete it. Linear types cannot have methods",
                  defDef.sourcePos
                )
            else
              // Override methods are fine because linear types cannot inherit from other types so this methods are only default methods.
              (
            )

            // TODO: check if the method is inherited, if no error.
            // TODO: be aware of methods that are defined by default.
            // TODO: for now, it is disabled by not allowing calling methods on linear types.
            assumptions

          case typeDef: tpd.TypeDef =>
            if typeDef.isClassDef then
              report.error(
                "You are not allowed to define new classes inside a linear type",
                typeDef.sourcePos
              )
            assumptions

          case e =>
            report.error("This is not allowed in a linear type", e.sourcePos)
            assumptions
      }

  // TODO: remove this returning booleans
  def checkNonLinearTypeDef(typeDef: tpd.TypeDef)(using Context): Boolean =
    typeDef.rhs
      .asInstanceOf[tpd.Template]
      .body
      .map {
        case valDef: tpd.ValDef =>
          if (isLinear(valDef.tpt.tpe)) then
            report.error(
              "Non-linear types are not allowed to store linear values",
              typeDef.sourcePos
            )
            false
          else
            checkExpr(valDef.rhs, emptyAssumptions)
            true

        case defDef: tpd.DefDef =>
          checkDefDef(defDef)
        case expr =>
          // if this is not a val or def, then check it like a normal expression
          checkExpr(expr, emptyAssumptions)
          true
      }
      .fold(true)(_ && _)

  def checkDefDef(defDef: tpd.DefDef)(using Context): Boolean =
    // TODO: find a solution for polymorphic functions, it can be solved by keeping track of
    // TODO: which types are being used as linear and then check the method's validity for that subset of arguments.
    // TODO: To be formal, check if a function is valid linear to a subset of its arguments.
    if defDef.name.toString == "unapply" then
      report.warning(
        "Ignoring unapply method, this is the only way to access multiple fields of a linear type",
        defDef.sourcePos
      )
      true
    else
      val assumptions: Assumptions =
        defDef.paramss.flatten
          .filter(param => param.tpe.isInstanceOf[Types.TermRef])
          .filter(param => isLinear(param.symbol))
          .map(param => param.name -> param.symbol)
          .toMap
      val usedAssumptions = checkExpr(defDef.rhs, assumptions)
      if usedAssumptions != assumptions then
        val notUsed = assumptions -- usedAssumptions
        notUsed.foreach((name, symbol) =>
          report.error(s"Linear argument ${name} is not used", symbol.sourcePos)
        )
        false
      else true

  def reachEntryPoints(tree: tpd.Tree)(using Context): Unit =
    tree match
      case packageDef: tpd.PackageDef =>
        packageDef.stats.foreach(reachEntryPoints)
      case typeDef: tpd.TypeDef =>
        if typeDef.isClassDef then
          if isLinear(typeDef.symbol) then checkLinearTypeDef(typeDef)
          else checkNonLinearTypeDef(typeDef)
      case defDef: tpd.DefDef =>
        checkDefDef(defDef)
      case _ =>
      // TODO: recursively check the tree

  def checkLinearTypeFieldUsages()(using Context): Unit =
    usageTracker.validate()

  override def transformUnit(tree: tpd.Tree)(using Context): tpd.Tree =
    checkLinearPolymorphicTypeArgument(tree)
    reachEntryPoints(tree)
    checkLinearTypeFieldUsages()
    tree

end ScinearPhase

object ScinearPhase:
  val name = "scinearphase"
end ScinearPhase
