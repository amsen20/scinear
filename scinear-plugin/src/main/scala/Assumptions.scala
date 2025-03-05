package scinear

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.Type

import scala.collection.mutable

def isLinear(tpe: Type)(using Context): Boolean =
  tpe.baseClasses.exists(_.fullName.toString == "scinear.Linear")

def isLinear(sym: Symbol)(using Context): Boolean =
  isLinear(sym.info)

/** Assumption list described in the main paper.
  */
type Assumptions = Map[Name, Symbol]

extension (assumptions: Assumptions)
  def --(that: Assumptions): Assumptions = assumptions.--(that.keySet)

val emptyAssumptions: Assumptions = Map[Name, Symbol]()

class StatementResult(val assumptionsUsed: Assumptions, val assumptionsCreated: Assumptions):
  override def toString(): String =
    s"Used: ${assumptionsUsed.mkString(", ")} & Created: ${assumptionsCreated.mkString(", ")}"
end StatementResult

class AssumptionBag(val notUsedAssumptions: Assumptions, val usedAssumptions: Assumptions = emptyAssumptions):

  def after(f: Assumptions => Assumptions): AssumptionBag =
    val usedByf = f(notUsedAssumptions)
    AssumptionBag(notUsedAssumptions -- usedByf, usedAssumptions ++ usedByf)
