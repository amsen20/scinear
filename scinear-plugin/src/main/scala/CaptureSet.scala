package scinear

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Annotations
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols
import dotty.tools.dotc.core.TypeUtils
import dotty.tools.dotc.core.Types

def isRetainsCaptureSetAnnotatedType(annotType: Types.AnnotatedType)(using
    Context
): Boolean =
  annotType.underlying match
    case Types.TypeRef(
          Types.TermRef(
            Types.TermRef(Types.TermRef(Types.NoPrefix, rootDesignator), scalaDesignator),
            capsDesignator
          ),
          capSetDesignator
        ) =>
      rootDesignator.toString == "object _root_" &&
      scalaDesignator.toString == "object scala" &&
      capsDesignator.toString == "object caps" &&
      capSetDesignator.toString == "trait CapSet" &&
      annotType.annot.symbol == Symbols.defn.RetainsAnnot
    case _ => false

def getLinearTermsMentionedInTypeParameters(sym: Symbols.Symbol)(using
    Context
): List[Symbols.Symbol] =

  val linearTerms = scala.collection.mutable.ListBuffer.empty[Symbols.Symbol]

  val traverser = new Types.TypeTraverser:
    def traverse(tp: Types.Type): Unit = tp match
      case appliedType @ Types.AppliedType(tycon, args) =>
        args.foreach {
          case annotatedType @ Types.AnnotatedType(underlying, annot)
              if isRetainsCaptureSetAnnotatedType(annotatedType) =>
            if annot.symbol == Symbols.defn.RetainsAnnot then
              annot.tree.tpe.argInfos
                .map(arg => flattenOrTypes(arg))
                .flatten
                .foreach {
                  case elem @ Types.TermRef(qualType, _) =>
                    if qualType eq Types.NoPrefix then linearTerms += elem.symbol
                  case _ => ()
                }
          case _ => ()
        }
        traverseChildren(appliedType)
      case Types.AnnotatedType(underlying, _) => traverse(underlying)
      case other                              => traverseChildren(other)

  traverser.traverse(sym.info)
  linearTerms.toList
