package scinear

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Annotations
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols
import dotty.tools.dotc.core.TypeUtils
import dotty.tools.dotc.core.Types

def getLinearTermsMentionedInTypeParameters(sym: Symbols.Symbol)(using
    Context
): List[Symbols.Symbol] =
  val retainedAnnots = sym.info.argInfos.foldRight(List.empty[Annotations.Annotation]) {
    (argType, acc) =>
      argType.getAnnotation(Symbols.defn.RetainsAnnot) match
        case Some(annot) => annot :: acc
        case None        => acc
  }

  retainedAnnots.flatMap(annot =>
    annot.tree.tpe.argInfos
      .map(arg => flattenOrTypes(arg))
      .flatten
      .map(_ match
        case elem @ Types.TermRef(qualType, _) =>
          if qualType eq Types.NoPrefix then Some(elem.symbol) else None
        case other => None)
      .map(_.toList)
      .flatten
  )
