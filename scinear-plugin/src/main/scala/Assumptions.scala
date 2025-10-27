package scinear

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.Symbols
import dotty.tools.dotc.core.TypeUtils
import dotty.tools.dotc.core.Types

import scala.collection.mutable

/** Assumption list described in the main paper.
  */
type Assumptions = Map[Name, Symbols.Symbol]

extension (assumptions: Assumptions)
  def --(that: Assumptions): Assumptions = assumptions.--(that.keySet)

val emptyAssumptions: Assumptions = Map[Name, Symbols.Symbol]()

class StatementResult(val assumptionsUsed: Assumptions, val assumptionsCreated: Assumptions):
  override def toString(): String =
    s"Used: ${assumptionsUsed.mkString(", ")} & Created: ${assumptionsCreated.mkString(", ")}"
end StatementResult

class AssumptionBag(
    val notUsedAssumptions: Assumptions,
    val usedAssumptions: Assumptions = emptyAssumptions
):
  def after(f: Assumptions => Assumptions): AssumptionBag =
    val usedByf = f(notUsedAssumptions)
    AssumptionBag(notUsedAssumptions -- usedByf, usedAssumptions ++ usedByf)
