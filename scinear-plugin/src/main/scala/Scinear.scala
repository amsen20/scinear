package scinear

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names
import dotty.tools.dotc.core.StdNames
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.plugins.PluginPhase
import dotty.tools.dotc.plugins.StandardPlugin
import dotty.tools.dotc.transform.Staging
import dotty.tools.dotc.report

class Scinear extends StandardPlugin:
  val name: String = "Scinear"
  override val description: String = "scinear"
  override def init(options: List[String]): List[PluginPhase] =
    ScinearPhase() :: Nil
end Scinear

class ScinearPhase() extends PluginPhase:
  override def phaseName: String = ScinearPhase.name
  override val runsAfter = Set("splicing")
  override def runsBefore: Set[String] = Set("pickleQuotes")
  override def transformApply(tree: Apply)(using Context): Tree =
    tree
end ScinearPhase

object ScinearPhase:
  val name = "scinearphase"
end ScinearPhase
