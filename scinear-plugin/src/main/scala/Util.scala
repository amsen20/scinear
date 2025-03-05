package scinear

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Contexts.Context

// Generated field access methods e.g.: _1
final val FIELD_ACCESS_PATTERN = "(_\\d+)".r
// Generated default copy methods e.g.: copy$default$1
final val COPY_DEFAULT_PATTERN = "(copy\\$default\\$\\d+)".r
// Generated copy method
final val COPY_PATTERN = "(copy)".r

def isCompilerGeneratedMethod(defDef: DefDef)(using Context): Boolean = {
  val name = defDef.name.toString

  if (FIELD_ACCESS_PATTERN.matches(name)) return true
  if (COPY_DEFAULT_PATTERN.matches(name)) return true
  if (COPY_PATTERN.matches(name)) return true
  false
}
