package scinear

import com.xebia.functional.munitCompilerToolkit.CompilerSuite
import dotty.tools.dotc.Compiler
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.core.Comments.ContextDoc
import dotty.tools.dotc.core.Comments.ContextDocstrings
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.reporting.{Diagnostic, Reporter, StoreReporter}
import munit.FunSuite

import scala.util.Properties
import dotty.tools.dotc.util.SourceFile
import dotty.tools.io.VirtualDirectory

class ThoroughCompilerSuite extends CompilerSuite:

  def compileAndGetErrors(source: String): List[Diagnostic.Error] =
    val reporter = new StoreReporter(null)
    val context =
      compilerContext().asInstanceOf[FreshContext]
    context.setReporter(reporter)
    context.setSetting(
      context.settings.outputDir,
      new VirtualDirectory("output", None)
    )
    given Context =
      context
    val run = new Compiler().newRun
    run.compileFromStrings(List(source))
    run.runContext
    reporter.allErrors
  end compileAndGetErrors
