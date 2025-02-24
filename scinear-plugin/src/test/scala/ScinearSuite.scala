package scinear

import com.xebia.functional.munitCompilerToolkit.CompilerSuite
import dotty.tools.dotc.core.Contexts.Context
import scala.compiletime.testing.typeCheckErrors
import scala.util.Properties
import dotty.tools.dotc.core.Contexts.FreshContext
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.reporting.{Diagnostic, Reporter, StoreReporter}
import scala.io.Source

/** An example generated CompilerSuite that will compile the string source code
  * using the Scinear plugin and check its output agains the expected output in
  * the assertion after pickleQuotes.
  *
  * You will replace this with the tests for your plugin.
  */
class ScinearSuite extends ThoroughCompilerSuite:

  val inputDirPath =
    "src/test/resources/input"

  // ! WARN: This is a temporary solution for testing compilation errors.
  // ! The correct way is to use the same mechanism as the scala3 compiler in tests/neg folder,
  // ! But I couldn't find a way to config sbt file for that (with plugin being enabled).
  // ! So, for now I am using this.
  def compareErrorsFromCompilation(file: java.io.File) =
    val sourceCode = Source.fromFile(file).mkString
    val got = compileAndGetErrors(sourceCode)
    val shouldErrorLines = sourceCode
      .split("\n")
      .zipWithIndex
      .collect {
        case (line, index) if line.contains("// error:") => index
      }
      .toSet
    val gotErrorLines = got.map { _.pos.line }.toSet

    val gotLines =
      gotErrorLines.toList.map(i => s"${i + 1}: ${sourceCode.split("\n")(i)}")
    val shouldLines = shouldErrorLines.toList.map(i =>
      s"${i + 1}: ${sourceCode.split("\n")(i)}"
    )

    assertEquals(
      gotLines.sorted,
      shouldLines.sorted,
      s"errors got from file ${file.getName()} are not the same as expected:"
    )
  end compareErrorsFromCompilation

  val directory = new java.io.File(inputDirPath)
  for {
    file <- directory.listFiles
    if file.isFile
  } test(s"Testing ${file.getName}") {
    compareErrorsFromCompilation(file)
  }
end ScinearSuite
