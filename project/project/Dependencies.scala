import sbt._
object Dependencies {

  object Versions {
    val sbtMunitCompilerToolkit = "0.1.7"
    val sbtCiRelease = "1.5.7"
    val sbtGithubActions = "0.13.0"
    val sbtDependencyUpdates = "1.2.7"
    val sbtHeader = "5.9.0"
    val sbtGithub = "0.11.2"
    val sbtGithubMdoc = "0.11.2"
    val sbtMdoc = "2.3.2"
    val sbtScalafmt = "2.4.6"
    val munit = "0.7.29"
  }

  object Compile {

  }

  object SbtPlugins {
    val sbtCiRelease = "com.geirsson" % "sbt-ci-release" % Versions.sbtCiRelease
    val sbtGithubActions =
      "com.codecommit" % "sbt-github-actions" % Versions.sbtGithubActions
    val sbtDependencyUpdates =
      "org.jmotor.sbt" % "sbt-dependency-updates" % Versions.sbtDependencyUpdates
    val sbtHeader = "de.heikoseeberger" % "sbt-header" % Versions.sbtHeader
    val sbtGithub = "com.alejandrohdezma" %% "sbt-github" % Versions.sbtGithub
    val sbtGithubMdoc =
      "com.alejandrohdezma" % "sbt-github-mdoc" % Versions.sbtGithubMdoc
    val sbtMdoc = "org.scalameta" % "sbt-mdoc" % Versions.sbtMdoc
    val sbtScalafmt = "org.scalameta" % "sbt-scalafmt" % Versions.sbtScalafmt
    val sbtMunitCompilerToolkit = "com.xebia" % "sbt-munit-compiler-toolkit" % Versions.sbtMunitCompilerToolkit
  }

  object Test {
  }

  lazy val dependencies =
    Seq(
      SbtPlugins.sbtCiRelease,
      SbtPlugins.sbtGithubActions,
      SbtPlugins.sbtDependencyUpdates,
      SbtPlugins.sbtHeader,
      SbtPlugins.sbtGithub,
      SbtPlugins.sbtGithubMdoc,
      SbtPlugins.sbtMdoc,
      SbtPlugins.sbtScalafmt,
    )

}
