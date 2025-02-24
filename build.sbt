import Dependencies.Compile._

ThisBuild / scalaVersion := "3.6.3"
ThisBuild / organization := "ca.uwaterloo.plg"
ThisBuild / versionScheme := Some("early-semver")
ThisBuild / homepage := Some(
  url("https://github.com/amsen20/scinear")
)
ThisBuild / licenses := List(
  "Apache-2.0" -> new URL(
    "https://www.apache.org/licenses/LICENSE-2.0.txt"
  )
)
ThisBuild / developers := List(
  Developer(
    "amirhossein-pashaeehir",
    "Amirhossein Pashaeehir",
    "ahph1380@gmail.com",
    url("https://github.com/amsen20")
  )
)

//YOU MAY REMOVE THIS IF YOU HAVE THE PROJECT IN A LOCAL GIT CHECKOUT OF A REMOTE REPOSITORY
ThisBuild / scmInfo := Some(
  ScmInfo(
    url(
      "https://github.com/plg/scinear"
    ),
    "scm:git:https://github.com/plg/scinear.git"
  )
)
//END-YOU MAY REMOVE THIS IF YOU HAVE THE PROJECT IN A LOCAL GIT CHECKOUT OF A REMOTE REPOSITORY
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / githubWorkflowTargetTags ++= Seq("v*")
ThisBuild / githubWorkflowPublishTargetBranches :=
  Seq(RefPredicate.StartsWith(Ref.Tag("v")))
ThisBuild / githubWorkflowPublish := Seq(
  WorkflowStep.Sbt(
    List("ci-release"),
    env = Map(
      "PGP_PASSPHRASE" -> "${{ secrets.PGP_PASSPHRASE }}",
      "PGP_SECRET" -> "${{ secrets.PGP_SECRET }}",
      "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
      "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}"
    )
  )
)
addCommandAlias(
  "ci-test",
  "scalafmtCheckAll; scalafmtSbtCheck; github; documentation / mdoc; root / Test / test"
)
addCommandAlias("ci-it-test", "scinear-plugin / IntegrationTest / test")
addCommandAlias("ci-docs", "github; documentation / mdoc")
addCommandAlias("ci-publish", "github; ci-release")

lazy val commonSettings = Seq(
  organizationName := "PLG",
  startYear := Some(2023)
)

lazy val root = project
  .in(file("."))
  .settings(commonSettings)
  .settings(
    name := "scinear-root",
    publish / skip := true,
  )
  .aggregate(`scinear-lib`, `scinear-plugin`, `scinear-examples`, `documentation`)

lazy val `scinear-lib` = project
  .in(file("./scinear-lib"))
  .settings(autoAPIMappings := true)
  .settings(commonSettings)

lazy val `scinear-plugin` = project
  .in(file("./scinear-plugin"))
  .settings(commonSettings)
  .dependsOn(`scinear-lib`)
  .enablePlugins(SbtMunitCompilerToolkitPlugin)

lazy val `scinear-examples` = project
  .in(file("./scinear-examples"))
  .dependsOn(`scinear-lib`, `scinear-plugin`)
  .settings(
    publish / skip := true,
    autoCompilerPlugins := true,
    Compile / fork := true,
    Compile / scalacOptions += s"""-Xplugin:${(`scinear-plugin` / Compile / packageBin).value}""",
    Compile / scalacOptions += s"""-Xprint:scinearphase""",
  )

lazy val documentation = project
  .dependsOn(`scinear-lib`, `scinear-plugin`)
  .settings(commonSettings)
  .enablePlugins(MdocPlugin)
  .settings(mdocOut := file("."))
  .settings(publish / skip := true)
