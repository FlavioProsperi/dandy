import java.nio.file._
import java.nio.file.attribute._
import macroRevolver._

lazy val baseSettings = Seq(
  organization := "com.bumnetworks",
  version := "0.0.1-SNAPSHOT",
  scalaVersion := "2.11.8",
  scalacOptions ++= Seq(
    "-deprecation",
    "-unchecked",
    "-feature",
    "-language:higherKinds"
  ),
  initialCommands := """
    import dandy._
    import scala.reflect.runtime.universe._
  """
) ++ scalariformSettings ++ tutSettings

lazy val cats = "org.typelevel" %% "cats" % "0.4.1"

lazy val deps = Seq(
 libraryDependencies ++= Seq(
   "com.chuusai" %% "shapeless" % "2.3.0",
   "org.specs2" %% "specs2-core" % "3.7.2" % "test",
   "org.specs2" %% "specs2-matcher-extra" % "3.7.2" % "test"))

lazy val updateReadme = taskKey[Unit]("copy tut-generated README.md to project root")

lazy val core = project
  .in(file("."))
  .settings(baseSettings)
  .settings(MacroRevolverPlugin.useMacroParadise)
  .settings(MacroRevolverPlugin.testCleanse)
  .settings(deps)
  .settings(name := "dandy", moduleName := "dandy")
  .settings(updateReadme := {
    val README = "README.md"
    tut.value.foreach {
      case (generated, README) =>
        Files.copy(
          Paths.get(generated.toURI),
          Paths.get(baseDirectory.value.toURI).resolve(README),
          StandardCopyOption.REPLACE_EXISTING
        )
      case _ =>
    }
  })

lazy val tests = project
  .in(file("tests"))
  .settings(baseSettings)
  .settings(deps)
  .settings(libraryDependencies += cats)
  .settings(name := "tests", moduleName := "dandy-tests")
  .settings(publish := {})
  .settings(MacroRevolverPlugin.useMacroParadise)
  .settings(MacroRevolverPlugin.mainCleanse)
  .settings(MacroRevolverPlugin.testCleanse)
  .settings(initialCommands += "import cats._, cats.implicits._, cats.data._, algebra.std.all._")
  .dependsOn(core)

lazy val dandy = project
  .aggregate(core, tests)
  .settings(baseSettings)
