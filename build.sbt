name := "z80_basic"

version := "0.1"

scalaVersion := "2.13.8"

organization := "org.kr.scala.z80"

val coreDependencies = Seq(
    "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1"
)

val testDependencies = Seq(
  "org.scalatest" %% "scalatest" % "3.2.12" % Test
)

libraryDependencies ++= coreDependencies ++ testDependencies

// from: https://githubhelp.com/sbt/sbt-assembly
Compile / run := Defaults.runTask(Compile / fullClasspath, Compile / run / mainClass, Compile / run / runner).evaluated
Compile / runMain := Defaults.runMainTask(Compile / fullClasspath, Compile / run / runner).evaluated
ThisBuild / scalacOptions ++= Seq("-deprecation", "-feature")

// adding fat jar to list of artifacts (sbt-assembly plugin)
assembly / artifact := {
  val art = (assembly / artifact).value
  art.withClassifier(Some("assembly"))
}
//simplified merge strategy
assembly / assemblyMergeStrategy := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x => MergeStrategy.first
}
addArtifact(assembly / artifact, assembly)
