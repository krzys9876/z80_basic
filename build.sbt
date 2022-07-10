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
