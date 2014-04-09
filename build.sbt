name := "Warthog"

organization := "org.warthog"

version := "0.1-SNAPSHOT"

libraryDependencies ++= Seq(
    "org.specs2" %% "specs2" % "2.3.10",
    "net.java.dev.jna" % "jna" % "3.3.0",
    "org.scalacheck" %% "scalacheck" % "1.10.0" % "test")

resolvers ++= Seq(
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
  "sbt-idea-repo" at "http://mpeltonen.github.com/maven/"
)

scalaVersion := "2.10.4"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

parallelExecution in Test := false

initialCommands in console := """
    import org.warthog.generic.formulas._
    import org.warthog.pl.formulas._
    import org.warthog.fol.formulas._
    import org.warthog.pl.parsers.tptp._
    import org.warthog.fol.parsers.tptp._
"""
