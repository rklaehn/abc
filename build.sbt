import scoverage.ScoverageSbtPlugin.ScoverageKeys._

name := "array based collections"

scalaVersion := "2.11.6"

version := "0.1-SNAPSHOT"

// I would prefer just referencing non/algebra at some point
libraryDependencies += "org.spire-math" %% "spire" % "0.10.1"

libraryDependencies += "org.spire-math" %% "spire-scalacheck-binding" % "0.9.0" % "test"

libraryDependencies += "junit" % "junit" % "4.11" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.6" % "test"

libraryDependencies in Test += "com.google.code.java-allocation-instrumenter" % "java-allocation-instrumenter" % "3.0"

unmanagedBase in Test <<= baseDirectory { base => base / "test-lib" }

coverageMinimum := 100

coverageFailOnMinimum := true

scalacOptions ++= Seq("-unchecked", "-feature")

initialCommands in console += """
import com.rklaehn.abc._
import spire.math._
import spire.implicits._
"""