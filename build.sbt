import scoverage.ScoverageSbtPlugin.ScoverageKeys._

lazy val root = project.aggregate(core, tests)

lazy val core = project.in(file("core"))
  .settings(commonSettings:_*)
  .settings(coreSettings:_*)

lazy val tests = project.in(file("tests"))
  .settings(commonSettings:_*)
  .settings(testSettings:_*)
  .dependsOn(core)

lazy val commonSettings = Seq(
  scalaVersion := "2.11.6",
  version := "0.1-SNAPSHOT",
  organization := "com.rklaehn",
  libraryDependencies += "junit" % "junit" % "4.11" % "test",
  libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
)

lazy val testSettings = {
  def makeAgentOptions(classpath:Classpath) : String = {
    val jammJar = classpath.map(_.data).filter(_.toString.contains("jamm")).head
    val result = s"-javaagent:$jammJar"
    println(s"Using JVM options $result")
    result
  }
  Seq(
    javaOptions in Test <+= (dependencyClasspath in Test).map(makeAgentOptions),
    libraryDependencies ++= Seq(
      "com.google.code.java-allocation-instrumenter" % "java-allocation-instrumenter" % "3.0" % "test",
      "com.github.jbellis" % "jamm" % "0.3.0" % "test"
    ),
    fork := true
  )
}

lazy val coreSettings = Seq(
  name := "abc",
  // I would prefer just referencing non/algebra at some point
  libraryDependencies += "org.spire-math" %% "spire" % "0.10.1",
  libraryDependencies += "org.spire-math" %% "spire-scalacheck-binding" % "0.9.0" % "test",
  libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.6" % "test",
  unmanagedBase in Test <<= baseDirectory { base => base / "test-lib" },
  coverageMinimum := 100,
  coverageFailOnMinimum := true,
  scalacOptions ++= Seq("-unchecked", "-feature", "–no-specialization"),
  initialCommands in console +=
    """import com.rklaehn.abc._
      |import spire.math._
      |import spire.implicits._
    """.stripMargin
)