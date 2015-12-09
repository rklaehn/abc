import scoverage.ScoverageSbtPlugin.ScoverageKeys._

lazy val root = project.aggregate(core, tests, benchmarks)

lazy val core = project.in(file("core"))
  .settings(commonSettings:_*)
  .settings(coreSettings:_*)

lazy val tests = project.in(file("tests"))
  .settings(commonSettings:_*)
  .settings(testSettings:_*)
  .dependsOn(core)

lazy val benchmarks = project.in(file("benchmarks"))
  .settings(commonSettings:_*)
  .dependsOn(core)
  .enablePlugins(JmhPlugin)

lazy val commonSettings = Seq(
  scalaVersion := "2.11.7",
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
  libraryDependencies += "org.spire-math" %% "algebra" % "0.3.1",
  libraryDependencies += "org.spire-math" %% "algebra-std" % "0.3.1",
  libraryDependencies += "org.spire-math" %% "algebra-laws" % "0.3.1" % "test",
  libraryDependencies += "com.rklaehn" %% "sonicreducer" % "0.2.0",
  libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.6" % "test",
  libraryDependencies += "ichi.bench" % "thyme" % "0.1.1" % "test" from "https://github.com/Ichoran/thyme/raw/master/Thyme.jar",
  coverageMinimum := 100,
  coverageFailOnMinimum := true,
  scalacOptions ++= Seq("-unchecked", "-feature"),
  // scalacOptions ++= Seq("-no-specialization"),
  initialCommands in console +=
    """import com.rklaehn.abc._
      |import spire.math._
      |import spire.implicits._
    """.stripMargin
)