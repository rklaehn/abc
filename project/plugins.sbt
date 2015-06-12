addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.5.2")

addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.7.0")

addSbtPlugin("org.scoverage" %% "sbt-scoverage" % "1.0.4")

initialCommands in console += "import com.rklaehn.abc._"

unmanagedBase in Test <<= baseDirectory { base => base / "test-lib" }