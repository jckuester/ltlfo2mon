import AssemblyKeys._

name := "ltlfo2mon"

version := "1.0"

scalaVersion := "2.10.1"

//mainClass in (Compile, run) := Some("ltlfo2mon.cli.Ltlfo2mon")

//mainClass in assembly := Some("ltlfo2mon.cli.Ltlfo2mon")

retrieveManaged := true

assemblySettings

jarName in assembly := "ltlfo2mon.jar"

test in assembly := {}

libraryDependencies += "com.github.scopt" %% "scopt" % "2.1.0" withSources() withJavadoc()

resolvers += "sonatype-public" at "https://oss.sonatype.org/content/groups/public"

libraryDependencies += "com.dongxiguo" %% "zero-log" % "0.3.3"

//fork in run := true
