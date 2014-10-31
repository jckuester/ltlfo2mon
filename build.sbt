import AssemblyKeys._

assemblySettings

name := "ltlfo2mon"

version := "1.1"

scalaVersion := "2.10.1"

mainClass in (Compile, run) := Some("ltlfo2mon.cli.Ltlfo2mon")

retrieveManaged := true

jarName in assembly := "ltlfo2mon.jar"

test in assembly := {}

//mainClass in assembly := Some("ltlfo2mon.cli.Ltlfo2mon")

libraryDependencies += "com.github.scopt" %% "scopt" % "2.1.0" withSources() withJavadoc()

resolvers += "sonatype-public" at "https://oss.sonatype.org/content/groups/public"

//fork in run := true
