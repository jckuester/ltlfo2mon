import AssemblyKeys._

assemblySettings

name := "ltlfo2mon"

version := "1.1"

scalaVersion := "2.10.4"

mainClass in (Compile, run) := Some("ltlfo2mon.cli.Ltlfo2mon")

retrieveManaged := true

jarName in assembly := "ltlfo2mon.jar"

test in assembly := {}

//mainClass in assembly := Some("ltlfo2mon.cli.Ltlfo2mon")

libraryDependencies ++= Seq(
  "com.github.scopt" %% "scopt" % "2.1.0" withSources() withJavadoc(),
  "org.scalatest" %% "scalatest" % "1.9.1" % "test",
  "junit" % "junit" % "4.11" % "test"
)


//resolvers += Resolver.sonatypeRepo("public")

resolvers += "sonatype-public" at "https://oss.sonatype.org/content/groups/public"

//fork in run := true
