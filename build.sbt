organization := "com.github.retronym"

name := "macrocosm"

version := "0.1-SNAPSHOT"

scalaHome := Some(file(System.getProperty("user.home")) / "usr" / "scala-kepler")

scalacOptions ++= Seq("-Xmacros", "-unchecked", "-Yvirtpatmat", "-Xexperimental")

resolvers += "sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies <+= (scalaVersion)(sv => "org.scala-lang" % "scala-compiler" % sv)