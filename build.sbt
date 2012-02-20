organization := "com.github.retronym"

name := "macrocosm"

version := "0.1-SNAPSHOT"

seq(npSettings: _*)

scalaVersion := "2.10.0-SNAPSHOT"

scalacOptions ++= Seq("-Xmacros", "-unchecked", "-Yvirtpatmat")

resolvers += "sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies <+= (scalaVersion)(sv => "org.scala-lang" % "scala-compiler" % sv)