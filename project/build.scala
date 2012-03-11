import sbt._
import Keys._

object build extends Build {
	val sharedSettings = Defaults.defaultSettings ++ Seq(
		organization := "com.github.retronym",
		version := "0.1-SNAPSHOT",
		scalaHome := Some(file(System.getProperty("user.home")) / "usr" / "scala-kepler"),
		scalacOptions ++= Seq("-Xmacros", "-unchecked", "-Yvirtpatmat", "-Xexperimental" /*, "-Ymacro-debug"*/),
		resolvers += "sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
		libraryDependencies <+= (scalaVersion)(sv => "org.scala-lang" % "scala-compiler" % sv)
	)

	lazy val root = Project(
		id = "macrocosm",
		base = file("."),
		settings = sharedSettings,
		dependencies = Seq(ground)
	)

	lazy val ground = Project(
		id = "macrocosm-ground",
		base = file("ground"),
		settings = sharedSettings
	)
}
