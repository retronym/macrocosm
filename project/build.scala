import sbt._
import Keys._

object build extends Build {
	val sharedSettings = Defaults.defaultSettings ++ Seq(
		organization := "com.github.retronym",
    version := "0.2-SNAPSHOT",
    // scalaHome := Some(file(System.getProperty("user.home")) / "usr" / "scala-kepler"),
    scalaVersion := "2.10.0-SNAPSHOT",
    scalacOptions ++= Seq(), // Seq("-Xlog-free-terms", "-unchecked", /*, "-Ymacro-debug"*/),
		resolvers += "sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _)
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
