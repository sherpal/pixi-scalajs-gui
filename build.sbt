
lazy val main = project.in(file("."))
  .enablePlugins(ScalaJSPlugin)
  .enablePlugins(JSDependenciesPlugin)

name := "pixi-scalajs-gui"

version := "0.1.0-SNAPSHOT"

organization := "be.adoeraene"

scalaVersion := "2.12.1"

libraryDependencies +=
  "org.scala-js" %%% "scalajs-dom" % "0.9.3"

scalacOptions ++= Seq("-deprecation")


