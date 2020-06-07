
lazy val main = project.in(file("."))
  .enablePlugins(ScalaJSPlugin)

name := "pixi-scalajs-gui"

version := "0.1.0-SNAPSHOT"

organization := "be.adoeraene"

scalaVersion := "2.13.1"

libraryDependencies +=
  "org.scala-js" %%% "scalajs-dom" % "1.0.0"


scalacOptions ++= Seq("-deprecation")


