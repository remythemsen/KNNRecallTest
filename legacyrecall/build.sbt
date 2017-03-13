name := "legacyrecall"

version := "1.0"

scalaVersion := "2.11.8"

organization := "dk.aknniss"

homepage := Some(url("https://github.com/remythemsen/KNNRecallTest"))

startYear := Some(2017)

libraryDependencies ++= Seq(
  "com.github.scopt" %% "scopt" % "3.5.0",
  "org.scalanlp" %% "breeze" % "0.12",
  "org.scalanlp" %% "breeze-natives" % "0.12",
  "org.scalanlp" %% "breeze-viz" % "0.12",
  "org.scalactic" %% "scalactic" % "3.0.0",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)

resolvers += "typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

