name := "KNNRecallTest"

version := "1.0"

scalaVersion := "2.11.8"

resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "com.github.scopt" %% "scopt" % "3.5.0",
  "org.scalanlp" %% "breeze" % "0.12",
  "org.scalanlp" %% "breeze-natives" % "0.12",
  "org.scalanlp" %% "breeze-viz" % "0.12",
  "org.scalactic" %% "scalactic" % "3.0.0",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)

lazy val KNNRecallTest = project in file(".") dependsOn(tools)

lazy val tools = project in file("tools")

lazy val reducer = project in file("reducer") dependsOn(tools)

lazy val recall = project in file("recall") dependsOn(tools)

lazy val legacyrecall = project in file("legacyrecall") dependsOn(tools)

lazy val dataanalyzer = project in file("dataanalyzer") dependsOn(tools)