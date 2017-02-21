name := "KNNRecallTest"

version := "1.0"

scalaVersion := "2.12.1"

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.0.0",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)

lazy val KNNRecallTest = project in file(".")
lazy val recall = project in file("recall")
lazy val reducer = project in file("reducer")
