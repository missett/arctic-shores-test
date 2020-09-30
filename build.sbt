organization := "io.github.missett"
name := "arctic-shores-test"
scalaVersion := "2.12.11"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.8" % Test,
  "org.scalacheck" %% "scalacheck" % "1.14.1" % Test,
)
  .map(_ exclude("javax.ws.rs", "javax.ws.rs-api"))
  .map(_ exclude("org.slf4j", "slf4j-log4j12"))

scalacOptions ++= ScalaCOptions.opts

