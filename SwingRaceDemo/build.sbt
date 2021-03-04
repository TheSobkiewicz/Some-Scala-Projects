import sbt.Keys.libraryDependencies

name := "SwingRaceDemo"
libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
version := "0.1"
lazy val akkaVersion = "2.6.10"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion,
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.typesafe.akka" %% "akka-actor-testkit-typed" % akkaVersion % Test,
  "org.scalatest" %% "scalatest" % "3.1.0" % Test
)


scalaVersion := "2.13.4"
