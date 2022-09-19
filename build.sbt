name := "boboddy"

version := "0.1"

scalaVersion := "2.13.6"

val AkkaVersion = "2.6.8"
val AkkaHttpVersion = "10.2.7"
val ScalaTestVersion = "3.2.13"


lazy val commonSettings = Seq(
  libraryDependencies ++= Seq(
    "com.typesafe.akka" %% "akka-actor" % AkkaVersion,
    "com.typesafe.akka" %% "akka-stream" % AkkaVersion,
    "com.typesafe.akka" %% "akka-http" % AkkaHttpVersion,
    "org.scalactic" %% "scalactic" % ScalaTestVersion,
    "org.scalatest" %% "scalatest" % ScalaTestVersion % "test"
  )
)

lazy val root = (project in file("."))
  .aggregate(misc, asciiClock, ctciSolutions, leetcode, scalaFunctionalExercises, wordlebot)
  .settings(
    name := "boboddy"
  )

lazy val misc = (project in file("misc"))
  .settings(commonSettings)
lazy val asciiClock = project in file("ascii-clock-scala")
lazy val ctciSolutions = project in file("ctci-solutions-scala")
lazy val leetcode = project in file("leetcode")
lazy val scalaFunctionalExercises = project in file("scala-functional-exercises")
lazy val wordlebot = project in file("wordlebot")