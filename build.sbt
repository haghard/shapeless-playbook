name := """shapeless-playbook"""

version := "1.0"

scalaVersion := "2.11.7"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

val scalazVersion = "7.1.5"
val scalazCore = "org.scalaz" %% "scalaz-core" % scalazVersion
val scalazConcurrent = "org.scalaz" %% "scalaz-concurrent" % scalazVersion

initialCommands in (console) := """ammonite.Main().run()"""

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "com.chuusai" %% "shapeless" % "2.3.1",
  scalazCore, scalazConcurrent,
  "com.lihaoyi"  % "ammonite" % "0.7.0" cross CrossVersion.full
)

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-free" % "0.6.1",
  "co.fs2" %% "fs2-cats" % "0.1.0-SNAPSHOT"
)