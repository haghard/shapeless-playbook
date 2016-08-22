name := """shapeless-playbook"""

version := "1.0"

scalaVersion := "2.11.8"

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
  "com.chuusai" %% "shapeless" % "2.3.2",
  scalazCore, scalazConcurrent,
  "com.lihaoyi"  % "ammonite" % "0.7.0" cross CrossVersion.full
)

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-free" % "0.6.1",
  "co.fs2" %% "fs2-cats" % "0.1.0-RC1"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.8.0")

//http://harrylaou.com/scala/shapeless/resources/