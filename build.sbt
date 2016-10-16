name := """shapeless-playbook"""

version := "1.0"

scalaVersion := "2.11.8"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

val scalazVersion = "7.2.2"
val scalazCore = "org.scalaz" %% "scalaz-core" % scalazVersion
val scalazConcurrent = "org.scalaz" %% "scalaz-concurrent" % scalazVersion

scalacOptions ++= Seq("-deprecation", "-unchecked", "-Xexperimental", "-Xlog-implicits")

initialCommands in (console) := """ammonite.Main().run()"""

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "com.chuusai" %% "shapeless" % "2.3.2",
  scalazCore, scalazConcurrent,
  "com.lihaoyi"  % "ammonite" % "0.7.8" cross CrossVersion.full
)

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-free" % "0.7.2",
  "co.fs2" %% "fs2-cats" % "0.1.0",
  "com.github.mpilquist" %%   "simulacrum"  % "0.8.0",
  "io.monix"          %% "monix"            % "2.0.3",
  "io.monix"          %%  "monix-cats"      % "2.0.3",
  "com.thangiee" %% "freasy-monad" % "0.1.0" // requires cats version 0.7.0+
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.8.0")
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

//http://harrylaou.com/scala/shapeless/resources/
//http://enear.github.io/2016/09/27/bits-of-shapeless-2/