name := """shapeless-playbook"""

version := "1.0"

scalaVersion := "2.12.0"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

val scalazVersion = "7.2.8"
val scalazCore = "org.scalaz" %% "scalaz-core" % scalazVersion
val scalazConcurrent = "org.scalaz" %% "scalaz-concurrent" % scalazVersion

//scalacOptions ++= Seq("-deprecation", "-unchecked", "-Xexperimental", "-Xlog-implicits")

initialCommands in (console) := """ammonite.Main().run()"""

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "com.chuusai" %% "shapeless" % "2.3.2",
  scalazCore, scalazConcurrent,
  "com.lihaoyi" % "ammonite" % "COMMIT-057440b" cross CrossVersion.full //0.8.0
)

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-free" % "0.8.1", //1
  "co.fs2" %% "fs2-cats" % "0.2.0",
  "com.github.mpilquist" %%  "simulacrum"  % "0.10.0",
  "io.monix"          %% "monix"            % "2.1.0",
  "io.monix"          %%  "monix-cats"      % "2.1.0",
  "com.github.thangiee" %% "freasy-monad" % "0.5.0" // requires cats version 0.8.1+
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

//http://harrylaou.com/scala/shapeless/resources/
//http://enear.github.io/2016/09/27/bits-of-shapeless-2/