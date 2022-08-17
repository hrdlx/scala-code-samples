ThisBuild / organization := "com.example"
ThisBuild / scalaVersion := "2.13.5"

lazy val root = (project in file(".")).settings(
  name := "scala-code-samples",
  libraryDependencies ++= Seq(
    // "core" module - IO, IOApp, schedulers
    // This pulls in the kernel and std modules automatically.
    "org.typelevel" %% "cats-effect" % "3.3.12",
    // concurrency abstractions and primitives (Concurrent, Sync, Async etc.)
    "org.typelevel" %% "cats-effect-kernel" % "3.3.12",
    // standard "effect" library (Queues, Console, Random etc.)
    "org.typelevel" %% "cats-effect-std" % "3.3.12",
    // better monadic for compiler plugin as suggested by documentation
    "io.estatico" %% "newtype" % "0.4.4",
    "eu.timepit" %% "refined" % "0.9.25",
    "eu.timepit" %% "refined-cats" % "0.9.25",
 
    compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
  ),
  scalacOptions ++= Seq(
    "-Ymacro-annotations",
    "-Wconf:cat=unused:info"
  )
)
