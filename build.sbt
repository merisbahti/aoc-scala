lazy val root = (project in file(".")).settings(
  inThisBuild(
    List(
      organization := "com.example",
      scalaVersion := "2.13.12"
    )
  ),
  name := "scala-aoc"
)

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0"
