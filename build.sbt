ThisBuild / version := "1.0"

ThisBuild / scalaVersion := "3.1.3"

lazy val root = (project in file("."))
  .settings(
    name := "physical",
    scalacOptions ++= Seq(
      "-Ycheck-all-patmat",
      "-Ycheck-reentrant",
      "-Ycook-comments",
      "-Ydebug-pos",
      "-Yexplicit-nulls",
      "-Ysafe-init",
      "-explain",
      "-language:strictEquality",
    ),
  )
