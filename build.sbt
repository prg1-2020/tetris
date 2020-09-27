lazy val common_project = Seq(
  organization := "prg20",
  version := "0.1-SNAPSHOT",

  fork in run          := true,
  connectInput in run  := true,
  cancelable in Global := true)

lazy val scala_project = common_project ++ Seq(
  scalaVersion := "2.13.3",   // コンパイルに使う scalac のバージョン
  scalacOptions := Seq("-feature", "-unchecked", "-deprecation"),

  // Scalaのプロジェクトのファイル構成を設定。
  // https://www.scala-sbt.org/1.x/docs/Multi-Project.html
  scalaSource in Compile := baseDirectory.value / "scala"
)

lazy val java_project = scala_project ++ Seq(
  javacOptions ++= Seq("-source", "1.8", "-target", "1.8", "-encoding", "UTF-8"),
  javaSource in Compile := baseDirectory.value / "java"
)

lazy val java3d_project = java_project ++ Seq(
  // java3d-core https://maven-repository.com/artifact/com.massisframework.j3d/java3d-core/1.6.0.1
  // vecmath https://maven-repository.com/artifact/com.massisframework.j3d/vecmath
)

// サブプロジェクト群の定義。
lazy val root = (project in file(".")).settings(common_project)
lazy val support = (project in file ("src/support")).settings(java_project)
lazy val tetris = (project in file ("src/tetris")).settings(scala_project).dependsOn(support)
