import sbt._

object EmmBuild extends Build {
  val shimsVersion = settingKey[String]("Shims version shared between projects")
}