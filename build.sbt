enablePlugins(GitVersioning)

val ReleaseTag = """^v([\d\.]+)$""".r

organization := "com.codecommit"

name := "emm"

scalaVersion := "2.11.7"

crossScalaVersions := Seq(scalaVersion.value, "2.10.6")

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.1.6",

  "org.specs2" %% "specs2-core" % "3.6.6" % "test")

scalacOptions += "-language:_"      // I really can't be bothered with SIP-18

scalacOptions in Test += "-Yrangepos"

git.baseVersion := "0.1"

git.gitTagToVersionNumber := {
  case ReleaseTag(version) => Some(version)
  case _ => None
}

git.formattedShaVersion := {
  val suffix = git.makeUncommittedSignifierSuffix(git.gitUncommittedChanges.value, git.uncommittedSignifier.value)

  git.gitHeadCommit.value map { _.substring(0, 7) } map { sha =>
    git.baseVersion.value + "-" + sha + suffix
  }
}