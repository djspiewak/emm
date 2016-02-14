name := "emm-cats"

libraryDependencies ++= Seq(
  "com.codecommit" %% "shims-cats"  % shimsVersion.value,

  "org.scalaz"     %% "scalaz-core" % "7.1.6" % "test")
