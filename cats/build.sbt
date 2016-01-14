name := "emm-cats"

libraryDependencies ++= Seq(
  "com.codecommit" %% "shims-cats"  % shimsVersion.value,
  "org.spire-math" %% "cats"        % "0.3.0",

  "org.scalaz"     %% "scalaz-core" % "7.1.6" % "test")
