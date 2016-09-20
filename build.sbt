lazy val root = (project in file(".")).
  settings(
    name := "FunctorsAndFriends",
    scalaVersion := "2.11.8",

    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats" % catsVersion,// exclude("org.scalacheck", "scalacheck_2.11" /*1.12.5*/),
      "org.scalaz" %% "scalaz-core" % scalazVersion,
      "org.spire-math" %% "spire" % spireMathVerion,
      "org.scalamacros" %% "resetallattrs" % resetAllAttrsVersion,
      compilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full),
      compilerPlugin("org.spire-math" %% "kind-projector" % kindProjectorVersion),
      "org.scalacheck" %% "scalacheck" % scalacheckVersion % Test,
      "org.scalatest" %% "scalatest" % scalatestVersion % Test,
      "org.typelevel" %% "discipline" % disciplineVersion % Test
    ),

    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-language:_"
    )
  )

lazy val catsVersion = "0.7.2"
lazy val scalazVersion = "7.2.2"
lazy val spireMathVerion = "0.12.0"
lazy val scalacheckVersion = "1.13.2"
lazy val resetAllAttrsVersion = "1.0.0-M1"
lazy val specs2Version = "3.6.4" //discipline
lazy val scalatestVersion = "3.0.0"
lazy val kindProjectorVersion = "0.6.3"
lazy val paradiseVersion = "2.1.0"
lazy val disciplineVersion = "0.7" //cats-kernel-laws introduces old version of discipline and scalacheck. watch out!
