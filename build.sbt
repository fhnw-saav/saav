lazy val root = (project in file(".")).
  settings(
    name := "SAAV",
    scalaVersion := Settings.versions.scala,
    version := Settings.versions.saav,
    libraryDependencies ++= Settings.libraryDependencies.value
  )
  .enablePlugins(ScalaJSPlugin)

persistLauncher in Compile := true

persistLauncher in Test := false

mainClass in Compile := Some("ch.fhnw.saav.MainApp")