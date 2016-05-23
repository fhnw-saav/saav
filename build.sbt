lazy val root = (project in file(".")).
  enablePlugins(ScalaJSPlugin).
  settings(
    name := "SAAV",
    scalaVersion := "2.11.8",
    version := "0.1-SNAPSHOT"
  )

libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "0.9.0",
  "com.lihaoyi" %%% "scalatags" % "0.5.4"
)

persistLauncher in Compile := true

persistLauncher in Test := false

mainClass in Compile := Some("ch.fhnw.saav.MainApp")