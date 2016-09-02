import sbtrelease.ReleasePlugin.autoImport.ReleaseTransformations._

lazy val saav = (project in file("."))
  .enablePlugins(ScalaJSPlugin)
  .enablePlugins(PreprocessPlugin)
  .settings(
    name := Settings.name,
    scalaVersion := Settings.versions.scala,
    scalacOptions ++= Settings.scalacOptions,
    ghpages.settings,
    commands += deployCommand
  )

// dependencies needed by Scala.js
libraryDependencies ++= Settings.libraryDependencies.value

// JS dependencies needed at runtime
jsDependencies ++= Settings.jsDependencies.value

// yes, we want to package JS dependencies
skip in packageJSDependencies := false

// use launcher code to start the client app (see launcher.js in index.html)
persistLauncher := true
persistLauncher in Test := false

// make the referenced paths on source maps relative to target path
relativeSourceMaps := true

// custom release process
// https://github.com/sbt/sbt-release#can-we-finally-customize-that-release-process-please
releaseProcess := Seq[ReleaseStep](
  inquireVersions,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  setNextVersion,
  commitNextVersion
)

git.remoteRepo := "https://github.com/fhnw-saav/saav.git"

// replace paths in index.html
sources in EditSource += file("index.html")
flatten in EditSource := true
substitutions in EditSource ++= Seq(
  sub("""\.\/target\/scala-2.11""".r, "js", SubAll),
  sub("""fastopt""".r, "opt", SubAll)
)

// assemble site for gh-pages branch
siteMappings ++=
  Seq(
    target.value / "index.html" -> "index.html",
    toJsFolder((fullOptJS in Compile).value.data),
    toJsFolder(file((fullOptJS in Compile).value.data.getPath + ".map")),
    toJsFolder((packageScalaJSLauncher in Compile).value.data),
    toJsFolder((packageJSDependencies in Compile).value)
  ) ++
    directory(saav.base / "css") ++
    directory(saav.base / "js") ++
    directory(saav.base / "fonts")

def toJsFolder(f: File) = f -> s"js/${f.name}"

// Helper method to map complete dir contents (http://bit.ly/2aIlez9)
def directory(sourceDir: File): Seq[(File, String)] = {
  Option(sourceDir.getParentFile)
    .map(parent => sourceDir.*** pair relativeTo(parent))
    .getOrElse(sourceDir.*** pair basic)
}

lazy val cleanSite = taskKey[Unit]("Cleans contents of 'target/site'")
cleanSite := IO.delete(siteDirectory.value)

makeSite <<= makeSite.dependsOn(cleanSite, fullOptJS in Compile, edit in EditSource)

lazy val deployCommand = Command.command("deploy") {
  state =>
    "clean" ::
    "release with-defaults" ::
    "git checkout HEAD~1" ::
    "reload" ::
    "makeSite" ::
    "ghpagesPushSite" ::
    "git checkout master" ::
    "git push" ::
    "git push --tags" ::
    state
}