import sbtrelease.ReleasePlugin.autoImport.ReleaseTransformations._

lazy val root = (project in file("."))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := Settings.name,
    scalaVersion := Settings.versions.scala,
    scalacOptions ++= Settings.scalacOptions,
    ghpages.settings
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

// assemble site for gh-pages branch
git.remoteRepo := "https://github.com/fhnw-saav/saav.git"

// replace paths in index.html
sources in EditSource += file("index.html")
flatten in EditSource := true
substitutions in EditSource ++= Seq(
  sub("""\.\/target\/scala-2.11""".r, "js", SubAll),
  sub("""fastopt""".r, "opt", SubAll)
)

siteMappings ++=
  Seq(file("target/index.html") -> "index.html") ++
    ((target.value / "scala-2.11" ** ("saav-opt.js" || "saav-fullopt.js.map" || "saav-jsdeps.js" || "saav-launcher.js")).get map {
      (f: File) => (f, "js/" + f.getName)
    }) ++
    directory(root.base / "css") ++
    directory(root.base / "js")

// http://bit.ly/28Jcd5K
def directory(sourceDir: File): Seq[(File, String)] = {
  Option(sourceDir.getParentFile)
    .map(parent => sourceDir.*** pair relativeTo(parent))
    .getOrElse(sourceDir.*** pair basic)
}

makeSite <<= makeSite.dependsOn(Keys.clean in Compile, fullOptJS in Compile, edit in EditSource)