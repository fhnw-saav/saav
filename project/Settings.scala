import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import sbt._

object Settings {

  val name = "SAAV"

  val scalacOptions = Seq(
    "-Xlint",
    "-unchecked",
    "-deprecation",
    "-feature"
  )

  object versions {
    val saav = "0.1.0-SNAPSHOT"
    val scala = "2.11.8"

    // libraryDependencies
    val scalaJsReact = "0.11.1"

    // jsDependencies
    val react = "15.0.2"
    val bootstrap = "3.3.6"
    val jQuery = "1.11.3"
  }

  val libraryDependencies = Def.setting(Seq(
    // using 'extra' instead of 'core' to get routing functionality
    "com.github.japgolly.scalajs-react" %%% "extra" % versions.scalaJsReact
  ))

  val jsDependencies = Def.setting(Seq(
    "org.webjars.bower" % "react" % versions.react / "react-with-addons.js" minified "react-with-addons.min.js" commonJSName "React",
    "org.webjars.bower" % "react" % versions.react / "react-dom.js" minified "react-dom.min.js" dependsOn "react-with-addons.js" commonJSName "ReactDOM",
    "org.webjars" % "jquery" % versions.jQuery / "jquery.js" minified "jquery.min.js",
    "org.webjars" % "bootstrap" % versions.bootstrap / "bootstrap.js" minified "bootstrap.min.js" dependsOn "jquery.js"
  ))

}