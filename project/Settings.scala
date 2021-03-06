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
    val scala = "2.12.1"

    // libraryDependencies
    val scalaJsReact = "0.11.3"
    val scalaCSS = "0.5.1"
    val diode = "1.1.0"
    val circe = "0.6.0"

    // libraryDependencies (test)
    val scalaTest = "3.0.0"

    // jsDependencies
    val react = "15.3.2"
    val bootstrap = "3.3.7"
    val jQuery = "1.12.4"
  }

  val libraryDependencies = Def.setting(Seq(

    "com.github.japgolly.scalajs-react" %%% "core" % versions.scalaJsReact,
    "com.github.japgolly.scalacss" %%% "ext-react" % versions.scalaCSS,
    "me.chrons" %%% "diode" % versions.diode,
    "me.chrons" %%% "diode-react" % versions.diode,
    "io.circe" %%% "circe-core" % versions.circe,
    "io.circe" %%% "circe-parser" % versions.circe,
    "io.circe" %%% "circe-generic" % versions.circe,
    "io.circe" %%% "circe-scalajs" % versions.circe,

    // test
    "org.scalatest" %%% "scalatest" % versions.scalaTest % "test"
  ))

  val jsDependencies = Def.setting(Seq(
    "org.webjars.bower" % "react" % versions.react / "react-with-addons.js" minified "react-with-addons.min.js" commonJSName "React",
    "org.webjars.bower" % "react" % versions.react / "react-dom.js" minified "react-dom.min.js" dependsOn "react-with-addons.js" commonJSName "ReactDOM",
    "org.webjars" % "jquery" % versions.jQuery / "jquery.js" minified "jquery.min.js",
    "org.webjars" % "bootstrap" % versions.bootstrap / "bootstrap.js" minified "bootstrap.min.js" dependsOn "jquery.js",
    RuntimeDOM % "test"
  ))

}