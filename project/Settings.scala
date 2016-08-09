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
    val scala = "2.11.8"

    // libraryDependencies
    val scalaJsReact = "0.11.1"
    val scalaCSS = "0.4.1"
    val scalaJsD3 = "0.3.3"
    val diode = "1.0.0"

    // libraryDependencies (test)
    val scalaTest = "3.0.0"

    // jsDependencies
    val react = "15.2.1"
    val bootstrap = "3.3.7"
    val jQuery = "1.12.4"
  }

  val libraryDependencies = Def.setting(Seq(

    // using 'extra' instead of 'core' to get routing functionality
    "com.github.japgolly.scalajs-react" %%% "extra" % versions.scalaJsReact,
    "com.github.japgolly.scalacss" %%% "ext-react" % versions.scalaCSS,
    "org.singlespaced" %%% "scalajs-d3" % versions.scalaJsD3,
    "me.chrons" %%% "diode" % versions.diode,
    "me.chrons" %%% "diode-react" % versions.diode,

    // test
    "org.scalatest" %%% "scalatest" % versions.scalaTest % "test",
    // JVM rather than JS dependency to use ScalaTest in an IDE (https://github.com/scalatest/scalatest/issues/911)
    "org.scalatest" %% "scalatest" % versions.scalaTest % "test"
  ))

  val jsDependencies = Def.setting(Seq(
    "org.webjars.bower" % "react" % versions.react / "react-with-addons.js" minified "react-with-addons.min.js" commonJSName "React",
    "org.webjars.bower" % "react" % versions.react / "react-dom.js" minified "react-dom.min.js" dependsOn "react-with-addons.js" commonJSName "ReactDOM",
    "org.webjars" % "jquery" % versions.jQuery / "jquery.js" minified "jquery.min.js",
    "org.webjars" % "bootstrap" % versions.bootstrap / "bootstrap.js" minified "bootstrap.min.js" dependsOn "jquery.js",
    RuntimeDOM % "test"
  ))

}