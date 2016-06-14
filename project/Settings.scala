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
    val scalaCSS = "0.4.1"

    // libraryDependencies (test)
    val scalaTest = "3.0.0-RC2"

    // jsDependencies
    val react = "15.0.2"
    val bootstrap = "3.3.6"
    val jQuery = "1.11.3"
  }

  val libraryDependencies = Def.setting(Seq(

    // using 'extra' instead of 'core' to get routing functionality
    "com.github.japgolly.scalajs-react" %%% "extra" % versions.scalaJsReact,
    "com.github.japgolly.scalacss" %%% "ext-react" % versions.scalaCSS,

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