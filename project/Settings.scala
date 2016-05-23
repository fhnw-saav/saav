import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import sbt._

object Settings {

  object versions {
    val saav = "0.1-SNAPSHOT"
    val scala = "2.11.8"
    val dom = "0.9.0"
    val tags = "0.5.4"
    val d3 = "0.3.3"
  }

  val libraryDependencies = Def.setting(Seq(
    "org.scala-js" %%% "scalajs-dom" % versions.dom,
    "com.lihaoyi" %% "scalatags" % versions.tags,
    "org.singlespaced" %%% "scalajs-d3" % versions.d3
  ))

}