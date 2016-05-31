import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import sbt._

object Settings {

  object versions {
    val saav = "0.1-SNAPSHOT"
    val scala = "2.11.8"
    val scalaDom = "0.9.0"
  }

  val libraryDependencies = Def.setting(Seq(
    "org.scala-js" %%% "scalajs-dom" % versions.scalaDom
  ))

}