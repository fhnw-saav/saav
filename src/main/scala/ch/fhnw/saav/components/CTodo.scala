package ch.fhnw.saav.components

import japgolly.scalajs.react.ReactComponentB
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

// A simple placeholder component for contents that are not yet implemented
object CTodo {

  val component = ReactComponentB[String]("Todo")
    .render_P(msg => <.div(^.className := "alert alert-info", s"TODO: $msg"))
    .build

  def apply(msg: String) = component(msg)

}
