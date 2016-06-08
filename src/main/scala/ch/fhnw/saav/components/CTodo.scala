package ch.fhnw.saav.components

import ch.fhnw.saav.styles.GlobalStyles
import japgolly.scalajs.react.ReactComponentB
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import scalacss.ScalaCssReact._

// A simple placeholder component for contents that are not yet implemented
object CTodo {

  val css = GlobalStyles

  val component = ReactComponentB[String]("Todo")
    .render_P(msg => <.div(css.infoBox, s"TODO: $msg"))
    .build

  def apply(msg: String) = component(msg)

}
