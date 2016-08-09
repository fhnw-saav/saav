package ch.fhnw.ima.saav
package component

import japgolly.scalajs.react.{ReactComponentB, _}
import japgolly.scalajs.react.vdom.prefix_<^._

import scalacss.ScalaCssReact._

/**
  * A simple placeholder component for contents that are not yet implemented.
  */
object TodoComponent {

  private val component = ReactComponentB[String](TodoComponent.getClass.getSimpleName)
    .render_P(msg => <.div(css.infoBox, s"TODO: $msg"))
    .build

  def apply(msg: String) = component(msg)

}
