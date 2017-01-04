package ch.fhnw.ima.saav
package view

import ch.fhnw.ima.saav.circuit.UpdateWeightsAction
import ch.fhnw.ima.saav.model.app.ExpertConfig
import ch.fhnw.ima.saav.model.weight.Weights
import diode.Action
import diode.react.ModelProxy
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import scalacss.ScalaCssReact._

object ExpertConfigResetComponent {

  case class Props(isExpertConfigModified: Boolean, defaultWeights: Weights, dispatchCB: Action => Callback)

  private val component = ReactComponentB[Props](ExpertConfigResetComponent.getClass.getSimpleName)
    .render_P { p =>
      def reset = p.dispatchCB(UpdateWeightsAction(p.defaultWeights))
      if (p.isExpertConfigModified)
        <.div(css.expertConfigReset, "Configuration Modified (", <.a(^.cursor.pointer, ^.onClick --> reset, "Reset to Defaults"), ")")
      else
        <.div()
    }
    .build

  def apply(modelProxy: ModelProxy[ExpertConfig]): ReactComponentU[Props, Unit, Unit, TopNode] =
    component(Props(modelProxy.value.isModified, modelProxy.value.defaultWeights, modelProxy.dispatchCB[Action]))

  def apply(isExpertConfigModified: Boolean, defaultWeights: Weights, dispatch: Action => Callback): ReactComponentU[Props, Unit, Unit, TopNode] =
    component(Props(isExpertConfigModified, defaultWeights, dispatch))

}
