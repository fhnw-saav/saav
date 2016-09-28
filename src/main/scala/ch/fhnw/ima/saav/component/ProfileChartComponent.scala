package ch.fhnw.ima.saav.component

import ch.fhnw.ima.saav.model.app.AppModel
import diode.react.ModelProxy
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{BackendScope, ReactComponentB, Ref}
import org.scalajs.dom.raw.SVGSVGElement

object ProfileChartComponent {

  case class Props(proxy: ModelProxy[AppModel])

  case class State()

  private val svgRef = Ref[SVGSVGElement]("svgRef")

  class Backend($: BackendScope[Props, State]) {

    def render(p: Props, s: State) = {
      val model = p.proxy.value.profileModel
      <.div(
        TodoComponent("Profile Chart"),
        <.ul(
          for (criteria <- model.criteria) yield {
            <.li(criteria.name,
              <.ul(
                for (subCriteria <- criteria.subCriteria) yield {
                  <.li(subCriteria.displayName)
                }
              )
            )
          }
        )
      )
    }

  }

  private val component = ReactComponentB[Props](ProfileChartComponent.getClass.getSimpleName)
    .initialState(State())
    .renderBackend[Backend]
    .build

  def apply(proxy: ModelProxy[AppModel]) = component(Props(proxy))

}