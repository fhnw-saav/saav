package ch.fhnw.ima.saav
package component

import ch.fhnw.ima.saav.controller.SaavController.{AutoColorizeAction, UpdateEntityColorAction}
import ch.fhnw.ima.saav.model.DataModel
import ch.fhnw.ima.saav.model.colors.WebColor
import ch.fhnw.ima.saav.model.domain.Entity
import diode.react.ModelProxy
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{ReactComponentB, _}
import org.scalajs.dom.raw.HTMLInputElement

import scala.language.postfixOps
import scalacss.ScalaCssReact._

object LegendComponent {

  case class Props(proxy: ModelProxy[DataModel])

  class Backend($: BackendScope[Props, Unit]) {

    def dispatchAutoColorizeAction(entities: Seq[Entity]) = {
      $.props >>= (_.proxy.dispatch(AutoColorizeAction(entities)))
    }

    def dispatchUpdateEntityColorAction(entity: Entity, color: WebColor)(e: SyntheticEvent[HTMLInputElement]) = {
      val newColor = WebColor(e.target.value)
      $.props >>= (_.proxy.dispatch(UpdateEntityColorAction(entity, newColor)))
    }

    def header(entities: Seq[Entity]) = {
      val autoColorize = <.i(css.glyph.magic, ^.onClick --> dispatchAutoColorizeAction(entities), ^.title := "Auto-Colorize")
      <.tr(<.th("#"), <.th("Name"), <.th(^.textAlign.center, autoColorize))
    }

    def createRow(entity: Entity, index: Int, color: WebColor) =
      <.tr(
        <.th(^.scope := "row", index + 1),
        <.td(entity.name),
        <.td(^.textAlign.center, colorPicker(entity, color))
      )

    def colorPicker(entity: Entity, color: WebColor) = {
      <.input.color(^.value := color.hexValue, ^.onChange ==> dispatchUpdateEntityColorAction(entity, color))
    }

    def render(p: Props) = {

      val entities = p.proxy.value.analysis.entities
      val colors = p.proxy.value.colors
      val rows = entities.zipWithIndex.map {
        case (e, i) => createRow(e, i, colors(e))
      }

      <.table(css.legendTable,
        <.thead(header(entities)),
        <.tbody(rows))
    }

  }

  private final val component = ReactComponentB[Props](LegendComponent.getClass.getSimpleName)
    .renderBackend[Backend]
    .componentDidMount(scope => {
      val entities = scope.props.proxy.value.analysis.entities
      scope.props.proxy.dispatch(AutoColorizeAction(entities))
    })
    .build

  def apply(proxy: ModelProxy[DataModel]) = component(Props(proxy))

}
