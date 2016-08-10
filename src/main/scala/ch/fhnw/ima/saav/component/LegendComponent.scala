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

  // TODO: Introduce Backend to avoid passing around proxy

  case class Props(proxy: ModelProxy[DataModel])

  def header(entities: Seq[Entity], proxy: ModelProxy[DataModel]) = {
    def dispatch = {
      proxy.dispatch(AutoColorizeAction(entities))
    }
    val autoColorize = <.i(css.glyph.magic, ^.onClick --> dispatch, ^.title := "Auto-Colorize")
    <.tr(<.th("#"), <.th("Name"), <.th(^.textAlign.center, autoColorize))
  }

  def createRow(entity: Entity, index: Int, color: WebColor, proxy: ModelProxy[DataModel]) =
    <.tr(
      <.th(^.scope := "row", index + 1),
      <.td(entity.name),
      <.td(^.textAlign.center, colorPicker(entity, color, proxy))
    )

  def colorPicker(entity: Entity, color: WebColor, proxy: ModelProxy[DataModel]) = {
    def dispatch(e: SyntheticEvent[HTMLInputElement]) = {
      val newColor = WebColor(e.target.value)
      proxy.dispatch(UpdateEntityColorAction(entity, newColor))
    }
    <.input.color(^.value := color.hexValue, ^.onChange ==> dispatch)
  }

  private final val component = ReactComponentB[Props](LegendComponent.getClass.getSimpleName)
    .render_P(p => {

      val entities = p.proxy.value.analysis.entities
      val colors = p.proxy.value.colors
      val rows = entities.zipWithIndex.map {
        case (e, i) => createRow(e, i, colors(e), p.proxy)
      }

      <.table(css.legendTable,
        <.thead(header(entities, p.proxy)),
        <.tbody(rows))
    })
    .componentDidMount(scope => {
      val entities = scope.props.proxy.value.analysis.entities
      scope.props.proxy.dispatch(AutoColorizeAction(entities))
    })
    .build

  def apply(proxy: ModelProxy[DataModel]) = component(Props(proxy))

}
