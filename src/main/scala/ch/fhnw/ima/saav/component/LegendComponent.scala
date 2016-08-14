package ch.fhnw.ima.saav
package component

import ch.fhnw.ima.saav.controller.SaavController.{AutoColorizeAction, UpdateEntityColorAction, UpdateEntitySelectionAction}
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

  // TODO: Maybe this should be a first class citizen?
  case class DisplayableEntity(entity: Entity, selected: Boolean, color: WebColor)

  case class Props(proxy: ModelProxy[DataModel], entities: Seq[DisplayableEntity])

  class Backend($: BackendScope[Props, Unit]) {

    def dispatchAutoColorizeAction(entities: Seq[Entity]) = {
      $.props >>= (_.proxy.dispatch(AutoColorizeAction(entities)))
    }

    def dispatchUpdateEntityColorAction(entity: DisplayableEntity)(e: SyntheticEvent[HTMLInputElement]) = {
      val newColor = WebColor(e.target.value)
      $.props >>= (_.proxy.dispatch(UpdateEntityColorAction(entity.entity, newColor)))
    }

    def dispatchToggleEntitySelectionAction(entity: DisplayableEntity) = {
      $.props >>= (_.proxy.dispatch(UpdateEntitySelectionAction(entity.entity, !entity.selected)))
    }

    def header(entities: Seq[Entity]) = {
      val autoColorize = <.i(css.glyph.magic, ^.onClick --> dispatchAutoColorizeAction(entities), ^.title := "Auto-Colorize")
      <.tr(
        <.th("#"),
        <.th("Name"),
        <.th("Selected"),
        <.th(^.textAlign.center, autoColorize))
    }

    def createRow(entity: DisplayableEntity, index: Int) =
      <.tr(
        <.th(^.scope := "row", index + 1),
        <.td(entity.entity.name),
        <.td(checkbox(entity)),
        <.td(^.textAlign.center, colorPicker(entity))
      )

    def checkbox(entity: DisplayableEntity) = {
      <.input.checkbox(^.checked := entity.selected, ^.onChange --> dispatchToggleEntitySelectionAction(entity))
    }

    def colorPicker(entity: DisplayableEntity) = {
      <.input.color(^.value := entity.color.hexValue, ^.onChange ==> dispatchUpdateEntityColorAction(entity))
    }

    def render(p: Props) = {

      val entities = p.entities
      val rows = entities.zipWithIndex.map {
        case (e, i) => createRow(e, i)
      }

      <.table(css.legendTable,
        <.thead(header(entities.map(_.entity))),
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

  def apply(proxy: ModelProxy[DataModel]) = {
    val m = proxy.value
    val entities = m.analysis.entities
    val selected = m.selectedEntities
    val colors = m.colors
    val displayableEntities = entities.map(e => DisplayableEntity(e, selected.contains(e), colors(e)))
    val props = Props(proxy, displayableEntities)
    component(props)
  }

}
