package ch.fhnw.ima.saav
package component

import ch.fhnw.ima.saav.controller.SaavController.{AutoColorizeAction, UpdateEntityColorAction, UpdateEntitySelectionAction}
import ch.fhnw.ima.saav.model.DataModel
import ch.fhnw.ima.saav.model.colors._
import ch.fhnw.ima.saav.model.domain.Entity
import diode.react.ModelProxy
import japgolly.scalajs.react.extra.components.TriStateCheckbox
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{ReactComponentB, _}
import org.scalajs.dom.raw.HTMLInputElement

import scala.language.postfixOps
import scalacss.ScalaCssReact._

object LegendComponent {

  // TODO: Maybe this should be a first class citizen?
  case class DisplayableEntity(entity: Entity, selected: Boolean, color: WebColor)

  case class Props(proxy: ModelProxy[DataModel], entities: Seq[DisplayableEntity], allSelectionState: TriStateCheckbox.State)

  class Backend($: BackendScope[Props, Unit]) {

    def autoColorize(entities: Seq[Entity]) = {
      $.props >>= (_.proxy.dispatch(AutoColorizeAction(entities)))
    }

    def updateEntityColor(entity: DisplayableEntity)(e: SyntheticEvent[HTMLInputElement]) = {
      val newColor = WebColor(e.target.value)
      $.props >>= (_.proxy.dispatch(UpdateEntityColorAction(entity.entity, newColor)))
    }

    def toggleEntitySelection(entity: DisplayableEntity) = {
      $.props >>= (_.proxy.dispatch(UpdateEntitySelectionAction(Seq(entity.entity), !entity.selected)))
    }

    def updateAllEntitySelections() = {
      $.props >>= { p =>
        val newSelected = p.allSelectionState match {
          case TriStateCheckbox.Checked => false
          case _ => true
        }
        p.proxy.dispatch(UpdateEntitySelectionAction(p.entities.map(_.entity), selected = newSelected))
      }
    }

    def header(entities: Seq[Entity], allSelectionState: TriStateCheckbox.State) = {
      val autoColorizeGlyph = <.i(css.glyph.magic, ^.onClick --> autoColorize(entities), ^.title := "Auto-Colorize")
      <.tr(
        <.th("#"),
        <.th("Name"),
        <.th(allCheckbox(allSelectionState)),
        <.th(^.textAlign.center, autoColorizeGlyph))
    }

    def createRow(entity: DisplayableEntity, index: Int) = {
      val style = if (entity.selected) css.empty else css.textMuted
      <.tr(style,
        <.th(^.scope := "row", index + 1),
        <.td(entity.entity.name),
        <.td(checkbox(entity)),
        <.td(^.textAlign.center, colorPicker(entity))
      )
    }

    def allCheckbox(allSelection: TriStateCheckbox.State) = {
      TriStateCheckbox.Component(TriStateCheckbox.Props(allSelection, updateAllEntitySelections()))
    }

    def checkbox(entity: DisplayableEntity) = {
      <.input.checkbox(^.checked := entity.selected, ^.onChange --> toggleEntitySelection(entity))
    }

    def colorPicker(entity: DisplayableEntity) = {
      <.input.color(
        ^.value := (if (entity.selected) entity.color else DisabledColor).hexValue,
        ^.disabled := !entity.selected,
        ^.onChange ==> updateEntityColor(entity))
    }

    def render(p: Props) = {

      val entities = p.entities
      val rows = entities.zipWithIndex.map {
        case (e, i) => createRow(e, i)
      }

      <.table(css.legendTable,
        <.thead(header(entities.map(_.entity), p.allSelectionState)),
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

    val allSelectionState = displayableEntities.map(_.selected).distinct match {
      case Seq(true) => TriStateCheckbox.Checked
      case Seq(false) => TriStateCheckbox.Unchecked
      case _ => TriStateCheckbox.Indeterminate
    }

    val props = Props(proxy, displayableEntities, allSelectionState)
    component(props)
  }

}
