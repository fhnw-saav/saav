package ch.fhnw.ima.saav
package component

import ch.fhnw.ima.saav.controller.SaavController.{AutoColorizeAction, UpdateEntityColorAction, UpdateEntityPinningAction, UpdateEntitySelectionAction}
import ch.fhnw.ima.saav.model.app.DataModel
import ch.fhnw.ima.saav.model.color._
import ch.fhnw.ima.saav.model.domain.Entity
import diode.react.ModelProxy
import japgolly.scalajs.react.extra.components.TriStateCheckbox
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{ReactComponentB, _}
import org.scalajs.dom.raw.{HTMLInputElement, HTMLTableCellElement}

import scala.language.postfixOps
import scalacss.ScalaCssReact._

object LegendComponent {

  // TODO: Maybe this should be a first class citizen?
  case class DisplayableEntity(entity: Entity, isSelected: Boolean, color: WebColor, isPinned: Boolean)

  case class Props(proxy: ModelProxy[DataModel], entities: Seq[DisplayableEntity], allSelectionState: TriStateCheckbox.State)

  class Backend($: BackendScope[Props, Unit]) {

    def autoColorize(entities: Seq[DisplayableEntity]) = {
      $.props >>= (_.proxy.dispatch(AutoColorizeAction(entities.map(_.entity))))
    }

    def updateEntityColor(entity: DisplayableEntity)(e: SyntheticEvent[HTMLInputElement]) = {
      val newColor = WebColor(e.target.value)
      $.props >>= (_.proxy.dispatch(UpdateEntityColorAction(entity.entity, newColor)))
    }

    def toggleEntitySelection(entity: DisplayableEntity) = {
      $.props >>= (_.proxy.dispatch(UpdateEntitySelectionAction(Seq(entity.entity), !entity.isSelected)))
    }

    def updateAllEntitySelections() = {
      $.props >>= { p =>
        val newSelected = p.allSelectionState match {
          case TriStateCheckbox.Checked => false
          case _ => true
        }
        p.proxy.dispatch(UpdateEntitySelectionAction(p.entities.map(_.entity), isSelected = newSelected))
      }
    }

    def toggleEntityPinning(entity: DisplayableEntity)(e: ReactEvent) = {
      // only control pinning if the click happens in a blank table row area (i.e NOT on the checkbox, color widget)
      if (e.target.isInstanceOf[HTMLTableCellElement]) {
        $.props >>= { p =>
          val pinnedEntity = if (entity.isPinned) None else Some(entity.entity)
          p.proxy.dispatch(UpdateEntityPinningAction(pinnedEntity))
        }
      } else {
        Callback.empty
      }
    }

    val pinGlyph = <.i(css.glyph.pin, ^.title := "Pin")

    def header(entities: Seq[DisplayableEntity], allSelectionState: TriStateCheckbox.State) = {

      val autoColorizeGlyph = <.i(
        css.glyph.magic,
        ^.cursor.pointer,
        ^.onClick --> autoColorize(entities.filter(_.isSelected)),
        ^.title := "Auto-Colorize")

      <.tr(
        <.th("#"),
        <.th("Name"),
        <.th(allCheckbox(allSelectionState)),
        <.th(^.textAlign.center, autoColorizeGlyph),
        <.th(^.textAlign.center, pinGlyph)
      )
    }

    def createRow(entity: DisplayableEntity, index: Int) = {

      val selectionStyle = if (entity.isSelected) css.empty else css.textMuted
      val pinStyle = if (entity.isPinned) css.active else css.empty
      val cursor = if (entity.isSelected) ^.cursor.pointer else EmptyTag
      val togglePinOnClick = if (entity.isSelected) ^.onClick ==> toggleEntityPinning(entity) else EmptyTag

      <.tr(selectionStyle, pinStyle, cursor, togglePinOnClick,
        <.th(^.scope := "row", index + 1),
        <.td(entity.entity.name),
        <.td(checkbox(entity)),
        <.td(^.textAlign.center, colorPicker(entity)),
        <.td(^.textAlign.center, if (entity.isPinned) pinGlyph else EmptyTag)
      )
    }

    def allCheckbox(allSelection: TriStateCheckbox.State) = {
      TriStateCheckbox.Component(TriStateCheckbox.Props(allSelection, updateAllEntitySelections()))
    }

    def checkbox(entity: DisplayableEntity) = {
      <.input.checkbox(^.checked := entity.isSelected, ^.onChange --> toggleEntitySelection(entity))
    }

    def colorPicker(entity: DisplayableEntity) = {
      <.input.color(
        ^.value := (if (entity.isSelected) entity.color else DisabledColor).hexValue,
        ^.disabled := !entity.isSelected,
        ^.onChange ==> updateEntityColor(entity))
    }

    def render(p: Props) = {

      val entities = p.entities
      val rows = entities.zipWithIndex.map {
        case (e, i) => createRow(e, i)
      }

      <.table(css.legendTable,
        <.thead(header(entities, p.allSelectionState)),
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
    val pinned = m.pinnedEntity
    val colors = m.colors
    val displayableEntities = entities.map(e => DisplayableEntity(e, selected.contains(e), colors(e), pinned.contains(e)))

    val allSelectionState = displayableEntities.map(_.isSelected).distinct match {
      case Seq(true) => TriStateCheckbox.Checked
      case Seq(false) => TriStateCheckbox.Unchecked
      case _ => TriStateCheckbox.Indeterminate
    }

    val props = Props(proxy, displayableEntities, allSelectionState)
    component(props)
  }

}
