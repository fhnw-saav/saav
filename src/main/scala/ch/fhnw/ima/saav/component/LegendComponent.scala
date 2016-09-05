package ch.fhnw.ima.saav
package component

import ch.fhnw.ima.saav.controller.SaavController.{AutoColorizeAction, UpdateEntityColorAction, UpdateEntityPinningAction, UpdateEntitySelectionAction}
import ch.fhnw.ima.saav.model.app.{AppModel, GroupedEntity}
import ch.fhnw.ima.saav.model.color._
import ch.fhnw.ima.saav.model.domain.Entity
import diode.react.ModelProxy
import japgolly.scalajs.react.extra.components.TriStateCheckbox
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{ReactComponentB, _}
import org.scalajs.dom.raw.HTMLInputElement

import scala.language.postfixOps
import scalacss.ScalaCssReact._

object LegendComponent {

  case class Props(proxy: ModelProxy[AppModel], allSelectionState: TriStateCheckbox.State)

  class Backend($: BackendScope[Props, Unit]) {

    def autoColorize() = {
      $.props >>= { p =>
        val sm = p.proxy.value.entitySelectionModel
        val selectedEntities = p.proxy.value.qualityModel.rankedEntities.map(_.id).filter(sm.selected.contains)
        p.proxy.dispatch(AutoColorizeAction(selectedEntities))}
    }

    def updateEntityColor(entity: Entity)(e: SyntheticEvent[HTMLInputElement]) = {
      val newColor = WebColor(e.target.value)
      $.props >>= (_.proxy.dispatch(UpdateEntityColorAction(entity, newColor)))
    }

    def toggleEntitySelection(entity: Entity) = {
      $.props >>= { p =>
        val isSelected = p.proxy.value.entitySelectionModel.selected.contains(entity)
        p.proxy.dispatch(UpdateEntitySelectionAction(Set(entity), !isSelected))
      }
    }

    def updateAllEntitySelections() = {
      $.props >>= { p =>
        val newSelected = p.allSelectionState match {
          case TriStateCheckbox.Checked => false
          case _ => true
        }
        val allEntities = p.proxy.value.qualityModel.rankedEntities.map(_.id).toSet
        p.proxy.dispatch(UpdateEntitySelectionAction(allEntities, isSelected = newSelected))
      }
    }

    def toggleEntityPinning(entity: Entity)(e: ReactEvent) = {
      // only control pinning if the click happens in a blank table row area (i.e NOT on the checkbox, color widget)
      if (e.target.isInstanceOf[HTMLInputElement]) {
        Callback.empty
      } else {
        $.props >>= { p =>
          val isPinned = p.proxy.value.entitySelectionModel.pinned.contains(entity)
          val pinnedOrNone = if (isPinned) None else Some(entity)
          p.proxy.dispatch(UpdateEntityPinningAction(pinnedOrNone))
        }
      }
    }

    val pinGlyph = <.i(css.glyph.pin, ^.title := "Pin")

    def header(entities: Seq[GroupedEntity], allSelectionState: TriStateCheckbox.State) = {

      val autoColorizeGlyph = <.i(
        css.glyph.magic,
        ^.cursor.pointer,
        ^.onClick --> autoColorize,
        ^.title := "Auto-Colorize")

      <.tr(
        <.th("#"),
        <.th("Name"),
        <.th(allCheckbox(allSelectionState)),
        <.th(^.textAlign.center, autoColorizeGlyph),
        <.th(^.textAlign.center, pinGlyph)
      )
    }

    def createRow(entity: Entity, index: Int, isSelected: Boolean, isPinned: Boolean, color: WebColor) = {

      val selectionStyle = if (isSelected) css.empty else css.textMuted
      val pinStyle = if (isPinned) css.active else css.empty
      val cursor = if (isSelected) ^.cursor.pointer else EmptyTag
      val togglePinOnClick =
        if (isSelected)
          ^.onClick ==> toggleEntityPinning(entity)
        else EmptyTag

      <.tr(selectionStyle, pinStyle, cursor, togglePinOnClick,
        <.th(^.scope := "row", index + 1),
        <.td(entity.name),
        <.td(checkbox(entity, isSelected)),
        <.td(^.textAlign.center, colorPicker(entity, isSelected, color)),
        <.td(^.textAlign.center, if (isPinned) pinGlyph else EmptyTag)
      )
    }

    def allCheckbox(allSelection: TriStateCheckbox.State) = {
      TriStateCheckbox.Component(TriStateCheckbox.Props(allSelection, updateAllEntitySelections()))
    }

    def checkbox(entity: Entity, isSelected: Boolean) = {
      <.input.checkbox(^.checked := isSelected, ^.onChange --> toggleEntitySelection(entity))
    }

    def colorPicker(entity: Entity, isSelected: Boolean, color: WebColor) = {
      <.input.color(
        ^.value := (if (isSelected) color else DisabledColor).hexValue,
        ^.disabled := !isSelected,
        ^.onChange ==> updateEntityColor(entity))
    }

    def render(p: Props) = {

      val dm = p.proxy.value
      val entities = dm.qualityModel.rankedEntities
      val selectionModel = dm.entitySelectionModel

      val rows = entities.map(_.id).zipWithIndex.map {
        case (e, i) =>
          val isSelected = selectionModel.selected.contains(e)
          val isPinned = selectionModel.pinned.contains(e)
          val color = dm.colorMap(e)
          createRow(e, i, isSelected, isPinned, color)
      }

      <.table(css.legendTable,
        <.thead(header(entities, p.allSelectionState)),
        <.tbody(rows))
    }

  }

  private final val component = ReactComponentB[Props](LegendComponent.getClass.getSimpleName)
    .renderBackend[Backend]
    .build

  def apply(proxy: ModelProxy[AppModel]) = {

    val selectedEntitiesCount = proxy.value.entitySelectionModel.selected.size
    val entitiesCount = proxy.value.qualityModel.rankedEntities.size
    val allSelectionState =
      if (selectedEntitiesCount == 0) TriStateCheckbox.Unchecked
      else if (selectedEntitiesCount == entitiesCount) TriStateCheckbox.Checked
      else TriStateCheckbox.Indeterminate

    val props = Props(proxy, allSelectionState)
    component(props)
  }

}
