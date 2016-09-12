package ch.fhnw.ima.saav
package component

import ch.fhnw.ima.saav.controller.SaavController.{AutoColorizeAction, UpdateEntityColorAction, UpdateEntityPinningAction, UpdateEntitySelectionAction}
import ch.fhnw.ima.saav.model.app.{AppModel, EntitySelectionModel, GroupedEntity}
import ch.fhnw.ima.saav.model.color._
import ch.fhnw.ima.saav.model.domain.Entity
import diode.Action
import diode.react.ModelProxy
import japgolly.scalajs.react.extra.components.TriStateCheckbox
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{ReactComponentB, _}
import org.scalajs.dom.raw.HTMLInputElement

import scala.language.postfixOps
import scalacss.ScalaCssReact._

object LegendComponent {

  case class Props(
    showRank: Boolean,
    entities: Seq[GroupedEntity],
    entitySelectionModel: EntitySelectionModel,
    allSelectionState: TriStateCheckbox.State,
    colorMap: Map[Entity, WebColor],
    dispatch: Action => Callback
  )

  class Backend($: BackendScope[Props, Unit]) {

    def autoColorize() = {
      $.props >>= { p =>
        val sm = p.entitySelectionModel
        val selectedEntities = p.entities.map(_.id).filter(sm.selected.contains)
        p.dispatch(AutoColorizeAction(selectedEntities))
      }
    }

    def updateEntityColor(entity: Entity)(e: SyntheticEvent[HTMLInputElement]) = {
      val newColor = WebColor(e.target.value)
      $.props >>= (_.dispatch(UpdateEntityColorAction(entity, newColor)))
    }

    def toggleEntitySelection(entity: Entity) = {
      $.props >>= { p =>
        val isSelected = p.entitySelectionModel.selected.contains(entity)
        p.dispatch(UpdateEntitySelectionAction(Set(entity), !isSelected))
      }
    }

    def updateAllEntitySelections() = {
      $.props >>= { p =>
        val newSelected = p.allSelectionState match {
          case TriStateCheckbox.Checked => false
          case _ => true
        }
        val allEntities = p.entities.map(_.id).toSet
        p.dispatch(UpdateEntitySelectionAction(allEntities, isSelected = newSelected))
      }
    }

    def toggleEntityPinning(entity: Entity)(e: ReactEvent) = {
      // only control pinning if the click happens in a blank table row area (i.e NOT on the checkbox, color widget)
      if (e.target.isInstanceOf[HTMLInputElement]) {
        Callback.empty
      } else {
        $.props >>= { p =>
          val isPinned = p.entitySelectionModel.pinned.contains(entity)
          val pinnedOrNone = if (isPinned) None else Some(entity)
          p.dispatch(UpdateEntityPinningAction(pinnedOrNone))
        }
      }
    }

    val pinGlyph = <.i(css.glyph.pin, ^.title := "Pin")

    def header(entities: Seq[GroupedEntity], allSelectionState: TriStateCheckbox.State, isShowRank: Boolean) = {

      val autoColorizeGlyph = <.i(
        css.glyph.magic,
        ^.cursor.pointer,
        ^.onClick --> autoColorize,
        ^.title := "Auto-Colorize")

      <.tr(
        isShowRank ?= <.th("#"),
        <.th("Name"),
        <.th(allCheckbox(allSelectionState)),
        <.th(^.textAlign.center, autoColorizeGlyph),
        <.th(^.textAlign.center, pinGlyph)
      )
    }

    def createRow(entity: Entity, index: Int, isSelected: Boolean, isPinned: Boolean, color: WebColor, isShowRank: Boolean) = {

      val selectionStyle = if (isSelected) css.empty else css.textMuted
      val pinStyle = if (isPinned) css.active else css.empty
      val cursor = if (isSelected) ^.cursor.pointer else EmptyTag
      val togglePinOnClick =
        if (isSelected)
          ^.onClick ==> toggleEntityPinning(entity)
        else EmptyTag

      <.tr(selectionStyle, pinStyle, cursor, togglePinOnClick,
        isShowRank ?= <.th(^.scope := "row", index + 1),
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

      val rows = p.entities.map(_.id).zipWithIndex.map {
        case (e, i) =>
          val isSelected = p.entitySelectionModel.selected.contains(e)
          val isPinned = p.entitySelectionModel.pinned.contains(e)
          val color = p.colorMap(e)
          createRow(e, i, isSelected, isPinned, color, p.showRank)
      }

      <.table(css.legendTable,
        <.thead(header(p.entities, p.allSelectionState, p.showRank)),
        <.tbody(rows))
    }

  }

  private final val component = ReactComponentB[Props](LegendComponent.getClass.getSimpleName)
    .renderBackend[Backend]
    .build

  def apply(proxy: ModelProxy[AppModel], entityProvider: (AppModel) => Seq[GroupedEntity], showRank: Boolean = true) = {
    val model = proxy.value
    val entitySelectionModel = model.entitySelectionModel
    val selectedEntitiesCount = entitySelectionModel.selected.size
    val entities = entityProvider(model)
    val entitiesCount = entities.size
    val allSelectionState =
      if (selectedEntitiesCount == 0) TriStateCheckbox.Unchecked
      else if (selectedEntitiesCount == entitiesCount) TriStateCheckbox.Checked
      else TriStateCheckbox.Indeterminate

    val props = Props(showRank, entities, entitySelectionModel, allSelectionState, model.colorMap, proxy.theDispatch)
    component(props)
  }

}
