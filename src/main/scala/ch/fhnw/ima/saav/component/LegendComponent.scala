package ch.fhnw.ima.saav
package component

import ch.fhnw.ima.saav.controller.{AutoColorizeAction, UpdateEntityColorAction, UpdateEntityPinningAction, UpdateEntityVisibilityAction}
import ch.fhnw.ima.saav.model.app.{AppModel, EntitySelectionModel, GroupedEntity}
import ch.fhnw.ima.saav.model.color._
import ch.fhnw.ima.saav.model.domain.EntityId
import diode.Action
import diode.react.ModelProxy
import japgolly.scalajs.react.extra.components.TriStateCheckbox
import japgolly.scalajs.react.vdom.ReactTagOf
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{ReactComponentB, _}
import org.scalajs.dom.raw.{HTMLElement, HTMLInputElement}

import scalacss.ScalaCssReact._

object LegendComponent {

  case class Props(
    showRank: Boolean,
    entities: Seq[GroupedEntity],
    entitySelectionModel: EntitySelectionModel,
    allVisibilityState: TriStateCheckbox.State,
    colorMap: Map[EntityId, WebColor],
    dispatch: Action => Callback
  )

  class Backend($: BackendScope[Props, Unit]) {

    private def autoColorize() = {
      $.props >>= { p =>
        val sm = p.entitySelectionModel
        val visibleEntities = p.entities.map(_.id).filter(sm.visible.contains)
        p.dispatch(AutoColorizeAction(visibleEntities))
      }
    }

    private def updateEntityColor(entity: EntityId)(e: SyntheticEvent[HTMLInputElement]) = {
      val newColor = WebColor(e.target.value)
      $.props >>= (_.dispatch(UpdateEntityColorAction(entity, newColor)))
    }

    private def toggleEntityVisibility(entity: EntityId) = {
      $.props >>= { p =>
        val isVisible = p.entitySelectionModel.visible.contains(entity)
        p.dispatch(UpdateEntityVisibilityAction(Set(entity), !isVisible))
      }
    }

    private def updateAllEntityVisibility() = {
      $.props >>= { p =>
        val newVisible = p.allVisibilityState match {
          case TriStateCheckbox.Checked => false
          case _ => true
        }
        val allEntities = p.entities.map(_.id).toSet
        p.dispatch(UpdateEntityVisibilityAction(allEntities, visible = newVisible))
      }
    }

    private def toggleEntityPinning(entity: EntityId)(e: ReactEvent) = {
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

    private def header(entities: Seq[GroupedEntity], allVisibilityState: TriStateCheckbox.State, isShowRank: Boolean) = {

      val autoColorizeGlyph = <.i(
        css.glyph.magic,
        ^.cursor.pointer,
        ^.onClick --> autoColorize,
        ^.title := "Auto-Colorize")

      <.tr(
        isShowRank ?= <.th(""),
        <.th("Name"),
        <.th(allCheckbox(allVisibilityState)),
        <.th(^.textAlign.center, autoColorizeGlyph)
      )
    }

    private def createRow(entity: GroupedEntity, index: Int, isVisible: Boolean, isPinned: Boolean, color: WebColor, isShowRank: Boolean) = {

      val visibleStyle = if (isVisible) css.empty else css.textMuted
      val pinnedStyle = if (isPinned) css.bgPrimary else css.empty
      val cursor = if (isVisible) ^.cursor.pointer else EmptyTag
      val togglePinOnClick =
        if (isVisible)
          ^.onClick ==> toggleEntityPinning(entity.id)
        else EmptyTag

      <.tr(visibleStyle, pinnedStyle, cursor, togglePinOnClick,
        isShowRank ?= <.th(css.rankTableCell, ^.scope := "row", index + 1 + "."),
        <.td(css.overflowHidden, ^.textOverflow.ellipsis, ^.title := entity.displayName, entity.displayName),
        <.td(checkbox(entity.id, isVisible)),
        <.td(^.textAlign.center, colorPicker(entity.id, isVisible, color))
      )
    }

    private def allCheckbox(allVisibilityState: TriStateCheckbox.State) = {
      TriStateCheckbox.Component(TriStateCheckbox.Props(allVisibilityState, updateAllEntityVisibility()))
    }

    private def checkbox(entity: EntityId, isVisible: Boolean) = {
      <.input.checkbox(^.checked := isVisible, ^.onChange --> toggleEntityVisibility(entity))
    }

    private def colorPicker(entity: EntityId, isVisible: Boolean, color: WebColor) = {
      <.input.color(css.colorCell,
        ^.value := (if (isVisible) color else DisabledColor).hexValue,
        ^.disabled := !isVisible,
        ^.onChange ==> updateEntityColor(entity))
    }

    def render(p: Props): ReactTagOf[HTMLElement] = {

      val rows = p.entities.zipWithIndex.map {
        case (e, i) =>
          val isVisible = p.entitySelectionModel.visible.contains(e.id)
          val isPinned = p.entitySelectionModel.pinned.contains(e.id)
          val color = p.colorMap(e.id)
          createRow(e, i, isVisible, isPinned, color, p.showRank)
      }

      val legendTable = <.table(css.legendTable,
        <.thead(header(p.entities, p.allVisibilityState, p.showRank)),
        <.tbody(rows))

      if (p.showRank) {
        <.div(css.row,
          <.div(css.colXs2, VisualRankingComponent(p.entities, p.entitySelectionModel, p.colorMap)),
          <.div(css.colXs10, legendTable)
        )
      } else {
        legendTable
      }

    }

  }

  private final val component = ReactComponentB[Props](LegendComponent.getClass.getSimpleName)
    .renderBackend[Backend]
    .build

  def apply(proxy: ModelProxy[AppModel], entityProvider: (AppModel) => Seq[GroupedEntity], showRank: Boolean = true): ReactComponentU[Props, Unit, Backend, TopNode] = {
    val model = proxy.value
    val entitySelectionModel = model.entitySelectionModel
    val visibleEntitiesCount = entitySelectionModel.visible.size
    val entities = entityProvider(model)
    val entitiesCount = entities.size
    val allVisibilityState =
      if (visibleEntitiesCount == 0) TriStateCheckbox.Unchecked
      else if (visibleEntitiesCount == entitiesCount) TriStateCheckbox.Checked
      else TriStateCheckbox.Indeterminate

    val props = Props(showRank, entities, entitySelectionModel, allVisibilityState, model.colorMap, proxy.theDispatch)
    component(props)
  }

}
