package ch.fhnw.ima.saav
package component

import ch.fhnw.ima.saav.controller.{UpdateEntityColorAction, UpdateEntityHoveringAction, UpdateEntityPinningAction, UpdateEntityVisibilityAction}
import ch.fhnw.ima.saav.model.app.{AppModel, EntitySelectionModel, GroupedEntity}
import ch.fhnw.ima.saav.model.color._
import ch.fhnw.ima.saav.model.domain.EntityId
import diode.Action
import diode.react.ModelProxy
import japgolly.scalajs.react.vdom.ReactTagOf
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB, _}
import org.scalajs.dom.raw.{HTMLElement, HTMLInputElement}

import scalacss.ScalaCssReact._

object LegendComponent {

  case class Props(
    showRank: Boolean,
    entities: Seq[GroupedEntity],
    entitySelectionModel: EntitySelectionModel,
    colorMap: Map[EntityId, WebColor],
    dispatch: Action => Callback
  )

  class Backend($: BackendScope[Props, Unit]) {

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

    private def updateAllEntityVisibility(visible: Boolean) = {
      $.props >>= { p =>
        val allEntities = p.entities.map(_.id).toSet
        p.dispatch(UpdateEntityVisibilityAction(allEntities, visible = visible))
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

    private def setHoveredEntity(hoveredEntity: Option[EntityId]) =
      $.props >>= { p =>
        p.dispatch(UpdateEntityHoveringAction(hoveredEntity))
      }

    private def header(entities: Seq[GroupedEntity], isShowRank: Boolean) = {
      <.tr(
        <.th(^.colSpan := (if (isShowRank) 4 else 3),
          <.a(^.cursor.pointer, ^.onClick --> updateAllEntityVisibility(true), "All"),
          <.a(css.hSpaced, ^.cursor.pointer, ^.onClick --> updateAllEntityVisibility(false), "None"))
      )
    }

    private def createRow(entity: GroupedEntity, index: Int, isVisible: Boolean, isPinned: Boolean, isHovered: Boolean, color: WebColor, isShowRank: Boolean) = {

      val visibleStyle = if (isVisible) css.empty else css.textMuted
      val bgStyle = if (isPinned) css.bgPrimary else if (isHovered) css.bgInfo else css.empty

      <.tr(visibleStyle, bgStyle,
        isVisible ?= ^.cursor.pointer,
        isVisible ?= ^.onClick ==> toggleEntityPinning(entity.id),
        isVisible ?= ^.onMouseOver --> setHoveredEntity(Some(entity.id)),
        <.td(checkbox(entity.id, isVisible)),
        isShowRank ?= <.th(^.scope := "row", index + 1 + "."),
        <.td(css.overflowHidden, ^.textOverflow.ellipsis, ^.width := "100%", ^.title := entity.displayName, entity.displayName),
        <.td(^.textAlign.center, colorPicker(entity.id, isVisible, color))
      )
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
          val isHovered = p.entitySelectionModel.hovered.contains(e.id)
          val color = p.colorMap(e.id)
          createRow(e, i, isVisible, isPinned, isHovered, color, p.showRank)
      }

      val legendTable = <.table(css.legendTable, ^.onMouseLeave --> setHoveredEntity(None),
        <.thead(header(p.entities, p.showRank)),
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
    val entities = entityProvider(model)
    val props = Props(showRank, entities, entitySelectionModel, model.colorMap, proxy.theDispatch)
    component(props)
  }

}
