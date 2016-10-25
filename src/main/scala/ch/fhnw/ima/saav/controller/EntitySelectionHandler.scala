package ch.fhnw.ima.saav.controller

import ch.fhnw.ima.saav.model.app.{EntitySelectionModel, SaavModel}
import ch.fhnw.ima.saav.model.domain.EntityId
import diode.{Action, ActionHandler, ActionResult, ModelRW}

final case class UpdateEntityVisibilityAction(entities: Set[EntityId], visible: Boolean) extends Action

final case class UpdateEntityPinningAction(pinnedEntity: Option[EntityId]) extends Action

final case class UpdateEntityHoveringAction(hoveredEntity: Option[EntityId]) extends Action

class EntitySelectionHandler[M](modelRW: ModelRW[M, EntitySelectionModel]) extends ActionHandler(modelRW) {

  override def handle: PartialFunction[Any, ActionResult[M]] = {
    case UpdateEntityVisibilityAction(entities, visible) =>
      val newVisible = if (visible) {
        value.visible ++ entities
      } else {
        value.visible -- entities
      }
      // clear pinning for invisible entities
      val newPinned: Option[EntityId] = value.pinned.flatMap { currentlyPinned =>
        if (newVisible.contains(currentlyPinned)) {
          value.pinned
        } else {
          None
        }
      }
      updated(EntitySelectionModel(newVisible, newPinned))
    case UpdateEntityPinningAction(newPinned) => updated(value.copy(pinned = newPinned))
    case UpdateEntityHoveringAction(newHovered) => updated(value.copy(hovered = newHovered))
  }

}

object EntitySelectionHandler {

  def modelGet: (SaavModel) => EntitySelectionModel =
    _.model.right.toOption.map(_.entitySelectionModel).get

  def modelSet: (SaavModel, EntitySelectionModel) => SaavModel = (m, v) =>
    m.copy(model = m.model.right.map(_.copy(entitySelectionModel = v)))

}