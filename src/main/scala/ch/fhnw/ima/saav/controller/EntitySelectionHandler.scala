package ch.fhnw.ima.saav.controller

import ch.fhnw.ima.saav.model.app.{EntitySelectionModel, SaavModel}
import ch.fhnw.ima.saav.model.domain.Entity
import diode.{Action, ActionHandler, ActionResult, ModelRW}

final case class UpdateEntitySelectionAction(entities: Set[Entity], isSelected: Boolean) extends Action

final case class UpdateEntityPinningAction(pinnedEntity: Option[Entity]) extends Action

class EntitySelectionHandler[M](modelRW: ModelRW[M, EntitySelectionModel]) extends ActionHandler(modelRW) {

  override def handle: PartialFunction[Any, ActionResult[M]] = {
    case UpdateEntitySelectionAction(entities, isSelected) =>
      val newSelection = if (isSelected) {
        value.selected ++ entities
      } else {
        value.selected -- entities
      }
      // clear pinning upon de-selection
      val newPinned: Option[Entity] = value.pinned.flatMap { currentlyPinned =>
        if (newSelection.contains(currentlyPinned)) {
          value.pinned
        } else {
          None
        }
      }
      updated(EntitySelectionModel(newSelection, newPinned))
    case UpdateEntityPinningAction(newPinned) => updated(value.copy(pinned = newPinned))
  }

}

object EntitySelectionHandler {

  def modelGet: (SaavModel) => EntitySelectionModel =
    _.model.right.toOption.map(_.entitySelectionModel).get

  def modelSet: (SaavModel, EntitySelectionModel) => SaavModel = (m, v) =>
    m.copy(model = m.model.right.map(_.copy(entitySelectionModel = v)))

}