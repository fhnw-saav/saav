package ch.fhnw.ima.saav.controller

import ch.fhnw.ima.saav.model.app.{SaavModel, SubCriteriaSelectionModel}
import ch.fhnw.ima.saav.model.domain.SubCriteriaId
import diode.{Action, ActionHandler, ActionResult, ModelRW}

final case class UpdateSubCriteriaHoveringAction(hoveredSubCriteria: Option[SubCriteriaId]) extends Action

final case class UpdateSubCriteriaPinningAction(pinnedSubCriteria: Option[SubCriteriaId]) extends Action

class SubCriteriaSelectionHandler[M](modelRW: ModelRW[M, SubCriteriaSelectionModel]) extends ActionHandler(modelRW) {

  override def handle: PartialFunction[Any, ActionResult[M]] = {
    case UpdateSubCriteriaHoveringAction(hoveredSubCriteria) =>
      updated(value.copy(hovered = hoveredSubCriteria))
    case UpdateSubCriteriaPinningAction(pinnedSubCriteria) =>
      updated(value.copy(pinned = pinnedSubCriteria))
  }

}

object SubCriteriaSelectionHandler {

  def modelGet: (SaavModel) => SubCriteriaSelectionModel =
    _.model.right.toOption.map(_.subCriteriaSelectionModel).get

  def modelSet: (SaavModel, SubCriteriaSelectionModel) => SaavModel = (m, v) =>
    m.copy(model = m.model.right.map(_.copy(subCriteriaSelectionModel = v)))

}