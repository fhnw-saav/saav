package ch.fhnw.ima.saav.controller

import ch.fhnw.ima.saav.model.app._
import ch.fhnw.ima.saav.model.domain.{IndicatorId, SubCriteriaId}
import ch.fhnw.ima.saav.model.weight.{Weight, Weights}
import diode.{Action, ActionHandler, ActionResult, ModelRW}

// Manages Expert Config

final case class UpdateWeightsAction(weights: Weights) extends Action

final case class UpdateIndicatorWeightAction(indicatorId: IndicatorId, isEnabled: Boolean) extends Action

final case class UpdateSubCriteriaWeightAction(subCriteriaId: SubCriteriaId, weight: Weight) extends Action

final case class UpdateVisibility(visibility: ExpertConfigVisibility) extends Action

class ExpertConfigHandler[M](modelRW: ModelRW[M, ExpertConfig]) extends ActionHandler(modelRW) {

  override def handle: PartialFunction[Any, ActionResult[M]] = {
    case UpdateWeightsAction(weights) => updated(value.copy(actualWeights = weights))
    case UpdateIndicatorWeightAction(indicatorId, isEnabled) =>
      val newEnabledIndicators = if (isEnabled) {
        value.actualWeights.enabledIndicators + indicatorId
      } else {
        value.actualWeights.enabledIndicators - indicatorId
      }
      updated(value.copy(actualWeights = value.actualWeights.copy(enabledIndicators = newEnabledIndicators)))
    case UpdateSubCriteriaWeightAction(subCriteria, weight) =>
      val newSubCriteriaWeights = value.actualWeights.subCriteriaWeights.updated(subCriteria, weight)
      updated(value.copy(actualWeights = value.actualWeights.copy(subCriteriaWeights = newSubCriteriaWeights)))
    case UpdateVisibility(newVisibility) => updated(value.copy(visibility = newVisibility))
  }

}

object ExpertConfigHandler {

  def modelGet: (SaavModel) => ExpertConfig =
    _.model.right.toOption.map(_.expertConfig).get

  def modelSet: (SaavModel, ExpertConfig) => SaavModel = (m, v) => {
    m.copy(model = m.model.right.map { am =>
      am.copy(
        expertConfig = v,
        qualityModel = QualityModel(am.analysis, v.actualWeights, am.qualityModel.layout.width),
        profileModel = ProfileModel(am.analysis, v.actualWeights, am.profileModel.layout.width)
      )
    })
  }

}