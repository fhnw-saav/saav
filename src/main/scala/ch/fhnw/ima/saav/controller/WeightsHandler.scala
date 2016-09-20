package ch.fhnw.ima.saav.controller

import ch.fhnw.ima.saav.model.app._
import ch.fhnw.ima.saav.model.domain.{Indicator, SubCriteria}
import ch.fhnw.ima.saav.model.weight.{Weight, Weights}
import diode.{Action, ActionHandler, ActionResult, ModelRW}

// Manages Weights

final case class UpdateWeightsAction(weights: Weights) extends Action

final case class UpdateIndicatorWeightAction(indicator: Indicator, isEnabled: Boolean) extends Action

final case class UpdateSubCriteriaWeightAction(subCriteria: SubCriteria, weight: Weight) extends Action

class WeightsHandler[M](modelRW: ModelRW[M, Weights]) extends ActionHandler(modelRW) {

  override def handle: PartialFunction[Any, ActionResult[M]] = {
    case UpdateWeightsAction(weights) => updated(weights)
    case UpdateIndicatorWeightAction(indicator, isEnabled) =>
      val newEnabledIndicators = if (isEnabled) {
        value.enabledIndicators + indicator
      } else {
        value.enabledIndicators - indicator
      }
      updated(value.copy(enabledIndicators = newEnabledIndicators))
    case UpdateSubCriteriaWeightAction(subCriteria, weight) =>
      val newSubCriteriaWeights = value.subCriteriaWeights.updated(subCriteria, weight)
      updated(value.copy(subCriteriaWeights = newSubCriteriaWeights))
  }

}

object WeightsHandler {

  def modelGet: (SaavModel) => Weights =
    _.model.right.toOption.map(_.weights).get

  def modelSet: (SaavModel, Weights) => SaavModel = (m, v) => {
    m.copy(model = m.model.right.map { am =>
      am.copy(
        weights = v,
        qualityModel = QualityModel(am.analysis, v, am.qualityModel.layout.width),
        profileModel = ProfileModel(am.analysis, v, am.profileModel.layout.width)
      )
    })
  }

}