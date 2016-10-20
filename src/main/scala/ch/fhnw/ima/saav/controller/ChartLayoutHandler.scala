package ch.fhnw.ima.saav.controller

import ch.fhnw.ima.saav.model.app.SaavModel
import ch.fhnw.ima.saav.model.layout.{ProfileChartLayout, QualityChartLayout}
import diode.{Action, ActionHandler, ActionResult, ModelRW}

final case class UpdateChartWidthAction(width: Int) extends Action

class ChartLayoutHandler[M](modelRW: ModelRW[M, (QualityChartLayout, ProfileChartLayout)]) extends ActionHandler(modelRW) {

  override def handle: PartialFunction[Any, ActionResult[M]] = {
    case UpdateChartWidthAction(width) =>

      val ql = value._1
      val newQualityLayout = new QualityChartLayout(width, ql.criteria, Some(ql.minValue), Some(ql.maxValue))

      val pl = value._2
      val newProfileChartLayout = new ProfileChartLayout(width, pl.criteria, Some(pl.minValue), Some(pl.maxValue))

      updated((newQualityLayout, newProfileChartLayout))
  }

}

object ChartLayoutHandler {

  def modelGet: (SaavModel) => (QualityChartLayout, ProfileChartLayout) =
    _.model.right.toOption.map(am => (am.qualityModel.layout, am.profileModel.layout)).get

  def modelSet: (SaavModel, (QualityChartLayout, ProfileChartLayout)) => SaavModel = (m, v) =>
    m.copy(model = m.model.right.map { am =>
      am.copy(
        qualityModel = am.qualityModel.copy(layout = v._1),
        profileModel = am.profileModel.copy(layout = v._2)
      )
    })

}