package ch.fhnw.ima.saav.controller.logic

import ch.fhnw.ima.saav.model.app.AppModel
import ch.fhnw.ima.saav.model.config.{AnalysisConfig, Config}
import ch.fhnw.ima.saav.model.domain.{Analysis, CriteriaId, SubCriteriaId}
import ch.fhnw.ima.saav.model.weight.{Quality, Weight, Weights}
import scala.language.postfixOps

object AppModelFactory {

  def createAppModel(analysisConfig: AnalysisConfig, analysis: Analysis): AppModel = {

    val configuredSubCriteriaWeights: Map[SubCriteriaId, Weight] = (for {
      criteria <- analysisConfig.criteria
      subCriteria <- criteria.subCriteria
    } yield SubCriteriaId(CriteriaId(criteria.name), subCriteria.name) -> subCriteria.weight) toMap

    val subCriteria = analysis.criteria.flatMap(_.subCriteria)
    val subCriteriaWeights = subCriteria.map { sc =>
      sc.id -> configuredSubCriteriaWeights.getOrElse(sc.id, Quality(1.0))
    } toMap

    // TODO: Read enabled/disabled indicators from config

    val indicatorIds = subCriteria.flatMap(_.indicators).map(_.id)
    val weights = Weights(subCriteriaWeights, indicatorIds.toSet)

    val config = new Config {
      val defaultWeights: Weights = weights
    }

    AppModel(analysis, config)
  }

}
