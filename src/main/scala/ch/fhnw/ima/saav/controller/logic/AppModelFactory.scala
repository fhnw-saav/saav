package ch.fhnw.ima.saav.controller.logic

import ch.fhnw.ima.saav.model.app.AppModel
import ch.fhnw.ima.saav.model.config.{AnalysisConfig, Config}
import ch.fhnw.ima.saav.model.domain._
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

    val configuredDisabledIndicators: Set[IndicatorId] =
      (for {
        criteria <- analysisConfig.criteria
        subCriteria <- criteria.subCriteria
        indicator <- subCriteria.indicators
        if !indicator.enabled
      } yield {
        val criteriaId = CriteriaId(criteria.name)
        val subCriteriaId = SubCriteriaId(criteriaId, subCriteria.name)
        IndicatorId(subCriteriaId, indicator.name)
      }).toSet

    val enabledIndicators = subCriteria.flatMap(_.indicators).map(_.id).filterNot(configuredDisabledIndicators.contains)


    val config = new Config {
      val defaultWeights: Weights = Weights(subCriteriaWeights, enabledIndicators.toSet)

      val nonAggregatableCriteria: Set[CriteriaId] = analysisConfig.criteria.filterNot(_.aggregatable).map(c => CriteriaId(c.name)).toSet

      private val (expectedIndicators, actualIndicators) = gatherExpectedVsActualIndicators(analysis, analysisConfig)

      val missingIndicators: Seq[IndicatorId] = expectedIndicators.diff(actualIndicators)

      val unexpectedIndicators: Seq[IndicatorId] = actualIndicators.diff(expectedIndicators)
    }

    logConfigMismatch(config)

    AppModel(analysis, config)
  }

  private def logConfigMismatch(config: Config) = {

    def log(indicatorId: IndicatorId) = {
      val c = indicatorId.subCriteriaId.criteriaId.name
      val sc = indicatorId.subCriteriaId.name
      val i = indicatorId.name

      println(s"- $c | $sc | $i")
    }

    if (config.missingIndicators.nonEmpty) {
      println(s"[WARN] ${config.missingIndicators.size} Missing Indicator(s):")
      config.missingIndicators.foreach(log)
    }

    if (config.unexpectedIndicators.nonEmpty) {
      println(s"[WARN] ${config.unexpectedIndicators.size} Unexpected Indicator(s):")
      config.unexpectedIndicators.foreach(log)
    }

  }

  private def gatherExpectedVsActualIndicators(analysis: Analysis, analysisConfig: AnalysisConfig) = {

    val expectedIndicators: Seq[IndicatorId] = for {
      c <- analysisConfig.criteria
      sc <- c.subCriteria
      i <- sc.indicators
    } yield IndicatorId(SubCriteriaId(CriteriaId(c.name), sc.name), i.name)

    val actualIndicators: Seq[IndicatorId] = analysis.criteria.flatMap(_.subCriteria.flatMap(_.indicators.map(_.id)))

    (expectedIndicators, actualIndicators)
  }

}
