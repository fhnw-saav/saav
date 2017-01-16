package ch.fhnw.ima.saav
package model

import ch.fhnw.ima.saav.model.domain.{CriteriaId, IndicatorId}
import ch.fhnw.ima.saav.model.weight.{Weight, Weights}
import io.circe.Error
import io.circe.generic.auto._
import io.circe.parser._

object config {

  /** Application defaults and potential mismatches between config and data. */
  trait Config {
    def title: String
    def allowedValueRange: (Double, Double)
    def defaultWeights: Weights
    def nonAggregatableCriteria: Set[CriteriaId]
    def mismatch: ConfigMismatch
  }

  trait ConfigMismatch {
    def missingIndicators: Seq[IndicatorId]
    def unexpectedIndicators: Seq[IndicatorId]
  }

  object ConfigMismatch {
    val none = new ConfigMismatch {
      val missingIndicators: Seq[IndicatorId] = Seq.empty
      val unexpectedIndicators: Seq[IndicatorId] = Seq.empty
    }
  }

  /** Entry point to JSON config (aka catalog) */
  object AnalysisConfig {
    val default = AnalysisConfig("", (Double.NegativeInfinity, Double.PositiveInfinity), Seq())
    def fromJson(json: String): Either[Error, AnalysisConfig] = decode[AnalysisConfig](json)
  }

  final case class AnalysisConfig(title: String, allowedValueRange: (Double, Double), criteria: Seq[CriteriaConfig])

  final case class CriteriaConfig(name: String, aggregatable: Boolean, subCriteria: Seq[SubCriteriaConfig])

  final case class SubCriteriaConfig(name: String, weight: Weight, indicators: Seq[IndicatorConfig])

  final case class IndicatorConfig(name: String, enabled: Boolean)

}