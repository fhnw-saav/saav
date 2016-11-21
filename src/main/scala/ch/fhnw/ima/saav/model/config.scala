package ch.fhnw.ima.saav
package model

import ch.fhnw.ima.saav.model.domain.CriteriaId
import ch.fhnw.ima.saav.model.weight.{Weight, Weights}
import io.circe.Error
import io.circe.generic.auto._
import io.circe.parser._

object config {

  trait Config {
    def defaultWeights: Weights
    def nonAggregatableCriteria: Set[CriteriaId] // blacklist semantics until our configs are complete
  }

  object AnalysisConfig {
    val empty = AnalysisConfig(Seq())
    def fromJson(json: String): Either[Error, AnalysisConfig] = decode[AnalysisConfig](json)
  }

  final case class AnalysisConfig(criteria: Seq[CriteriaConfig])

  final case class CriteriaConfig(name: String, aggregatable: Boolean, subCriteria: Seq[SubCriteriaConfig])

  final case class SubCriteriaConfig(name: String, weight: Weight, indicators: Seq[IndicatorConfig])

  final case class IndicatorConfig(name: String, enabled: Boolean)

}