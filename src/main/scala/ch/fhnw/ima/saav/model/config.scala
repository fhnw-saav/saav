package ch.fhnw.ima.saav
package model

import ch.fhnw.ima.saav.model.weight.{Weight, Weights}

object config {

  trait Config {

    def defaultWeights: Weights

  }

  final case class AnalysisConfig(criteria: Seq[CriteriaConfig])

  final case class CriteriaConfig(name: String, subCriteria: Seq[SubCriteriaConfig])

  final case class SubCriteriaConfig(name: String, indicators: Seq[IndicatorConfig])

  final case class IndicatorConfig(name: String, weight: Weight)

}
