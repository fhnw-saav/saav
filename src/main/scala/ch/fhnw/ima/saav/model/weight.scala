package ch.fhnw.ima.saav.model

import ch.fhnw.ima.saav._
import ch.fhnw.ima.saav.model.domain.{IndicatorId, SubCriteriaId}

object weight {

  sealed trait Weight

  final case class Quality(weight: Double) extends Weight

  case object Profile extends Weight

  final case class Weights(subCriteriaWeights: Map[SubCriteriaId, Weight], enabledIndicators: Set[IndicatorId])

  // Works around SI-7046, a scalac issue which will be fixed in 2.11.9 / 2.12.1
  // https://gitter.im/travisbrown/circe?at=582b833a37fbab5354b90cba
  object Weight {
    implicit val decodeWeight: io.circe.Decoder[Weight] = io.circe.generic.semiauto.deriveDecoder
  }

  private[model] def weightedMean(valuesWithWeight: Seq[(Double, Double)]): Option[Double] = {
    if (valuesWithWeight.isEmpty) None
    else {
      val (sumOfProducts, sumOfWeights) = valuesWithWeight.foldLeft((0d, 0d)) { case ((products, weights), (v, w)) =>
        (products + (v * w), weights + w)
      }
      if (sumOfWeights == 0) {
        None
      } else {
        Some(sumOfProducts / sumOfWeights)
      }
    }
  }

  private[model] def mean(values: Seq[Double]): Option[Double] = {
    if (values.isEmpty) None
    else Some(values.sum / values.length)
  }

}
