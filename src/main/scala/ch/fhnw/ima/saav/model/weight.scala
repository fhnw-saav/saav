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

  private[model] def weightedMedian(valuesWithWeight: Seq[(Double, Double)]) = {

    // TODO: Replace this poor man's calculation with something more efficient

    def explode = (value: Double, weight: Double) => {
      val count = (weight * 100).toInt
      Seq.fill(count)(value)
    }

    val explodedValues = valuesWithWeight.flatMap(explode.tupled)
    median(explodedValues)
  }

  private[model] def median(values: Seq[Double]) = {
    val sortedValues = values.sorted
    sortedValues.size match {
      case 0 => None
      case length if length % 2 == 0 =>
        val i = (length - 1) / 2
        Some((sortedValues(i) + sortedValues(i + 1)) / 2)
      case length => Some(sortedValues(length / 2))
    }
  }

}
