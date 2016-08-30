package ch.fhnw.ima.saav
package model

import ch.fhnw.ima.saav.model.color._
import ch.fhnw.ima.saav.model.domain._

/** Application models (incl. presentation state). */
object app {

  case class SaavModel(model: Either[NoDataModel, DataModel] = Left(NoDataModel(ImportNotStarted())))

  case class NoDataModel(importState: ImportState)

  sealed trait ImportState

  final case class ImportNotStarted() extends ImportState

  final case class ImportInProgress(progress: Float) extends ImportState

  final case class ImportFailed(throwable: Throwable) extends ImportState

  final case class EntitySelectionModel(selected: Set[Entity] = Set.empty, pinned: Option[Entity] = None)

  final case class DataModel(
    rankedEntities: Seq[GroupedEntity],
    criteria: Seq[GroupedCriteria],
    selectionModel: EntitySelectionModel,
    colorMap: Map[Entity, WebColor]
  ) {

    val minValue = if (criteria.isEmpty) None else criteria.map(_.minValue).min
    val maxValue = if (criteria.isEmpty) None else criteria.map(_.maxValue).max

    def updateWeights(analysis: Analysis, weights: Weights): DataModel = {

      // create new model to trigger value calculation with new weights
      val newModel = DataModel(analysis, weights)

      // keep selections and colors of current model
      newModel.copy(selectionModel = selectionModel, colorMap = colorMap)

    }

  }

  object DataModel {

    def apply(analysis: Analysis, weights: Weights = Weights()): DataModel = {

      val categories = analysis.criteria.map { c =>
        GroupedCriteria(analysis.entities, c, analysis.reviews, weights)
      }

      val rankedEntities = analysis.entities.map { e =>
        val value = median(categories.flatMap(_.groupedValues(e)))
        GroupedEntity(e, value = value)
      }.sortBy(_.value).reverse

      val selectionModel = EntitySelectionModel(analysis.entities.toSet, None)

      // colorize _after_ ranking to get optimally distinct colors
      val colorMap = autoColorMap(rankedEntities.map(_.id))

      DataModel(rankedEntities, categories, selectionModel, colorMap)
    }

  }

  final case class GroupedEntity(id: Entity, value: Option[Double] = None) {
    def name = id.name
  }

  final case class GroupedSubCriteria(id: SubCriteria, groupedValues: Map[Entity, Option[Double]], indicators: Seq[Indicator]) {
    def name = id.name
    val minValue = groupedValues.values.min
    val maxValue = groupedValues.values.max
  }

  object GroupedSubCriteria {

    def apply(entities: Seq[Entity], subCriteria: SubCriteria, reviews: Seq[Review], disabledIndicators: Set[Indicator]): GroupedSubCriteria = {
      val indicators = subCriteria.indicators.filter(!disabledIndicators.contains(_))

      def groupedValue(entity: Entity): Option[Double] = {
        val values = for {
          review <- reviews
          indicator <- indicators
          values <- indicator.values.get((entity, review))
        } yield values
        median(values)
      }

      val groupedValues = entities.map(e => e -> groupedValue(e)).toMap

      GroupedSubCriteria(subCriteria, groupedValues, indicators)
    }

  }

  final case class GroupedCriteria(id: Criteria, subCriteria: Seq[GroupedSubCriteria], groupedValues: Map[Entity, Option[Double]]) {
    def name = id.name

    // Deliberately not using min/max of groupedValues for our purpose
    val minValue = subCriteria.map(_.minValue).min
    val maxValue = subCriteria.map(_.maxValue).max
  }

  object GroupedCriteria {

    def apply(entities: Seq[Entity], criteria: Criteria, reviews: Seq[Review], weights: Weights): GroupedCriteria = {

      val subCriteria = criteria.subCriteria.map(sc => GroupedSubCriteria(entities, sc, reviews, weights.disabledIndicators))

      def groupedValue(entity: Entity): Option[Double] = {
        val valuesWithWeights = for {
          subCriterion <- subCriteria
          value <- subCriterion.groupedValues(entity)
          weight = weights.subCriteriaWeights.getOrElse(subCriterion.id, Quality(1f))
          weightValue <- weight match {
            case Quality(wv) => Some(wv)
            case _ => None
          }
        } yield {
          (value, weightValue)
        }
        weightedMedian(valuesWithWeights)
      }

      val groupedValues = entities.map(e => e -> groupedValue(e)).toMap

      GroupedCriteria(criteria, subCriteria, groupedValues)

    }

  }

  sealed trait Weight

  final case class Quality(weight: Double) extends Weight

  case object Profile extends Weight

  final case class Weights(subCriteriaWeights: Map[SubCriteria, Weight] = Map(), disabledIndicators: Set[Indicator] = Set.empty)

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