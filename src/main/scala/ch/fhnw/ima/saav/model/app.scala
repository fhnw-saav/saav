package ch.fhnw.ima.saav
package model

import ch.fhnw.ima.saav.model.color._
import ch.fhnw.ima.saav.model.domain._
import ch.fhnw.ima.saav.model.layout.{ProfileLayout, QualityLayout}

/** Application models (incl. presentation state). */
object app {

  case class SaavModel(model: Either[NoDataAppModel, AppModel] = Left(NoDataAppModel(ImportNotStarted())))

  case class NoDataAppModel(importState: ImportState)

  sealed trait ImportState

  final case class ImportNotStarted() extends ImportState

  final case class ImportInProgress(progress: Float) extends ImportState

  final case class ImportFailed(throwable: Throwable) extends ImportState

  final case class EntitySelectionModel(selected: Set[Entity] = Set.empty, pinned: Option[Entity] = None)

  final case class SubCriteriaSelectionModel(hovered: Option[SubCriteria] = None)

  final case class AppModel(
    analysis: Analysis,
    weights: Weights,
    entitySelectionModel: EntitySelectionModel,
    subCriteriaSelectionModel: SubCriteriaSelectionModel,
    colorMap: Map[Entity, WebColor],
    qualityModel: QualityModel,
    profileModel: ProfileModel
  ) {

    def updateWeights(weights: Weights): AppModel = {
      copy(weights = weights, qualityModel = QualityModel(analysis, weights))
    }

  }

  object AppModel {

    def apply(analysis: Analysis, weights: Weights): AppModel = {
      val qualityModel = QualityModel(analysis, weights)
      val profileModel = ProfileModel(analysis, weights)
      val entitySelectionModel = EntitySelectionModel(analysis.entities.toSet, None)
      val subCriteriaSelectionModel = SubCriteriaSelectionModel()

      // colorize _after_ ranking to get optimally distinct colors by default
      val colorMap = autoColorMap(qualityModel.rankedEntities.map(_.id))

      AppModel(analysis, weights, entitySelectionModel, subCriteriaSelectionModel, colorMap, qualityModel, profileModel)
    }

  }

  final case class QualityModel(rankedEntities: Seq[GroupedEntity], criteria: Seq[GroupedCriteria], layout: QualityLayout)

  object QualityModel {

    def apply(analysis: Analysis, weights: Weights): QualityModel = {

      val criteria = analysis.criteria.map { c =>
        GroupedCriteria.forQuality(analysis.entities, c, analysis.reviews, weights)
      }

      val rankedEntities = analysis.entities.map { e =>
        val value = median(criteria.flatMap(_.groupedValues(e)))
        GroupedEntity(e, value = value)
      }.sortBy(_.value).reverse

      val (minValue, maxValue) = minMaxValues(criteria)

      val layout = new QualityLayout(criteria, minValue, maxValue)

      QualityModel(rankedEntities, criteria, layout)
    }

  }

  final case class ProfileModel(sortedEntities: Seq[GroupedEntity], entitySortingStrategy: EntitySortingStrategy, criteria: Seq[GroupedCriteria], layout: ProfileLayout)

  object ProfileModel {

    def apply(analysis: Analysis, weights: Weights): ProfileModel = {

      val criteria = analysis.criteria.map { c =>
        GroupedCriteria.forProfile(analysis.entities, c, analysis.reviews, weights)
      }

      // TODO: Support different sorting strategies
      val sortedEntities = analysis.entities.map { e =>
        val value = median(criteria.flatMap(_.groupedValues(e)))
        GroupedEntity(e, value = value)
      }.sortBy(_.name)

      val (minValue, maxValue) = minMaxValues(criteria)

      val layout = new ProfileLayout(criteria, minValue, maxValue)

      ProfileModel(sortedEntities, ByAlphabetEntitySortingStrategy, criteria, layout)
    }

  }

  trait EntitySortingStrategy
  case object ByAlphabetEntitySortingStrategy extends EntitySortingStrategy
  final case class ByCriteriaEntitySortingStrategy(criteria: Criteria)

  final case class GroupedEntity(id: Entity, value: Option[Double] = None) {
    def name = id.name
  }

  final case class GroupedCriteria(id: Criteria, subCriteria: Seq[GroupedSubCriteria], groupedValues: Map[Entity, Option[Double]]) {
    def name = id.name

    // Deliberately not using min/max of groupedValues for our purpose
    val minValue = subCriteria.map(_.minValue).min
    val maxValue = subCriteria.map(_.maxValue).max
  }

  object GroupedCriteria {

    def forQuality(entities: Seq[Entity], criteria: Criteria, reviews: Seq[Review], weights: Weights): GroupedCriteria = {

      // TODO: Filter according to profile vs. quality (only makes sense once we have good defaults)
      val subCriteria = criteria.subCriteria.map(sc => GroupedSubCriteria(entities, sc, reviews, weights.enabledIndicators))

      val groupedValue = (entity: Entity) => {
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

      GroupedCriteria(entities, criteria, subCriteria, groupedValue)

    }

    def forProfile(entities: Seq[Entity], criteria: Criteria, reviews: Seq[Review], weights: Weights): GroupedCriteria = {

      // TODO: Filter according to profile vs. quality (only makes sense once we have good defaults)
      val subCriteria = criteria.subCriteria.map(sc => GroupedSubCriteria(entities, sc, reviews, weights.enabledIndicators))

      val groupedValue = (entity: Entity) => {
        val valuesWithWeights = for {
          subCriterion <- subCriteria
          value <- subCriterion.groupedValues(entity)
        } yield {
          (value, 1d) // no weighting for profile chart
        }
        weightedMedian(valuesWithWeights)
      }

      GroupedCriteria(entities, criteria, subCriteria, groupedValue)

    }

    private def apply(entities: Seq[Entity], criteria: Criteria, subCriteria: Seq[GroupedSubCriteria], groupedValue: Entity => Option[Double]): GroupedCriteria = {
      val groupedValues = entities.map(e => e -> groupedValue(e)).toMap
      GroupedCriteria(criteria, subCriteria, groupedValues)
    }

  }

  final case class GroupedSubCriteria(id: SubCriteria, groupedValues: Map[Entity, Option[Double]], indicators: Seq[GroupedIndicator]) {
    def name = id.name

    val minValue = groupedValues.values.min
    val maxValue = groupedValues.values.max
  }

  object GroupedSubCriteria {

    def apply(entities: Seq[Entity], subCriteria: SubCriteria, reviews: Seq[Review], enabledIndicators: Set[Indicator]): GroupedSubCriteria = {
      val indicators = subCriteria.indicators.filter(enabledIndicators.contains).map { i =>
        GroupedIndicator(i, entities, reviews)
      }

      def groupedValue(entity: Entity): Option[Double] = {
        val values = for {
          indicator <- indicators
          value <- indicator.groupedValues(entity)
        } yield value
        median(values)
      }

      val groupedValues = entities.map(e => e -> groupedValue(e)).toMap

      GroupedSubCriteria(subCriteria, groupedValues, indicators)
    }

  }

  final case class GroupedIndicator(id: Indicator, groupedValues: Map[Entity, Option[Double]]) {
    def name = id.name
  }

  object GroupedIndicator {

    def apply(indicator: Indicator, entities: Seq[Entity], reviews: Seq[Review]): GroupedIndicator = {
      val groupedValues =
        (for {
          entity <- entities
        } yield {
          val values = for {
            review <- reviews
            value <- indicator.values.get((entity, review))
          } yield value
          entity -> median(values)
        }).toMap
      GroupedIndicator(indicator, groupedValues)
    }

  }

  sealed trait Weight

  final case class Quality(weight: Double) extends Weight

  case object Profile extends Weight

  final case class Weights(subCriteriaWeights: Map[SubCriteria, Weight] = Map(), enabledIndicators: Set[Indicator])

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

  private[model] def minMaxValues(criteria: Seq[GroupedCriteria]) = {
    val minValue = if (criteria.isEmpty) None else criteria.map(_.minValue).min
    val maxValue = if (criteria.isEmpty) None else criteria.map(_.maxValue).max
    (minValue, maxValue)
  }

}