package ch.fhnw.ima.saav
package model

import ch.fhnw.ima.saav.model.color._
import ch.fhnw.ima.saav.model.config.Config
import ch.fhnw.ima.saav.model.domain._
import ch.fhnw.ima.saav.model.layout.{ProfileChartLayout, QualityChartLayout}
import ch.fhnw.ima.saav.model.weight._

/** Application models (incl. presentation state). */
object app {

  case class SaavModel(model: Either[NoDataAppModel, AppModel] = Left(NoDataAppModel(ImportNotStarted())))

  case class NoDataAppModel(importState: ImportState)

  sealed trait ImportState

  final case class ImportNotStarted() extends ImportState

  final case class ImportInProgress(progress: Float) extends ImportState

  final case class ImportFailed(throwable: Throwable) extends ImportState

  final case class EntitySelectionModel(visible: Set[Entity] = Set.empty, pinned: Option[Entity] = None)

  final case class SubCriteriaSelectionModel(hovered: Option[SubCriteria] = None)

  final case class AppModel(
    config: Config,
    analysis: Analysis,
    weights: Weights,
    entitySelectionModel: EntitySelectionModel,
    subCriteriaSelectionModel: SubCriteriaSelectionModel,
    colorMap: Map[Entity, WebColor],
    qualityModel: QualityModel,
    profileModel: ProfileModel
  )

  object AppModel {

    def apply(analysis: Analysis, config: Config): AppModel = {
      val defaultLayoutWidth = 1000
      val qualityModel = QualityModel(analysis, config.defaultWeights, defaultLayoutWidth)
      val profileModel = ProfileModel(analysis, config.defaultWeights, defaultLayoutWidth)
      val entitySelectionModel = EntitySelectionModel(analysis.entities.toSet, None)
      val subCriteriaSelectionModel = SubCriteriaSelectionModel()

      // colorize _after_ ranking to get optimally distinct colors by default
      val colorMap = autoColorMap(qualityModel.rankedEntities.map(_.id))

      AppModel(config, analysis, config.defaultWeights, entitySelectionModel, subCriteriaSelectionModel, colorMap, qualityModel, profileModel)
    }

  }

  final case class QualityModel(rankedEntities: Seq[GroupedEntity], criteria: Seq[GroupedCriteria], layout: QualityChartLayout)

  object QualityModel {

    def apply(analysis: Analysis, weights: Weights, layoutWidth: Int): QualityModel = {

      val allCriteria = analysis.criteria.map { c =>
        GroupedCriteria.forQuality(analysis.entities, c, analysis.reviews, weights)
      }
      val criteria = allCriteria.filter(_.subCriteria.nonEmpty)

      val rankedEntities = analysis.entities.map { e =>
        val value = median(criteria.flatMap(_.groupedValues(e)))
        GroupedEntity(e, value = value)
      }.sortBy(_.value).reverse

      val (minValue, maxValue) = safeMinMax(criteria)

      val layout = new QualityChartLayout(width = layoutWidth, criteria = criteria, minValueOption = minValue, maxValueOption = maxValue)

      QualityModel(rankedEntities, criteria, layout)
    }

  }

  final case class ProfileModel(sortedEntities: Seq[GroupedEntity], entitySortingStrategy: EntitySortingStrategy, criteria: Seq[GroupedCriteria], layout: ProfileChartLayout)

  object ProfileModel {

    def apply(analysis: Analysis, weights: Weights, layoutWidth: Int): ProfileModel = {

      val allCriteria = analysis.criteria.map { c =>
        GroupedCriteria.forProfile(analysis.entities, c, analysis.reviews, weights)
      }
      val criteria = allCriteria.filter(_.subCriteria.nonEmpty)

      // TODO: Support different sorting strategies
      val sortedEntities = analysis.entities.map { e =>
        val value = median(criteria.flatMap(_.groupedValues(e)))
        GroupedEntity(e, value = value)
      }.sortBy(_.name)

      val (minValue, maxValue) = safeMinMax(criteria)

      val layout = new ProfileChartLayout(width = layoutWidth, criteria = criteria, minValueOption = minValue, maxValueOption = maxValue)

      ProfileModel(sortedEntities, ByAlphabetEntitySortingStrategy, criteria, layout)
    }

  }

  trait EntitySortingStrategy
  case object ByAlphabetEntitySortingStrategy extends EntitySortingStrategy
  final case class ByCriteriaEntitySortingStrategy(criteria: Criteria)

  final case class GroupedEntity(id: Entity, value: Option[Double] = None) {
    def name: String = id.name
  }

  final case class GroupedCriteria(id: Criteria, subCriteria: Seq[GroupedSubCriteria], groupedValues: Map[Entity, Option[Double]]) {
    def name: String = id.name

    // Deliberately not using min/max of groupedValues for our purpose
    val minValue: Option[Double] = safeMinMax(subCriteria.map(_.minValue))._1
    val maxValue: Option[Double] = safeMinMax(subCriteria.map(_.maxValue))._2
  }

  object GroupedCriteria {

    def forQuality(entities: Seq[Entity], criteria: Criteria, reviews: Seq[Review], weights: Weights): GroupedCriteria = {

      val allSubCriteria = criteria.subCriteria.map(sc => GroupedSubCriteria(entities, sc, reviews, weights.enabledIndicators))
      val nonEmptySubCriteria = allSubCriteria.filter(_.indicators.nonEmpty)
      val qualitySubCriteria = nonEmptySubCriteria.filter { sc =>
        weights.subCriteriaWeights(sc.id) match {
          case Quality(_) => true
          case _ => false
        }
      }

      val groupedValue = (entity: Entity) => {
        val valuesWithWeights = for {
          subCriterion <- qualitySubCriteria
          value <- subCriterion.groupedValues(entity)
          weight = weights.subCriteriaWeights(subCriterion.id)
          weightValue <- weight match {
            case Quality(wv) => Some(wv)
            case _ => None
          }
        } yield {
          (value, weightValue)
        }
        weightedMedian(valuesWithWeights)
      }

      GroupedCriteria(entities, criteria, qualitySubCriteria, groupedValue)

    }

    def forProfile(entities: Seq[Entity], criteria: Criteria, reviews: Seq[Review], weights: Weights): GroupedCriteria = {

      val allSubCriteria = criteria.subCriteria.map(sc => GroupedSubCriteria(entities, sc, reviews, weights.enabledIndicators))
      val nonEmptySubCriteria = allSubCriteria.filter(_.indicators.nonEmpty)
      val profileSubCriteria = nonEmptySubCriteria.filter { sc =>
        weights.subCriteriaWeights(sc.id) == Profile
      }

      val groupedValue = (entity: Entity) => {
        val valuesWithWeights = for {
          subCriterion <- profileSubCriteria
          value <- subCriterion.groupedValues(entity)
        } yield {
          (value, 1d) // no weighting for profile chart
        }
        weightedMedian(valuesWithWeights)
      }

      GroupedCriteria(entities, criteria, profileSubCriteria, groupedValue)

    }

    private def apply(entities: Seq[Entity], criteria: Criteria, subCriteria: Seq[GroupedSubCriteria], groupedValue: Entity => Option[Double]): GroupedCriteria = {
      val groupedValues = entities.map(e => e -> groupedValue(e)).toMap
      GroupedCriteria(criteria, subCriteria, groupedValues)
    }

  }

  final case class GroupedSubCriteria(id: SubCriteria, groupedValues: Map[Entity, Option[Double]], indicators: Seq[GroupedIndicator]) {
    def name: String = id.name

    val (minValue, maxValue) = safeMinMax(groupedValues.values)
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
    def name: String = id.name
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

  private[model] def safeMinMax(criteria: Seq[GroupedCriteria]): (Option[Double], Option[Double]) = {
    if (criteria.isEmpty) {
      (None, None)
    } else {
      (criteria.map(_.minValue).min, criteria.map(_.maxValue).max)
    }
  }

  private[model] def safeMinMax(optionalValues: Iterable[Option[Double]]): (Option[Double], Option[Double]) = {
    val values = optionalValues.flatten
    if (values.isEmpty) {
      (None, None)
    } else {
      (Some(values.min), Some(values.max))
    }
  }

}