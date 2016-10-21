package ch.fhnw.ima.saav
package model

import ch.fhnw.ima.saav.model.color._
import ch.fhnw.ima.saav.model.config.Config
import ch.fhnw.ima.saav.model.domain._
import ch.fhnw.ima.saav.model.layout.{ProfileChartLayout, QualityChartLayout}
import ch.fhnw.ima.saav.model.weight._

/**
 * Application model classes complement the domain model with grouped values (medians) and presentation state.
 */
object app {

  // --------------------------------------------------------------------------
  // Algebraic data types expressing model import flow
  // --------------------------------------------------------------------------

  case class SaavModel(model: Either[NoDataAppModel, AppModel] = Left(NoDataAppModel(ImportNotStarted())))

  case class NoDataAppModel(importState: ImportState)

  sealed trait ImportState

  final case class ImportNotStarted() extends ImportState

  final case class ImportInProgress(progress: Float) extends ImportState

  final case class ImportFailed(throwable: Throwable) extends ImportState

  // --------------------------------------------------------------------------
  // Actual application model (available once import is complete)
  // --------------------------------------------------------------------------

  final case class AppModel(
    config: Config,
    analysis: Analysis,
    weights: Weights,
    entitySelectionModel: EntitySelectionModel,
    subCriteriaSelectionModel: SubCriteriaSelectionModel,
    colorMap: Map[EntityId, WebColor],
    qualityModel: QualityModel,
    profileModel: ProfileModel
  )

  object AppModel {

    def apply(analysis: Analysis, config: Config): AppModel = {

      val defaultLayoutWidth = 1000
      val qualityModel = QualityModel(analysis, config.defaultWeights, defaultLayoutWidth)
      val profileModel = ProfileModel(analysis, config.defaultWeights, defaultLayoutWidth)
      val entitySelectionModel = EntitySelectionModel(analysis.entities.map(_.id).toSet, None)
      val subCriteriaSelectionModel = SubCriteriaSelectionModel()

      // colorize _after_ ranking to get optimally distinct colors by default
      val colorMap = autoColorMap(qualityModel.rankedEntities.map(_.id))

      AppModel(config, analysis, config.defaultWeights, entitySelectionModel, subCriteriaSelectionModel, colorMap, qualityModel, profileModel)
    }

  }

  // --------------------------------------------------------------------------
  // Selection Models
  // --------------------------------------------------------------------------

  final case class EntitySelectionModel(visible: Set[EntityId] = Set.empty, pinned: Option[EntityId] = None)

  final case class SubCriteriaSelectionModel(hovered: Option[SubCriteriaId] = None)

  // --------------------------------------------------------------------------
  // Presentation model behind the 'Quality' tab
  // --------------------------------------------------------------------------

  final case class QualityModel(rankedEntities: Seq[GroupedEntity], criteria: Seq[GroupedCriteria], layout: QualityChartLayout)

  object QualityModel {

    def apply(analysis: Analysis, weights: Weights, layoutWidth: Int): QualityModel = {

      val allCriteria = analysis.criteria.map { c =>
        GroupedCriteria.forQuality(analysis.entities.map(_.id), c, weights)
      }
      val criteria = allCriteria.filter(_.subCriteria.nonEmpty)

      val rankedEntities = analysis.entities.zipWithIndex.map { case (e, i) =>
        val value = median(criteria.flatMap(_.groupedValues.get(e.id)))
        GroupedEntity(e.id, e.displayName, value = value, sortingPosition = i)
      }.sortBy(_.value).reverse

      val (minValue, maxValue) = safeMinMax(criteria)

      val layout = new QualityChartLayout(width = layoutWidth, criteria = criteria, minValueOption = minValue, maxValueOption = maxValue)

      QualityModel(rankedEntities, criteria, layout)
    }

  }

  // --------------------------------------------------------------------------
  // Presentation model behind the 'Profile' tab
  // --------------------------------------------------------------------------

  final case class ProfileModel(sortedEntities: Seq[GroupedEntity], entitySortingStrategy: EntitySortingStrategy, criteria: Seq[GroupedCriteria], layout: ProfileChartLayout)

  object ProfileModel {

    def apply(analysis: Analysis, weights: Weights, layoutWidth: Int): ProfileModel = {

      val allCriteria = analysis.criteria.map { c =>
        GroupedCriteria.forProfile(analysis.entities.map(_.id), c, weights)
      }
      val criteria = allCriteria.filter(_.subCriteria.nonEmpty)

      // TODO: Support different sorting strategies
      val sortedEntities = analysis.entities.zipWithIndex.map { case (e, i) =>
        val value = median(criteria.flatMap(_.groupedValues.get(e.id)))
        GroupedEntity(e.id, e.displayName, value = value, sortingPosition = i)
      }.sortBy(_.displayName)

      val (minValue, maxValue) = safeMinMax(criteria)

      val layout = new ProfileChartLayout(width = layoutWidth, criteria = criteria, minValueOption = minValue, maxValueOption = maxValue)

      ProfileModel(sortedEntities, ByAlphabetEntitySortingStrategy, criteria, layout)
    }

  }

  trait EntitySortingStrategy

  case object ByAlphabetEntitySortingStrategy extends EntitySortingStrategy

  final case class ByCriteriaEntitySortingStrategy(criteria: Criteria)

  final case class GroupedEntity(id: EntityId, displayName: String, value: Option[Double], sortingPosition: Int)

  // --------------------------------------------------------------------------
  // GroupedXXX classes enrich the plain domain classes with grouped values
  // --------------------------------------------------------------------------

  final case class GroupedCriteria(id: CriteriaId, displayName: String, subCriteria: Seq[GroupedSubCriteria], groupedValues: Map[EntityId, Double]) {

    // Deliberately not using min/max of groupedValues for our purpose
    val minValue: Option[Double] = safeMinMax(subCriteria.flatMap(_.minValue))._1
    val maxValue: Option[Double] = safeMinMax(subCriteria.flatMap(_.maxValue))._2

  }

  object GroupedCriteria {

    def forQuality(entities: Seq[EntityId], criteria: Criteria, weights: Weights): GroupedCriteria = {

      val allSubCriteria = criteria.subCriteria.map(sc => GroupedSubCriteria(entities, sc, weights.enabledIndicators))
      val nonEmptySubCriteria = allSubCriteria.filter(_.indicators.nonEmpty)
      val qualitySubCriteria = nonEmptySubCriteria.filter { sc =>
        weights.subCriteriaWeights(sc.id) match {
          case Quality(_) => true
          case _ => false
        }
      }

      val groupedValue = (entity: EntityId) => {
        val valuesWithWeights = for {
          subCriterion <- qualitySubCriteria
          value <- subCriterion.groupedValues.get(entity)
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

    def forProfile(entities: Seq[EntityId], criteria: Criteria, weights: Weights): GroupedCriteria = {

      val allSubCriteria = criteria.subCriteria.map(sc => GroupedSubCriteria(entities, sc, weights.enabledIndicators))
      val nonEmptySubCriteria = allSubCriteria.filter(_.indicators.nonEmpty)
      val profileSubCriteria = nonEmptySubCriteria.filter { sc =>
        weights.subCriteriaWeights(sc.id) == Profile
      }

      val groupedValue = (entity: EntityId) => {
        val valuesWithWeights = for {
          subCriterion <- profileSubCriteria
          value <- subCriterion.groupedValues.get(entity)
        } yield {
          (value, 1d) // no weighting for profile chart
        }
        weightedMedian(valuesWithWeights)
      }

      GroupedCriteria(entities, criteria, profileSubCriteria, groupedValue)

    }

    private def apply(entities: Seq[EntityId], criteria: Criteria, subCriteria: Seq[GroupedSubCriteria], groupedValue: EntityId => Option[Double]): GroupedCriteria = {
      val groupedValues = (for {
        entity <- entities
        groupedValue <- groupedValue(entity)
      } yield entity -> groupedValue).toMap
      GroupedCriteria(criteria.id, criteria.displayName, subCriteria, groupedValues)
    }

  }

  final case class GroupedSubCriteria(id: SubCriteriaId, displayName: String, groupedValues: Map[EntityId, Double], indicators: Seq[GroupedIndicator]) {
    val (minValue, maxValue) = safeMinMax(groupedValues.values)
  }

  object GroupedSubCriteria {

    def apply(entities: Seq[EntityId], subCriteria: SubCriteria, enabledIndicators: Set[IndicatorId]): GroupedSubCriteria = {

      val indicators = subCriteria.indicators.filter(i => enabledIndicators.contains(i.id)).map { i =>
        GroupedIndicator(i)
      }

      val groupedValues = (for {
        entity <- entities
        values = indicators.flatMap(i => i.groupedValues.get(entity))
        groupedValue <- median(values)
      } yield {
        entity -> groupedValue
      }
        ).toMap

      GroupedSubCriteria(subCriteria.id, subCriteria.displayName, groupedValues, indicators)
    }

  }

  final case class GroupedIndicator(id: IndicatorId, displayName: String, groupedValues: Map[EntityId, Double])

  object GroupedIndicator {

    def apply(indicator: Indicator): GroupedIndicator = {

      val (entities, reviews) = indicator.values.keys.unzip
      val groupedValues = (
        for {
          entity <- entities
          values = reviews.flatMap(r => indicator.values.get((entity, r))).toSeq
          groupedValue <- median(values)
        } yield {
          entity -> groupedValue
        }
        ).toMap


      GroupedIndicator(indicator.id, indicator.displayName, groupedValues)
    }

  }

  // --------------------------------------------------------------------------
  // Utility methods
  // --------------------------------------------------------------------------

  private[model] def safeMinMax(criteria: Seq[GroupedCriteria]): (Option[Double], Option[Double]) = {
    if (criteria.isEmpty) {
      (None, None)
    } else {
      (criteria.map(_.minValue).min, criteria.map(_.maxValue).max)
    }
  }

  private[model] def safeMinMax(values: Iterable[Double]): (Option[Double], Option[Double]) = {
    if (values.isEmpty) {
      (None, None)
    } else {
      (Some(values.min), Some(values.max))
    }
  }

}