package ch.fhnw.ima.saav
package model

import ch.fhnw.ima.saav.model.color._
import ch.fhnw.ima.saav.model.config.Config
import ch.fhnw.ima.saav.model.domain._
import ch.fhnw.ima.saav.model.layout.{ProfileChartLayout, QualityChartLayout}
import ch.fhnw.ima.saav.model.weight._

/**
 * Application model classes complement the domain model with grouped values (means) and presentation state.
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
  // Algebraic data types expressing PDF export flow
  // --------------------------------------------------------------------------

  sealed trait PdfExport
  final case object PdfExportDialogHidden extends PdfExport
  final case object PdfExportDialogVisible extends PdfExport
  final case object PdfExportInProgress extends PdfExport

  // --------------------------------------------------------------------------
  // Actual application model (available once import is complete)
  // --------------------------------------------------------------------------

  final case class AppModel(
    config: Config,
    analysis: Analysis,
    qualityModel: QualityModel,
    profileModel: ProfileModel,
    expertConfig: ExpertConfig,
    entitySelectionModel: EntitySelectionModel,
    subCriteriaSelectionModel: SubCriteriaSelectionModel,
    colorMap: Map[EntityId, WebColor],
    pdfExport: PdfExport = PdfExportDialogHidden
  )

  object AppModel {

    def apply(analysis: Analysis, config: Config): AppModel = {

      val defaultLayoutWidth = 1000
      val qualityModel = QualityModel(analysis, config.defaultWeights, defaultLayoutWidth)
      val profileModel = ProfileModel(analysis, config.defaultWeights, config.nonAggregatableCriteria, defaultLayoutWidth)
      val expertConfig = ExpertConfig(visibility = ExpertConfigHidden, config.defaultWeights, config.defaultWeights)

      val entitySelectionModel = EntitySelectionModel(analysis.entities.map(_.id).toSet, None)
      val subCriteriaSelectionModel = SubCriteriaSelectionModel()

      // colorize _after_ ranking to get optimally distinct colors by default
      val colorMap = qualityModel.rankedEntities.zipWithIndex.map {
        case (e, i) => e.id -> ColorPalette(i % ColorPalette.size)
      }.toMap

      AppModel(config, analysis, qualityModel, profileModel, expertConfig, entitySelectionModel, subCriteriaSelectionModel, colorMap)
    }

  }

  // --------------------------------------------------------------------------
  // Selection Models
  // --------------------------------------------------------------------------

  final case class EntitySelectionModel(visible: Set[EntityId] = Set.empty, pinned: Option[EntityId] = None, hovered: Option[EntityId] = None)

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

      // calculate mean values and sort entity/value pairs in descending value order
      val sortedEntityValuePairs = analysis.entities.map { entity =>
        val value = mean(criteria.flatMap(_.groupedValues.get(entity.id)))
        (entity, value)
      }.reverse.sortBy(_._2).reverse // first `reverse` assures that ties appear in import order, second `reverse` for descending values

      // calculate rank (identical ranks for ties)
      val distinctValues = sortedEntityValuePairs.unzip._2.distinct
      val ranks = distinctValues.zipWithIndex.toMap
      val rankedEntities = for ((entity, value) <- sortedEntityValuePairs) yield {
        val rank = ranks(value)
        GroupedEntity(entity.id, entity.displayName, value = value, position = rank)
      }

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

    def apply(analysis: Analysis, weights: Weights, nonAggregatableCriteria: Set[CriteriaId], layoutWidth: Int): ProfileModel = {

      val allCriteria = analysis.criteria.map { c =>
        val aggregatable = !nonAggregatableCriteria.contains(c.id)
        GroupedCriteria.forProfile(analysis.entities.map(_.id), c, weights, aggregatable)
      }
      val criteria = allCriteria.filter(_.subCriteria.nonEmpty)

      // TODO: Support different sorting strategies
      val sortedEntities = analysis.entities.zipWithIndex.map { case (e, i) =>
        val value = mean(criteria.flatMap(_.groupedValues.get(e.id)))
        GroupedEntity(e.id, e.displayName, value = value, position = i)
      }.sortBy(_.displayName)

      val (minValue, maxValue) = safeMinMax(criteria)

      val layout = new ProfileChartLayout(width = layoutWidth, criteria = criteria, minValueOption = minValue, maxValueOption = maxValue)

      ProfileModel(sortedEntities, ByAlphabetEntitySortingStrategy, criteria, layout)
    }

  }

  trait EntitySortingStrategy

  case object ByAlphabetEntitySortingStrategy extends EntitySortingStrategy

  final case class ByCriteriaEntitySortingStrategy(criteria: Criteria)

  /**
    * Combines an entity with its fully aggregated value.
    *
    * @param position rank (quality) or sorting position (profile)
    */
  final case class GroupedEntity(id: EntityId, displayName: String, value: Option[Double], position: Int)

  // --------------------------------------------------------------------------
  // GroupedXXX classes enrich the plain domain classes with grouped values
  // --------------------------------------------------------------------------

  final case class GroupedCriteria(id: CriteriaId, displayName: String, aggregated: Boolean, subCriteria: Seq[GroupedSubCriteria], groupedValues: Map[EntityId, Double]) {

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
        weightedMean(valuesWithWeights)
      }

      val aggregatable = true // not relevant for quality
      GroupedCriteria(entities, criteria, aggregatable, qualitySubCriteria, groupedValue)

    }

    def forProfile(entities: Seq[EntityId], criteria: Criteria, weights: Weights, aggregatable: Boolean): GroupedCriteria = {

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
        weightedMean(valuesWithWeights)
      }

      GroupedCriteria(entities, criteria, aggregatable, profileSubCriteria, groupedValue)

    }

    private def apply(entities: Seq[EntityId], criteria: Criteria, aggregatable: Boolean, subCriteria: Seq[GroupedSubCriteria], groupedValue: EntityId => Option[Double]): GroupedCriteria = {
      val groupedValues = (for {
        entity <- entities
        groupedValue <- groupedValue(entity)
      } yield entity -> groupedValue).toMap
      GroupedCriteria(criteria.id, criteria.displayName, aggregatable, subCriteria, groupedValues)
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
        groupedValue <- mean(values)
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

      val (entities, _) = indicator.values.keySet.unzip

      val groupedValues = (
        for {
          entity <- entities
          // collect values across all reviews
          values = indicator.values.collect { case ((e, _), value) if e == entity => value }
          groupedValue <- mean(values.toSeq)
        } yield {
          entity -> groupedValue
        }
        ).toMap


      GroupedIndicator(indicator.id, indicator.displayName, groupedValues)
    }

  }

  // --------------------------------------------------------------------------
  // Expert Config
  // --------------------------------------------------------------------------

  sealed trait ExpertConfigVisibility
  case object ExpertConfigHidden extends ExpertConfigVisibility
  case object ExpertConfigVisible extends ExpertConfigVisibility

  final case class ExpertConfig(visibility: ExpertConfigVisibility, defaultWeights: Weights, actualWeights: Weights) {
    def isModified: Boolean = defaultWeights != actualWeights
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