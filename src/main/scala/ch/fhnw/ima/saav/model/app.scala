package ch.fhnw.ima.saav
package model

import ch.fhnw.ima.saav.model.color._
import ch.fhnw.ima.saav.model.domain._

/** Application models (incl. presentation state). */
object app {

  case class SaavModel(model: Either[NoDataModel, PlottableQualityDataModel] = Left(NoDataModel(ImportNotStarted())))

  case class NoDataModel(importState: ImportState)

  sealed trait ImportState

  final case class ImportNotStarted() extends ImportState

  final case class ImportInProgress(progress: Float) extends ImportState

  final case class ImportFailed(throwable: Throwable) extends ImportState

  final case class PlottableQualityDataModel(rankedEntities: Seq[PlottableEntity], categories: Seq[PlottableCategory]) {

    def updateWeights(analysis: Analysis[Entity], weights: Weights): PlottableQualityDataModel = {

      val entityMap: Map[Entity, PlottableEntity] = rankedEntities.map(e => e.id -> e).toMap
      val newModel = PlottableQualityDataModel(analysis, weights)

      val newRankedEntities = newModel.rankedEntities.map { e =>
        val template = entityMap(e.id)
        e.copy(color = template.color, isSelected = template.isSelected, isPinned = template.isPinned)
      }

      newModel.copy(rankedEntities = newRankedEntities)

    }

  }

  object PlottableQualityDataModel {

    def apply(analysis: Analysis[Entity], weights: Weights = Weights()): PlottableQualityDataModel = {

      val categories = analysis.categories.map { c =>
        PlottableCategory(analysis.entities, c, analysis.reviews, weights)
      }

      val rankedEntities = analysis.entities.map { e =>
        val value = median(categories.flatMap(_.groupedValues(e)))
        PlottableEntity(e, value = value)
      }.sortBy(_.value).reverse

      // colorize _after_ ranking to get optimally distinct colors
      val colors = autoColorMap(rankedEntities)
      val rankedAndAutoColoredEntities = rankedEntities.map(e => e.copy(color = colors(e)))

      PlottableQualityDataModel(rankedAndAutoColoredEntities, categories)
    }

  }

  final case class PlottableEntity(id: Entity, isSelected: Boolean = true, color: WebColor = DefaultColor, isPinned: Boolean = false, value: Option[Double] = None) {
    def name = id.name
  }

  final case class PlottableSubCategory(id: SubCategory[Entity], groupedValues: Map[Entity, Option[Double]], indicators: Seq[Indicator[Entity]]) {
    def name = id.name
  }

  object PlottableSubCategory {

    def apply(entities: Seq[Entity], subCategory: SubCategory[Entity], reviews: Seq[Review], disabledIndicators: Set[Indicator[Entity]]): PlottableSubCategory = {
      val indicators = subCategory.indicators.filter(!disabledIndicators.contains(_))

      def groupedValue(entity: Entity): Option[Double] = {
        val values = for {
          review <- reviews
          indicator <- indicators
          values <- indicator.values.get((entity, review))
        } yield values
        median(values)
      }

      val groupedValues = entities.map(e => e -> groupedValue(e)).toMap

      PlottableSubCategory(subCategory, groupedValues, indicators)
    }

  }

  final case class PlottableCategory(name: String, subCategories: Seq[PlottableSubCategory], groupedValues: Map[Entity, Option[Double]])

  object PlottableCategory {

    def apply(entities: Seq[Entity], category: Category[Entity], reviews: Seq[Review], weights: Weights): PlottableCategory = {

      val subCategories = category.subCategories.map(sc => PlottableSubCategory(entities, sc, reviews, weights.disabledIndicators))

      def groupedValue(entity: Entity): Option[Double] = {
        val valuesWithWeights = for {
          subCategory <- subCategories
          value <- subCategory.groupedValues(entity)
          weight = weights.subCategoryWeights.getOrElse(subCategory.id, Quality(1f))
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

      PlottableCategory(category.name, subCategories, groupedValues)

    }

  }

  sealed trait Weight

  final case class Quality(weight: Double) extends Weight

  case object Profile extends Weight

  final case class Weights(subCategoryWeights: Map[SubCategory[Entity], Weight] = Map(), disabledIndicators: Set[Indicator[Entity]] = Set.empty)

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