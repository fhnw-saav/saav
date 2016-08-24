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
    rankedEntities: Seq[PlottableEntity],
    categories: Seq[PlottableCategory],
    selectionModel: EntitySelectionModel,
    colorMap: Map[Entity, WebColor]
  ) {

    def updateWeights(analysis: Analysis, weights: Weights): DataModel = {

      // create new model to trigger value calculation with new weights
      val newModel = DataModel(analysis, weights)

      // keep selections and colors of current model
      newModel.copy(selectionModel = selectionModel, colorMap = colorMap)

    }

  }

  object DataModel {

    def apply(analysis: Analysis, weights: Weights = Weights()): DataModel = {

      val categories = analysis.categories.map { c =>
        PlottableCategory(analysis.entities, c, analysis.reviews, weights)
      }

      val rankedEntities = analysis.entities.map { e =>
        val value = median(categories.flatMap(_.groupedValues(e)))
        PlottableEntity(e, value = value)
      }.sortBy(_.value).reverse

      val selectionModel = EntitySelectionModel(analysis.entities.toSet, None)

      // colorize _after_ ranking to get optimally distinct colors
      val colorMap = autoColorMap(rankedEntities.map(_.id))

      DataModel(rankedEntities, categories, selectionModel, colorMap)
    }

  }

  final case class PlottableEntity(id: Entity, value: Option[Double] = None) {
    def name = id.name
  }

  final case class PlottableSubCategory(id: SubCategory, groupedValues: Map[Entity, Option[Double]], indicators: Seq[Indicator]) {
    def name = id.name
  }

  object PlottableSubCategory {

    def apply(entities: Seq[Entity], subCategory: SubCategory, reviews: Seq[Review], disabledIndicators: Set[Indicator]): PlottableSubCategory = {
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

    def apply(entities: Seq[Entity], category: Category, reviews: Seq[Review], weights: Weights): PlottableCategory = {

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

  final case class Weights(subCategoryWeights: Map[SubCategory, Weight] = Map(), disabledIndicators: Set[Indicator] = Set.empty)

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