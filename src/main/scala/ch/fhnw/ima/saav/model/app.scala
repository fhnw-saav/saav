package ch.fhnw.ima.saav
package model

import ch.fhnw.ima.saav.model.color._
import ch.fhnw.ima.saav.model.domain._

import scala.collection.immutable.ListSet

/** Application models (incl. presentation state). */
object app {

  case class SaavModel(model: Either[NoDataModel, DataModel] = Left(NoDataModel(ImportNotStarted())))

  case class NoDataModel(importState: ImportState)

  sealed trait ImportState

  final case class ImportNotStarted() extends ImportState

  final case class ImportInProgress(progress: Float) extends ImportState

  final case class ImportFailed(throwable: Throwable) extends ImportState

  case class DataModel(
    analysis: Analysis[Entity],
    colors: Map[Entity, WebColor] = Map().withDefaultValue(DefaultColor),
    selectedEntities: ListSet[Entity],
    pinnedEntity: Option[Entity] = None
  )

  /** Work in Progress */

  class PlottableProfileDataModel(private val dataModel: DataModel, private val weights: Weights) {

    val categories: Seq[PlottableCategory] = dataModel.analysis.categories.map { c =>
      new PlottableCategory(c, dataModel.analysis.reviews, weights)
    }

    val rankedEntities: Seq[PlottableEntity] = dataModel.analysis.entities.map { e =>
      val dm = dataModel
      val selected = dm.selectedEntities
      val pinned = dm.pinnedEntity
      val colors = dm.colors
      val value = median(categories.flatMap(_.groupedValue(e)))
      PlottableEntity(e, selected.contains(e), colors(e), pinned.contains(e), value)
    }.sortBy(_.value)

  }

  case class PlottableEntity(entity: Entity, isSelected: Boolean, color: WebColor, isPinned: Boolean, value: Option[Double])

  class PlottableSubCategory(private[app] val subCategory: SubCategory[Entity], private val reviews: Seq[Review], private val disabledIndicators: Set[Indicator[_]]) {

    val name = subCategory.name

    val indicators = subCategory.indicators.filter(!disabledIndicators.contains(_))

    def groupedValue(entity: Entity): Option[Double] = {
      val values = for {
        review <- reviews
        indicator <- indicators
        values <- indicator.values.get((entity, review))
      } yield values
      median(values)
    }

  }

  class PlottableCategory(category: Category[Entity], reviews: Seq[Review], weights: Weights) {

    val name = category.name

    val subCategories = category.subCategories.map(sc => new PlottableSubCategory(sc, reviews, weights.disabledIndicators))

    def groupedValue(entity: Entity): Option[Double] = {
      val valuesWithWeights = for {
        subCategory <- subCategories
        value <- subCategory.groupedValue(entity)
        weight = weights.subCategoryWeights.getOrElse(subCategory.subCategory, Quality(1f))
        weightValue <- weight match {
          case Quality(wv) => Some(wv)
          case _ => None
        }
      } yield {
        (value, weightValue)
      }
      weightedMedian(valuesWithWeights)
    }

  }

  sealed trait Weight

  final case class Quality(weight: Double) extends Weight

  case object Profile extends Weight

  final case class Weights(subCategoryWeights: Map[SubCategory[_], Weight] = Map(), disabledIndicators: Set[Indicator[_]] = Set.empty)

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