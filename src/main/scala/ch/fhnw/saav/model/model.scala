package ch.fhnw.saav.model

import ch.fhnw.saav.model.model.Entity.{Organisation, Person, Project}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object model {

  trait Analysis[E <: Entity] {

    def categories: Seq[Category]

    def entities: Seq[E]

    def reviews: Seq[Review]

    def value(entity: E, indicator: Indicator, review: Review): Option[Double]

    def groupedValue(entity: E, indicator: Indicator): Option[Double]

    def groupedValue(entity: E, subCategory: SubCategory): Option[Double]

    def groupedValue(entity: E, category: Category): Option[Double]

  }

  trait Category {
    def name: String
    def subCategories: Seq[SubCategory]
  }

  trait SubCategory {
    def name: String
    def indicators: Seq[Indicator]
  }

  trait Indicator {
    def name: String
  }

  case class Review(name: String)

  abstract class Entity(name: String)

  object Entity {

    final case class Project(name: String) extends Entity(name)

    final case class Person(name: String) extends Entity(name)

    final case class Organisation(name: String) extends Entity(name)

  }

  object AnalysisBuilder {
    def projectAnalysisBuilder = AnalysisBuilder[Project]()

    def personAnalysisBuilder = AnalysisBuilder[Person]()

    def organisationAnalysisBuilder = AnalysisBuilder[Organisation]()
  }

  case class AnalysisBuilder[E <: Entity]() {

    private val categories: ListBuffer[MutableCategoryScope] = ListBuffer()

    def category(categoryName: String): CategoryScope = {
      val existingCategory = categories.find(_.name == categoryName)
      existingCategory match {
        case Some(c) => c
        case None =>
          val category = new MutableCategoryScope(categoryName)
          categories.append(category)
          category
      }
    }

    def build(): Analysis[E] = {

      val valuesByIndicator: Map[Indicator, Map[(E, Review), Double]] = (for {
        category <- categories
        subCategory <- category.subCategories
        indicator <- subCategory.indicators
      } yield {
        indicator -> indicator.values.toMap
      }).toMap

      // gather unique entities/reviews used throughout complete analysis
      val uniqueEntities = valuesByIndicator.values.flatMap(valueMap => valueMap.keys.map(_._1)).toSet.toSeq
      val uniqueReviews = valuesByIndicator.values.flatMap(valueMap => valueMap.keys.map(_._2)).toSet.toSeq

      new AnalysisImpl[E](categories, uniqueEntities, uniqueReviews, valuesByIndicator)
    }

    trait CategoryScope extends Category {
      def subCategory(name: String): SubCategoryScope
    }

    private class MutableCategoryScope(val name: String, val subCategories: ListBuffer[MutableSubCategoryScope] = ListBuffer()) extends CategoryScope {
      override def subCategory(subCategoryName: String): SubCategoryScope = {
        val existingSubCategory = subCategories.find(_.name == subCategoryName)
        existingSubCategory match {
          case Some(c) => c
          case None =>
            val subCategory = new MutableSubCategoryScope(subCategoryName)
            subCategories.append(subCategory)
            subCategory
        }
      }
    }

    trait SubCategoryScope extends SubCategory {
      def indicator(name: String): IndicatorScope
    }

    private class MutableSubCategoryScope(val name: String, val indicators: ListBuffer[MutableIndicatorScope] = ListBuffer()) extends SubCategoryScope {
      override def indicator(indicatorName: String): IndicatorScope = {
        val existingIndicator = indicators.find(_.name == indicatorName)
        existingIndicator match {
          case Some(i) => i
          case None =>
            val indicator = new MutableIndicatorScope(indicatorName)
            indicators.append(indicator)
            indicator
        }
      }
    }

    trait IndicatorScope extends Indicator {
      def addValue(entity: E, review: Review, value: Double): AnalysisBuilder[E]
    }

    private class MutableIndicatorScope(val name: String, val values: mutable.LinkedHashMap[(E, Review), Double] = mutable.LinkedHashMap()) extends IndicatorScope {
      override def addValue(entity: E, review: Review, value: Double): AnalysisBuilder[E] = {
        values.put((entity, review), value)
        AnalysisBuilder.this
      }
    }

  }

  private class AnalysisImpl[E <: Entity](val categories: Seq[Category], val entities: Seq[E], val reviews: Seq[Review], private val valuesByIndicator: Map[Indicator, Map[(E, Review), Double]]) extends Analysis[E] {

    override def value(entity: E, indicator: Indicator, review: Review): Option[Double] = {
      valuesByIndicator.get(indicator) match {
        case None => None
        case Some(values) => values.get((entity, review))
      }
    }

    override def groupedValue(e: E, indicator: Indicator): Option[Double] = ???

    override def groupedValue(e: E, subCategory: SubCategory): Option[Double] = ???

    override def groupedValue(e: E, category: Category): Option[Double] = ???
  }

}
