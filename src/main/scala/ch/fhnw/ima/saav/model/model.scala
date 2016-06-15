package ch.fhnw.ima.saav.model

import ch.fhnw.ima.saav.model.model.Entity.{Organisation, Person, Project}
import scala.collection.immutable.Seq // Seq in Predef is mutable (http://bit.ly/261xOxR)

object model {

  final case class Analysis[E <: Entity](categories: Seq[Category], entities: Seq[E], reviews: Seq[Review], private val valuesByIndicator: Map[Indicator, Map[(E, Review), Double]]) {

    def value(entity: E, indicator: Indicator, review: Review): Option[Double] = {
      valuesByIndicator.get(indicator) match {
        case None => None
        case Some(values) => values.get((entity, review))
      }
    }

    def groupedValue(entity: E, indicator: Indicator): Option[Double] = {
      valuesByIndicator.get(indicator) match {
        case None => None
        case Some(valueMap) =>
          val values = reviews.flatMap(review => valueMap.get((entity, review)))
          median(values)
      }
    }

    def groupedValue(entity: E, subCategory: SubCategory): Option[Double] = {
      val values = subCategory.indicators.flatMap(groupedValue(entity, _))
      median(values)
    }

    def groupedValue(entity: E, category: Category): Option[Double] = {
      val values = category.subCategories.flatMap(groupedValue(entity, _))
      median(values)
    }

    private def median(values: Seq[Double]) = {
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

  final case class Category(name: String, subCategories: Seq[SubCategory])

  final case class SubCategory(name: String, indicators: Seq[Indicator])

  final case class Indicator(name: String)

  final case class Review(name: String)

  sealed abstract class Entity(name: String)

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

  final case class AnalysisBuilder[E <: Entity]() {

    private var categoryScopes: Seq[CategoryScopeImpl] = Seq()
    private var entities: Seq[E] = Seq()
    private var reviews: Seq[Review] = Seq()

    def category(categoryName: String): CategoryScope = {
      val existingCategory = categoryScopes.find(_.name == categoryName)
      existingCategory match {
        case Some(c) => c
        case None =>
          val categoryScope = new CategoryScopeImpl(categoryName)
          categoryScopes :+= categoryScope
          categoryScope
      }
    }

    def build(): Analysis[E] = {
      val categories = categoryScopes.map(_.toCategory)
      val valuesByIndicator = categoryScopes.flatMap(_.valuesByIndicator).toMap
      Analysis[E](categories, entities.distinct, reviews.distinct, valuesByIndicator)
    }

    trait CategoryScope {
      def subCategory(name: String): SubCategoryScope
    }

    private class CategoryScopeImpl(val name: String, var subCategoryScopes: Seq[SubCategoryScopeImpl] = Seq()) extends CategoryScope {

      override def subCategory(subCategoryName: String): SubCategoryScope = {
        val existing = subCategoryScopes.find(_.name == subCategoryName)
        existing match {
          case Some(c) => c
          case None =>
            val subCategoryScope = new SubCategoryScopeImpl(subCategoryName)
            subCategoryScopes :+= subCategoryScope
            subCategoryScope
        }
      }

      def toCategory = Category(name, subCategoryScopes.map(_.toSubCategory))

      def valuesByIndicator = subCategoryScopes.flatMap(_.valuesByIndicator).toMap

    }

    trait SubCategoryScope {
      def indicator(name: String): IndicatorScope
    }

    private class SubCategoryScopeImpl(val name: String, var indicatorScopes: Seq[IndicatorScopeImpl] = Seq()) extends SubCategoryScope {

      override def indicator(indicatorName: String): IndicatorScope = {
        val existing = indicatorScopes.find(_.name == indicatorName)
        existing match {
          case Some(i) => i
          case None =>
            val indicatorScope = new IndicatorScopeImpl(indicatorName)
            indicatorScopes :+= indicatorScope
            indicatorScope
        }
      }

      def toSubCategory = SubCategory(name, indicatorScopes.map(_.toIndicator))

      def valuesByIndicator = indicatorScopes.flatMap(_.valuesByIndicator).toMap

    }

    trait IndicatorScope {

      def addValue(entity: E, review: Review, value: Double): IndicatorScope

      // exposing this makes testing easier; it is safe, as indicators only wrap a name, thus no scope state is leaking
      def toIndicator: Indicator

    }

    private class IndicatorScopeImpl(val name: String, var values: Map[(E, Review), Double] = Map()) extends IndicatorScope {

      override def addValue(entity: E, review: Review, value: Double): IndicatorScope = {
        values += (entity, review) -> value
        // track entities and reviews in insertion order
        entities :+= entity
        reviews :+= review
        this
      }

      override def toIndicator = Indicator(name)

      def valuesByIndicator = Map(toIndicator -> values)

    }

  }

}
