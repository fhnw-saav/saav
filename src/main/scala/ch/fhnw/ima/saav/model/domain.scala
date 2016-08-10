package ch.fhnw.ima.saav
package model

import ch.fhnw.ima.saav.model.domain.Entity.{Organisation, Person, Project}

object domain {

  final case class Analysis[E <: Entity](categories: Seq[Category], entities: Seq[E], reviews: Seq[Review], valuesByIndicator: Map[Indicator, Map[(E, Review), Double]]) {

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

    def groupedValue(entity: E): Option[Double] = {
      val values = categories.flatMap(groupedValue(entity, _))
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

  sealed abstract class Entity() {
    def name: String
  }

  object Entity {

    final case class Project(name: String) extends Entity

    final case class Person(name: String) extends Entity

    final case class Organisation(name: String) extends Entity

  }

  object AnalysisBuilder {

    def projectAnalysisBuilder = AnalysisBuilder[Project]()

    def personAnalysisBuilder = AnalysisBuilder[Person]()

    def organisationAnalysisBuilder = AnalysisBuilder[Organisation]()

  }

  final case class AnalysisBuilder[E <: Entity]() {

    private var categoryBuilders: Seq[CategoryBuilderImpl] = Seq()
    private var entities: Seq[E] = Seq()
    private var reviews: Seq[Review] = Seq()

    def category(categoryName: String): CategoryBuilder = {
      val existing = categoryBuilders.find(_.name == categoryName)
      existing match {
        case Some(c) => c
        case None =>
          val categoryBuilder = new CategoryBuilderImpl(categoryName)
          categoryBuilders :+= categoryBuilder
          categoryBuilder
      }
    }

    def build: Analysis[E] = {
      val categories = categoryBuilders.map(_.toCategory)
      val valuesByIndicator = categoryBuilders.flatMap(_.valuesByIndicator).toMap
      Analysis[E](categories, entities.distinct, reviews.distinct, valuesByIndicator)
    }

    trait CategoryBuilder {
      def subCategory(name: String): SubCategoryBuilder
      def build: AnalysisBuilder[E]
    }

    private class CategoryBuilderImpl(val name: String, var subCategoryBuilders: Seq[SubCategoryBuilderImpl] = Seq()) extends CategoryBuilder {

      override def subCategory(subCategoryName: String): SubCategoryBuilder = {
        val existing = subCategoryBuilders.find(_.name == subCategoryName)
        existing match {
          case Some(c) => c
          case None =>
            val subCategoryBuilder = new SubCategoryBuilderImpl(this, subCategoryName)
            subCategoryBuilders :+= subCategoryBuilder
            subCategoryBuilder
        }
      }

      def toCategory = Category(name, subCategoryBuilders.map(_.toSubCategory))

      def valuesByIndicator = subCategoryBuilders.flatMap(_.valuesByIndicator).toMap

      override def build = AnalysisBuilder.this
    }

    trait SubCategoryBuilder {
      def indicator(name: String): IndicatorBuilder
      def build: CategoryBuilder
    }

    private class SubCategoryBuilderImpl(private val categoryBuilder: CategoryBuilder, val name: String, var indicatorBuilders: Seq[IndicatorBuilderImpl] = Seq()) extends SubCategoryBuilder {

      override def indicator(indicatorName: String): IndicatorBuilder = {
        val existing = indicatorBuilders.find(_.name == indicatorName)
        existing match {
          case Some(i) => i
          case None =>
            val indicatorBuilder = new IndicatorBuilderImpl(this, indicatorName)
            indicatorBuilders :+= indicatorBuilder
            indicatorBuilder
        }
      }

      def toSubCategory = SubCategory(name, indicatorBuilders.map(_.toIndicator))

      def valuesByIndicator = indicatorBuilders.flatMap(_.valuesByIndicator).toMap

      def build = categoryBuilder

    }

    trait IndicatorBuilder {

      def addValue(entity: E, review: Review, value: Double): IndicatorBuilder

      // exposing this makes testing easier; it is safe, as indicators only wrap a name, thus no scope state is leaking
      def toIndicator: Indicator

      def build: SubCategoryBuilder

    }

    private class IndicatorBuilderImpl(private val subCategoryBuilder: SubCategoryBuilder, val name: String, var values: Map[(E, Review), Double] = Map()) extends IndicatorBuilder {

      override def addValue(entity: E, review: Review, value: Double): IndicatorBuilder = {
        values += (entity, review) -> value
        // track entities and reviews in insertion order
        entities :+= entity
        reviews :+= review
        this
      }

      override def toIndicator = Indicator(name)

      def valuesByIndicator = Map(toIndicator -> values)

      def build = subCategoryBuilder

    }

  }

}
