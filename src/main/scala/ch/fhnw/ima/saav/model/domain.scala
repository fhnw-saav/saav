package ch.fhnw.ima.saav
package model

import ch.fhnw.ima.saav.model.domain.Entity.{Organisation, Person, Project}

object domain {

  final case class Analysis[E <: Entity](categories: Seq[Category[E]], entities: Seq[E], reviews: Seq[Review]) {

    def groupedValue(entity: E, indicator: Indicator[E]): Option[Double] = {
      val values = reviews.flatMap(review => indicator.values.get((entity, review)))
      median(values)
    }

    def groupedValue(entity: E, subCategory: SubCategory[E]): Option[Double] = {
      val values = subCategory.indicators.flatMap(groupedValue(entity, _))
      median(values)
    }

    def groupedValue(entity: E, category: Category[E]): Option[Double] = {
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

  final case class Category[E <: Entity](name: String, subCategories: Seq[SubCategory[E]])

  final case class SubCategory[E <: Entity](name: String, indicators: Seq[Indicator[E]])

  final case class Indicator[E <: Entity](name: String, values: Map[(E, Review), Double])

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
      Analysis[E](categories, entities.distinct, reviews.distinct)
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

      def build = categoryBuilder

    }

    trait IndicatorBuilder {

      def addValue(entity: E, review: Review, value: Double): IndicatorBuilder

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

      def toIndicator = Indicator(name, values)

      def build = subCategoryBuilder

    }

  }

}
