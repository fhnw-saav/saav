package ch.fhnw.saav.model

import ch.fhnw.saav.model.model.Entity.{Organisation, Person, Project}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object model {

  trait Analysis[E <: Entity] {

    def categories: Seq[Category]

    def entities: Seq[E]

    def reviews: Seq[Review]

    def value(e: E, indicator: Indicator, review: Review): Option[Double]

    def groupedValue(e: E, indicator: Indicator): Option[Double]

    def groupedValue(e: E, subCategory: SubCategory): Option[Double]

    def groupedValue(e: E, category: Category): Option[Double]

  }

  case class Category(name: String, subCategories: Seq[SubCategory])

  case class SubCategory(name: String, indicators: Seq[Indicator])

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

    private val categoryScopes: ListBuffer[CategoryScopeImpl] = ListBuffer()

    trait CategoryScope {
      def name: String

      def subCategory(name: String): SubCategoryScope
    }

    private class CategoryScopeImpl(val name: String, val subCategoryScopes: ListBuffer[SubCategoryScopeImpl] = ListBuffer()) extends CategoryScope {
      override def subCategory(subCategoryName: String): SubCategoryScope = {
        val existingSubCategory = subCategoryScopes.find(_.name == subCategoryName)
        existingSubCategory match {
          case Some(c) => c
          case None =>
            val subCategoryScope = new SubCategoryScopeImpl(subCategoryName)
            subCategoryScopes.append(subCategoryScope)
            subCategoryScope
        }
      }
    }

    trait SubCategoryScope {
      def name: String

      def indicator(name: String): IndicatorScope
    }

    private class SubCategoryScopeImpl(val name: String, val indicatorScopes: ListBuffer[IndicatorScopeImpl] = ListBuffer()) extends SubCategoryScope {
      override def indicator(indicatorName: String): IndicatorScope = {
        val existingIndicator = indicatorScopes.find(_.name == indicatorName)
        existingIndicator match {
          case Some(i) => i
          case None =>
            val indicatorScope = new IndicatorScopeImpl(indicatorName)
            indicatorScopes.append(indicatorScope)
            indicatorScope
        }
      }
    }

    trait IndicatorScope extends Indicator {
      def addValue(entity: E, review: Review, value: Double): AnalysisBuilder[E]
    }

    private class IndicatorScopeImpl(val name: String, val values: mutable.LinkedHashMap[(E, Review), Double] = mutable.LinkedHashMap()) extends IndicatorScope {
      override def addValue(entity: E, review: Review, value: Double): AnalysisBuilder[E] = {
        values.put((entity, review), value)
        AnalysisBuilder.this
      }
    }

    def category(categoryName: String): CategoryScope = {
      val existingCategory = categoryScopes.find(_.name == categoryName)
      existingCategory match {
        case Some(c) => c
        case None =>
          val categoryScope = new CategoryScopeImpl(categoryName)
          categoryScopes.append(categoryScope)
          categoryScope
      }
    }

    def build(): Analysis[E] = {

      // TODO: First draft -> needs massive simplification...

      val categories = categoryScopes.map(c => {
        val subCategories = c.subCategoryScopes.map(s => {
          SubCategory(s.name, s.indicatorScopes)
        })
        Category(c.name, subCategories)
      })

      val (entities, reviews) = (for {
        categoryScope <- categoryScopes
        subCategoryScope <- categoryScope.subCategoryScopes
        indicatorScope <- subCategoryScope.indicatorScopes
      } yield indicatorScope.values.keys).flatten.unzip

      val uniqueEntities = entities.toSet.toSeq
      val uniqueReviews = reviews.toSet.toSeq

      // convert from indicator-specific value maps to one big map (where indicator is part of key)
      val valueMap: mutable.HashMap[(E, Indicator, Review), Double] = mutable.HashMap[(E, Indicator, Review), Double]()
      for {
        categoryScope <- categoryScopes
        subCategoryScope <- categoryScope.subCategoryScopes
        indicatorScope <- subCategoryScope.indicatorScopes
        valueEntry <- indicatorScope.values
      } {
        val (entity, review) = valueEntry._1
        val value = valueEntry._2
        valueMap.put((entity, indicatorScope, review), value)
      }

      new AnalysisImpl[E](categories, uniqueEntities, uniqueReviews, valueMap.toMap)
    }

  }

  private class AnalysisImpl[E <: Entity](val categories: Seq[Category], val entities: Seq[E], val reviews: Seq[Review], private val values: Map[(E, Indicator, Review), Double]) extends Analysis[E] {

    override def value(e: E, indicator: Indicator, review: Review): Option[Double] = values.get((e, indicator, review))

    override def groupedValue(e: E, indicator: Indicator): Option[Double] = ???

    override def groupedValue(e: E, subCategory: SubCategory): Option[Double] = ???

    override def groupedValue(e: E, category: Category): Option[Double] = ???
  }

}
