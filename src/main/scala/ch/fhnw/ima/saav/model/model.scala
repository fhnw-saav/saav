package ch.fhnw.ima.saav.model

import ch.fhnw.ima.saav.model.model.Entity.{Organisation, Person, Project}

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

  case class Category(name: String, subCategories: Seq[SubCategory])

  case class SubCategory(name: String, indicators: Seq[Indicator])

  case class Indicator(name: String)

  case class Review(name: String)

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

  case class AnalysisBuilder[E <: Entity]() {

    private var categoryScopes: Seq[CategoryScopeImpl] = Seq()
    private var entities: Seq[E] = Seq()
    private var reviews: Seq[Review] = Seq()

    def category(categoryName: String): CategoryScope = {
      val existingCategory = categoryScopes.find(_.name == categoryName)
      existingCategory match {
        case Some(c) => c
        case None =>
          val categoryScope = new CategoryScopeImpl(categoryName)
          categoryScopes = categoryScopes :+ categoryScope
          categoryScope
      }
    }

    def build(): Analysis[E] = {
      val categories = categoryScopes.map { categoryScope =>
        val subCategories = categoryScope.subCategoryScopes.map { subCategoryScope =>
          val indicators = subCategoryScope.indicatorScopes.map(indicatorScope => Indicator(indicatorScope.name))
          SubCategory(subCategoryScope.name, indicators)
        }
        Category(categoryScope.name, subCategories)
      }

      val valuesByIndicator = (for {
        categoryScope <- categoryScopes
        subCategoryScope <- categoryScope.subCategoryScopes
        indicatorScope <- subCategoryScope.indicatorScopes
      } yield indicatorScope.toIndicator -> indicatorScope.values).toMap

      new AnalysisImpl[E](categories, entities.distinct, reviews.distinct, valuesByIndicator)
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
            subCategoryScopes = subCategoryScopes :+ subCategoryScope
            subCategoryScope
        }
      }

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
            indicatorScopes = indicatorScopes :+ indicatorScope
            indicatorScope
        }
      }

    }

    trait IndicatorScope {
      def addValue(entity: E, review: Review, value: Double): IndicatorScope
      def toIndicator: Indicator
    }

    private class IndicatorScopeImpl(val name: String, var values: Map[(E, Review), Double] = Map()) extends IndicatorScope {

      override def addValue(entity: E, review: Review, value: Double): IndicatorScope = {
        values += (entity, review) -> value
        // track entities and reviews in insertion order
        entities = entities :+ entity
        reviews = reviews :+ review
        this
      }

      override def toIndicator = Indicator(name)
    }

  }

  private class AnalysisImpl[E <: Entity](val categories: Seq[Category], val entities: Seq[E], val reviews: Seq[Review], private val valuesByIndicator: Map[Indicator, Map[(E, Review), Double]]) extends Analysis[E] {

    override def value(entity: E, indicator: Indicator, review: Review): Option[Double] = {
      valuesByIndicator.get(indicator) match {
        case None => None
        case Some(values) => values.get((entity, review))
      }
    }

    override def groupedValue(entity: E, indicator: Indicator): Option[Double] = {
      valuesByIndicator.get(indicator) match {
        case None => None
        case Some(valueMap) =>
          val values = reviews.flatMap(review => valueMap.get((entity, review)))
          median(values)
      }
    }

    override def groupedValue(entity: E, subCategory: SubCategory): Option[Double] = {
      val values = for (indicator <- subCategory.indicators) yield groupedValue(entity, indicator)
      median(values.flatten)
    }

    override def groupedValue(entity: E, category: Category): Option[Double] = {
      val values = for (subCategory <- category.subCategories) yield groupedValue(entity, subCategory)
      median(values.flatten)
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

}
