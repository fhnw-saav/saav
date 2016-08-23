package ch.fhnw.ima.saav
package model

object domain {

  final case class Analysis(categories: Seq[Category], entities: Seq[Entity], reviews: Seq[Review])

  final case class Category(name: String, subCategories: Seq[SubCategory])

  final case class SubCategory(name: String, indicators: Seq[Indicator])

  final case class Indicator(name: String, values: Map[(Entity, Review), Double])

  final case class Review(name: String)

  final case class Entity(name: String)

  final case class AnalysisBuilder() {

    private var categoryBuilders: Seq[CategoryBuilderImpl] = Seq()
    private var entities: Seq[Entity] = Seq()
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

    def build: Analysis = {
      val categories = categoryBuilders.map(_.toCategory)
      Analysis(categories, entities.distinct, reviews.distinct)
    }

    trait CategoryBuilder {
      def subCategory(name: String): SubCategoryBuilder

      def build: AnalysisBuilder
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

      def addValue(entity: Entity, review: Review, value: Double): IndicatorBuilder

      def build: SubCategoryBuilder

    }

    private class IndicatorBuilderImpl(private val subCategoryBuilder: SubCategoryBuilder, val name: String, var values: Map[(Entity, Review), Double] = Map()) extends IndicatorBuilder {

      override def addValue(entity: Entity, review: Review, value: Double): IndicatorBuilder = {
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
