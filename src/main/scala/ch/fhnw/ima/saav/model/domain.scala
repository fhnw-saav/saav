package ch.fhnw.ima.saav
package model

object domain {

  final case class Analysis(criteria: Seq[Criteria], entities: Seq[Entity], reviews: Seq[Review])

  final case class Criteria(name: String, subCriteria: Seq[SubCriteria]) {
    override lazy val hashCode = super.hashCode()
  }

  final case class SubCriteria(name: String, indicators: Seq[Indicator]) {
    override lazy val hashCode = super.hashCode()
  }

  final case class Indicator(name: String, values: Map[(Entity, Review), Double])

  final case class Review(name: String)

  final case class Entity(name: String)

  final case class AnalysisBuilder() {

    private var criteriaBuilders: Seq[CriteriaBuilderImpl] = Seq()
    private var entities: Seq[Entity] = Seq()
    private var reviews: Seq[Review] = Seq()

    def criteria(criteriaName: String): CriteriaBuilder = {
      val existing = criteriaBuilders.find(_.name == criteriaName)
      existing match {
        case Some(c) => c
        case None =>
          val criteriaBuilder = new CriteriaBuilderImpl(criteriaName)
          criteriaBuilders :+= criteriaBuilder
          criteriaBuilder
      }
    }

    def build: Analysis = {
      val criteria = criteriaBuilders.map(_.toCriteria)
      Analysis(criteria, entities.distinct, reviews.distinct)
    }

    trait CriteriaBuilder {
      def subCriteria(name: String): SubCriteriaBuilder

      def build: AnalysisBuilder
    }

    private class CriteriaBuilderImpl(val name: String, var subCriteriaBuilders: Seq[SubCriteriaBuilderImpl] = Seq()) extends CriteriaBuilder {

      override def subCriteria(subCriteriaName: String): SubCriteriaBuilder = {
        val existing = subCriteriaBuilders.find(_.name == subCriteriaName)
        existing match {
          case Some(c) => c
          case None =>
            val subCriteriaBuilder = new SubCriteriaBuilderImpl(this, subCriteriaName)
            subCriteriaBuilders :+= subCriteriaBuilder
            subCriteriaBuilder
        }
      }

      def toCriteria = Criteria(name, subCriteriaBuilders.map(_.toSubCriteria))

      override def build = AnalysisBuilder.this
    }

    trait SubCriteriaBuilder {
      def indicator(name: String): IndicatorBuilder

      def build: CriteriaBuilder
    }

    private class SubCriteriaBuilderImpl(private val criteriaBuilder: CriteriaBuilder, val name: String, var indicatorBuilders: Seq[IndicatorBuilderImpl] = Seq()) extends SubCriteriaBuilder {

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

      def toSubCriteria = SubCriteria(name, indicatorBuilders.map(_.toIndicator))

      def build = criteriaBuilder

    }

    trait IndicatorBuilder {

      def addValue(entity: Entity, review: Review, value: Double): IndicatorBuilder

      def build: SubCriteriaBuilder

    }

    private class IndicatorBuilderImpl(private val subCriteriaBuilder: SubCriteriaBuilder, val name: String, var values: Map[(Entity, Review), Double] = Map()) extends IndicatorBuilder {

      override def addValue(entity: Entity, review: Review, value: Double): IndicatorBuilder = {
        values += (entity, review) -> value
        // track entities and reviews in insertion order
        entities :+= entity
        reviews :+= review
        this
      }

      def toIndicator = Indicator(name, values)

      def build = subCriteriaBuilder

    }

  }

}
