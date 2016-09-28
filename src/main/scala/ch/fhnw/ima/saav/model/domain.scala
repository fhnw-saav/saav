package ch.fhnw.ima.saav
package model

object domain {

  final case class Analysis(criteria: Seq[Criteria], entities: Seq[Entity], reviews: Seq[Review])

  final case class Criteria(name: String, subCriteria: Seq[SubCriteria]) {
    override lazy val hashCode: Int = super.hashCode()
  }

  final case class SubCriteria(name: String, indicators: Seq[Indicator]) {
    override lazy val hashCode: Int = super.hashCode()
  }

  final case class IndicatorId(value: Int) {
    override lazy val hashCode: Int = super.hashCode()
  }

  final case class Indicator(id: IndicatorId, name: String, values: Map[(Entity, Review), Double])

  final case class Review(name: String)

  final case class Entity(name: String)

  final case class AnalysisBuilder() {

    private var criteriaBuilders: Seq[CriteriaBuilderImpl] = Seq()
    private var entities: Seq[Entity] = Seq()
    private var reviews: Seq[Review] = Seq()

    def criteria(criteriaName: String): CriteriaBuilder = {
      val existing = criteriaBuilders.find(_.criteriaName == criteriaName)
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
      def criteriaName: String
      def subCriteria(name: String): SubCriteriaBuilder
      def build: AnalysisBuilder
    }

    private class CriteriaBuilderImpl(val criteriaName: String, var subCriteriaBuilders: Seq[SubCriteriaBuilderImpl] = Seq()) extends CriteriaBuilder {

      override def subCriteria(subCriteriaName: String): SubCriteriaBuilder = {
        val existing = subCriteriaBuilders.find(_.subCriteriaName == subCriteriaName)
        existing match {
          case Some(c) => c
          case None =>
            val subCriteriaBuilder = new SubCriteriaBuilderImpl(this, subCriteriaName)
            subCriteriaBuilders :+= subCriteriaBuilder
            subCriteriaBuilder
        }
      }

      def toCriteria = Criteria(criteriaName, subCriteriaBuilders.map(_.toSubCriteria))

      override def build: AnalysisBuilder = AnalysisBuilder.this
    }

    trait SubCriteriaBuilder {
      def subCriteriaName: String
      def indicator(name: String): IndicatorBuilder
      def build: CriteriaBuilder
    }

    private class SubCriteriaBuilderImpl(private val criteriaBuilder: CriteriaBuilder, val subCriteriaName: String, var indicatorBuilders: Seq[IndicatorBuilderImpl] = Seq()) extends SubCriteriaBuilder {

      override def indicator(indicatorName: String): IndicatorBuilder = {
        val existing = indicatorBuilders.find(_.indicatorName == indicatorName)
        existing match {
          case Some(i) => i
          case None =>

            // uniquely identified by hierarchical path --> taking the hashCode to prevent any semantic abuse
            val indicatorId = IndicatorId(s"${criteriaBuilder.criteriaName}/$subCriteriaName/$indicatorName".hashCode)

            val indicatorBuilder = new IndicatorBuilderImpl(this, indicatorId, indicatorName)
            indicatorBuilders :+= indicatorBuilder
            indicatorBuilder
        }
      }

      def toSubCriteria = SubCriteria(subCriteriaName, indicatorBuilders.map(_.toIndicator))

      def build: CriteriaBuilder = criteriaBuilder

    }

    trait IndicatorBuilder {

      def addValue(entity: Entity, review: Review, value: Double): IndicatorBuilder

      def build: SubCriteriaBuilder

    }

    private class IndicatorBuilderImpl(private val subCriteriaBuilder: SubCriteriaBuilder, val id: IndicatorId, val indicatorName: String, var values: Map[(Entity, Review), Double] = Map()) extends IndicatorBuilder {

      override def addValue(entity: Entity, review: Review, value: Double): IndicatorBuilder = {
        values += (entity, review) -> value
        // track entities and reviews in insertion order
        entities :+= entity
        reviews :+= review
        this
      }

      def toIndicator = Indicator(id, indicatorName, values)

      def build: SubCriteriaBuilder = subCriteriaBuilder

    }

  }

}
