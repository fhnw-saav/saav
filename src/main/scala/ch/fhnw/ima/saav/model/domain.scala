package ch.fhnw.ima.saav
package model

import scala.collection.immutable.ListSet

object domain {

  // For the time being, our input format does not contain unique identifiers. Names are used in the role of
  // an identifying 'path' as well as for display ==> for the sake of maintainability, these aspects should be kept
  // separate, i.e. GUI code should only ever use 'displayName'

  final case class CriteriaId(path: String)

  final case class SubCriteriaId(criteriaId: CriteriaId, path: String)

  final case class IndicatorId(subCriteriaId: SubCriteriaId, path: String)

  final case class EntityId(path: String)

  // Reviews are not yet used in the UI --> the Id class is all we need
  final case class ReviewId(name: String)

  final case class Analysis(criteria: Seq[Criteria], entities: Seq[Entity], reviews: Seq[ReviewId])

  final case class Criteria(id: CriteriaId, subCriteria: Seq[SubCriteria]) {
    val displayName: String = id.path
  }

  final case class SubCriteria(id: SubCriteriaId, indicators: Seq[Indicator]) {
    val displayName: String = id.path
  }

  final case class Indicator(id: IndicatorId, values: Map[(EntityId, ReviewId), Double]) {
    val displayName: String = id.path
  }

  final case class Entity(id: EntityId) {
    val displayName: String = id.path
    override lazy val hashCode: Int = super.hashCode()
  }

  final case class AnalysisBuilder() {

    private var criteriaBuilders: Seq[CriteriaBuilderImpl] = Seq()
    private var entities: ListSet[Entity] = ListSet()
    private var reviews: ListSet[ReviewId] = ListSet()

    def criteria(criteriaName: String): CriteriaBuilder = {
      val existing = criteriaBuilders.find(_.id.path == criteriaName)
      existing match {
        case Some(c) => c
        case None =>
          val criteriaBuilder = new CriteriaBuilderImpl(CriteriaId(criteriaName))
          criteriaBuilders :+= criteriaBuilder
          criteriaBuilder
      }
    }

    def build: Analysis = {
      val criteria = criteriaBuilders.map(_.toCriteria)
      Analysis(criteria, entities.toList.reverse, reviews.toList.reverse)
    }

    trait CriteriaBuilder {
      def id: CriteriaId
      def subCriteria(name: String): SubCriteriaBuilder
      def build: AnalysisBuilder
    }

    private class CriteriaBuilderImpl(val id: CriteriaId, var subCriteriaBuilders: Seq[SubCriteriaBuilderImpl] = Seq()) extends CriteriaBuilder {

      override def subCriteria(subCriteriaName: String): SubCriteriaBuilder = {
        val existing = subCriteriaBuilders.find(_.id.path == subCriteriaName)
        existing match {
          case Some(c) => c
          case None =>
            val subCriteriaBuilder = new SubCriteriaBuilderImpl(this, SubCriteriaId(id, subCriteriaName))
            subCriteriaBuilders :+= subCriteriaBuilder
            subCriteriaBuilder
        }
      }

      def toCriteria = Criteria(id, subCriteriaBuilders.map(_.toSubCriteria))

      override def build: AnalysisBuilder = AnalysisBuilder.this
    }

    trait SubCriteriaBuilder {
      def id: SubCriteriaId
      def indicator(name: String): IndicatorBuilder
      def build: CriteriaBuilder
    }

    private class SubCriteriaBuilderImpl(private val criteriaBuilder: CriteriaBuilder, val id: SubCriteriaId, var indicatorBuilders: Seq[IndicatorBuilderImpl] = Seq()) extends SubCriteriaBuilder {

      override def indicator(indicatorName: String): IndicatorBuilder = {
        val existing = indicatorBuilders.find(_.id.path == indicatorName)
        existing match {
          case Some(i) => i
          case None =>
            val indicatorBuilder = new IndicatorBuilderImpl(this, IndicatorId(id, indicatorName), indicatorName)
            indicatorBuilders :+= indicatorBuilder
            indicatorBuilder
        }
      }

      def toSubCriteria = SubCriteria(id, indicatorBuilders.map(_.toIndicator))

      def build: CriteriaBuilder = criteriaBuilder

    }

    trait IndicatorBuilder {

      def addValue(entity: Entity, review: ReviewId, value: Double): IndicatorBuilder

      def build: SubCriteriaBuilder

    }

    private class IndicatorBuilderImpl(private val subCriteriaBuilder: SubCriteriaBuilder, val id: IndicatorId, val indicatorName: String, var values: Map[(EntityId, ReviewId), Double] = Map()) extends IndicatorBuilder {

      override def addValue(entity: Entity, review: ReviewId, value: Double): IndicatorBuilder = {
        values += (entity.id, review) -> value
        // track entities and reviews in insertion order
        entities += entity
        reviews += review
        this
      }

      def toIndicator = Indicator(id, values)

      def build: SubCriteriaBuilder = subCriteriaBuilder

    }

  }

}
