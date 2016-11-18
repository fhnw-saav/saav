package ch.fhnw.ima.saav

import ch.fhnw.ima.saav.controller.io.AnalysisDataImporter.Row
import ch.fhnw.ima.saav.model.domain._

trait TestUtil {

  val entityOne = Entity(EntityId("P1"))
  val entityTwo = Entity(EntityId("P2"))
  val entityThree = Entity(EntityId("P3"))
  val allEntities = Set(entityOne, entityTwo, entityThree)
  val allEntityIds: Set[EntityId] = allEntities.map(_.id)

  val reviewOne = ReviewId("Review1")
  val reviewTwo = ReviewId("Review2")
  val reviewThree = ReviewId("Review3")

  val analysis: Analysis = AnalysisBuilder()
    .criteria("Cat 1")
      .subCriteria("Sub-Cat 11")
        .indicator("Indicator 111")
          .addValue(entityOne, reviewOne, 1)
          .addValue(entityOne, reviewTwo, 0.1)
          .addValue(entityOne, reviewThree, 2)
          .addValue(entityTwo, reviewOne, 101)
          .addValue(entityTwo, reviewTwo, 100)
          .addValue(entityTwo, reviewThree, 102)
        .build
        .indicator("Indicator 112")
          .addValue(entityOne, reviewOne, 3)
          .addValue(entityTwo, reviewOne, 101)
          .addValue(entityThree, reviewOne, 0)
        .build
      .build
    .build
    .criteria("Cat 2")
      .subCriteria("Sub-Cat 21")
        .indicator("Indicator 211")
          .addValue(entityOne, reviewOne, 41)
          .addValue(entityOne, reviewTwo, 43)
          .addValue(entityTwo, reviewOne, 99)
        .build
        .indicator("Indicator 212")
          .addValue(entityOne, reviewOne, 43)
        .build
        .build
      .build
    .build

  val subCriteriaId: SubCriteriaId = analysis.criteria.head.subCriteria.head.id

  val allSubCriteriaIds: Set[SubCriteriaId] = (for {
    c <- analysis.criteria
    sc <- c.subCriteria
  } yield sc.id).toSet

  val indicatorId: IndicatorId = analysis.criteria.head.subCriteria.head.indicators.head.id

  val allIndicatorIds: Set[IndicatorId] = (for {
    c <- analysis.criteria
    sc <- c.subCriteria
    i <- sc.indicators
  } yield i.id).toSet


}

object TestUtil {

  def createTestRow(project: String, criteria: String, subCriteria: String, indicator: String): Row = {
    Array(project, s"$criteria:::$subCriteria:::$indicator", "testReviewer", "42")
  }

  def createTestRows(rowCount: Int): Seq[Row] = for (i <- 0 until rowCount) yield createTestRow(s"project-$i", s"criteria-$i", s"subCriteria-$i", s"indicator-$i")

}
