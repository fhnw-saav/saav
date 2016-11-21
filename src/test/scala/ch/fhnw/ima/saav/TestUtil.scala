package ch.fhnw.ima.saav

import ch.fhnw.ima.saav.controller.io.AnalysisDataImporter.Row
import ch.fhnw.ima.saav.model.config.Config
import ch.fhnw.ima.saav.model.domain._
import ch.fhnw.ima.saav.model.weight.{Profile, Quality, Weights}

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
    .criteria("C_1")
      .subCriteria("SC_11")
        .indicator("I_111")
          .addValue(entityOne, reviewOne, 1)
          .addValue(entityOne, reviewTwo, 0.1)
          .addValue(entityOne, reviewThree, 2)
          .addValue(entityTwo, reviewOne, 101)
          .addValue(entityTwo, reviewTwo, 100)
          .addValue(entityTwo, reviewThree, 102)
        .build
        .indicator("I_112")
          .addValue(entityOne, reviewOne, 3)
          .addValue(entityTwo, reviewOne, 101)
          .addValue(entityThree, reviewOne, 0)
        .build
      .build
    .build
    .criteria("C_2")
      .subCriteria("SC_21")
        .indicator("I_211")
          .addValue(entityOne, reviewOne, 41)
          .addValue(entityOne, reviewTwo, 43)
          .addValue(entityTwo, reviewOne, 99)
        .build
        .indicator("I_212")
          .addValue(entityOne, reviewOne, 43)
        .build
        .build
      .build
    .criteria("C_3")
      .subCriteria("SC_31")
        .indicator("I_311")
          .addValue(entityOne, reviewOne, 1)
          .addValue(entityOne, reviewTwo, 2)
          .addValue(entityTwo, reviewOne, 3)
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

  val config: Config = {
    new Config {
      val defaultWeights: Weights = Weights(
        subCriteriaWeights = Map(
          SubCriteriaId(CriteriaId("C_1"), "SC_11") -> Quality(1.0),
          SubCriteriaId(CriteriaId("C_2"), "SC_21") -> Quality(1.0),
          SubCriteriaId(CriteriaId("C_3"), "SC_31") -> Profile
        ),
        enabledIndicators = allIndicatorIds)
      val nonAggregatableCriteria: Set[CriteriaId] = Set(CriteriaId("C_3"))
    }
  }

}

object TestUtil {

  def createTestRow(project: String, criteria: String, subCriteria: String, indicator: String): Row = {
    Array(project, s"$criteria:::$subCriteria:::$indicator", "testReviewer", "42")
  }

  def createTestRows(rowCount: Int): Seq[Row] = for (i <- 0 until rowCount) yield createTestRow(s"project-$i", s"criteria-$i", s"subCriteria-$i", s"indicator-$i")

}
