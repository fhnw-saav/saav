package ch.fhnw.ima.saav

import ch.fhnw.ima.saav.circuit.io.AnalysisDataImporter.Row
import ch.fhnw.ima.saav.model.config._
import ch.fhnw.ima.saav.model.domain._
import ch.fhnw.ima.saav.model.weight.{Profile, Quality}

trait TestUtil {

  val entityOne = Entity(EntityId("P1"))
  val entityTwo = Entity(EntityId("P2"))
  val entityThree = Entity(EntityId("P3"))
  val allEntities = Set(entityOne, entityTwo, entityThree)
  val allEntityIds: Set[EntityId] = allEntities.map(_.id)

  val reviewOne = ReviewId("Review1")
  val reviewTwo = ReviewId("Review2")
  val reviewThree = ReviewId("Review3")

  val analysisConfig = AnalysisConfig(
    title = "Title",
    allowedValueRange = (-1000, 1000),
    criteria = Seq(
      CriteriaConfig(
        "C_1",
        aggregatable = true,
        subCriteria = Seq(
          SubCriteriaConfig(
            "SC_11",
            Quality(1.0),
            Seq(
              IndicatorConfig("I_111", enabled = true),
              IndicatorConfig("I_112", enabled = true)
            )
          )
        )
      ),
      CriteriaConfig(
        name = "C_2",
        aggregatable = true,
        subCriteria = Seq(
          SubCriteriaConfig(
            "SC_21",
            Quality(0.42),
            Seq(
              IndicatorConfig("I_211", enabled = true),
              IndicatorConfig("I_212", enabled = true)
            )
          )
        )
      ),
      CriteriaConfig(
        name = "C_3",
        aggregatable = false,
        subCriteria = Seq(
          SubCriteriaConfig(
            "SC_31",
            Profile,
            Seq(
              IndicatorConfig("I_311", enabled = false),
              IndicatorConfig("I_312", enabled = true)
            )
          )
        )
      )

    )
  )

  val analysis: Analysis = AnalysisBuilder()
    .criteria("C_1")
      .subCriteria("SC_11")
        .indicator("I_111")

          .addValue(entityOne, reviewOne, 1)
          .addValue(entityOne, reviewTwo, 1)
          .addValue(entityOne, reviewThree, 1)

          .addValue(entityTwo, reviewOne, 1)
          .addValue(entityTwo, reviewTwo, 2)
          .addValue(entityTwo, reviewThree, 3)

        .build
        .indicator("I_112")
          .addValue(entityOne, reviewOne, 1)
          .addValue(entityTwo, reviewOne, 2)
          .addValue(entityThree, reviewOne, 3)
        .build
      .build
    .build
    .criteria("C_2")
      .subCriteria("SC_21")
        .indicator("I_211")
          .addValue(entityOne, reviewOne, 100)
          .addValue(entityOne, reviewTwo, 100)

          .addValue(entityTwo, reviewOne, 200)
        .build
        .indicator("I_212")
          .addValue(entityOne, reviewOne, 200)
        .build
        .build
      .build
    .criteria("C_3")
      .subCriteria("SC_31")
        .indicator("I_311")
          .addValue(entityOne, reviewOne, 1001)
          .addValue(entityOne, reviewTwo, 1002)

          .addValue(entityTwo, reviewOne, 2000)
        .build
        .indicator("I_312")
          .addValue(entityOne, reviewOne, 42)
          .addValue(entityOne, reviewTwo, 42)

          .addValue(entityTwo, reviewOne, 42)
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
    Array(project, s"$criteria:::$subCriteria:::$indicator", "testReviewer", "3")
  }

  def createTestRows(rowCount: Int): Seq[Row] = for (i <- 0 until rowCount) yield createTestRow(s"project-$i", s"criteria-$i", s"subCriteria-$i", s"indicator-$i")

}
