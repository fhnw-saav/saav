package ch.fhnw.ima.saav.circuit.io

import ch.fhnw.ima.saav.TestUtil
import ch.fhnw.ima.saav.circuit.io.AnalysisDataImporter.{BatchSize, ImportState, Row, parseRow}
import ch.fhnw.ima.saav.model.config.AnalysisConfig
import ch.fhnw.ima.saav.model.domain.AnalysisBuilder
import org.scalatest.FlatSpec

class AnalysisDataImporterSpec extends FlatSpec {

  "An importer" should "import a single row" in {

    val projectName = "project"
    val criteriaName = "testCriteria"
    val subCriteriaName = "testSubCriteria"
    val indicatorName = "testIndicator"

    val row: Row = TestUtil.createTestRow(projectName, criteriaName, subCriteriaName, indicatorName)

    val builder = AnalysisBuilder()

    parseRow(builder, 0, row)

    val analysis = builder.build
    assert(analysis.criteria.size == 1)
    assert(analysis.criteria(0).displayName == criteriaName)
    assert(analysis.criteria(0).subCriteria.size == 1)
    assert(analysis.criteria(0).subCriteria(0).displayName == subCriteriaName)
    assert(analysis.criteria(0).subCriteria(0).indicators.length == 1)
    assert(analysis.criteria(0).subCriteria(0).indicators(0).displayName == indicatorName)

  }

  it should "import multiple rows in batches" in {

    val rowCount: Int = (1.5 * BatchSize).toInt
    val rows = TestUtil.createTestRows(rowCount)

    val builder = AnalysisBuilder()

    val stateAfterFirstBatch = AnalysisDataImporter.parseRowBatch(ImportState(AnalysisConfig.empty, builder, rows, 0))
    stateAfterFirstBatch match {
      case Right(ImportState(_, _, _, batchIndex)) =>
        assert(batchIndex === 1)
        assert(builder.build.criteria.size === BatchSize)
      case a@_ => fail(s"Unexpected parsing result $a")
    }

    val stateAfterSecondBatch = AnalysisDataImporter.parseRowBatch(ImportState(AnalysisConfig.empty, builder, rows, 1))
    stateAfterSecondBatch match {
      case Left(analysis) =>
        assert(analysis.entities.size === rowCount)
      case a@_ => fail(s"Unexpected parsing result $a")
    }
  }



}
