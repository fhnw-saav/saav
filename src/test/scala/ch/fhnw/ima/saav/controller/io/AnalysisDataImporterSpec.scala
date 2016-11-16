package ch.fhnw.ima.saav.controller.io

import ch.fhnw.ima.saav.controller.{AnalysisReadyAction, DataImportInProgressAction}
import ch.fhnw.ima.saav.controller.io.AnalysisDataImporter.{BatchSize, Row, parseRow}
import ch.fhnw.ima.saav.model.domain.AnalysisBuilder
import diode.Action
import org.scalatest.FlatSpec

class AnalysisDataImporterSpec extends FlatSpec {

  "An importer" should "import a single row" in {

    val projectName = "project"
    val criteriaName = "testCriteria"
    val subCriteriaName = "testSubCriteria"
    val indicatorName = "testIndicator"

    val row: Row = createTestRow(projectName, criteriaName, subCriteriaName, indicatorName)

    val builder = AnalysisBuilder()

    parseRow(builder, row)

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
    val rows = for (i <- 0 until rowCount) yield createTestRow(s"project-$i", s"criteria-$i", s"subCriteria-$i", s"indicator-$i")

    val builder = AnalysisBuilder()

    val firstBatch: Action = AnalysisDataImporter.parseRowBatch(builder, rows, 0)
    firstBatch match {
      case DataImportInProgressAction(progress, _, _, batchIndex) =>
        assert(progress === (BatchSize.toFloat / rowCount))
        assert(batchIndex === 1)
      case a @ _ => fail(s"Unexpected action $a")
    }

    val secondBatch: Action = AnalysisDataImporter.parseRowBatch(builder, rows, 1)
    secondBatch match {
      case AnalysisReadyAction(analysis) =>
        assert(analysis.entities.size === rowCount)
      case a @ _ => fail(s"Unexpected action $a")
    }
  }

  private def createTestRow(project: String, criteria: String, subCriteria: String, indicator: String): Row = {
    Array(project, s"$criteria:::$subCriteria:::$indicator", "testReviewer", "42")
  }

}
