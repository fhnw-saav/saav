package ch.fhnw.ima.saav.component

import ch.fhnw.ima.saav.component.FileImportComponent.Row
import ch.fhnw.ima.saav.model.domain.{Analysis, AnalysisBuilder}
import org.scalatest.FlatSpec

import scala.scalajs.js

class FileImportComponentSpec extends FlatSpec {

  "A file import component" should "import a single row" in {

    val projectName = "project"
    val criteriaName = "testCriteria"
    val subCriteriaName = "testSubCriteria"
    val indicatorName = "testIndicator"

    val row: Row = createTestRow(projectName, criteriaName, subCriteriaName, indicatorName)

    val builder = AnalysisBuilder()

    FileImportComponent.parseRow(builder, row)

    val analysis = builder.build
    assert(analysis.criteria.size == 1)
    assert(analysis.criteria(0).displayName == criteriaName)
    assert(analysis.criteria(0).subCriteria.size == 1)
    assert(analysis.criteria(0).subCriteria(0).displayName == subCriteriaName)
    assert(analysis.criteria(0).subCriteria(0).indicators.length == 1)
    assert(analysis.criteria(0).subCriteria(0).indicators(0).displayName == indicatorName)

  }

  it should "import multiple rows in batches" in {

    val rowCount: Int = 17
    val rows = for (i <- 0 until rowCount) yield createTestRow(s"project-$i", s"criteria-$i", s"subCriteria-$i", s"indicator-$i")

    // poor man's synchronization (all that we got in JS)
    var done = false

    val builder = AnalysisBuilder()

    val handleProgress = (progress: Float) => assert(Math.round(100 * progress) == Math.round(100 * (10f / rowCount)))

    val handleReady = (analysis: Analysis) => {
      assert(analysis.entities.length == rowCount)
      done = true
    }

    FileImportComponent.parseRowBatchAsync(builder, rows, 0, handleProgress, handleReady, { _ => })

    js.timers.setTimeout(1000) {
      assert(done, "Async parsing not complete -> consider setting a higher timeout")
    }

  }

  private def createTestRow(project: String, criteria: String, subCriteria: String, indicator: String): Row = {
    Array(project, s"$criteria:::$subCriteria:::$indicator","testReviewer","42")
  }

}
