package ch.fhnw.ima.saav.component

import ch.fhnw.ima.saav.component.FileImportComponent.Row
import ch.fhnw.ima.saav.model.domain.{Analysis, AnalysisBuilder}
import org.scalatest.FlatSpec

import scala.scalajs.js
import scala.scalajs.js.Dictionary

class FileImportComponentSpec extends FlatSpec {

  "A file import component" should "import a single row" in {

    val projectName = "project"
    val categoryName = "testCategory"
    val subCategoryName = "testSubCategory"
    val indicatorName = "testIndicator"

    val row: Row = createTestRow(projectName, categoryName, subCategoryName, indicatorName)

    val builder = AnalysisBuilder()

    FileImportComponent.parseRow(builder, row)

    val analysis = builder.build
    assert(analysis.categories.size == 1)
    assert(analysis.categories(0).name == categoryName)
    assert(analysis.categories(0).subCategories.size == 1)
    assert(analysis.categories(0).subCategories(0).name == subCategoryName)
    assert(analysis.categories(0).subCategories(0).indicators.length == 1)
    assert(analysis.categories(0).subCategories(0).indicators(0).name == indicatorName)

  }

  it should "import multiple rows in batches" in {

    val rowCount: Int = 17
    val rows = for (i <- 0 until rowCount) yield createTestRow(s"project-$i", s"category-$i", s"subCategory-$i", s"indicator-$i")

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

  private def createTestRow(project: String, category: String, subCategory: String, indicator: String): Row = {
    Dictionary(
      ("Projekt", project),
      ("Indikator", s"$category:::$subCategory:::$indicator"),
      ("Reviewer", "testReviewer"),
      ("Value", "42")
    )
  }

}
