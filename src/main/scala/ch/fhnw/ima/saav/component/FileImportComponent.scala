package ch.fhnw.ima.saav.component

import ch.fhnw.ima.saav.component.pages.ProjectAnalysisPageComponent.{Empty, ImportState, InProgress, Ready}
import ch.fhnw.ima.saav.model.model.Entity.Project
import ch.fhnw.ima.saav.model.model.{AnalysisBuilder, Review}
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB}
import org.scalajs.dom
import org.scalajs.dom.DragEvent
import org.singlespaced.d3js.d3

import scala.scalajs.js
import scala.scalajs.js.annotation.JSName
import scalacss.ScalaCssReact._

/**
  * A component which accepts a CSV file via drag and drop and imports its contents.
  */
object FileImportComponent {

  case class Props(importState: ImportState, onNewImportState: ImportState => Callback)

  type Row = js.Dictionary[String]

  class Backend($: BackendScope[Props, Unit]) {

    def handleDragOver(e: DragEvent) = Callback {
      e.stopPropagation()
      e.preventDefault()
      e.dataTransfer.dropEffect = "copy"
    }

    def handleFileDropped(onNewImportState: ImportState => Callback)(e: DragEvent): Callback = {
      e.stopPropagation()
      e.preventDefault()
      val files = e.dataTransfer.files
      if (files.length > 0) {
        val file = files(0)
        val url = URL.createObjectURL(file)

        parseModel(url, onNewImportState)

        Callback.empty
      } else {
        Callback.log("No files to import")
      }
    }

    def parseModel(url: String, onNewImportState: ImportState => Callback): Unit = {
      d3.csv(url, (rows: js.Array[Row]) => {
        val builder = AnalysisBuilder.projectAnalysisBuilder

        // to report progress, rows are parsed asynchronously, giving react a chance to update the UI in between
        // to avoid timer congestion, we don't spawn a timer for each row, but parse row batches
        parseRowBatchAsync(onNewImportState, builder, rows, 0)
      })
    }

    def render(p: Props) = {
      p.importState match {
        case Empty =>
          <.div(css.fileDropZone,
            ^.onDragOver ==> handleDragOver,
            ^.onDrop ==> handleFileDropped(p.onNewImportState),
            <.div(<.h1("Drag and drop"), <.p("To import data from CSV file")))
        case InProgress(progress) =>
          <.div(css.fileDropZone, <.h1("Import in progress"), <.p((progress * 100).toInt + "%"))
        case Ready(_) => <.div()
      }
    }

  }

  def parseRowBatchAsync(onNewImportState: (ImportState) => Callback, builder: AnalysisBuilder[Project], rows: Seq[Row], batchIndex: Int): Unit = {

    val batchSize = 10

    js.timers.setTimeout(0) {

      for (
        indexWithinBatch <- 0 until batchSize;
        rowIndex = (batchIndex * batchSize) + indexWithinBatch
        if rowIndex < rows.length
      ) {

        val row = rows(rowIndex)

        parseRow(builder, row)

        // parsing is complete -> model can be built
        if (rowIndex == rows.length - 1) {
          val analysis = builder.build
          onNewImportState(Ready(analysis)).runNow()
        }
        // report progress and schedule next batch
        else if (indexWithinBatch == batchSize - 1) {
          val progress = (rowIndex + 1).toFloat / rows.length
          onNewImportState(InProgress(progress)).runNow()
          parseRowBatchAsync(onNewImportState, builder, rows, batchIndex + 1)
        }

      }

    }

  }

  def parseRow(builder: AnalysisBuilder[Project], row: Row): AnalysisBuilder[Project] = {

    val keyIt = row.keys.iterator

    val project = Project(row(keyIt.next()))
    val hierarchyLevels = row(keyIt.next()).split(":::")
    val category = hierarchyLevels(0)
    val subCategory = hierarchyLevels(1)
    val indicator = hierarchyLevels(2)
    val review = Review(row(keyIt.next()))
    val value = row(keyIt.next()).toDouble

    builder
      .category(category)
      .subCategory(subCategory)
      .indicator(indicator)
      .addValue(project, review, value)

    builder

  }

  private val component = ReactComponentB[Props](FileImportComponent.getClass.getSimpleName)
    .renderBackend[Backend]
    .build

  def apply(importState: ImportState, onNewImportState: ImportState => Callback) = component(Props(importState, onNewImportState))

}
