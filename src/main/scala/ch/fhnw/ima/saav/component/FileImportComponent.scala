package ch.fhnw.ima.saav.component

import java.util.concurrent.atomic.AtomicInteger

import ch.fhnw.ima.saav.component.Pages.ProjectAnalysisPageComponent.{Empty, ImportState, InProgress, Ready}
import ch.fhnw.ima.saav.model.model.Entity.Project
import ch.fhnw.ima.saav.model.model.{AnalysisBuilder, Review}
import ch.fhnw.ima.saav.style.GlobalStyles
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB}
import org.scalajs.dom
import org.scalajs.dom.DragEvent
import org.singlespaced.d3js.d3

import scala.scalajs.js
import scala.scalajs.js.annotation.JSName
import scala.scalajs.js.timers.SetTimeoutHandle
import scalacss.ScalaCssReact._

/**
  * A component which accepts a CSV file via drag and drop and imports its contents.
  */
object FileImportComponent {

  case class Props(importState: ImportState, onNewImportState: ImportState => Callback)

  private val css = GlobalStyles

  @JSName("URL")
  @js.native
  object URL extends dom.URL

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

      // D3.js parsing happens asynchronously
      // in order to report progress, parsing of each row will be dispatched as a separate thunk, giving
      // react a chance to update the UI in between
      d3.csv(url, (data: js.Array[Row]) => {

        val rowCount = data.size
        val builder = AnalysisBuilder.projectAnalysisBuilder

        // tracks how far along we are (execution order of async thunks is not guaranteed, but we need
        // to know when all rows have been parsed)
        val parsedRowCountRef = new AtomicInteger()

        for ((row, rowIndex) <- data.zipWithIndex) {
          parseRowAsync(onNewImportState, builder, rowCount, parsedRowCountRef, row, rowIndex)
        }

      })
    }


    def parseRowAsync(onNewImportState: (ImportState) => Callback, builder: AnalysisBuilder[Project], rowCount: Int, parsedRowCountRef: AtomicInteger, row: Row, rowIndex: Int): SetTimeoutHandle = {

      js.timers.setTimeout(10) {

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

        val parsedRowCount = parsedRowCountRef.incrementAndGet()

        // report progress (every 10th row is enough)
        if (rowIndex % 10 == 0) {
          val progress = (rowIndex + 1) / rowCount.toFloat
          onNewImportState(InProgress(progress)).runNow()
        }

        // parsing is complete -> model can be built
        if (parsedRowCount == rowCount) {
          val analysis = builder.build
          onNewImportState(Ready(analysis)).runNow()
        }

      }
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

  private val component = ReactComponentB[Props](FileImportComponent.getClass.getSimpleName)
    .renderBackend[Backend]
    .build

  def apply(importState: ImportState, onNewImportState: ImportState => Callback) = component(Props(importState, onNewImportState))

}
