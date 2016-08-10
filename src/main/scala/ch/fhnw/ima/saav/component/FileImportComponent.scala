package ch.fhnw.ima.saav.component

import ch.fhnw.ima.saav.controller.SaavController.{AnalysisImportFailedAction, AnalysisImportInProgressAction, AnalysisReadyAction}
import ch.fhnw.ima.saav.model.domain.Entity.Project
import ch.fhnw.ima.saav.model.domain.{Analysis, AnalysisBuilder, Review}
import ch.fhnw.ima.saav.model.{ImportFailed, ImportInProgress, ImportNotStarted, SaavModel}
import diode.react.ModelProxy
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{Callback, ReactComponentB}
import org.scalajs.dom.DragEvent
import org.singlespaced.d3js.d3

import scala.scalajs.js
import scalacss.ScalaCssReact._

/**
  * A component which accepts a CSV file via drag and drop and imports its contents.
  */
object FileImportComponent {

  case class Props(proxy: ModelProxy[SaavModel])

  type Row = js.Dictionary[String]

  private def handleDragOver(e: DragEvent) = Callback {
    e.stopPropagation()
    e.preventDefault()
    e.dataTransfer.dropEffect = "copy"
  }

  // callbacks which are invoked during file parsing
  // 'runNow' is needed because all parsing happens asynchronously

  private def handleProgress(proxy: ModelProxy[SaavModel])(progress: Float): Unit =
    proxy.dispatch(AnalysisImportInProgressAction(progress)).runNow()

  private def handleReady(proxy: ModelProxy[SaavModel])(analysis: Analysis[Project]): Unit =
    proxy.dispatch(AnalysisReadyAction(analysis)).runNow()

  private def handleError(proxy: ModelProxy[SaavModel])(t: Throwable): Unit =
    proxy.dispatch(AnalysisImportFailedAction(t)).runNow()

  private def handleFileDropped(proxy: ModelProxy[SaavModel])(e: DragEvent): Callback = {
    e.stopPropagation()
    e.preventDefault()

    // actual file parsing

    try {
      val files = e.dataTransfer.files
      if (files.length > 0) {
        val file = files(0)
        val url = URL.createObjectURL(file)

        parseModel(url, handleProgress(proxy), handleReady(proxy), handleError(proxy))

      } else {
        Callback.log("No files to import")
      }
    } catch {
      case t: Throwable => handleError(proxy)(t)
    }
    Callback.empty
  }

  private def parseModel(url: String, handleProgress: Float => Any, handleReady: Analysis[Project] => Any, handleError: Throwable => Any): Unit = {
    d3.csv(url, (rows: js.Array[Row]) => {
      val builder = AnalysisBuilder.projectAnalysisBuilder

      // to report progress, rows are parsed asynchronously, giving react a chance to update the UI in between
      // to avoid timer congestion, we don't spawn a timer for each row, but parse row batches
      parseRowBatchAsync(builder, rows, 0, handleProgress, handleReady, handleError)
    })
  }


  def parseRowBatchAsync(builder: AnalysisBuilder[Project], rows: Seq[Row], batchIndex: Int, handleProgress: Float => Any, handleReady: Analysis[Project] => Any, handleError: Throwable => Any): Unit = {

    val batchSize = 10

    js.timers.setTimeout(0) {

      try {

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
            handleReady(analysis)
          }
          // report progress and schedule next batch
          else if (indexWithinBatch == batchSize - 1) {
            val progress = (rowIndex + 1).toFloat / rows.length
            handleProgress(progress)
            parseRowBatchAsync(builder, rows, batchIndex + 1, handleProgress, handleReady, handleError)
          }

        }
      } catch {
        case t: Throwable => handleError(t)
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
    .render_P(p => {
      p.proxy.value.analysis match {
        case Left(ImportNotStarted()) =>
          <.div(css.fileDropZone,
            ^.onDragOver ==> handleDragOver,
            ^.onDrop ==> handleFileDropped(p.proxy),
            <.div(<.h1("Drag and drop"), <.p("To import data from CSV file")))
        case Left(ImportInProgress(progress)) =>
          <.div(css.fileDropZone, <.h1("Import in progress"), <.p((progress * 100).toInt + "%"))
        case Left(ImportFailed(t)) =>
          <.div(css.fileDropZone, <.h1("Import failed"), <.p("See console log for details"))
        case _ => <.div()
      }
    })
    .build

  def apply(proxy: ModelProxy[SaavModel]) = component(Props(proxy))

}
