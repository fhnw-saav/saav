package ch.fhnw.ima.saav.component

import ch.fhnw.ima.saav.component.bootstrap.Button
import ch.fhnw.ima.saav.controller.{AnalysisImportFailedAction, AnalysisImportInProgressAction, AnalysisReadyAction}
import ch.fhnw.ima.saav.model._
import ch.fhnw.ima.saav.model.app._
import ch.fhnw.ima.saav.model.domain._
import diode.react.ModelProxy
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{Callback, ReactComponentB, ReactComponentU, TopNode}
import org.scalajs.dom
import org.scalajs.dom.{DragEvent, UIEvent}

import scala.scalajs.js
import scalacss.ScalaCssReact._

/**
  * A component which accepts a CSV file via drag and drop and imports its contents.
  */
object FileImportComponent {

  case class Props(proxy: ModelProxy[NoDataAppModel])

  type Row = Array[String]

  private def handleDragOver(e: DragEvent) = Callback {
    e.stopPropagation()
    e.preventDefault()
    e.dataTransfer.dropEffect = "copy"
  }

  // callbacks which are invoked during file parsing
  // 'runNow' is needed because all parsing happens asynchronously

  private def handleProgress(proxy: ModelProxy[NoDataAppModel])(progress: Float): Unit =
    proxy.dispatch(AnalysisImportInProgressAction(progress)).runNow()

  private def handleReady(proxy: ModelProxy[NoDataAppModel])(analysis: Analysis): Unit =
    proxy.dispatch(AnalysisReadyAction(analysis)).runNow()

  private def handleError(proxy: ModelProxy[NoDataAppModel])(t: Throwable): Unit =
    proxy.dispatch(AnalysisImportFailedAction(t)).runNow()

  private def handleFileDropped(proxy: ModelProxy[NoDataAppModel])(e: DragEvent): Callback = {
    e.stopPropagation()
    e.preventDefault()

    // actual file parsing

    try {
      val files = e.dataTransfer.files
      if (files.length > 0) {
        val reader = new dom.FileReader()
        reader.readAsText(files.item(0))
        reader.onload = (e: UIEvent) => {
          // Cast is OK since we are calling readAsText
          val contents = reader.result.asInstanceOf[String]
          parseModel(contents, handleProgress(proxy), handleReady(proxy), handleError(proxy))
        }

      } else {
        Callback.log("No files to import")
      }
    } catch {
      case t: Throwable => handleError(proxy)(t)
    }
    Callback.empty
  }

  private def parseModel(contents: String, handleProgress: Float => Any, handleReady: Analysis => Any, handleError: Throwable => Any): Unit = {

    // to be used as a basis to fill analysis model
    val rows: Seq[Row] = splitContentsIntoRows(contents)

    val builder = AnalysisBuilder()

    // to report progress, rows are parsed asynchronously, giving react a chance to update the UI in between
    // to avoid timer congestion, we don't spawn a timer for each row, but parse row batches
    parseRowBatchAsync(builder, rows, 0, handleProgress, handleReady, handleError)
  }

  private def splitContentsIntoRows(contents: String) = {
    (for {
      line <- contents.lines
    } yield {

      def unquote(str: String) = {
        if (str != null && str.length >= 2 && str.charAt(0) == '\"' && str.charAt(str.length - 1) == '\"')
          str.substring(1, str.length - 1)
        else
          str
      }

      // split at comma (but handle quotes > http://stackoverflow.com/questions/15738918/splitting-a-csv-file-with-quotes-as-text-delimiter-using-string-split)
      line.split(",(?=([^\"]*\"[^\"]*\")*[^\"]*$)")
        .map(_.trim)
        .map(unquote)

    }).toStream
      .drop(1) // skip header
  }

  def parseRowBatchAsync(builder: AnalysisBuilder, rows: Seq[Row], batchIndex: Int, handleProgress: Float => Any, handleReady: Analysis => Any, handleError: Throwable => Any): Unit = {

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

  def parseRow(builder: AnalysisBuilder, row: Row): AnalysisBuilder = {

    val columnIt = row.iterator

    val project = Entity(EntityId(columnIt.next()))
    val hierarchyLevels = columnIt.next().split(":::")
    val criteria = hierarchyLevels(0)
    val subCriteria = hierarchyLevels(1)
    val indicator = hierarchyLevels(2)
    val review = ReviewId(columnIt.next())
    val value = columnIt.next().toDouble

    builder
      .criteria(criteria)
      .subCriteria(subCriteria)
      .indicator(indicator)
      .addValue(project, review, value)

    builder

  }

  private def importMockAnalysis(proxy: ModelProxy[NoDataAppModel]) =
    proxy.dispatch(AnalysisReadyAction(mockAnalysis))

  private def importAlphabetSoupAnalysis(proxy: ModelProxy[NoDataAppModel]) =
    proxy.dispatch(AnalysisReadyAction(alphabetSoupAnalysis))

  private val component = ReactComponentB[Props](FileImportComponent.getClass.getSimpleName)
    .render_P(p => {
      p.proxy.value.importState match {
        case ImportNotStarted() =>
          <.div(
            <.div(css.fileDropZone,
              ^.onDragOver ==> handleDragOver,
              ^.onDrop ==> handleFileDropped(p.proxy),
              <.div(
                <.h1("Drag and drop"),
                <.p("To import data from CSV file")
              )),
            <.p(^.textAlign.center, css.vSpaced, Button(onClick = importMockAnalysis(p.proxy), "Quick, some mock data, please!")),
            <.p(^.textAlign.center, css.vSpaced, Button(onClick = importAlphabetSoupAnalysis(p.proxy), "Quick, some alphabet soup, please!"))
          )
        case ImportInProgress(progress) =>
          <.div(css.fileDropZone, <.h1("Import in progress"), <.p((progress * 100).toInt + "%"))
        case ImportFailed(_) =>
          <.div(css.fileDropZone, <.h1("Import failed"), <.p("See console log for details"))
        case _ => <.div()
      }
    })
    .build

  def apply(proxy: ModelProxy[NoDataAppModel]): ReactComponentU[Props, Unit, Unit, TopNode] = component(Props(proxy))

}
