package ch.fhnw.ima.saav.controller.io

import ch.fhnw.ima.saav.controller.{AnalysisReadyAction, DataImportFailedAction, DataImportInProgressAction}
import ch.fhnw.ima.saav.model.config.AnalysisConfig
import ch.fhnw.ima.saav.model.domain._
import diode.Action
import org.scalajs.dom
import org.scalajs.dom._

import scala.concurrent.{Future, Promise}
import scala.scalajs.js

object AnalysisDataImporter {

  val BatchSize = 10

  def importData(analysisConfig: AnalysisConfig, dataFile: File): Future[Action] = {
    val reader = new dom.FileReader()
    reader.readAsText(dataFile)
    val resultPromise = Promise[Action]()
    reader.onload = (_: UIEvent) => {
      // Cast is OK since we are calling readAsText
      val contents = reader.result.asInstanceOf[String]
      resultPromise.success(parseModel(analysisConfig, contents))
    }
    resultPromise.future
  }

  type Row = Array[String]

  private def parseModel(analysisConfig: AnalysisConfig, contents: String): Action = {

    // to be used as a basis to fill analysis model
    val rows: Seq[Row] = splitContentsIntoRows(contents)

    val builder = AnalysisBuilder()

    DataImportInProgressAction(analysisConfig, 0, builder, rows, 0)
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

  def parseRowBatchAsync(analysisConfig: AnalysisConfig, builder: AnalysisBuilder, rows: Seq[Row], batchIndex: Int): Future[Action] = {

    val resultPromise = Promise[Action]()
    js.timers.setTimeout(0) {
      try {
        val action = parseRowBatch(analysisConfig, builder, rows, batchIndex)
        resultPromise.success(action)
      } catch {
        case t: Throwable => resultPromise.success(DataImportFailedAction(t))
      }
    }

    resultPromise.future

  }

  def parseRowBatch(analysisConfig: AnalysisConfig, builder: AnalysisBuilder, rows: Seq[Row], batchIndex: Int): Action = {
    for (
      indexWithinBatch <- 0 until BatchSize;
      rowIndex = (batchIndex * BatchSize) + indexWithinBatch
      if rowIndex < rows.length
    ) {
      val row = rows(rowIndex)
      parseRow(builder, row)
    }
    val parsedRowCount = (batchIndex * BatchSize) + BatchSize
    val isLastBatch = parsedRowCount >= rows.length
    if (isLastBatch) {
      val analysis = builder.build
      AnalysisReadyAction(analysisConfig, analysis)
    } else {
      val progress = parsedRowCount.toFloat / rows.length
      DataImportInProgressAction(analysisConfig, progress, builder, rows, batchIndex + 1)
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

}
