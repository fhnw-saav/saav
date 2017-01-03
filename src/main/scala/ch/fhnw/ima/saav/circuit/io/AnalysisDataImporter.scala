package ch.fhnw.ima.saav.circuit.io

import ch.fhnw.ima.saav.model.config.AnalysisConfig
import ch.fhnw.ima.saav.model.domain._
import org.scalajs.dom
import org.scalajs.dom._

import scala.concurrent.{Future, Promise}
import scala.scalajs.js

/**
  * Asynchronous data import, scheduled in batches in order to give the UI a chance to render progress (the pleasures
  * of single-threaded JavaScript).
  */
object AnalysisDataImporter {

  val BatchSize = 10

  type Row = Array[String]

  /** Accumulates imported data in a builder and carries current import state from one batch to the next */
  final case class ImportState(analysisConfig: AnalysisConfig, analysisBuilder: AnalysisBuilder, allRows: Seq[Row], nextBatchIndex: Int)

  /** Reads all rows from the given data file and prepares the necessary data structures for subsequent, batched import. */
  def importDataAsync(analysisConfig: AnalysisConfig, dataFile: File): Future[ImportState] = {
    val reader = new dom.FileReader()
    reader.readAsText(dataFile)
    val resultPromise = Promise[ImportState]()
    reader.onload = (_: UIEvent) => {
      // Cast is OK since we are calling readAsText
      val contents = reader.result.asInstanceOf[String]
      val rows: Seq[Row] = splitContentsIntoRows(contents)
      val builder = AnalysisBuilder()
      val firstBatch = ImportState(analysisConfig, builder, rows, 0)
      resultPromise.success(firstBatch)
    }
    resultPromise.future
  }

  def splitContentsIntoRows(contents: String): Seq[Row] = {
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

  /**
    * Asynchronously parses one batch of rows. Either returns the complete analysis (if this parsing invocation managed
    * to parse all remaining rows), or it returns a configuration for the next batch to be parsed.
    */
  def parseRowBatchAsync(importState: ImportState): Future[Either[Analysis, ImportState]] = {

    val resultPromise = Promise[Either[Analysis, ImportState]]()
    js.timers.setTimeout(0) {
      try {
        val action = parseRowBatch(importState)
        resultPromise.success(action)
      } catch {
        case t: Throwable => resultPromise.failure(t)
      }
    }

    resultPromise.future

  }

  def parseRowBatch(importState: ImportState): Either[Analysis, ImportState] = {
    val batchIndex = importState.nextBatchIndex
    val rows = importState.allRows
    val builder = importState.analysisBuilder
    val analysisConfig = importState.analysisConfig
    for (
      indexWithinBatch <- 0 until BatchSize;
      rowIndex = (batchIndex * BatchSize) + indexWithinBatch
      if rowIndex < rows.length
    ) {
      val row = rows(rowIndex)
      parseRow(builder, rowIndex, row)
    }
    val parsedRowCount = (batchIndex * BatchSize) + BatchSize
    val isLastBatch = parsedRowCount >= rows.length
    if (isLastBatch) {
      val analysis = builder.build
      Left(analysis)
    } else {
      Right(ImportState(analysisConfig, builder, rows, batchIndex + 1))
    }
  }

  def parseRow(builder: AnalysisBuilder, rowIndex: Int, row: Row, allowValuesOutsideRange: Boolean = false): AnalysisBuilder = {

    val columnIt = row.iterator

    val project = Entity(EntityId(columnIt.next()))
    val hierarchyLevels = columnIt.next().split(":::")
    val criteria = hierarchyLevels(0)
    val subCriteria = hierarchyLevels(1)
    val indicator = hierarchyLevels(2)
    val review = ReviewId(columnIt.next())
    val value = columnIt.next().toDouble

    if ((value < 1 || value > 5) && !allowValuesOutsideRange) {
      val humanFriendlyRowIndex = rowIndex + 2 // off-by-one, header
      throw new IllegalStateException(s"Line #$humanFriendlyRowIndex: Value '$value' outside of allowed range [1,5]")
    }

    builder
      .criteria(criteria)
      .subCriteria(subCriteria)
      .indicator(indicator)
      .addValue(project, review, value)

    builder

  }

}
