package ch.fhnw.ima.saav.controller

import ch.fhnw.ima.saav.controller.io.{AnalysisConfigImporter, AnalysisDataImporter}
import ch.fhnw.ima.saav.model.app.{SaavModel, _}
import ch.fhnw.ima.saav.model.config.Config
import ch.fhnw.ima.saav.model.domain.{Analysis, AnalysisBuilder}
import ch.fhnw.ima.saav.model.weight.{Quality, Weights}
import diode._
import org.scalajs.dom.File

import scala.concurrent.Future
import scala.language.postfixOps
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.util.{Failure, Success}

final case class StartImportAction(configFileUrl: String, dataFile: File) extends Action

final case class DataImportInProgressAction(progress: Float, builder: AnalysisBuilder, rows: Seq[Array[String]], batchIndex: Int) extends Action

final case class DataImportFailedAction(throwable: Throwable, logToConsole: Boolean = true) extends Action

final case class AnalysisReadyAction(analysis: Analysis) extends Action

class AnalysisImportHandler[M](modelRW: ModelRW[M, Either[NoDataAppModel, AppModel]]) extends ActionHandler(modelRW) {

  override def handle: PartialFunction[Any, ActionResult[M]] = {

    case StartImportAction(configFileUrl, dataFile) =>
      AnalysisConfigImporter.importConfig(configFileUrl).andThen {
        case Success(analysisConfig) =>
          println(s"[${getClass.getSimpleName}] Parsed config:\n$analysisConfig")

        case Failure(_) =>
          // TODO: Fail once all valid JSONs are in place
      }
      val importData = AnalysisDataImporter.importData(dataFile)
      val nextAction = Effect(importData)
      effectOnly(nextAction)

    case AnalysisReadyAction(analysis) =>

      // TODO: Read config from external JSON

      val subCriteria = analysis.criteria.flatMap(_.subCriteria)
      val subCriteriaWeights = subCriteria.map(_.id -> Quality(1.0)) toMap
      val indicatorIds = subCriteria.flatMap(_.indicators).map(_.id)
      val weights = Weights(subCriteriaWeights, indicatorIds.toSet)

      val config = new Config {
        val defaultWeights: Weights = weights
      }

      val model = AppModel(analysis, config)
      updated(Right(model))

    case DataImportInProgressAction(progress, builder, rows, batchIndex) =>
      val parseAction: Future[Action] = AnalysisDataImporter.parseRowBatchAsync(builder, rows, batchIndex)
      val newModel = Left(NoDataAppModel(ImportInProgress(progress)))
      val nextAction = Effect(parseAction)
      updated(newModel, nextAction)

    case DataImportFailedAction(t, logToConsole) =>
      if (logToConsole) {
        println(s"[${getClass.getSimpleName}] Error: ${String.valueOf(t.getMessage)}")
        t.printStackTrace()
      }
      updated(Left(NoDataAppModel(ImportFailed(t))))

  }

}

object AnalysisImportHandler {

  def modelGet: (SaavModel) => Either[NoDataAppModel, AppModel] =
    _.model

  def modelSet: (SaavModel, Either[NoDataAppModel, AppModel]) => SaavModel = (m, v) =>
    m.copy(model = v)

}