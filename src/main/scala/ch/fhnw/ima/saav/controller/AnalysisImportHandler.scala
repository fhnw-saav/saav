package ch.fhnw.ima.saav.controller

import ch.fhnw.ima.saav.controller.io.AnalysisDataImporter.ImportState
import ch.fhnw.ima.saav.controller.io.{AnalysisConfigImporter, AnalysisDataImporter}
import ch.fhnw.ima.saav.controller.logic.AppModelFactory
import ch.fhnw.ima.saav.model.app.{SaavModel, _}
import ch.fhnw.ima.saav.model.config.AnalysisConfig
import ch.fhnw.ima.saav.model.domain.Analysis
import diode._
import org.scalajs.dom.File

import scala.language.postfixOps
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.util.{Failure, Success}

final case class StartImportAction(configFileUrl: String, dataFile: File) extends Action

final case class AnalysisConfigReadyAction(analysisConfig: AnalysisConfig, dataFile: File) extends Action

final case class AnalysisDataImportInProgressAction(importState: ImportState) extends Action

final case class ImportFailedAction(throwable: Throwable, logToConsole: Boolean = true) extends Action

final case class AnalysisReadyAction(analysisConfig: AnalysisConfig = AnalysisConfig.empty, analysis: Analysis) extends Action

class AnalysisImportHandler[M](modelRW: ModelRW[M, Either[NoDataAppModel, AppModel]]) extends ActionHandler(modelRW) {

  override def handle: PartialFunction[Any, ActionResult[M]] = {

    case StartImportAction(configFileUrl, dataFile) =>
      val importConfigFuture = AnalysisConfigImporter.importConfigAsync(configFileUrl)
      val nextAction = importConfigFuture.transform {
        case Success(analysisConfig) => Success(AnalysisConfigReadyAction(analysisConfig, dataFile))
        // error handling is a first class citizen which our UI can handle --> map to Success
        case Failure(t) => Success(ImportFailedAction(t))
      }
      effectOnly(Effect(nextAction))

    case AnalysisConfigReadyAction(analysisConfig, dataFile) =>
      println(s"[${getClass.getSimpleName}] Parsed config:\n$analysisConfig")
      val importDataFuture = AnalysisDataImporter.importDataAsync(analysisConfig, dataFile)
      val nextAction = importDataFuture.transform {
        case Success(importState) => Success(AnalysisDataImportInProgressAction(importState))
        // error handling is a first class citizen which our UI can handle --> map to Success
        case Failure(t) => Success(ImportFailedAction(t))
      }
      effectOnly(Effect(nextAction))

    case AnalysisDataImportInProgressAction(importState) =>
      val progress = (importState.nextBatchIndex * AnalysisDataImporter.BatchSize).toFloat / importState.allRows.length
      val newModel = Left(NoDataAppModel(ImportInProgress(progress)))
      val importDataFuture = AnalysisDataImporter.parseRowBatchAsync(importState)
      val nextAction = importDataFuture.transform {
        // either we're done
        case Success(Left(analysis)) => Success(AnalysisReadyAction(importState.analysisConfig, analysis))
        // or there's another batch
        case Success(Right(nextImportState)) => Success(AnalysisDataImportInProgressAction(nextImportState))
        // error handling is a first class citizen which our UI can handle --> map to Success
        case Failure(t) => Success(ImportFailedAction(t))
      }
      updated(newModel, Effect(nextAction))

    case AnalysisReadyAction(analysisConfig, analysis) =>
      val model = AppModelFactory.createAppModel(analysisConfig, analysis)
      updated(Right(model))

    case ImportFailedAction(t, logToConsole) =>
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