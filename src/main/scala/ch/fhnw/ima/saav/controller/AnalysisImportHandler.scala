package ch.fhnw.ima.saav.controller

import ch.fhnw.ima.saav.controller.io.{AnalysisConfigImporter, AnalysisDataImporter}
import ch.fhnw.ima.saav.controller.logic.AppModelFactory
import ch.fhnw.ima.saav.model.app.{SaavModel, _}
import ch.fhnw.ima.saav.model.config.AnalysisConfig
import ch.fhnw.ima.saav.model.domain.{Analysis, AnalysisBuilder}
import diode._
import org.scalajs.dom.File

import scala.concurrent.Future
import scala.language.postfixOps
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.util.{Failure, Success}

final case class StartImportAction(configFileUrl: String, dataFile: File) extends Action

final case class DataImportInProgressAction(analysisConfig: AnalysisConfig, progress: Float, builder: AnalysisBuilder, rows: Seq[Array[String]], batchIndex: Int) extends Action

final case class DataImportFailedAction(throwable: Throwable, logToConsole: Boolean = true) extends Action

final case class AnalysisConfigReadyAction(analysisConfig: AnalysisConfig, dataFile: File) extends Action

final case class AnalysisReadyAction(analysisConfig: AnalysisConfig = AnalysisConfig.empty, analysis: Analysis) extends Action

class AnalysisImportHandler[M](modelRW: ModelRW[M, Either[NoDataAppModel, AppModel]]) extends ActionHandler(modelRW) {

  override def handle: PartialFunction[Any, ActionResult[M]] = {

    case StartImportAction(configFileUrl, dataFile) =>
      val importConfigFuture = AnalysisConfigImporter.importConfig(configFileUrl)
      val nextAction = importConfigFuture.transform {
        case Success(analysisConfig) => Success(AnalysisConfigReadyAction(analysisConfig, dataFile))
        case Failure(t) => Success(DataImportFailedAction(t))
      }
      effectOnly(Effect(nextAction))

    case AnalysisConfigReadyAction(analysisConfig, dataFile) =>
      println(s"[${getClass.getSimpleName}] Parsed config:\n$analysisConfig")
      val nextAction = AnalysisDataImporter.importData(analysisConfig, dataFile)
      effectOnly(Effect(nextAction))

    case DataImportInProgressAction(analysisConfig, progress, builder, rows, batchIndex) =>
      val parseAction: Future[Action] = AnalysisDataImporter.parseRowBatchAsync(analysisConfig, builder, rows, batchIndex)
      val newModel = Left(NoDataAppModel(ImportInProgress(progress)))
      val nextAction = Effect(parseAction)
      updated(newModel, nextAction)

    case AnalysisReadyAction(analysisConfig, analysis) =>
      val model = AppModelFactory.createAppModel(analysisConfig, analysis)
      updated(Right(model))

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