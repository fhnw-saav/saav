package ch.fhnw.ima.saav.controller

import ch.fhnw.ima.saav.model.app.{SaavModel, _}
import ch.fhnw.ima.saav.model.config.Config
import ch.fhnw.ima.saav.model.domain.{Analysis, Entity}
import ch.fhnw.ima.saav.model.weight.{Quality, Weights}
import diode._

import scala.language.postfixOps

final case class AnalysisImportInProgressAction(progress: Float) extends Action

final case class AnalysisImportFailedAction(throwable: Throwable, logToConsole: Boolean = true) extends Action

final case class AnalysisReadyAction[E <: Entity](analysis: Analysis) extends Action

class AnalysisImportHandler[M](modelRW: ModelRW[M, Either[NoDataAppModel, AppModel]]) extends ActionHandler(modelRW) {

  override def handle: PartialFunction[Any, ActionResult[M]] = {
    case AnalysisImportInProgressAction(progress) => updated(Left(NoDataAppModel(ImportInProgress(progress))))
    case a@AnalysisImportFailedAction(t, logToConsole) =>
      if (logToConsole) {
        println(s"[${getClass.getSimpleName}] Error: ${String.valueOf(a)}")
        t.printStackTrace()
      }
      updated(Left(NoDataAppModel(ImportFailed(t))))
    case AnalysisReadyAction(analysis) =>

      // TODO: Read config from external JSON

      val subCriteria = analysis.criteria.flatMap(_.subCriteria)
      val subCriteriaWeights = subCriteria.map(_ -> Quality(1.0)) toMap
      val indicators = subCriteria.flatMap(_.indicators)
      val weights = Weights(subCriteriaWeights, indicators.toSet)

      val config = new Config {
        val defaultWeights: Weights = weights
      }

      val model = AppModel(analysis, config)
      updated(Right(model))

  }

}

object AnalysisImportHandler {

  def modelGet: (SaavModel) => Either[NoDataAppModel, AppModel] =
    _.model

  def modelSet: (SaavModel, Either[NoDataAppModel, AppModel]) => SaavModel = (m, v) =>
    m.copy(model = v)

}