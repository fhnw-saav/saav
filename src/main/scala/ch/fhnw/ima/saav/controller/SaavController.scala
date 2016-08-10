package ch.fhnw.ima.saav
package controller

import ch.fhnw.ima.saav.model._
import ch.fhnw.ima.saav.model.colors._
import ch.fhnw.ima.saav.model.domain.{Analysis, Entity}
import diode._
import diode.react.ReactConnector

object SaavController {

  // Analysis Actions

  final case class AnalysisImportInProgressAction(progress: Float) extends Action

  final case class AnalysisImportFailedAction(throwable: Throwable, logToConsole: Boolean = true) extends Action

  final case class AnalysisReadyAction[E <: Entity](analysis: Analysis[E]) extends Action

  class AnalysisHandler[M](modelRW: ModelRW[M, Either[ImportState, Analysis[Entity]]]) extends ActionHandler(modelRW) {

    override def handle = {
      case AnalysisImportInProgressAction(progress) => updated(Left(ImportInProgress(progress)))
      case a@AnalysisImportFailedAction(t, logToConsole) =>
        if (logToConsole) {
          logException(this.getClass.getSimpleName, String.valueOf(a), t)
        }
        updated(Left(ImportFailed(t)))
      case AnalysisReadyAction(analysis) => updated(Right(analysis))
    }

  }

  // Color Actions

  final case class AutoColorizeAction(entities: Seq[Entity]) extends Action
  final case class UpdateEntityColorAction(entity: Entity, webColor: WebColor) extends Action

  class ColorHandler[M](modelRW: ModelRW[M, Map[Entity, WebColor]]) extends ActionHandler(modelRW) {

    private def colorize(entities: Seq[Entity]) =
      entities.zipWithIndex.map {
        case (e, i) => (e, solarizedPalette(i % solarizedPalette.size))
      }.toMap

    override def handle = {
      case AutoColorizeAction(entities) => updated(colorize(entities))
      case UpdateEntityColorAction(entity, color) => updated(value + (entity -> color))
    }

  }

  // Circuit

  object SaavCircuit extends Circuit[SaavModel] with ReactConnector[SaavModel] {

    override protected def initialModel = SaavModel()

    override protected val actionHandler = composeHandlers(
      new AnalysisHandler(zoomRW(_.analysis)((model, newValue) => model.copy(analysis = newValue))),
      new ColorHandler(zoomRW(_.colors)((model, newValue) => model.copy(colors = newValue)))
    )

    override def handleError(msg: String): Unit = {
      val name = SaavController.getClass.getSimpleName
      logError(name, msg)
    }

  }

  // Logging

  def logError(componentName: String, msg: String): Unit = {
    println(s"[$componentName] Error: $msg")
  }

  def logException(componentName: String, msg: String, throwable: Throwable): Unit = {
    logError(componentName, msg)
    throwable.printStackTrace()
  }

}
