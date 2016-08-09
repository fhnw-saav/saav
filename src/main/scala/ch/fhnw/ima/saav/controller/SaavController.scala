package ch.fhnw.ima.saav
package controller

import ch.fhnw.ima.saav.model._
import ch.fhnw.ima.saav.model.model.{Analysis, Entity}
import diode._
import diode.react.ReactConnector

object SaavController {

  // Actions

  final case class AnalysisImportInProgressAction(progress: Float) extends Action

  final case class AnalysisImportFailedAction(throwable: Throwable, logToConsole: Boolean = true) extends Action

  final case class AnalysisReadyAction[E <: Entity](analysis: Analysis[E]) extends Action

  // Handlers

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

  // Circuit

  object SaavCircuit extends Circuit[SaavModel] with ReactConnector[SaavModel] {

    override protected def initialModel = SaavModel()

    override protected val actionHandler = composeHandlers(
      new AnalysisHandler(zoomRW(_.analysis)((model, newValue) => model.copy(analysis = newValue)))
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
