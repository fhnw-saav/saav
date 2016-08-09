package ch.fhnw.ima.saav
package controller

import ch.fhnw.ima.saav.model._
import ch.fhnw.ima.saav.model.model.Analysis
import ch.fhnw.ima.saav.model.model.Entity.Project
import diode._
import diode.react.ReactConnector

object SaavController {

  // Actions

  final case class ProjectAnalysisImportInProgressAction(progress: Float) extends Action

  final case class ProjectAnalysisImportFailedAction(throwable: Throwable, logToConsole: Boolean = true) extends Action

  final case class ProjectAnalysisReadyAction(analysis: Analysis[Project]) extends Action

  // Handlers

  class ProjectAnalysisHandler[M](modelRW: ModelRW[M, Either[ImportState, Analysis[Project]]]) extends ActionHandler(modelRW) {

    override def handle = {
      case ProjectAnalysisImportInProgressAction(progress) => updated(Left(ImportInProgress(progress)))
      case a @ ProjectAnalysisImportFailedAction(t, logToConsole) =>
        if (logToConsole) {
          logException(this.getClass.getSimpleName, String.valueOf(a), t)
        }
        updated(Left(ImportFailed(t)))
      case ProjectAnalysisReadyAction(analysis) => updated(Right(analysis))
    }

  }

  // Circuit

  object SaavCircuit extends Circuit[SaavModel] with ReactConnector[SaavModel] {

    override protected def initialModel = SaavModel()

    override protected val actionHandler = composeHandlers(
      new ProjectAnalysisHandler(zoomRW(_.projectAnalysis)((model, newValue) => model.copy(projectAnalysis = newValue)))
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
