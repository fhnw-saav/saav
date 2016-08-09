package ch.fhnw.ima.saav
package controller

import ch.fhnw.ima.saav.model._
import ch.fhnw.ima.saav.model.model.Analysis
import ch.fhnw.ima.saav.model.model.Entity.Project
import diode._
import diode.react.ReactConnector

object SaavController {

  // Actions

  final case class ProjectAnalysisImportInProgress(progress: Float) extends Action

  final case class ProjectAnalysisImportFailed(throwable: Throwable) extends Action

  final case class ProjectAnalysisReady(analysis: Analysis[Project]) extends Action

  // Handlers

  class ProjectAnalysisHandler(modelRW: ModelRW[SaavModel, Either[ImportProgress, Analysis[Project]]]) extends ActionHandler(modelRW) {

    override def handle = {
      case ProjectAnalysisImportInProgress(progress) => updated(Left(InProgress(progress)))
      case a @ ProjectAnalysisImportFailed(t) =>
        logException(this.getClass.getSimpleName, String.valueOf(a), t)
        updated(Left(Failed(t)))
      case ProjectAnalysisReady(analysis) => updated(Right(analysis))
    }

  }

  // Circuit

  object SaavCircuit extends Circuit[SaavModel] with ReactConnector[SaavModel] {

    override protected def initialModel = SaavModel(Left(NotStarted()))

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
