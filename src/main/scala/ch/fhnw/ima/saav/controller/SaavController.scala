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

  class AnalysisHandler[M](modelRW: ModelRW[M, Either[NoDataModel, DataModel]]) extends ActionHandler(modelRW) {

    override def handle = {
      case AnalysisImportInProgressAction(progress) => updated(Left(NoDataModel(ImportInProgress(progress))))
      case a@AnalysisImportFailedAction(t, logToConsole) =>
        if (logToConsole) {
          logException(this.getClass.getSimpleName, String.valueOf(a), t)
        }
        updated(Left(NoDataModel(ImportFailed(t))))
      case AnalysisReadyAction(analysis) =>
        val model = DataModel(analysis, selectedEntities = analysis.entities.toSet)
        updated(Right(model))

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

  // Entity Selection

  final case class UpdateEntitySelectionAction(entity: Entity, selected: Boolean) extends Action

  class EntitySelectionHandler[M](modelRW: ModelRW[M, Set[Entity]]) extends ActionHandler(modelRW) {

    override def handle = {
      case UpdateEntitySelectionAction(entity, selected) =>
        val newModel = if (selected) {
          value + entity
        } else {
          value - entity
        }
        updated(newModel)
    }

  }

  // Circuit

  object SaavCircuit extends Circuit[SaavModel] with ReactConnector[SaavModel] {

    override protected def initialModel = SaavModel()

    private val analysisHandler = new AnalysisHandler(zoomRW(_.model)((m, v) => m.copy(model = v)))

    private val colorHandler = {
      def modelGet = (m: SaavModel) =>
        m.model.right.toOption.map(_.colors).getOrElse(Map())
      def modelSet = (m: SaavModel, v: Map[Entity, WebColor]) =>
        m.copy(model = m.model.right.map(_.copy(colors = v)))
      new ColorHandler(zoomRW(modelGet)(modelSet))
    }

    private val selectionHandler = {
      def modelGet = (m: SaavModel) =>
        m.model.right.toOption.map(_.selectedEntities).getOrElse(Set())
      def modelSet = (m: SaavModel, v: Set[Entity]) =>
        m.copy(model = m.model.right.map(_.copy(selectedEntities = v)))
      new EntitySelectionHandler(zoomRW(modelGet)(modelSet))
    }

    override protected val actionHandler = composeHandlers(analysisHandler, colorHandler, selectionHandler)

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
