package ch.fhnw.ima.saav
package controller

import ch.fhnw.ima.saav.model.app._
import ch.fhnw.ima.saav.model.color._
import ch.fhnw.ima.saav.model.domain.{Analysis, Entity}
import diode._
import diode.react.ReactConnector

object SaavController {

  // Analysis Actions

  final case class AnalysisImportInProgressAction(progress: Float) extends Action

  final case class AnalysisImportFailedAction(throwable: Throwable, logToConsole: Boolean = true) extends Action

  final case class AnalysisReadyAction[E <: Entity](analysis: Analysis[E]) extends Action

  class AnalysisHandler[M](modelRW: ModelRW[M, Either[NoDataModel, PlottableQualityDataModel]]) extends ActionHandler(modelRW) {

    override def handle = {
      case AnalysisImportInProgressAction(progress) => updated(Left(NoDataModel(ImportInProgress(progress))))
      case a@AnalysisImportFailedAction(t, logToConsole) =>
        if (logToConsole) {
          logException(this.getClass.getSimpleName, String.valueOf(a), t)
        }
        updated(Left(NoDataModel(ImportFailed(t))))
      case AnalysisReadyAction(analysis) =>
        val model = PlottableQualityDataModel(analysis)
        updated(Right(model))

    }

  }

  // Entity Update Actions

  final case class AutoColorizeAction() extends Action

  final case class UpdateEntityColorAction(entity: Entity, webColor: WebColor) extends Action

  final case class UpdateEntitySelectionAction(entities: Set[Entity], isSelected: Boolean) extends Action

  final case class UpdateEntityPinningAction(pinnedEntity: Option[Entity]) extends Action

  class EntityHandler[M](modelRW: ModelRW[M, Seq[PlottableEntity]]) extends ActionHandler(modelRW) {

    override def handle = {
      case AutoColorizeAction() =>
        val selectedEntities = value.filter(_.isSelected)
        val colors = autoColorMap(selectedEntities)
        val newEntities = value.map(e => e.copy(color = colors(e)))
        updated(newEntities)
      case UpdateEntityColorAction(entity, color) =>
        val entities = value.map {
          case e if e.entity == entity => e.copy(color = color)
          case e => e
        }
        updated(entities)
      case UpdateEntitySelectionAction(selection, isSelected) =>
        val oldEntities = value
        val newEntities = oldEntities.map { e =>
          if (selection.contains(e.entity)) {
            val newIsPinned = e.isPinned && isSelected
            e.copy(
              isSelected = isSelected,
              isPinned = newIsPinned
            )
          } else {
            e
          }
        }
        updated(newEntities)
      case UpdateEntityPinningAction(pinnedEntity) =>
        val oldEntities = value
        val newEntities = oldEntities.map {
          case e if pinnedEntity.contains(e.entity) => e.copy(isPinned = true)
          case e => e.copy(isPinned = false)
        }
        updated(newEntities)
    }

  }

  // Circuit

  object SaavCircuit extends Circuit[SaavModel] with ReactConnector[SaavModel] {

    override protected def initialModel = SaavModel()

    private val analysisHandler = new AnalysisHandler(zoomRW(_.model)((m, v) => m.copy(model = v)))

    private val entityHandler = {
      def get = (m: SaavModel) =>
        m.model.right.toOption.map(_.rankedEntities).getOrElse(Seq())
      def set = (m: SaavModel, v: Seq[PlottableEntity]) =>
        m.copy(model = m.model.right.map(_.copy(rankedEntities = v)))
      new EntityHandler(zoomRW(get)(set))
    }

    override protected val actionHandler = composeHandlers(analysisHandler, entityHandler)

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
