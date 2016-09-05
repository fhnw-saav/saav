package ch.fhnw.ima.saav
package controller

import ch.fhnw.ima.saav.model.app._
import ch.fhnw.ima.saav.model.color._
import ch.fhnw.ima.saav.model.domain.{Analysis, Entity, SubCriteria}
import diode._
import diode.react.ReactConnector

object SaavController {

  // Manages Analysis Import

  final case class AnalysisImportInProgressAction(progress: Float) extends Action

  final case class AnalysisImportFailedAction(throwable: Throwable, logToConsole: Boolean = true) extends Action

  final case class AnalysisReadyAction[E <: Entity](analysis: Analysis) extends Action

  class AnalysisImportHandler[M](modelRW: ModelRW[M, Either[NoDataAppModel, AppModel]]) extends ActionHandler(modelRW) {

    override def handle = {
      case AnalysisImportInProgressAction(progress) => updated(Left(NoDataAppModel(ImportInProgress(progress))))
      case a@AnalysisImportFailedAction(t, logToConsole) =>
        if (logToConsole) {
          logException(this.getClass.getSimpleName, String.valueOf(a), t)
        }
        updated(Left(NoDataAppModel(ImportFailed(t))))
      case AnalysisReadyAction(analysis) =>
        val model = AppModel(analysis)
        updated(Right(model))

    }

  }

  // Manages Entity Colors

  final case class AutoColorizeAction(entities: Seq[Entity]) extends Action

  final case class UpdateEntityColorAction(entity: Entity, webColor: WebColor) extends Action

  class ColorHandler[M](modelRW: ModelRW[M, Map[Entity, WebColor]]) extends ActionHandler(modelRW) {

    override def handle = {
      case AutoColorizeAction(entities) => updated(autoColorMap(entities))
      case UpdateEntityColorAction(entity, color) => updated(value + (entity -> color))
    }

  }

  // Manages EntitySelectionModel

  final case class UpdateEntitySelectionAction(entities: Set[Entity], isSelected: Boolean) extends Action

  final case class UpdateEntityPinningAction(pinnedEntity: Option[Entity]) extends Action

  class EntitySelectionHandler[M](modelRW: ModelRW[M, EntitySelectionModel]) extends ActionHandler(modelRW) {

    override def handle = {
      case UpdateEntitySelectionAction(entities, isSelected) =>
        val newSelection = if (isSelected) {
          value.selected ++ entities
        } else {
          value.selected -- entities
        }
        // clear pinning upon de-selection
        val newPinned: Option[Entity] = value.pinned.flatMap { currentlyPinned =>
          if (newSelection.contains(currentlyPinned)) {
            value.pinned
          } else {
            None
          }
        }
        updated(EntitySelectionModel(newSelection, newPinned))
      case UpdateEntityPinningAction(newPinned) => updated(value.copy(pinned = newPinned))
    }

  }

  // Manages SubCriteriaSelectionModel

  final case class UpdateSubCriteriaHoveringAction(hoveredSubCriteria: Option[SubCriteria]) extends Action

  class SubCriteriaSelectionHandler[M](modelRW: ModelRW[M, SubCriteriaSelectionModel]) extends ActionHandler(modelRW) {

    override def handle = {
      case UpdateSubCriteriaHoveringAction(hoveredSubCriteria) =>
        updated(value.copy(hovered = hoveredSubCriteria))
    }

  }

  // Circuit

  object SaavCircuit extends Circuit[SaavModel] with ReactConnector[SaavModel] {

    override protected def initialModel = SaavModel()

    private val analysisImportHandler = new AnalysisImportHandler(zoomRW(_.model)((m, v) => m.copy(model = v)))

    private val colorHandler = {
      def modelGet = (m: SaavModel) =>
        m.model.right.toOption.map(_.colorMap).getOrElse(Map())
      def modelSet = (m: SaavModel, v: Map[Entity, WebColor]) =>
        m.copy(model = m.model.right.map(_.copy(colorMap = v)))
      new ColorHandler(zoomRW(modelGet)(modelSet))
    }

    private val entitySelectionHandler = {
      def modelGet = (m: SaavModel) =>
        m.model.right.toOption.map(_.entitySelectionModel).get
      def modelSet = (m: SaavModel, v: EntitySelectionModel) =>
        m.copy(model = m.model.right.map(_.copy(entitySelectionModel = v)))
      new EntitySelectionHandler(zoomRW(modelGet)(modelSet))
    }

    private val subCriteriaSelectionHandler = {
      def modelGet = (m: SaavModel) =>
        m.model.right.toOption.map(_.subCriteriaSelectionModel).get
      def modelSet = (m: SaavModel, v: SubCriteriaSelectionModel) =>
        m.copy(model = m.model.right.map(_.copy(subCriteriaSelectionModel = v)))
      new SubCriteriaSelectionHandler(zoomRW(modelGet)(modelSet))
    }

    override protected val actionHandler =
      composeHandlers(analysisImportHandler, colorHandler, entitySelectionHandler, subCriteriaSelectionHandler)

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
