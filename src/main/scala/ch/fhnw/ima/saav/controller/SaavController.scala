package ch.fhnw.ima.saav
package controller

import ch.fhnw.ima.saav.model._
import ch.fhnw.ima.saav.model.app._
import ch.fhnw.ima.saav.model.color._
import ch.fhnw.ima.saav.model.domain.{Analysis, Entity}
import diode._
import diode.react.ReactConnector

import scala.collection.immutable.ListSet

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
        val model = DataModel(analysis, selectedEntities = ListSet.empty ++ analysis.entities)
        updated(Right(model))

    }

  }

  // Color Actions

  final case class AutoColorizeAction(entities: Seq[PlottableEntity]) extends Action

  final case class UpdateEntityColorAction(entity: PlottableEntity, webColor: WebColor) extends Action

  class ColorHandler[M](modelRW: ModelRW[M, Map[Entity, WebColor]]) extends ActionHandler(modelRW) {

    private def colorize(entities: Seq[Entity]) =
      entities.zipWithIndex.map {
        case (e, i) => (e, solarizedPalette(i % solarizedPalette.size))
      }.toMap.withDefaultValue(color.DefaultColor)

    override def handle = {
      case AutoColorizeAction(entities) => updated(colorize(entities.map(_.entity)))
      case UpdateEntityColorAction(entity, color) => updated(value + (entity.entity -> color))
    }

  }

  // Entity Selection & Pinning

  final case class UpdateEntitySelectionAction(entities: Seq[PlottableEntity], isSelected: Boolean) extends Action

  final case class UpdateEntityPinningAction(pinnedEntity: Option[PlottableEntity]) extends Action

  case class SelectionAndPinning(selected: ListSet[Entity] = ListSet.empty, pinned: Option[Entity] = None)

  class SelectionAndPinningHandler[M](modelRW: ModelRW[M, SelectionAndPinning]) extends ActionHandler(modelRW) {

    override def handle = {
      case UpdateEntitySelectionAction(entities, isSelected) =>
        val newSelection = if (isSelected) {
          value.selected ++ entities.map(_.entity)
        } else {
          value.selected -- entities.map(_.entity)
        }
        // clear pinning upon de-selection
        val newPinned: Option[Entity] = value.pinned.flatMap { currentlyPinned =>
          if (newSelection.contains(currentlyPinned)) {
            value.pinned
          } else {
            None
          }
        }
        updated(SelectionAndPinning(newSelection, newPinned))
      case UpdateEntityPinningAction(pinnedEntity) => updated(value.copy(pinned = pinnedEntity.map(_.entity)))
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

    private val selectionAndPinningHandler = {
      def modelGet = (m: SaavModel) =>
        m.model.right.toOption.map(dm => SelectionAndPinning(dm.selectedEntities, dm.pinnedEntity)).getOrElse(SelectionAndPinning())
      def modelSet = (m: SaavModel, v: SelectionAndPinning) =>
        m.copy(model = m.model.right.map(_.copy(selectedEntities = v.selected, pinnedEntity = v.pinned)))
      new SelectionAndPinningHandler(zoomRW(modelGet)(modelSet))
    }

    override protected val actionHandler = composeHandlers(analysisHandler, colorHandler, selectionAndPinningHandler)

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
