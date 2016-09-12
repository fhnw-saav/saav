package ch.fhnw.ima.saav
package controller

import ch.fhnw.ima.saav.model.app.{EntitySelectionModel, _}
import ch.fhnw.ima.saav.model.color._
import ch.fhnw.ima.saav.model.domain.{Analysis, Entity, Indicator, SubCriteria}
import ch.fhnw.ima.saav.model.layout.{ProfileChartLayout, QualityChartLayout}
import diode._
import diode.react.ReactConnector

object SaavController {

  // Manages Analysis Import

  final case class AnalysisImportInProgressAction(progress: Float) extends Action

  final case class AnalysisImportFailedAction(throwable: Throwable, logToConsole: Boolean = true) extends Action

  final case class AnalysisReadyAction[E <: Entity](analysis: Analysis) extends Action

  class AnalysisImportHandler[M](modelRW: ModelRW[M, Either[NoDataAppModel, AppModel]]) extends ActionHandler(modelRW) {

    override def handle: PartialFunction[Any, ActionResult[M]] = {
      case AnalysisImportInProgressAction(progress) => updated(Left(NoDataAppModel(ImportInProgress(progress))))
      case a@AnalysisImportFailedAction(t, logToConsole) =>
        if (logToConsole) {
          logException(this.getClass.getSimpleName, String.valueOf(a), t)
        }
        updated(Left(NoDataAppModel(ImportFailed(t))))
      case AnalysisReadyAction(analysis) =>
        val indicators = analysis.criteria.flatMap(_.subCriteria.flatMap(_.indicators))
        val weights = Weights(enabledIndicators = indicators.toSet)
        val model = AppModel(analysis, weights)
        updated(Right(model))

    }

  }

  // Manages Entity Colors

  final case class AutoColorizeAction(entities: Seq[Entity]) extends Action

  final case class UpdateEntityColorAction(entity: Entity, webColor: WebColor) extends Action

  class ColorHandler[M](modelRW: ModelRW[M, Map[Entity, WebColor]]) extends ActionHandler(modelRW) {

    override def handle: PartialFunction[Any, ActionResult[M]] = {
      case AutoColorizeAction(entities) => updated(autoColorMap(entities))
      case UpdateEntityColorAction(entity, color) => updated(value + (entity -> color))
    }

  }

  // Manages EntitySelectionModel

  final case class UpdateEntitySelectionAction(entities: Set[Entity], isSelected: Boolean) extends Action

  final case class UpdateEntityPinningAction(pinnedEntity: Option[Entity]) extends Action

  class EntitySelectionHandler[M](modelRW: ModelRW[M, EntitySelectionModel]) extends ActionHandler(modelRW) {

    override def handle: PartialFunction[Any, ActionResult[M]] = {
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

    override def handle: PartialFunction[Any, ActionResult[M]] = {
      case UpdateSubCriteriaHoveringAction(hoveredSubCriteria) =>
        updated(value.copy(hovered = hoveredSubCriteria))
    }

  }

  // Manages Weights

  final case class UpdateIndicatorWeightAction(indicator: Indicator, isEnabled: Boolean) extends Action

  final case class UpdateSubCriteriaWeightAction(subCriteria: SubCriteria, weight: Weight) extends Action

  class WeightsHandler[M](modelRW: ModelRW[M, Weights]) extends ActionHandler(modelRW) {

    override def handle: PartialFunction[Any, ActionResult[M]] = {
      case UpdateIndicatorWeightAction(indicator, isEnabled) =>
        val newEnabledIndicators = if (isEnabled) {
          value.enabledIndicators + indicator
        } else {
          value.enabledIndicators - indicator
        }
        updated(value.copy(enabledIndicators = newEnabledIndicators))
      case UpdateSubCriteriaWeightAction(subCriteria, weight) =>
        val newSubCriteriaWeights = value.subCriteriaWeights.updated(subCriteria, weight)
        updated(value.copy(subCriteriaWeights = newSubCriteriaWeights))
    }

  }

  final case class UpdateChartWidth(width: Int) extends Action

  class ChartLayoutHandler[M](modelRW: ModelRW[M, (QualityChartLayout, ProfileChartLayout)]) extends ActionHandler(modelRW) {

    override def handle: PartialFunction[Any, ActionResult[M]] = {
      case UpdateChartWidth(width) =>

        val ql = value._1
        val newQualityLayout = new QualityChartLayout(width, ql.criteria, Some(ql.minValue), Some(ql.maxValue))

        val pl = value._1
        val newProfileChartLayout = new ProfileChartLayout(width, pl.criteria, Some(pl.minValue), Some(pl.maxValue))

        updated((newQualityLayout, newProfileChartLayout))
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

    private val weightsHandler = {
      def modelGet = (m: SaavModel) =>
        m.model.right.toOption.map(_.weights).get

      def modelSet = (m: SaavModel, v: Weights) => {
        val newAppModel = m.model.right.map(_.updateWeights(v))
        m.copy(model = m.model.right.flatMap(_ => newAppModel))
      }

      new WeightsHandler(zoomRW(modelGet)(modelSet))
    }

    private val chartLayoutHandler = {
      def modelGet = (m: SaavModel) =>
        m.model.right.toOption.map(am => (am.qualityModel.layout, am.profileModel.layout)).get

      def modelSet = (m: SaavModel, v: (QualityChartLayout, ProfileChartLayout)) =>
        m.copy(model = m.model.right.map { am =>
          am.copy(
            qualityModel = am.qualityModel.copy(layout = v._1),
            profileModel = am.profileModel.copy(layout = v._2)
          )
        })

      new ChartLayoutHandler(zoomRW(modelGet)(modelSet))
    }

    override protected val actionHandler: HandlerFunction =
      composeHandlers(
        analysisImportHandler,
        colorHandler,
        entitySelectionHandler,
        subCriteriaSelectionHandler,
        weightsHandler,
        chartLayoutHandler
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
