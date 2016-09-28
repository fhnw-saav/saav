package ch.fhnw.ima.saav.controller

import ch.fhnw.ima.saav.model.app._
import diode._
import diode.react.ReactConnector

class SaavCircuit extends Circuit[SaavModel] with ReactConnector[SaavModel] {

  override protected def initialModel = SaavModel()

  private val analysisImportHandler =
    new AnalysisImportHandler(zoomRW(AnalysisImportHandler.modelGet)(AnalysisImportHandler.modelSet))

  private val colorHandler =
    new ColorHandler(zoomRW(ColorHandler.modelGet)(ColorHandler.modelSet))

  private val entitySelectionHandler =
    new EntitySelectionHandler(zoomRW(EntitySelectionHandler.modelGet)(EntitySelectionHandler.modelSet))

  private val subCriteriaSelectionHandler =
    new SubCriteriaSelectionHandler(zoomRW(SubCriteriaSelectionHandler.modelGet)(SubCriteriaSelectionHandler.modelSet))

  private val weightsHandler =
    new WeightsHandler(zoomRW(WeightsHandler.modelGet)(WeightsHandler.modelSet))

  private val chartLayoutHandler =
    new ChartLayoutHandler(zoomRW(ChartLayoutHandler.modelGet)(ChartLayoutHandler.modelSet))

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
    val name = getClass.getSimpleName
    println(s"[$name] Error: $msg")
  }

}