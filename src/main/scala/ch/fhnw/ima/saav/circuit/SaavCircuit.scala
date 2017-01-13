package ch.fhnw.ima.saav.circuit

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

  private val expertConfigHandler =
    new ExpertConfigHandler(zoomRW(ExpertConfigHandler.modelGet)(ExpertConfigHandler.modelSet))

  private val chartLayoutHandler =
    new ChartLayoutHandler(zoomRW(ChartLayoutHandler.modelGet)(ChartLayoutHandler.modelSet))

  private val pdfExportHandler =
    new PdfExportHandler(zoomRW(PdfExportHandler.modelGet)(PdfExportHandler.modelSet))

  override protected val actionHandler: HandlerFunction =
    composeHandlers(
      analysisImportHandler,
      colorHandler,
      entitySelectionHandler,
      subCriteriaSelectionHandler,
      expertConfigHandler,
      chartLayoutHandler,
      pdfExportHandler
    )

  override def handleError(msg: String): Unit = {
    val name = getClass.getSimpleName
    println(s"[$name] Error: $msg")
  }

}