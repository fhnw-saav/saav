package ch.fhnw.ima.saav.circuit

import ch.fhnw.ima.saav.model.app.{PdfExportDialogHidden, PdfExport, SaavModel}
import diode.{Action, ActionHandler, ActionResult, ModelRW}

final case class UpdatePdfExport(state: PdfExport) extends Action

class PdfExportHandler[M](modelRW: ModelRW[M, PdfExport]) extends ActionHandler(modelRW) {

  override def handle: PartialFunction[Any, ActionResult[M]] = {
    case UpdatePdfExport(state) => updated(state)
  }

}

object PdfExportHandler {

  def modelGet: (SaavModel) => PdfExport =
    _.model.right.toOption.map(_.pdfExport).getOrElse(PdfExportDialogHidden)

  def modelSet: (SaavModel, PdfExport) => SaavModel = (m, v) =>
    m.copy(model = m.model.right.map(_.copy(pdfExport = v)))

}