package ch.fhnw.ima.saav.component

import ch.fhnw.ima.saav.component.bootstrap.Button
import ch.fhnw.ima.saav.controller.{AnalysisReadyAction, DataImportFailedAction, StartImportAction}
import ch.fhnw.ima.saav.model._
import ch.fhnw.ima.saav.model.app._
import diode.react.ModelProxy
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{Callback, ReactComponentB, ReactComponentU, TopNode}
import org.scalajs.dom.DragEvent

import scalacss.ScalaCssReact._

/**
  * A component which accepts a CSV file via drag and drop and imports its contents.
  */
object FileImportComponent {

  case class Props(configFileUrl: String, proxy: ModelProxy[NoDataAppModel])

  private def handleDragOver(e: DragEvent) = Callback {
    e.stopPropagation()
    e.preventDefault()
    e.dataTransfer.dropEffect = "copy"
  }

  private def handleFileDropped(proxy: ModelProxy[NoDataAppModel], configFileUrl: String)(e: DragEvent): Callback = {
    e.stopPropagation()
    e.preventDefault()
    try {
      val files = e.dataTransfer.files
      if (files.length > 0) {
        proxy.dispatchCB(StartImportAction(configFileUrl, files.item(0)))
      } else {
        Callback.log("No files to import")
      }
    } catch {
      case t: Throwable => proxy.dispatchCB(DataImportFailedAction(t))
    }
  }

  private def importMockAnalysis(proxy: ModelProxy[NoDataAppModel]) =
    proxy.dispatchCB(AnalysisReadyAction(mockAnalysis))

  private def importAlphabetSoupAnalysis(proxy: ModelProxy[NoDataAppModel]) =
    proxy.dispatchCB(AnalysisReadyAction(alphabetSoupAnalysis))

  private val component = ReactComponentB[Props](FileImportComponent.getClass.getSimpleName)
    .render_P(p => {
      p.proxy.value.importState match {
        case ImportNotStarted() =>
          <.div(
            <.div(css.fileDropZone,
              ^.onDragOver ==> handleDragOver,
              ^.onDrop ==> handleFileDropped(p.proxy, p.configFileUrl),
              <.div(
                <.h1("Drag & Drop"),
                <.p("To import data from CSV file")
              )),
            <.p(^.textAlign.center, css.vSpaced, Button(onClick = importMockAnalysis(p.proxy), "Quick, some mock data, please!")),
            <.p(^.textAlign.center, css.vSpaced, Button(onClick = importAlphabetSoupAnalysis(p.proxy), "Quick, some alphabet soup, please!"))
          )
        case ImportInProgress(progress) =>
          <.div(css.fileDropZone,
            <.h1("Import In Progress"),
            <.p((progress * 100).toInt + "%")
          )
        case ImportFailed(_) =>
          <.div(css.fileDropZone, <.h1("Import Failed"), <.p("See console log for details"))
        case _ => <.div()
      }
    })
    .build

  def apply(configFileUrl: String, proxy: ModelProxy[NoDataAppModel]): ReactComponentU[Props, Unit, Unit, TopNode] = component(Props(configFileUrl, proxy))

}
