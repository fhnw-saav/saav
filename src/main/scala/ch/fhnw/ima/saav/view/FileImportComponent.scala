package ch.fhnw.ima.saav
package view

import ch.fhnw.ima.saav.circuit.{AnalysisReadyAction, ImportFailedAction, StartImportAction}
import ch.fhnw.ima.saav.model._
import ch.fhnw.ima.saav.model.app._
import ch.fhnw.ima.saav.model.config.AnalysisConfig
import ch.fhnw.ima.saav.view.bootstrap.Button
import diode.react.ModelProxy
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{Callback, ReactComponentB, ReactComponentU, TopNode}
import org.scalajs.dom.{DragEvent, Event, XMLHttpRequest}
import org.scalajs.dom.raw.Blob

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
      case t: Throwable => proxy.dispatchCB(ImportFailedAction(t))
    }
  }

  private def importMockAnalysis(proxy: ModelProxy[NoDataAppModel]) =
    proxy.dispatchCB(AnalysisReadyAction(analysisConfig = AnalysisConfig.empty, analysis = mockAnalysis))

  private def importAlphabetSoupAnalysis(proxy: ModelProxy[NoDataAppModel]) =
    proxy.dispatchCB(AnalysisReadyAction(analysisConfig = AnalysisConfig.empty, analysis = alphabetSoupAnalysis))

  private val component = ReactComponentB[Props](FileImportComponent.getClass.getSimpleName)
    .render_P(p => {
      p.proxy.value.importState match {
        case ImportNotStarted() =>

          // trigger auto-import if a URL param is specified
          getDataFileUrl.foreach { customDataUrl =>
            val xhr = new XMLHttpRequest()
            xhr.open("GET", customDataUrl, async = true)
            xhr.responseType = "blob"
            xhr.onload = (_: Event) => {
              if (xhr.status == 200) {
                val dataBlob = xhr.response.asInstanceOf[Blob]
                p.proxy.dispatchCB(StartImportAction(p.configFileUrl, dataBlob)).runNow()
              } else {
                val t = new IllegalArgumentException(s"Could not load custom data from '$customDataUrl'")
                p.proxy.dispatchCB(ImportFailedAction(t)).runNow()
              }
            }
            xhr.send()
          }

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
        case ImportFailed(e) =>
          <.div(css.fileDropZone, <.h1("Import Failed"), <.p(e.getMessage, <.br, "See console log for details"))
        case _ => <.div()
      }
    })
    .build

  def apply(configFileUrl: String, proxy: ModelProxy[NoDataAppModel]): ReactComponentU[Props, Unit, Unit, TopNode] = component(Props(configFileUrl, proxy))

}
