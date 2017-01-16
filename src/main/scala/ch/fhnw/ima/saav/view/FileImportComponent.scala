package ch.fhnw.ima.saav
package view

import ch.fhnw.ima.saav.circuit.{ImportFailedAction, StartImportAction}
import ch.fhnw.ima.saav.model.app._
import ch.fhnw.ima.saav.view.bootstrap.Button
import diode.Action
import diode.react.ModelProxy
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{Callback, ReactComponentB, ReactComponentU, TopNode}
import org.scalajs.dom.raw.Blob
import org.scalajs.dom.{DragEvent, Event, XMLHttpRequest}

import scala.concurrent.Promise
import scala.concurrent.ExecutionContext.Implicits.global
import scalacss.ScalaCssReact._

/**
  * A component which accepts a CSV file via drag and drop and imports its contents.
  */
object FileImportComponent {

  case class Props(configFileUrl: Option[String], proxy: ModelProxy[NoDataAppModel])

  private def handleDragOver(e: DragEvent) = Callback {
    e.stopPropagation()
    e.preventDefault()
    e.dataTransfer.dropEffect = "copy"
  }

  private def handleFileDropped(proxy: ModelProxy[NoDataAppModel], configFileUrl: Option[String])(e: DragEvent): Callback = {
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

  private def importDemoData(proxy: ModelProxy[NoDataAppModel]) = {
    val futureAction = createImportDataAction(Some("demo/config.json"), "demo/data.csv")
    val f = futureAction.map(action => proxy.dispatchCB(action))
    Callback.future(f)
  }

  private def createImportDataAction(configFileUrl: Option[String], dataFileUrl: String) = {
    val xhr = new XMLHttpRequest()
    xhr.open("GET", dataFileUrl, async = true)
    xhr.responseType = "blob"
    val promise = Promise[Action]()
    xhr.onload = (_: Event) => {
      if (xhr.status == 200) {
        val dataBlob = xhr.response.asInstanceOf[Blob]
        promise.success(StartImportAction(configFileUrl, dataBlob))
      } else {
        val t = new IllegalArgumentException(s"Could not load data from '$dataFileUrl'")
        promise.success(ImportFailedAction(t))
      }
    }
    xhr.send()
    promise.future
  }

  private val component = ReactComponentB[Props](FileImportComponent.getClass.getSimpleName)
    .render_P(p => {
      p.proxy.value.importState match {
        case ImportNotStarted() =>

          // trigger auto-import if a URL param is specified
          getDataFileUrl.foreach { customDataUrl =>
            val futureAction = createImportDataAction(p.configFileUrl, customDataUrl)
            futureAction.map(p.proxy.dispatchCB(_).runNow())
          }

          <.div(
            <.div(css.fileDropZone,
              ^.onDragOver ==> handleDragOver,
              ^.onDrop ==> handleFileDropped(p.proxy, p.configFileUrl),
              <.div(
                <.h1("Drag & Drop"),
                <.p("To import data from CSV file")
              )),
            <.p(^.textAlign.center, css.vSpaced,
              Button(onClick = importDemoData(p.proxy), "Have a look around by loading demo data!")
            )
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

  def apply(configFileUrl: Option[String], proxy: ModelProxy[NoDataAppModel]): ReactComponentU[Props, Unit, Unit, TopNode] = component(Props(configFileUrl, proxy))

}
