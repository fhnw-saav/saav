package ch.fhnw.ima.saav
package component

import ch.fhnw.ima.saav.component.bootstrap.{Button, Modal}
import japgolly.scalajs.react.vdom.ReactTagOf
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{Callback, ReactComponentB, _}
import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.html.{Canvas, Div}
import org.scalajs.dom.raw.{HTMLImageElement, SVGSVGElement, XMLSerializer}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSName
import scalacss.ScalaCssReact._

object PdfExportComponent {

  case class Props(chartSvgRootElementId: String, defaultTitle: String)

  case class State(showReportForm: Boolean = false, title: String)

  class Backend($: BackendScope[Props, State]) {

    private def showReportForm = $.modState(_.copy(showReportForm = true))

    private def hideReportForm = $.modState(_.copy(showReportForm = false))

    // Exporting an SVG tree to a PNG is pretty cumbersome:
    // 1. build an SVG/XML string representation (including CSS)
    // 2. render SVG string onto an HTML canvas
    // 3. export the HTML canvas to PNG
    // 4. add PNG to PDF
    private def generatePdf(title: String) = Callback {
      val svgChart = document.getElementById(ChartComponent.ElementId).asInstanceOf[SVGSVGElement]
      val width = svgChart.width.baseVal.value
      val height = svgChart.height.baseVal.value

      // 1. build an SVG/XML string representation (including CSS)
      val svg = svgChart.cloneNode(true)
      svg.insertBefore(createDefsWithInlinedCss(), svg.firstChild)
      val svgString = new XMLSerializer().serializeToString(svg)

      // create a canvas, taking high-resolution displays into account (otherwise looks blurry on retina)
      val canvas = document.createElement("canvas").asInstanceOf[Canvas]
      val pixelRatio = window.devicePixelRatio.toInt
      canvas.width = (width * pixelRatio).toInt
      canvas.height = (height * pixelRatio).toInt

      // 2. render SVG string onto an HTML canvas
      val image = document.createElement("img").asInstanceOf[HTMLImageElement]
      image.src = "data:image/svg+xml," + svgString
      val ctx = canvas.getContext("2d")
      image.onload = (_: dom.Event) => {
        ctx.drawImage(image, 0, 0)

        // 3. export the HTML canvas to PNG
        val dataURL = canvas.toDataURL("image/png", 1.0)

        // 4. add PNG to PDF
        val landscape = "l"
        val doc = new jsPDF(landscape, "mm", "a4")

        // title
        val mmPageMarginX = 10
        val mmPageMarginY = 20
        doc.text(mmPageMarginX, mmPageMarginY, title)

        // chart image
        val mmPageWidth = 297 // A4 landscape
        val mmImageY = 30
        val mmImageWidth = mmPageWidth - (2 * mmPageMarginX)
        val mmImageHeight = (canvas.height / canvas.width) * mmImageWidth
        doc.addImage(dataURL, "png", mmPageMarginX, mmImageY, mmImageWidth, mmImageHeight)

        doc.save("Report.pdf")
      }
    }

    private def createDefsWithInlinedCss(): Element = {
      // include our style
      val cssStyle = document.querySelector("style").cloneNode(true).asInstanceOf[Element]
      // include font (should actually inline complete bootstrap CSS, but this is just a PoC)
      cssStyle.innerHTML = "* { font-family: \"Helvetica Neue\",Helvetica,Arial,sans-serif; } " + cssStyle.innerHTML
      // container for inlined CSS (according to SVG spec)
      val defsElement = document.createElement("defs")
      defsElement.appendChild(cssStyle)
      defsElement
    }

    private def onTitleChange(e: ReactEventI) = {
      val newValue = e.target.value
      $.modState(_.copy(title = newValue))
    }

    def render(p: Props, s: State): ReactTagOf[Div] = {
      val exportPdfLabel = "Export PDF"
      val button = Button(showReportForm, exportPdfLabel + "...")
      if (s.showReportForm) {
        val titleInput = <.input(css.form.control, ^.`type` := "text", ^.id := "title", ^.onChange ==> onTitleChange, ^.value := p.defaultTitle)
        val labelledTitleInput = <.div(
          <.label(css.form.group, ^.`for` := "title", "Title:"),
          titleInput
        )
        val modal = Modal(
          Modal.Props(
            header = _ => <.h2(exportPdfLabel),
            footer = hide => Button(generatePdf(s.title) >> hide >> hideReportForm, "OK")),
          labelledTitleInput
        )
        <.div(button, modal)
      } else {
        <.div(button)
      }
    }

  }

  private val component = ReactComponentB[Props](PdfExportComponent.getClass.getSimpleName)
    .initialState_P { p =>
      State(title = p.defaultTitle)
    }
    .renderBackend[Backend]
    .build

  def apply(chartSvgRootElementId: String, defaultTitle: String): ReactComponentU[Props, State, Backend, TopNode] = component(Props(chartSvgRootElementId, defaultTitle))

  // Scala facades to 3rd party JS libs

  @js.native
  @JSName("jsPDF")
  class jsPDF(orientation: String, unit: String, pageFormat: String) extends js.Object {

    def addImage(url: String, imageType: String, x: Int, y: Int, width: Int, height: Int): js.Any = js.native

    def text(x: Int, y: Int, text: String): js.Any = js.native

    def save(fileName: String): js.Any = js.native

  }

}
