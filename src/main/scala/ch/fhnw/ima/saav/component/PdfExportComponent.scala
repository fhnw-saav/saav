package ch.fhnw.ima.saav
package component

import ch.fhnw.ima.saav.component.bootstrap.{Button, Modal}
import ch.fhnw.ima.saav.model.domain.{Analysis, Entity}
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{Callback, ReactComponentB, _}
import org.scalajs.dom._
import org.scalajs.dom.html.Canvas
import org.scalajs.dom.raw.XMLSerializer

import scala.scalajs.js
import scala.scalajs.js.annotation.JSName
import scalacss.ScalaCssReact._

object PdfExportComponent {

  case class Props(analysis: Analysis[Entity])

  case class State(showReportForm: Boolean = false, title: String = "Title")

  class Backend($: BackendScope[Props, State]) {

    private def showReportForm = $.modState(_.copy(showReportForm = true))

    private def hideReportForm = $.modState(_.copy(showReportForm = false))

    // Exporting an SVG tree to a PNG is pretty cumbersome:
    // 1. build an SVG/XML string representation (including CSS)
    // 2. render SVG string onto an HTML canvas (via canvg lib)
    // 3. export the HTML canvas to PNG
    // 4. add PNG to PDF
    private def generatePdf(title: String) = Callback {
      val svgChart = document.querySelector("." + css.svgContentResponsive.htmlClass)
      val width = svgChart.getBoundingClientRect().width
      val height = svgChart.getBoundingClientRect().height

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
      canvg(canvas, svgString)

      // 3. export the HTML canvas to PNG
      val dataURL = canvas.toDataURL("image/png", 1.0)

      val doc = new jsPDF("p", "px", "a4")
      val x = 50
      doc.text(x, 50, title)

      // 4. add PNG to PDF
      val scaling = 0.3
      val imgWidth = (scaling * width).toInt
      val imgHeight = (scaling * height).toInt
      doc.addImage(dataURL, "png", x, 80, imgWidth, imgHeight)

      doc.save("Report.pdf")
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

    def onTitleChange(e: ReactEventI) = {
      val newValue = e.target.value
      $.modState(_.copy(title = newValue))
    }

    def render(s: State) = {
      val exportPdfLabel = "Export PDF"
      val button = Button(showReportForm, exportPdfLabel + "...")
      if (s.showReportForm) {
        val titleInput = <.input(css.form.control, ^.`type` := "text", ^.id := "title", ^.onChange ==> onTitleChange)
        val labelledTitleInput = <.div(
          <.label(css.form.group, ^.`for` := "title", "Title:"),
          titleInput
        )
        val modal = Modal(
          Modal.Props(
            header = hide => <.h2(exportPdfLabel),
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
    .initialState(State())
    .renderBackend[Backend]
    .build

  def apply(analysis: Analysis[Entity]) = component(Props(analysis))

  // Scala facades to 3rd party JS libs --> need more type-safety once we settle on a usage...

  @js.native
  @JSName("jsPDF")
  class jsPDF(orientation: String, unit: String, pageFormat: String) extends js.Object {

    def addImage(url: String, imageType: String, x: Int, y: Int, width: Int, height: Int): js.Any = js.native

    def text(x: Int, y: Int, text: String): js.Any = js.native

    def save(fileName: String): js.Any = js.native

  }

  @js.native
  @JSName("canvg")
  object canvg extends js.Object {
    def apply(canvas: Canvas, svg: String): Unit = js.native
  }

}
