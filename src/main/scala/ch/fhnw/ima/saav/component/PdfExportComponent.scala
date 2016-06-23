package ch.fhnw.ima.saav.component

import ch.fhnw.ima.saav.model.model.Analysis
import ch.fhnw.ima.saav.model.model.Entity.Project
import ch.fhnw.ima.saav.style.GlobalStyles
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{Callback, ReactComponentB}
import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.html.Canvas
import org.scalajs.dom.raw.XMLSerializer

import scala.scalajs.js
import scala.scalajs.js.annotation.JSName
import scalacss.ScalaCssReact._

object PdfExportComponent {

  @JSName("URL")
  @js.native
  object URL extends dom.URL

  // Exporting an SVG tree to a PNG is pretty cumbersome:
  // 1. build an SVG/XML string representation (including CSS)
  // 2. render SVG string onto an HTML canvas (via canvg lib)
  // 3. export the HTML canvas to PNG
  // 4. add PNG to PDF
  private def generatePdf = Callback {

    val css = GlobalStyles

    val svgChart = document.querySelector("." + css.svgContentResponsive.htmlClass)
    val width = svgChart.getBoundingClientRect().width
    val height = svgChart.getBoundingClientRect().height

    // 1. build an SVG/XML string representation (including CSS)
    val svg = svgChart.cloneNode(true)
    svg.insertBefore(createDefsWithInlinedCss(), svg.firstChild)
    val svgString = new XMLSerializer().serializeToString(svg)
    val canvas = document.createElement("canvas").asInstanceOf[Canvas]

    // take high-resolution displays into account (otherwise looks blurry on retina)
    val pixelRatio = window.devicePixelRatio.toInt
    canvas.width = (width * pixelRatio).toInt
    canvas.height = (height * pixelRatio).toInt

    // 2. render SVG string onto an HTML canvas
    canvg(canvas, svgString)

    // 3. export the HTML canvas to PNG
    val dataURL = canvas.toDataURL("image/png", 1.0)

    val doc = new jsPDF("p", "px", "a4")
    val x = 50
    doc.text(x, 50, Pages.Page.ProjectAnalysisPage.displayName)

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

  case class Props(analysis: Analysis[Project])

  private val css = GlobalStyles

  private val component = ReactComponentB[Props](PdfExportComponent.getClass.getSimpleName)
    .render { _ =>
        <.button(css.defaultButton, ^.onClick --> generatePdf, "Generate PDF")
    }
    .build

  def apply(analysis: Analysis[Project]) = component(Props(analysis))

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
