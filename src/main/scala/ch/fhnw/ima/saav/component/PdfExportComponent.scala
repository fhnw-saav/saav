package ch.fhnw.ima.saav.component

import ch.fhnw.ima.saav.model.model.Analysis
import ch.fhnw.ima.saav.model.model.Entity.Project
import ch.fhnw.ima.saav.style.GlobalStyles
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{Callback, ReactComponentB}
import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.html.{Canvas, Image}
import org.scalajs.dom.raw.{Blob, XMLSerializer}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSName
import scalacss.ScalaCssReact._

object PdfExportComponent {

  private val idPrefix = PdfExportComponent.getClass.getSimpleName
  private val hiddenImageId = idPrefix + "-hidden-image"
  private val hiddenCanvasId = idPrefix + "-hidden-canvas"

  @JSName("URL")
  @js.native
  object URL extends dom.URL

  // Exporting an SVG tree to a PNG is pretty cumbersome:
  // 1. build an SVG/XML string representation
  // 2. use an HTML img node to load the SVG/XML string representation
  // 3. render the HTML img node onto an HTML canvas
  // 4. export the HTML canvas to PNG
  // 5. add PNG to PDF
  private def generatePdf = Callback {

    val css = GlobalStyles

    val svgChart = document.querySelector("." + css.svgContentResponsive.htmlClass)
    val width = svgChart.clientWidth
    val height = svgChart.clientHeight

    // 1. build an SVG/XML string representation
    val svg = svgChart.cloneNode(true)
    svg.insertBefore(createDefsWithInlinedCss(), svg.firstChild)

    val svgString = new XMLSerializer().serializeToString(svg)
    val blob = new Blob(js.Array(svgString), BlobPropertyBag("image/svg+xml;charset=utf-8"))
    val svgStringUrl = URL.createObjectURL(blob)
    val img = document.getElementById(hiddenImageId).asInstanceOf[Image]
    val canvas = document.getElementById(hiddenCanvasId).asInstanceOf[Canvas]

    // take high-resolution displays into account (otherwise looks blurry on retina)
    val pixelRatio = window.devicePixelRatio.toInt
    canvas.width = width * pixelRatio
    canvas.height = height * pixelRatio
    canvas.style.width = canvas.width + "px"
    canvas.style.height = canvas.height + "px"

    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

    img.onload = { evt: Event =>

      // 3. render the HTML img node onto an HTML canvas
      ctx.drawImage(img, 0, 0)
      URL.revokeObjectURL(svgStringUrl)

      // 4. export the HTML canvas to PNG
      val dataURL = canvas.toDataURL("image/png", 1.0)

      // uncomment to download PNG
      // window.location.href = dataURL.replace("image/png", "image/octet-stream")

      val doc = new jsPDF("p", "px", "a4")
      val x = 50
      doc.text(x, 50, Pages.Page.ProjectAnalysisPage.displayName)

      // 5. add PNG to PDF
      doc.addImage(dataURL, "png", x, 80, img.naturalWidth, img.naturalHeight)

      doc.save("Report.pdf")
    }

    // 2. use an HTML img node to load the SVG/XML string representation
    img.src = svgStringUrl
  }

  private def createDefsWithInlinedCss(): Element = {
    // include our style
    val cssStyle = document.querySelector("style").cloneNode(true).asInstanceOf[Element]
    // include font (should actually inline complete bootstrap CSS, but this is just a PoC)
    cssStyle.innerHTML = "* { font-family: \"Helvetica Neue\",Helvetica,Arial,sans-serif; } " + cssStyle.innerHTML
    // container for inlined CSS
    val defsElement = document.createElement("defs")
    defsElement.appendChild(cssStyle)
    defsElement
  }

  case class Props(analysis: Analysis[Project])

  private val css = GlobalStyles

  private val component = ReactComponentB[Props](PdfExportComponent.getClass.getSimpleName)
    .render { _ =>
      <.div(
        <.button(css.defaultButton, ^.onClick --> generatePdf, "Generate PDF"),
        <.canvas(css.hidden, ^.id := hiddenCanvasId),
        <.img(css.hidden, ^.id := hiddenImageId)
      )
    }
    .build

  def apply(analysis: Analysis[Project]) = component(Props(analysis))

  // Facade to jsPDF library --> needs more type-safety once we settle on a usage...
  @js.native
  @JSName("jsPDF")
  class jsPDF(orientation: String, unit: String, pageFormat: String) extends js.Object {

    def addImage(url: String, imageType: String, x: Int, y: Int, width: Int, height: Int): js.Any = js.native

    def text(x: Int, y: Int, text: String): js.Any = js.native

    def save(fileName: String): js.Any = js.native

  }

}
