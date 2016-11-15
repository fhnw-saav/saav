package ch.fhnw.ima.saav
package component

import ch.fhnw.ima.saav.component.bootstrap.{Button, Modal}
import ch.fhnw.ima.saav.component.pages.PageWithDataComponent.{ProfileTab, QualityTab, Tab}
import ch.fhnw.ima.saav.jspdf.jsPDF
import ch.fhnw.ima.saav.model.app.AppModel
import japgolly.scalajs.react.vdom.ReactTagOf
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{Callback, ReactComponentB, _}
import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.ext.Color
import org.scalajs.dom.html.{Canvas, Div}
import org.scalajs.dom.raw.{HTMLImageElement, SVGSVGElement, XMLSerializer}

import scala.concurrent.{Future, Promise}
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import scala.util.Success
import scalacss.ScalaCssReact._

object PdfExportComponent {

  // all PDF metrics in mm unless indicated

  private object Page {
    val Width = 297
    val MarginX = 10
    val MarginY = 20
    val ContentWidth: Int = Width - 2 * MarginX
  }

  private object Chart {
    val ChartImageY = 30
  }

  private object LegendTable {
    val FontSize = 10 // pt
    val LineGap = 10

    case class ColumnPositions(rankColumnX: Option[Int], nameColumnX: Int, colorColumnX: Int)

    val WithRank = ColumnPositions(rankColumnX = Some(15), nameColumnX = 20, colorColumnX = 100)
    val WithoutRank = ColumnPositions(rankColumnX = None, nameColumnX = Page.MarginX, colorColumnX = 80)

    val ColorCellDimension = 6
    val ColorCellOffsetY = 1
  }

  case class Props(chartSvgRootElementId: String, defaultTitle: String, activeTab: Tab, model: AppModel)

  case class State(showReportForm: Boolean = false, title: String)

  class Backend($: BackendScope[Props, State]) {

    private def showReportForm = $.modState(_.copy(showReportForm = true))

    private def hideReportForm = $.modState(_.copy(showReportForm = false))

    // Exporting an SVG tree to a PNG is pretty cumbersome:
    // 1. build an SVG/XML string representation (including CSS)
    // 2. render SVG string onto an HTML canvas
    // 3. export the HTML canvas to PNG
    // 4. add PNG to PDF
    private def generatePdf(title: String, activeTab: Tab, model: AppModel) = Callback {

      // A4 landscape
      val landscape = "l"
      val pdf = new jsPDF(landscape, "mm", "a4")

      val chartImageFuture = exportChart()

      chartImageFuture.foreach { (imageInfo: ImageInfo) =>

        // Title
        pdf.text(title, Page.MarginX, Page.MarginY)

        val mmImageWidth = Page.Width - (2 * Page.MarginX)
        val mmImageHeight = imageInfo.aspectRatio * mmImageWidth
        pdf.addImage(imageInfo.dataURL, "png", Page.MarginX, Chart.ChartImageY, mmImageWidth, mmImageHeight)

        pdf.addPage()

        appendLegend(pdf, activeTab, model)

        pdf.save("Report.pdf")
      }
    }

    case class ImageInfo(dataURL: String, aspectRatio: Int)

    private def exportChart(): Future[ImageInfo] = {
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

      val resultPromise = Promise[ImageInfo]()

      image.onload = (_: dom.Event) => {
        ctx.drawImage(image, 0, 0)
        // 3. export the HTML canvas to PNG
        val dataURL = canvas.toDataURL("image/png", 1.0)
        val aspectRatio = canvas.height / canvas.width
        resultPromise.complete(Success(ImageInfo(dataURL, aspectRatio)))
      }

      resultPromise.future

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

    private def appendLegend(pdf: jsPDF, activeTab: Tab, model: AppModel) = {
      import LegendTable._

      val (entities, columnPositions) = activeTab match {
        case QualityTab =>
          (model.qualityModel.rankedEntities, WithRank)

        case ProfileTab =>
          (model.profileModel.sortedEntities, WithoutRank)
      }

      pdf.setFontSize(FontSize)

      var y = Page.MarginY
      for (e <- entities) {

        columnPositions match {
          case ColumnPositions(Some(rankColumnX), _, _) =>
            pdf.text(e.position + 1 + ".", rankColumnX, y, js.undefined, js.undefined, "right")
          case _ =>
        }

        // name
        pdf.text(e.displayName, columnPositions.nameColumnX, y)

        // color
        val webColor = model.colorMap(e.id)
        val color = Color(webColor.hexValue)
        pdf.setFillColor(color.r, color.g, color.b)
        val colorColumnY = y - ColorCellDimension + ColorCellOffsetY // no support for vertical centering
        pdf.rect(columnPositions.colorColumnX, colorColumnY, ColorCellDimension, ColorCellDimension, "F")

        y += LineGap
      }

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
            footer = hide => Button(generatePdf(s.title, p.activeTab, p.model) >> hide >> hideReportForm, "OK")),
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

  def apply(chartSvgRootElementId: String, defaultTitle: String, activeTab: Tab, model: AppModel): ReactComponentU[Props, State, Backend, TopNode] = component(Props(chartSvgRootElementId, defaultTitle, activeTab, model))

}
