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

  val PixelsPerMillimeter = 12 // ~300 dpi

  private object Page {
    val Width = 297
    val Height = 210
    val Margin = 10
    val ContentWidth: Int = Width - 2 * Margin
    val ContentHeight: Int = Height - 2 * Margin
  }

  private object Chart {
    val TitleY = 20
    val ChartImageY = 30
  }

  private object LegendTable {
    val Y = 28 // brute force alignment with visual ranking
    val Font = "arial"
    val FontSize = 10 // pt
    val LineGap = 10

    case class ColumnPositions(rankColumnX: Option[Int], nameColumnX: Int, colorColumnX: Int)

    val WithRank = ColumnPositions(rankColumnX = Some(15), nameColumnX = 20, colorColumnX = 100)
    val WithoutRank = ColumnPositions(rankColumnX = None, nameColumnX = Page.Margin, colorColumnX = 80)

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

      val chartImageFuture = exportSVG(ChartComponent.ElementId, MaxToPageWidth)
      val visualRankingFuture: Future[Option[ImageInfo]] = if (activeTab == QualityTab) {
        val future = exportSVG(VisualRankingComponent.ElementId, MaxToPageHeight)
        future.map(f => Some(f))
      } else {
        Future.successful(None)
      }

      for {
        chartImageInfo <- chartImageFuture
        optionalVisualRankingImageInfo <- visualRankingFuture
      } {

        // Page 1: Chart
        pdf.text(title, Page.Margin, Chart.TitleY)

        {
          val mmChartImageWidth = Page.ContentWidth
          val mmChartImageHeight = Page.ContentWidth / chartImageInfo.aspectRatio
          pdf.addImage(chartImageInfo.dataURL, "png", Page.Margin, Chart.ChartImageY, mmChartImageWidth, mmChartImageHeight)
        }

        // Page 2: Legend & Visual Ranking
        pdf.addPage()

        appendLegend(pdf, activeTab, model)

        optionalVisualRankingImageInfo.foreach { visualRankingImageInfo =>
          val mmWidth = (Page.ContentHeight * visualRankingImageInfo.aspectRatio).toInt
          val mmHeight = Page.ContentHeight
          val x = Page.Width - Page.Margin - mmWidth
          val y = Page.Margin
          pdf.addImage(visualRankingImageInfo.dataURL, "png", x, y, mmWidth, mmHeight)
        }

        // Done!
        pdf.save("Report.pdf")
      }
    }

    case class ImageInfo(dataURL: String, aspectRatio: Double)

    trait MaxStrategy
    case object MaxToPageWidth extends MaxStrategy
    case object MaxToPageHeight extends MaxStrategy

    private def exportSVG(elementId: String, maximization: MaxStrategy): Future[ImageInfo] = {
      val svgElement = document.getElementById(elementId).asInstanceOf[SVGSVGElement]

      val width = svgElement.width.baseVal.value
      val height = svgElement.height.baseVal.value
      val aspectRatio = width / height

      // 1. build an SVG/XML string representation (including CSS)
      val svg = svgElement.cloneNode(true).asInstanceOf[SVGSVGElement]
      svg.setAttribute("width", "100%")
      svg.insertBefore(createDefsWithInlinedCss(), svg.firstChild)
      val svgString = new XMLSerializer().serializeToString(svg)

      val pageContentWidthPx = Page.ContentWidth * PixelsPerMillimeter
      val pageContentHeightPx = Page.ContentHeight * PixelsPerMillimeter
      val (canvasWidth, canvasHeight) = maximization match {
        case MaxToPageWidth =>
          val w = pageContentWidthPx.toInt
          val h = (pageContentWidthPx / aspectRatio).toInt
          (w, h)

        case MaxToPageHeight =>
          val w = (pageContentHeightPx * aspectRatio).toInt
          val h = pageContentHeightPx.toInt
          (w, h)
      }
      val canvas = document.createElement("canvas").asInstanceOf[Canvas]
      canvas.width = canvasWidth
      canvas.height = canvasHeight

      // 2. render SVG string onto an HTML canvas
      val image = document.createElement("img").asInstanceOf[HTMLImageElement]
      image.src = "data:image/svg+xml," + svgString

      val ctx = canvas.getContext("2d")

      val resultPromise = Promise[ImageInfo]()

      image.onload = (_: dom.Event) => {
        ctx.drawImage(image, 0, 0)
        // 3. export the HTML canvas to PNG
        val dataURL = canvas.toDataURL("image/png", 1.0)
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

      var y = LegendTable.Y
      for {
        e <- entities
        isPinned = model.entitySelectionModel.pinned.contains(e.id)
        isVisible = model.entitySelectionModel.visible.contains(e.id)
        if isVisible
      } {
        if (isPinned) pdf.setFont(Font, "bold")
        else pdf.setFont(Font, "normal")

        columnPositions match {
          case ColumnPositions(Some(rankColumnX), _, _) =>
            pdf.text(e.position + 1 + ".", rankColumnX, y, js.undefined, js.undefined, "right")
          case _ =>
        }

        // name
        pdf.text(e.displayName, columnPositions.nameColumnX, y)

        // color
        if (isPinned) {
          pdf.setFillColor("black")
        } else {
          val webColor = model.colorMap(e.id)
          val color = Color(webColor.hexValue)
          pdf.setFillColor(color.r, color.g, color.b)
        }

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
