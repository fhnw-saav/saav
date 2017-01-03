package ch.fhnw.ima.saav
package component

import ch.fhnw.ima.saav.component.bootstrap.{Button, Modal}
import ch.fhnw.ima.saav.component.pages.PageWithDataComponent.{ProfileTab, QualityTab, Tab}
import ch.fhnw.ima.saav.jspdf.jsPDF
import ch.fhnw.ima.saav.model.app.AppModel
import ch.fhnw.ima.saav.model.domain.IndicatorId
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
import scala.scalajs.js.Date
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

  private object Font {
    val Name = "arial"
    val TitleSize = 16 // pt
    val DefaultSize = 10 // pt
    val LineGap = 6
    val LegendLineGap = 10 // must encompass color squares
  }

  private object LegendTable {
    val Y = 28 // brute force alignment with visual ranking

    case class ColumnPositions(rankColumnX: Option[Int], nameColumnX: Int, colorColumnX: Int)

    val WithRank = ColumnPositions(rankColumnX = Some(15), nameColumnX = 20, colorColumnX = 100)
    val WithoutRank = ColumnPositions(rankColumnX = None, nameColumnX = Page.Margin, colorColumnX = 80)

    val ColorCellDimension = 6
    val ColorCellOffsetY = 1
  }

  case class ReportConfig(title: String, creator: String)

  case class Props(chartSvgRootElementId: String, defaultReportConfig: ReportConfig, activeTab: Tab, model: AppModel)

  case class State(showReportForm: Boolean = false, reportConfig: ReportConfig)

  class Backend($: BackendScope[Props, State]) {

    private def showReportForm = $.modState(_.copy(showReportForm = true))

    private def hideReportForm = $.modState(_.copy(showReportForm = false))

    // Exporting an SVG tree to a PNG is pretty cumbersome:
    // 1. build an SVG/XML string representation (including CSS)
    // 2. render SVG string onto an HTML canvas
    // 3. export the HTML canvas to PNG
    // 4. add PNG to PDF
    private def generatePdf(reportConfig: ReportConfig, activeTab: Tab, model: AppModel) = Callback {

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
        // Page 1: Cover
        appendCover(pdf, reportConfig)

        // Page 2: Chart
        appendChart(pdf, chartImageInfo)

        // Page 3: Legend & Visual Ranking
        appendLegend(pdf, activeTab, model)
        appendVisualRanking(pdf, optionalVisualRankingImageInfo)

        // Page 4: Configuration Mismatch
        appendConfigMismatch(pdf, "Missing Indicator(s)", model.config.missingIndicators)
        appendConfigMismatch(pdf, "Unexpected Indicator(s)", model.config.unexpectedIndicators)

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

    private def appendCover(pdf: jsPDF, reportConfig: ReportConfig) = {
      val xTabbed = Page.Margin + 7
      pdf.setFont(Font.Name, "bold")
      pdf.setFontSize(Font.TitleSize)
      var y = Chart.TitleY
      pdf.text(reportConfig.title, Page.Margin, y)

      pdf.setFontSize(Font.DefaultSize)
      if (!reportConfig.creator.trim.isEmpty) {
        y += Font.LineGap + Font.LineGap
        pdf.setFont(Font.Name, "bold")
        pdf.text("Creator", Page.Margin, y)
        y += Font.LineGap
        pdf.setFont(Font.Name, "normal")
        pdf.text(reportConfig.creator, xTabbed, y)
      }

      y += Font.LineGap + Font.LineGap
      pdf.setFont(Font.Name, "bold")
      pdf.text("Creation Date", Page.Margin, y)
      y += Font.LineGap
      pdf.setFont(Font.Name, "normal")
      val date = new Date()
      val twoDigits = "%02d"
      val day = twoDigits.format(date.getDate())
      val month = twoDigits.format(date.getMonth() + 1)
      val year = date.getFullYear()
      val formattedDate = s"$day.$month.$year"
      pdf.text(formattedDate, xTabbed, y)
    }

    private def appendChart(pdf: jsPDF, chartImageInfo: Backend.this.ImageInfo) = {
      pdf.addPage()
      val mmChartImageWidth = Page.ContentWidth
      val mmChartImageHeight = Page.ContentWidth / chartImageInfo.aspectRatio
      pdf.addImage(chartImageInfo.dataURL, "png", Page.Margin, Chart.ChartImageY, mmChartImageWidth, mmChartImageHeight)
    }

    private def appendLegend(pdf: jsPDF, activeTab: Tab, model: AppModel) = {
      import LegendTable._

      val (entities, columnPositions) = activeTab match {
        case QualityTab =>
          (model.qualityModel.rankedEntities, WithRank)

        case ProfileTab =>
          (model.profileModel.sortedEntities, WithoutRank)
      }

      pdf.addPage()
      pdf.setFontSize(Font.DefaultSize)

      var y = LegendTable.Y
      for {
        e <- entities
        isPinned = model.entitySelectionModel.pinned.contains(e.id)
        isVisible = model.entitySelectionModel.visible.contains(e.id)
        if isVisible
      } {
        if (isPinned) pdf.setFont(Font.Name, "bold")
        else pdf.setFont(Font.Name, "normal")

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

        y += Font.LegendLineGap
      }

    }

    private def appendVisualRanking(pdf: jsPDF, optionalVisualRankingImageInfo: Option[Backend.this.ImageInfo]) = {
      optionalVisualRankingImageInfo.foreach { visualRankingImageInfo =>
        val mmWidth = (Page.ContentHeight * visualRankingImageInfo.aspectRatio).toInt
        val mmHeight = Page.ContentHeight
        val x = Page.Width - Page.Margin - mmWidth
        val y = Page.Margin
        pdf.addImage(visualRankingImageInfo.dataURL, "png", x, y, mmWidth, mmHeight)
      }
    }

    // TODO: Improve reporting of config mismatches
    // Once we include expert configuration options in the PDF, missing/unexpected indicators
    // can be reported directly as part of the expert configuration hierarchy (marked in color/bold)
    // Until then we just dump the first 10 for illustration purposes...
    private def appendConfigMismatch(pdf: jsPDF, title: String, indicators: Seq[IndicatorId]) = {

      val x = Page.Margin
      var y = Chart.TitleY

      if (indicators.nonEmpty) {
        pdf.addPage()
        pdf.setFontSize(Font.TitleSize)

        pdf.text(s"${indicators.size} $title", x, y)
        pdf.setFontSize(Font.DefaultSize)

        y += Font.LineGap

        indicators.take(10).foreach { indicator =>
          y += Font.LineGap

          val c = indicator.subCriteriaId.criteriaId.name
          val sc = indicator.subCriteriaId.name
          val i = indicator.name

          val hierarchy = s"$c | $sc | $i"

          pdf.text(hierarchy, x, y)
        }

        if (indicators.size > 10) {
          y += Font.LineGap
          pdf.text(s"(${indicators.size - 10} more...)", x, y)
        }
      }

    }

    private def onTitleChange(e: ReactEventI) = {
      val newValue = e.target.value
      $.modState(s => s.copy(reportConfig = s.reportConfig.copy(title = newValue)))
    }

    private def onCreatorChange(e: ReactEventI) = {
      val newValue = e.target.value
      $.modState(s => s.copy(reportConfig = s.reportConfig.copy(creator = newValue)))
    }

    def render(p: Props, s: State): ReactTagOf[Div] = {
      val exportPdfLabel = "Export PDF"
      val button = Button(showReportForm, exportPdfLabel + "...")
      if (s.showReportForm) {

        val title = <.div(css.form.group,
          <.label(^.`for` := "title", "Title:"),
          <.input(css.form.control, ^.`type` := "text", ^.id := "title", ^.onChange ==> onTitleChange, ^.value := s.reportConfig.title)
        )

        val creator = <.div(css.form.group,
          <.label(^.`for` := "creator", "Creator:"),
          <.input(css.form.control, ^.`type` := "text", ^.id := "creator", ^.onChange ==> onCreatorChange, ^.value := s.reportConfig.creator)
        )

        val modal = Modal(
          Modal.Props(
            header = _ => <.h2(exportPdfLabel),
            footer = hide => <.div(
              Button(hide >> hideReportForm, "Cancel"),
              Button(generatePdf(s.reportConfig, p.activeTab, p.model) >> hide >> hideReportForm, "OK")
            )),
          <.form(title, creator)
        )

        <.div(button, modal)
      } else {
        <.div(button)
      }
    }

  }

  private val component = ReactComponentB[Props](PdfExportComponent.getClass.getSimpleName)
    .initialState_P { p =>
      State(reportConfig = p.defaultReportConfig)
    }
    .renderBackend[Backend]
    .build

  def apply(chartSvgRootElementId: String, defaultTitle: String, activeTab: Tab, model: AppModel): ReactComponentU[Props, State, Backend, TopNode] =
    component(Props(chartSvgRootElementId, defaultReportConfig = ReportConfig(title = defaultTitle, creator = ""), activeTab, model))

}