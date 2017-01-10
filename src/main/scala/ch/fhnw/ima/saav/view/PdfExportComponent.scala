package ch.fhnw.ima.saav
package view

import ch.fhnw.ima.saav.circuit.UpdatePdfExport
import ch.fhnw.ima.saav.jspdf.jsPDF
import ch.fhnw.ima.saav.model.app.{AppModel, PdfExportDialogHidden, PdfExportDialogVisible, PdfExportInProgress}
import ch.fhnw.ima.saav.model.domain.IndicatorId
import ch.fhnw.ima.saav.model.weight.{Profile, Quality, Weight}
import ch.fhnw.ima.saav.view.bootstrap.{Button, Modal}
import ch.fhnw.ima.saav.view.pages.PageWithDataComponent.{ProfileTab, QualityTab, Tab}
import diode.Action
import diode.react.ModelProxy
import japgolly.scalajs.react.vdom.ReactTagOf
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{Callback, _}
import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.ext.Color
import org.scalajs.dom.html.{Canvas, Div}
import org.scalajs.dom.raw.{HTMLImageElement, SVGSVGElement, XMLSerializer}

import scala.concurrent.{Future, Promise}
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import scala.scalajs.js.Date
import scala.scalajs.js.JSConverters._
import scala.util.Success
import scalacss.ScalaCssReact._

object PdfExportComponent {

  // all PDF metrics in mm unless indicated

  val PixelsPerMillimeter = 12 // ~300 dpi

  private object Page {
    val Width = 297
    val Height = 210
    val Margin = 10
    val TitleY = 20
    val ContentWidth: Int = Width - 2 * Margin
    val ContentHeight: Int = Height - 2 * Margin
  }

  private object Chart {
    val ChartImageY = 30
  }

  private object Font {
    val Name = "arial"
    val TitleSize = 16 // pt
    val DefaultSize = 10 // pt
    val LineGap = 6
    val LegendLineGap = 10 // must encompass color squares
  }

  private object TextColor {
    val steelBlue = Color("#4682b4")
    val grey = Color("#999999")
  }

  private object LegendTable {
    val Y = 28 // brute force alignment with visual ranking

    case class ColumnPositions(rankColumnX: Option[Int], nameColumnX: Int, colorColumnX: Int)

    val WithRank = ColumnPositions(rankColumnX = Some(15), nameColumnX = 20, colorColumnX = 100)
    val WithoutRank = ColumnPositions(rankColumnX = None, nameColumnX = Page.Margin, colorColumnX = 80)

    val ColorCellDimension = 6
    val ColorCellOffsetY = 1
  }

  implicit class RichJSPdf(pdf: jsPDF) {
    def setTextColor(color: Color): jsPDF = pdf.setTextColor(color.r, color.g, color.b)
  }

  case class ReportConfig(title: String, creator: String, notes: String)

  case class Props(chartSvgRootElementId: String, defaultReportConfig: ReportConfig, activeTab: Tab, model: AppModel, dispatchCB: Action => Callback)

  case class State(reportConfig: ReportConfig)

  class Backend($: BackendScope[Props, State]) {

    private def showDialog = $.props >>= { p =>
      p.dispatchCB(UpdatePdfExport(PdfExportDialogVisible))
    }

    private def inProgress = $.props >>= { p =>
      p.dispatchCB(UpdatePdfExport(PdfExportInProgress))
    }

    private def hideDialog = $.props >>= { p =>
      p.dispatchCB(UpdatePdfExport(PdfExportDialogHidden))
    }

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

        // Page 4: Expert Configuration
        appendExpertConfig(pdf, model)

        // Page 5: Missing
        appendMissing(pdf, model.config.missingIndicators)

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
      var y = Page.TitleY
      pdf.text(reportConfig.title, Page.Margin, y)

      def append(label: String, value: Array[String], currentY: Int, textColor: Color = Color.Black): Int = {
        var y = currentY + Font.LineGap + Font.LineGap
        pdf.setFont(Font.Name, "bold")
        pdf.text(label, Page.Margin, y)
        y += Font.LineGap
        pdf.setFont(Font.Name, "normal")
        pdf.setTextColor(textColor)
        pdf.text(value.toJSArray, xTabbed, y)
        pdf.setTextColor(Color.Black)
        y
      }

      // Creator
      pdf.setFontSize(Font.DefaultSize)
      if (!reportConfig.creator.trim.isEmpty) {
        y = append("Creator", Array(reportConfig.creator), y)
      }

      // Creation Date
      val date = new Date()
      val twoDigits = "%02d"
      val day = twoDigits.format(date.getDate())
      val month = twoDigits.format(date.getMonth() + 1)
      val year = date.getFullYear()
      val formattedDate = s"$day.$month.$year"
      y = append("Creation Date", Array(formattedDate), y)

      // Software
      val url = dom.window.location.href
      y = append("Software URL", Array(url), y, TextColor.steelBlue)

      // GitHub
      val gitHub = "https://github.com/fhnw-saav/saav"
      y = append("GitHub URL", Array(gitHub), y, TextColor.steelBlue)

      // Notes
      if (!reportConfig.notes.trim.isEmpty) {
        val notes = pdf.splitTextToSize(reportConfig.notes, Page.ContentWidth - xTabbed, new js.Array[Any]())
        y = append("Notes", notes.toArray, y)
      }
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

    private def appendExpertConfig(pdf: jsPDF, model: AppModel) = {
      pdf.addPage()
      val xTab = 7
      val xWeightColumn = Page.Width - Page.Margin - 80
      val xDefaultWeightColumn = xWeightColumn + 30
      val actualWeights = model.expertConfig.actualWeights
      val defaultWeights = model.expertConfig.defaultWeights
      val unexpectedIndicators = model.config.unexpectedIndicators

      def formatted(w: Weight) = w match {
        case Quality(weight) => f"Quality $weight%.1f"
        case Profile => "Profile"
      }

      pdf.setFont(Font.Name, "bold")
      pdf.setFontSize(Font.TitleSize)
      var y = Page.TitleY
      pdf.text("Expert Configuration", Page.Margin, y)
      y += Font.LineGap
      pdf.setFontSize(Font.DefaultSize)

      if (actualWeights != defaultWeights) {
        pdf.setFont(Font.Name, "bold")
        pdf.text("Deviations from defaults are reported in bold", Page.Margin, y)
        y += Font.LineGap
      }

      if (unexpectedIndicators.nonEmpty) {
        pdf.setFont(Font.Name, "normal")
        pdf.setTextColor(Color.Red)
        pdf.text("Unexpected indicators are reported in red", Page.Margin, y)
        y += Font.LineGap
      }

      for (criteria <- model.analysis.criteria) {
        val (newY, isNewPage) = advanceToNewLineAddingNewPageIfNeeded(y, pdf)
        y = newY
        if (!isNewPage) {
          pdf.line(Page.Margin, y, Page.Margin + Page.ContentWidth, y)
          y += Font.LineGap
        }
        pdf.setFont(Font.Name, "normal")
        pdf.setTextColor(Color.Black)
        pdf.text(criteria.displayName, Page.Margin, y)
        for (subCriteria <- criteria.subCriteria) {
          y = advanceToNewLineAddingNewPageIfNeeded(y, pdf)._1
          pdf.setTextColor(Color.Black)
          pdf.setFont(Font.Name, "normal")
          val weight = actualWeights.subCriteriaWeights(subCriteria.id)
          val defaultWeight = defaultWeights.subCriteriaWeights(subCriteria.id)
          if (weight != defaultWeight) {
            pdf.setFont(Font.Name, "bold")
            pdf.text("Default: " + formatted(defaultWeight), xDefaultWeightColumn, y)
          }
          pdf.text(subCriteria.displayName, Page.Margin + xTab, y)
          pdf.text(formatted(weight), xWeightColumn, y)

          for (indicator <- subCriteria.indicators) {
            y = advanceToNewLineAddingNewPageIfNeeded(y, pdf)._1
            if (unexpectedIndicators.contains(indicator.id)) {
              pdf.setTextColor(Color.Red)
            } else {
              pdf.setTextColor(Color.Black)
            }
            pdf.setFont(Font.Name, "normal")
            val isIndicatorEnabled = actualWeights.enabledIndicators.contains(indicator.id)
            val isIndicatorEnabledByDefault = defaultWeights.enabledIndicators.contains(indicator.id)

            if (isIndicatorEnabled != isIndicatorEnabledByDefault) {
              pdf.setFont(Font.Name, "bold")
            } else {
              pdf.setFont(Font.Name, "normal")
            }

            if (isIndicatorEnabled) {
              // TODO: Replace with base64 encoded checkbox image (no UTF-8 support in jsPDF)
              pdf.text("[x]", Page.Margin + xTab, y)
            }
            pdf.text(indicator.displayName, Page.Margin + 2 * xTab, y)
          }
        }
      }
    }

    private def appendMissing(pdf: jsPDF, indicators: Seq[IndicatorId]) = {

      val x = Page.Margin
      var y = Page.TitleY

      if (indicators.nonEmpty) {
        pdf.addPage()
        pdf.setFontSize(Font.TitleSize)

        val title = "Missing Indicator" + (if (indicators.size > 1) "s" else "")
        pdf.text(s"${indicators.size} $title", x, y)
        pdf.setFontSize(Font.DefaultSize)

        y += Font.LineGap

        indicators.foreach { indicator =>
          y = advanceToNewLineAddingNewPageIfNeeded(y, pdf)._1

          val c = indicator.subCriteriaId.criteriaId.name
          val sc = indicator.subCriteriaId.name
          val i = indicator.name

          val hierarchy = s"$c | $sc | $i"

          pdf.text(hierarchy, x, y)
        }
      }
    }

    private def advanceToNewLineAddingNewPageIfNeeded(y: Int, pdf: jsPDF): (Int, Boolean) = {
      val newY = y + Font.LineGap
      if (newY > Page.Margin + Page.ContentHeight) {
        pdf.addPage()
        (Page.Margin + Font.LineGap, true)
      } else {
        (newY, false)
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

    private def onNotesChange(e: ReactEventI) = {
      val newValue = e.target.value
      $.modState(s => s.copy(reportConfig = s.reportConfig.copy(notes = newValue)))
    }

    def render(p: Props, s: State): ReactTagOf[Div] = {

      val exportPdfLabel = "Export PDF"
      val button = Button(showDialog, exportPdfLabel + "...")

      p.model.pdfExport match {

        case PdfExportDialogHidden => <.div(button)

        case PdfExportInProgress =>
          val modal = Modal(
            Modal.Props(
              footer = _ => <.div(""),
              header = _ => <.div("")
            ),
            <.div("Export In Progress...")
          )
          <.div(button, modal)

        case PdfExportDialogVisible =>
          val title = <.div(css.form.group,
            <.label(^.`for` := "title", "Title:"),
            <.input(css.form.control,
              ^.`type` := "text",
              ^.id := "title",
              ^.onChange ==> onTitleChange,
              ^.value := s.reportConfig.title)
          )

          val creator = <.div(css.form.group,
            <.label(^.`for` := "creator", "Creator:"),
            <.input(css.form.control,
              ^.`type` := "text",
              ^.id := "creator",
              ^.maxLength := 50,
              ^.onChange ==> onCreatorChange,
              ^.value := s.reportConfig.creator)
          )

          val notes = <.div(css.form.group,
            <.label(^.`for` := "notes", "Notes:"),
            <.textarea(css.form.control,
              ^.id := "notes",
              ^.maxLength := 1000,
              ^.rows := 10,
              ^.onChange ==> onNotesChange,
              ^.value := s.reportConfig.notes)
          )

          def onClick(hide: Callback): Callback =
            inProgress >> generatePdf(s.reportConfig, p.activeTab, p.model).thenRun {
              // 'hide' is a bootstrap/jQuery handle to hide the modal, 'hideDialog' is our own state management
              (hide >> hideDialog).async.runNow()
            }.void

          val modal = Modal(
            Modal.Props(
              header = _ => <.h2(exportPdfLabel),
              footer = hide => <.div(
                Button(hide >> hideDialog, "Cancel"),
                Button(onClick(hide), exportPdfLabel)
              )
            ),
            <.form(title, creator, notes)
          )
          <.div(button, modal)
        }
      }
  }

  private val component = ReactComponentB[Props](PdfExportComponent.getClass.getSimpleName)
    .initialState_P { p =>
      State(reportConfig = p.defaultReportConfig)
    }
    .renderBackend[Backend]
    .build

  def apply(chartSvgRootElementId: String, defaultTitle: String, activeTab: Tab, proxy: ModelProxy[AppModel]): ReactComponentU[Props, State, Backend, TopNode] = {
    val dispatchCB = proxy.dispatchCB[Action] _
    component(Props(chartSvgRootElementId, defaultReportConfig = ReportConfig(title = defaultTitle, creator = "", notes = ""), activeTab, proxy.value, dispatchCB))
  }

}