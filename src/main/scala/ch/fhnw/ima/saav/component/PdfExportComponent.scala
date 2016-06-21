package ch.fhnw.ima.saav.component

import ch.fhnw.ima.saav.model.model.Analysis
import ch.fhnw.ima.saav.model.model.Entity.Project
import ch.fhnw.ima.saav.style.GlobalStyles
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{Callback, ReactComponentB}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSName
import scalacss.ScalaCssReact._

object PdfExportComponent {

  private def generatePdf() = Callback {
    val doc = new jsPDF()
    doc.text(10, 10, "Hello PDF World")
    doc.save("Report.pdf")
  }

  case class Props(analysis: Analysis[Project])

  private val css = GlobalStyles

  private val component = ReactComponentB[Props](PdfExportComponent.getClass.getSimpleName)
    .render(_ => <.button(css.defaultButton, ^.onClick --> generatePdf(), "Generate PDF"))
    .build

  def apply(analysis: Analysis[Project]) = component(Props(analysis))

  // Facade to jsPDF library
  @js.native
  @JSName("jsPDF")
  class jsPDF extends js.Object {

    def text(x: Int, y: Int, text: String): js.Any = js.native

    def save(fileName: String): js.Any = js.native

  }

}
