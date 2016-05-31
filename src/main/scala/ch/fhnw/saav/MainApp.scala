package ch.fhnw.saav

import org.scalajs.dom
import org.scalajs.dom.{DragEvent, EventTarget, document}
import org.singlespaced.d3js.Ops._
import org.singlespaced.d3js.d3

import scala.scalajs.js

object MainApp extends js.JSApp {

  def main(): Unit = {

    val dropZone = document.getElementById("drop-zone")
    dropZone.addEventListener[DragEvent]("dragover", handleDragOver, useCapture = false)
    dropZone.addEventListener[DragEvent]("drop", handleFileDropped, useCapture = false)

    def handleDragOver = (e: DragEvent) => {
      e.stopPropagation()
      e.preventDefault()
      e.dataTransfer.dropEffect = "copy" // Explicitly show this is a copy.
    }

    def handleFileDropped = (e: DragEvent) => {
      e.stopPropagation()
      e.preventDefault()

      val files = e.dataTransfer.files

      if (files.length > 0) {
        val file = files(0)
        document.getElementById("file-info").innerHTML = s"${file.name}  ${file.size} byte(s)"
        val url = URL.createObjectURL(file)
        loadCSV(url)
      }
    }

    type Datum = js.Dictionary[String]

    def loadCSV(url: String) = {
      d3.csv(url, (data: js.Array[Datum]) => {
        val root = d3.select("#d3")
        root.append("h3").text("Contents (parsed via D3):")
        root.selectAll("div")
          .data(data)
          .enter()
          .append("div")
          .text((d: Datum) => d.mkString(", "))
        root.append("p").append("input")
          .attr("type", "button")
          .attr("class", "button")
          .attr("value", "Generate PDF")
          .on("click", (e: EventTarget) => createPdf(data))
        ()
      })
    }

    def createPdf(data: js.Array[Datum]): Unit = {
      println("Creating PDF...")
      val doc = new jsPDF()
      for ((datum, index) <- data.zipWithIndex) {
        val offset = (index + 1) * 20
        doc.text(20, offset, datum.mkString(", "))
      }
      doc.save("Report.pdf")
    }

  }

}

// Facade types for JavaScript APIs

@js.native
object URL extends dom.URL

@js.native
class jsPDF extends js.Object {

  def text(x: Int, y: Int, text: String): js.Any = js.native

  def save(fileName: String): js.Any = js.native

}
