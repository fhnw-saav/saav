package ch.fhnw.ima.saav.component

import ch.fhnw.ima.saav.model.model.{AnalysisBuilder, Review}
import ch.fhnw.ima.saav.model.model.Entity.Project
import ch.fhnw.ima.saav.style.GlobalStyles
import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB}
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom
import org.scalajs.dom.DragEvent
import org.singlespaced.d3js.d3

import scala.scalajs.js
import scala.scalajs.js.annotation.JSName
import scalacss.ScalaCssReact._

object CFileImport {

  private val css = GlobalStyles

  @JSName("URL")
  @js.native
  object URL extends dom.URL

  class Backend($: BackendScope[Unit, Unit]) {

    def handleDragOver = (e: DragEvent) => Callback {
      e.stopPropagation()
      e.preventDefault()
      e.dataTransfer.dropEffect = "copy" // Explicitly show this is a copy.
    }

    type Datum = js.Dictionary[String]

    def parseCSV(url: String) = {
      d3.csv(url, (data: js.Array[Datum]) => {

        val builder = AnalysisBuilder.projectAnalysisBuilder

        data.foreach(row => {

          val keyIt = row.keys.iterator
          val project = Project(row(keyIt.next()))
          val hierarchyLevels = row(keyIt.next()).split(":::")
          val category = hierarchyLevels(0)
          val subCategory = hierarchyLevels(1)
          val indicator = hierarchyLevels(2)
          val review = Review(row(keyIt.next()))
          val value = row(keyIt.next()).toDouble

          builder
              .category(category)
              .subCategory(subCategory)
              .indicator(indicator)
              .addValue(project, review, value)

        })

        val analysis = builder.build()

        for {
          project <- analysis.entities
          review <- analysis.reviews
          category <- analysis.categories
          subCategory <- category.subCategories
          indicator <- subCategory.indicators
        } {
          val value = analysis.value(project, indicator, review)
          println(s"${project.name} | ${category.name} | ${subCategory.name} | ${indicator.name} | ${review.name} | $value")
        }

      })
    }

    def handleFileDropped = (e: DragEvent) => Callback {
      e.stopPropagation()
      e.preventDefault()

      val files = e.dataTransfer.files

      if (files.length > 0) {
        val file = files(0)
        val url = URL.createObjectURL(file)

        parseCSV(url)
      }
    }

    def render() = <.div(css.fileDropZone, ^.onDragOver ==> handleDragOver, ^.onDrop ==> handleFileDropped,
        <.h1("Drag and drop"),
        <.p("To import data from CSV file")
      )

  }

  private val component = ReactComponentB[Unit](CFileImport.getClass.getSimpleName)
    .renderBackend[Backend]
    .build

  def apply() = component()

}
