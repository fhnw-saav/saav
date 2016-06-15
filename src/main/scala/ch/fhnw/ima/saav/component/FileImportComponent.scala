package ch.fhnw.ima.saav.component

import ch.fhnw.ima.saav.model.model.Entity.Project
import ch.fhnw.ima.saav.model.model.{Analysis, AnalysisBuilder, Review}
import ch.fhnw.ima.saav.style.GlobalStyles
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{BackendScope, Callback, CallbackTo, ReactComponentB}
import org.scalajs.dom
import org.scalajs.dom.DragEvent
import org.singlespaced.d3js.d3

import scala.concurrent.{Future, Promise}
import scala.scalajs.js
import scala.scalajs.js.annotation.JSName
import scalacss.ScalaCssReact._

/**
  * A component which accepts a CSV file via drag and drop and imports its contents.
  */
object FileImportComponent {

  type ModelAccepter = (Analysis[Project]) => Unit

  case class Props(modelAccepter: ModelAccepter)

  private val css = GlobalStyles

  @JSName("URL")
  @js.native
  object URL extends dom.URL

  class Backend($: BackendScope[Props, Unit]) {

    def handleDragOver = (e: DragEvent) => Callback {
      e.stopPropagation()
      e.preventDefault()
      e.dataTransfer.dropEffect = "copy" // Explicitly show this is a copy.
    }

    type Datum = js.Dictionary[String]

    def parseModel(url: String): Future[Analysis[Project]] = {

      val dataLoadedPromise: Promise[Analysis[Project]] = Promise()

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

        val analysis = builder.build

        // making compiler happy (must return Unit)
        val done: Unit = dataLoadedPromise.success(analysis)
      })

      dataLoadedPromise.future

    }

    def handleFileDropped(modelAccepter: ModelAccepter)(e: DragEvent): Callback = {

      import scala.concurrent.ExecutionContext.Implicits.global

      e.stopPropagation()
      e.preventDefault()

      val files = e.dataTransfer.files

      if (files.length > 0) {
        val file = files(0)
        val url = URL.createObjectURL(file)

        // parsing the model happens asynchronously -> returns a future
        val analysisFuture = parseModel(url)
        // hand new model to modelAccepter as soon as future completes successfully
        CallbackTo(analysisFuture.map(analysis => modelAccepter(analysis)))
      } else {
        Callback.log("No files to import")
      }
    }

    def render(p: Props) = <.div(css.fileDropZone, ^.onDragOver ==> handleDragOver, ^.onDrop ==> handleFileDropped(p.modelAccepter),
      <.h1("Drag and drop"),
      <.p("To import data from CSV file")
    )

  }

  private val component = ReactComponentB[Props](FileImportComponent.getClass.getSimpleName)
    .renderBackend[Backend]
    .build

  def apply(modelAccepter: ModelAccepter) = component(Props(modelAccepter))

}
