package ch.fhnw.saav.component

import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{ReactComponentB, _}
import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.raw.HTMLCanvasElement

/**
  * Draws a chart onto an HTML canvas.
  *
  * Accessing the canvas element with React is a bit problematic, since React builds a virtual DOM and
  * updates the real DOM behind the scenes. Therefore the real canvas element does not exist at the time of the `render`
  * function call. All painting to the canvas thus needs to be in the `componentDidMount` function, which
  * is called after the real DOM has been updated.
  *
  * Further complexity is caused by the fact that canvas dimensions must be specified in absolute pixels. Setting
  * width/height to e.g. 100% would just cause the rendered area to be scaled (which would look blurry). In order to
  * get dynamic resizing, the canvas must thus get re-rendered whenever the browser window is resized.
  */
object CChart {

  val component = ReactComponentB[Unit]("Chart")
    .render($ => <.canvas())
    .domType[HTMLCanvasElement]
    .componentDidMount(scope => Callback {

      def renderCanvas() = {

        // a reference to the (non-virtual) DOM node
        val canvas = scope.getDOMNode()

        // adjust canvas size to containing div
        canvas.width = canvas.parentElement.clientWidth
        canvas.height = canvas.parentElement.clientHeight

        // actual canvas drawing
        val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
        paintComponent(ctx, canvas.width, canvas.height)
      }

      // invoke for current window size
      renderCanvas()

      // refresh whenever window is resized
      window.onresize = (e: dom.Event) => renderCanvas()

    }).build

  private def paintComponent(ctx: dom.CanvasRenderingContext2D, width: Int, height: Int): Unit = {

    val colors = List("#b58900", "#cb4b16", "#dc322f", "#6c71c4", "#268bd2")
    val barWidth = width / colors.size

    for ((color, index) <- colors.zipWithIndex) {
      ctx.fillStyle = color
      ctx.fillRect(index * barWidth, 0, barWidth, 20)
    }

  }

  def apply() = component()

}
