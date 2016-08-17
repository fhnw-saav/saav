package ch.fhnw.ima.saav
package component

import ch.fhnw.ima.saav.model.app.PlottableQualityDataModel
import ch.fhnw.ima.saav.model.color._
import diode.react.ModelProxy
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{ReactComponentB, _}
import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.raw.HTMLCanvasElement

/**
  * A component to draw onto an HTML canvas.
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
object CanvasComponent {

  case class Props(proxy: ModelProxy[PlottableQualityDataModel])

  case class State(node: Option[HTMLCanvasElement] = None, e: Option[ReactMouseEvent] = None)

  class Backend($: BackendScope[Props, State]) {

    def handleClick(e: ReactMouseEvent) = {
      e.persist() // needed to keep event properties
      $.modState(_.copy(e = Some(e)))
    }

    def render() = {
      <.canvas(^.onClick ==> handleClick)
    }

  }

  private val component = ReactComponentB[Props](CanvasComponent.getClass.getSimpleName)
    .initialState(State())
    .renderBackend[Backend]
    .domType[HTMLCanvasElement]
    .componentDidMount(scope => {
      val canvas = scope.getDOMNode()
      scope.modState(s => State(Some(canvas)))
    })
    .shouldComponentUpdate(scope => {

      def renderCanvas() = {

        val model = scope.nextProps.proxy.value

        // a reference to the (non-virtual) DOM node
        val canvas = scope.nextState.node.get

        // TODO: adjust canvas size to containing div (dynamically respecting padding/margin)
        val bootstrapGutter = 30
        canvas.width = canvas.parentElement.clientWidth - bootstrapGutter
        canvas.height = 50

        // actual canvas drawing
        val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
        paintComponent(model, ctx, canvas.width, canvas.height, scope.nextState.e)
      }

      // invoke for current window size
      renderCanvas()

      // refresh whenever window is resized
      window.onresize = (e: dom.Event) => renderCanvas()

      // never need to update (re-using same virtual canvas element)
      false

    })
    .build

  private def paintComponent(model: PlottableQualityDataModel, ctx: dom.CanvasRenderingContext2D, width: Int, height: Int, event: Option[ReactMouseEvent]): Unit = {

    ctx.clearRect(0, 0, width, height)

    val colors = SolarizedPalette
    val barWidth = width / colors.size

    for ((color, index) <- colors.zipWithIndex) {
      ctx.fillStyle = color.hexValue
      ctx.fillRect(index * barWidth, 0, barWidth, height)
    }

    val text = event match {
      case Some(e) => s"Click @ ${e.screenX}/${e.screenY}"
      case _ => "Click me!"
    }
    ctx.font = s"12px Arial"
    ctx.fillStyle = "black"
    ctx.textBaseline = "middle"
    ctx.fillText(text, 10, height / 2d)
  }

  def apply(proxy: ModelProxy[PlottableQualityDataModel]) = component(Props(proxy))

}
