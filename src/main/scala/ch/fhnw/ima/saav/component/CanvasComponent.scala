package ch.fhnw.ima.saav
package component

import ch.fhnw.ima.saav.model.app.{PlottableCategory, PlottableQualityDataModel, PlottableSubCategory}
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
        canvas.height = 500

        // actual canvas drawing
        val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
//        paintComponent(model, ctx, canvas.width, canvas.height, scope.nextState.e)
        paintComponent2(model, ctx, canvas.width, canvas.height, scope.nextState.e)
      }

      // invoke for current window size
      renderCanvas()

      // refresh whenever window is resized
      window.onresize = (e: dom.Event) => {
        println("*** resize ***")
        renderCanvas()
      }

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
      case Some(e) => s"Click @ screen ${e.screenX}/${e.screenY}, client ${e.clientX}/${e.clientY}, page ${e.pageX}/${e.pageY}"
      case _ => "Click me!"
    }
    ctx.font = s"12px Arial"
    ctx.fillStyle = "black"
    ctx.textBaseline = "middle"
    ctx.fillText(text, 10, height / 2d)

  }

  private def paintComponent2(model: PlottableQualityDataModel, ctx: dom.CanvasRenderingContext2D, width: Int, height: Int, event: Option[ReactMouseEvent]): Unit = {

    ctx.clearRect(0, 0, width, height)

    ctx.fillStyle = "white"
    ctx.fillRect(0, 0, width, height)

    val layout = new Layout(model, width, height)

    // Draw the coordinate system

    for (category <- model.categories) {
      // draw the criteria boxes
      val (x, width) = layout.getCriteriaBox(category)
      ctx.fillStyle = "#eeeeee"
      ctx.fillRect(x, layout.MARGIN, width, height - 2*layout.MARGIN)

      // draw the criteria axes
      drawAxis(ctx, layout.getCriteriaAxisX(category), layout.getCategoryAxisTopY, layout.getCategoryAxisBotY, "#cccccc")

      // draw the subcriteria axes
      for (subCategory <- category.subCategories) {
        drawAxis(ctx, layout.getSubCriteriaAxisX(subCategory), layout.getSubCategoryAxisTopY, layout.getSubCategoryAxisBotY, "#cccccc")
      }

    }

    // Draw the entities

    for (plottableEntity <- model.rankedEntities) {

      if (plottableEntity.isSelected) {
        ctx.strokeStyle = plottableEntity.color.hexValue
        ctx.lineWidth = 2
      } else {
        ctx.strokeStyle = "#cccccc"
        ctx.lineWidth = 1
      }

      // draw the criteria values

      ctx.beginPath()
      var index = 0
      for (category <- model.categories) {
        val x = layout.getCriteriaAxisX(category)
        val value = category.groupedValue(plottableEntity.entity).get * (layout.getCategoryAxisBotY - layout.getCategoryAxisTopY) / 100.0
        val y = layout.getCategoryAxisBotY - value

        if (index == 0) ctx.moveTo(x, y) else ctx.lineTo(x, y)

        index += 1
      }
      ctx.stroke()

      // draw the subcriteria values

      ctx.beginPath()
      index = 0
      for (category <- model.categories) {
        for (subCategory <- category.subCategories) {
          val x = layout.getSubCriteriaAxisX(subCategory)
          val value = subCategory.groupedValue(plottableEntity.entity).get * (layout.getSubCategoryAxisBotY - layout.getSubCategoryAxisTopY) / 100.0
          val y = layout.getSubCategoryAxisBotY - value

          if (index == 0) ctx.moveTo(x, y) else ctx.lineTo(x, y)

          index += 1
        }
      }
      ctx.stroke()

    }

  }

  private def drawAxis(ctx: dom.CanvasRenderingContext2D, x: Int, y1: Int, y2: Int, color: String) = {
    ctx.strokeStyle = color
    ctx.beginPath()
    ctx.moveTo(x, y1)
    ctx.lineTo(x, y2)
    ctx.stroke()
  }

  private def computeCriteriaCount(model: PlottableQualityDataModel) = {
    model.categories.size
  }

  private def computeAxisCount(model: PlottableQualityDataModel) = {
    var count = 0
    for (category <- model.categories) {
      count += category.subCategories.size
    }
    count
  }

  /**
    * This class computes all the relevant layout parameters.
    */

  class Layout () {
    val MARGIN = 10
    val PADDING = 20
    val VERTICAL_AXIS_GAP = 40

    private var criteriaAxisTopY = 0
    private var criteriaAxisBotY = 0
    private var subCriteriaAxisTopY = 0
    private var subCriteriaAxisBotY = 0

    private val criteriaBoxesMap = new scala.collection.mutable.HashMap[PlottableCategory, (Int, Int)]
    private val criteriaAxesMap = new scala.collection.mutable.HashMap[PlottableCategory, Int]
    private val subCriteriaAxesMap = new scala.collection.mutable.HashMap[PlottableSubCategory, Int]

    def this(model: PlottableQualityDataModel, width: Int, height: Int) {
      this()

      // Compute general parameters

      val criteriaCount = computeCriteriaCount(model)
      val axisCount = computeAxisCount(model)

      val axisSpacing = (width - ((criteriaCount+1) * MARGIN) - (criteriaCount * 2 * PADDING)) / (axisCount - criteriaCount)

      val axisHeight = (height - 2*PADDING - VERTICAL_AXIS_GAP) / 2

      // Compute axes y positions

      criteriaAxisTopY = PADDING
      criteriaAxisBotY = PADDING + axisHeight

      subCriteriaAxisTopY = height - PADDING - axisHeight
      subCriteriaAxisBotY = height - PADDING

      // Compute boxes and axes x positions

      var index = 0
      var x = 0
      for (category <- model.categories) {

        x = x + MARGIN
        val criteriaWidth = 2*PADDING + ((category.subCategories.size-1) * axisSpacing)

        criteriaBoxesMap(category) = (x, criteriaWidth)
        criteriaAxesMap(category) = x + (criteriaWidth / 2)

        var subIndex = 0
        // intellij does not catch it if we forget the .subCategories
        for (subCategory <- category.subCategories) {
          subCriteriaAxesMap(subCategory) = x + PADDING + (subIndex * axisSpacing)
          subIndex += 1
        }

        index += 1
        x = x + criteriaWidth
      }
    }

    def getCategoryAxisTopY = criteriaAxisTopY
    def getCategoryAxisBotY = criteriaAxisBotY
    def getSubCategoryAxisTopY = subCriteriaAxisTopY
    def getSubCategoryAxisBotY = subCriteriaAxisBotY

    def getCriteriaBox(category: PlottableCategory): (Int, Int) = criteriaBoxesMap(category)
    def getCriteriaAxisX(category: PlottableCategory): Int = criteriaAxesMap(category)
    def getSubCriteriaAxisX(subCategory: PlottableSubCategory): Int = subCriteriaAxesMap(subCategory)
  }


  def apply(proxy: ModelProxy[PlottableQualityDataModel]) = component(Props(proxy))

}
