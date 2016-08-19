package ch.fhnw.ima.saav.component

import ch.fhnw.ima.saav.model.app.{PlottableEntity, PlottableQualityDataModel}
import diode.react.ModelProxy
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{BackendScope, ReactComponentB}

object SvgPlotComponent {

  case class Props(proxy: ModelProxy[PlottableQualityDataModel])

  // TODO: Once PlottableCategory is a case class, use (PlottableCategory, PlottableEntity) pair here
  type HoveredPoint = Option[(String, PlottableEntity)]

  case class State(hoveredPoint: HoveredPoint = None)

  class Backend($: BackendScope[Props, State]) {

    def setHoveredPoint(hoveredPoint: HoveredPoint) =
      $.modState(s => State(hoveredPoint))

    def clearHoveredPoint() = $.modState(s => State(None))

    def render(p: Props, s: State) = {

      val m = p.proxy.value

      // only relevant for aspect ratio and e.g. relative stroke width (svg will be scaled within parent element)
      val plotWidth = 1000
      val plotHeight = 200

      val padding = 10

      val background = <.svg.rect(
        ^.svg.fill := "lightgrey",
        ^.svg.x := "0", ^.svg.y := "0",
        ^.svg.width := "100%", ^.svg.height := "100%")

      val plot = {

        val lineHeight = plotHeight - 2 * padding
        val categoryLineDistance = (plotWidth - 2 * padding) / m.categories.length

        for {
          (category, categoryIndex) <- m.categories.zipWithIndex
          x = (categoryIndex * categoryLineDistance) + (categoryLineDistance / 2.0) + padding
        } yield {
          val lineStartY = padding
          val categoryLines = <.svg.line(
            ^.svg.x1 := x, ^.svg.y1 := lineStartY,
            ^.svg.x2 := x, ^.svg.y2 := lineStartY + lineHeight,
            ^.svg.stroke := "black", ^.svg.strokeWidth := "1"
          )

          val entities = m.rankedEntities.filter(_.isSelected)
          val entityPointDistance = lineHeight / entities.length

          val entityPoints = for {
            (entity, entityIndex) <- entities.zipWithIndex
            y = (entityIndex * entityPointDistance) + lineStartY + (entityPointDistance / 2)
          } yield {
            val pinnedOrHovered = if (entity.isPinned || s.hoveredPoint.contains((category.name, entity))) "black" else "transparent"
            <.svg.circle(
              ^.svg.cx := x, ^.svg.cy := y, ^.svg.r := 5,
              ^.svg.fill := entity.color.hexValue,
              ^.svg.strokeWidth := 2,
              ^.svg.stroke := pinnedOrHovered,
              ^.onMouseOver --> setHoveredPoint(Some((category.name, entity))),
              ^.onMouseOut --> clearHoveredPoint()
            )
          }

          <.svg.g(
            categoryLines,
            entityPoints
          )
        }

      }

      <.svg.svg(^.svg.viewBox := s"0 0 $plotWidth $plotHeight",
        background,
        plot
      )

    }

  }

  private val component = ReactComponentB[Props](SvgPlotComponent.getClass.getSimpleName)
    .initialState(State())
    .renderBackend[Backend]
    .build

  def apply(proxy: ModelProxy[PlottableQualityDataModel]) = component(Props(proxy))

}