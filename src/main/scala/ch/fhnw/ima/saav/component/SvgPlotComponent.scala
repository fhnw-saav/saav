package ch.fhnw.ima.saav.component

import ch.fhnw.ima.saav.model.app.{PlottableEntity, PlottableQualityDataModel}
import diode.react.ModelProxy
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{BackendScope, ReactComponentB}

object SvgPlotComponent {

  case class Props(proxy: ModelProxy[PlottableQualityDataModel])

  case class State(hovered: Option[PlottableEntity] = None)

  class Backend($: BackendScope[Props, State]) {

    def setHoveredEntity(entity: PlottableEntity) = $.modState(s => State(Some(entity)))
    def clearHoveredEntity() = $.modState(s => State(None))

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
          val categoryLine = <.svg.line(
            ^.svg.x1 := x, ^.svg.y1 := lineStartY,
            ^.svg.x2 := x, ^.svg.y2 := lineStartY + lineHeight,
            ^.svg.stroke := "black", ^.svg.strokeWidth := "1"
          )

          val entityPointDistance = lineHeight / m.rankedEntities.length

          val entityMedians = for {
            (entity, entityIndex) <- m.rankedEntities.zipWithIndex
            y = (entityIndex * entityPointDistance) + lineStartY + (entityPointDistance / 2)
          } yield {
            val pinnedOrHovered = if (entity.isPinned || s.hovered.contains(entity)) "black" else "transparent"
            <.svg.circle(
              ^.svg.cx := x, ^.svg.cy := y, ^.svg.r := 5,
              ^.svg.fill := entity.color.hexValue,
              ^.svg.strokeWidth := 2,
              ^.svg.stroke := pinnedOrHovered,
              ^.onMouseOver --> setHoveredEntity(entity),
              ^.onMouseOut --> clearHoveredEntity()
            )
          }

          <.svg.g(
            categoryLine,
            entityMedians
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