package ch.fhnw.ima.saav.component

import ch.fhnw.ima.saav.model.app.PlottableQualityDataModel
import diode.react.ModelProxy
import japgolly.scalajs.react.ReactComponentB
import japgolly.scalajs.react.vdom.svg.all._

object SvgPlotComponent {

  case class Props(proxy: ModelProxy[PlottableQualityDataModel])

  private val component = ReactComponentB[Props](SvgPlotComponent.getClass.getSimpleName)
    .render_P { p =>

      val m = p.proxy.value

      // only relevant for aspect ratio and e.g. relative stroke width (svg will be scaled within parent element)
      val plotWidth = 1000
      val plotHeight = 200

      val padding = 10

      val background = rect(fill := "lightgrey", x := "0", y := "0", width := "100%", height := "100%")

      val plot = {

        val lineHeight = plotHeight - 2 * padding
        val categoryLineDistance = (plotWidth - 2 * padding) / m.categories.length

        for {
          (category, categoryIndex) <- m.categories.zipWithIndex
          x = (categoryIndex * categoryLineDistance) + (categoryLineDistance / 2.0) + padding
        } yield {
          val lineStartY = padding
          val categoryLine = line(
            x1 := x, y1 := lineStartY,
            x2 := x, y2 := lineStartY + lineHeight,
            stroke := "black", strokeWidth := "1"
          )

          val entityPointDistance = lineHeight / m.rankedEntities.length

          val entityMedians = for {
            (entity, entityIndex) <- m.rankedEntities.zipWithIndex
            y = (entityIndex * entityPointDistance) + lineStartY + (entityPointDistance / 2)
          } yield {
            val strokeIfPinned = if (entity.isPinned) "black" else "transparent"
            circle(
              cx := x, cy := y, r := 5,
              fill := entity.color.hexValue,
              strokeWidth := 2,
              stroke := strokeIfPinned
            )
          }

          g(
            categoryLine,
            entityMedians
          )
        }

      }

      svg(viewBox := s"0 0 $plotWidth $plotHeight",
        background,
        plot
      )
    }
    .build

  def apply(proxy: ModelProxy[PlottableQualityDataModel]) = component(Props(proxy))


}