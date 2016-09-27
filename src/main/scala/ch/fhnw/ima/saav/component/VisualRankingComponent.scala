package ch.fhnw.ima.saav
package component

import ch.fhnw.ima.saav.model.app.{EntitySelectionModel, GroupedEntity}
import ch.fhnw.ima.saav.model.color.WebColor
import ch.fhnw.ima.saav.model.domain.Entity
import ch.fhnw.ima.saav.model.layout.QualityChartLayout
import japgolly.scalajs.react.vdom.ReactTagOf
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{BackendScope, ReactComponentB, ReactComponentU, TopNode}
import org.scalajs.dom.svg.SVG

import scalacss.ScalaCssReact._

object VisualRankingComponent {

  private val width = 20
  private val height = QualityChartLayout.height

  private val paddingTop = 47    // align with legend table header
  private val paddingBottom = 40 // align with QualityChartLayout bottom

  private val radius = 5

  final case class Props(entities: Seq[GroupedEntity], selectionModel: EntitySelectionModel, colorMap: Map[Entity, WebColor])

  class Backend($: BackendScope[Props, Unit]) {

    def render(p: Props): ReactTagOf[SVG] = {

      val axis = <.svg.line(
        ^.svg.x1 := width / 2, ^.svg.y1 := paddingTop,
        ^.svg.x2 := width / 2, ^.svg.y2 := height - paddingBottom,
        ^.svg.stroke := "#999999", ^.svg.strokeWidth := "1"
      )

      <.svg.svg(
        css.centerBlock,
        ^.svg.viewBox := s"0 0 $width $height",
        ^.svg.width := width,
        ^.svg.height := s"${height}px",
        axis,
        EntityDots(p)
      )

    }

  }

  private val EntityDots = ReactComponentB[VisualRankingComponent.Props]("EntityDots")
    .render_P { p =>

      val values = p.entities.map(_.value)
      val min = values.min.getOrElse(Double.NaN)
      val max = values.max.getOrElse(Double.NaN)
      val valueSpan = max - min

      def isVisible(e: GroupedEntity) = p.selectionModel.visible.contains(e.id)

      def isPinned(e: GroupedEntity) = p.selectionModel.pinned.contains(e.id)

      val entitiesInPaintingOrder = p.entities.sortBy { e =>
        (isPinned(e), isVisible(e), e.sortingPosition) // higher ranks should be painted last (i.e. in front)
      }

      val dots = for {
        entity <- entitiesInPaintingOrder
        value <- entity.value
      } yield {

        val ySpan = height - paddingTop - paddingBottom

        val y = valueSpan match {
          case 0 => ySpan // display circle at very top if all entity values are identical
          case _ => (value - min) / valueSpan * ySpan
        }

        val color =
          if (isVisible(entity)) {
            if (isPinned(entity)) "black"
            else p.colorMap(entity.id).hexValue
          } else "#cccccc"

        val formattedValue = entity.value.map(_.toString).getOrElse("-")

        <.svg.circle(
          ^.svg.cx := width / 2,
          ^.svg.cy := paddingTop + (ySpan - y),
          ^.svg.r := radius,
          ^.svg.fill := color,
          <.svg.title(s"${entity.name}: $formattedValue")
        )

      }
      <.svg.g(dots)
    }
    .build

  private final val component = ReactComponentB[Props](VisualRankingComponent.getClass.getSimpleName)
    .renderBackend[Backend]
    .build

  def apply(entities: Seq[GroupedEntity], selectionModel: EntitySelectionModel, colorMap: Map[Entity, WebColor]): ReactComponentU[Props, Unit, Backend, TopNode] = component(Props(entities, selectionModel, colorMap))

}
