package ch.fhnw.ima.saav
package component

import ch.fhnw.ima.saav.model.app.{AppModel, EntitySelectionModel, GroupedEntity, QualityModel}
import ch.fhnw.ima.saav.model.color.WebColor
import ch.fhnw.ima.saav.model.domain.EntityId
import ch.fhnw.ima.saav.model.layout.QualityChartLayout
import diode.react.ModelProxy
import japgolly.scalajs.react.vdom.ReactTagOf
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{BackendScope, ReactComponentB, ReactComponentU, TopNode}
import org.scalajs.dom.raw.SVGElement

import scalacss.ScalaCssReact._

object VisualRankingComponent {

  private val width = 100
  private val height = QualityChartLayout.Height

  private val xMiddle = width / 2

  private val axisTop = QualityChartLayout.BoxTopY
  private val axisBottom = QualityChartLayout.BoxBotY
  private val axisHeight = axisBottom - axisTop

  private val titleText = "Ranking"
  private val titleY = QualityChartLayout.Padding / 2
  private val titleHeight = QualityChartLayout.Padding

  private val radius = 5
  private val xJitterGap = 3

  final case class Props(model: QualityModel, selectionModel: EntitySelectionModel, colorMap: Map[EntityId, WebColor])

  class Backend($: BackendScope[Props, Unit]) {

    def render(p: Props): ReactTagOf[SVGElement] = {

      val background = <.svg.rect(
        ^.svg.fill := "white",
        ^.svg.stroke := "#eeeeee",
        ^.svg.strokeWidth := 4,
        ^.svg.x := "0", ^.svg.y := "0",
        ^.svg.width := width, ^.svg.height := "100%")

      val axis = <.svg.line(
        ^.svg.x1 := xMiddle, ^.svg.y1 := axisTop,
        ^.svg.x2 := xMiddle, ^.svg.y2 := axisBottom,
        ^.svg.stroke := "#cccccc", ^.svg.strokeWidth := "1"
      )

      val title = <.svg.foreignObject(
        ^.svg.x := 0,
        ^.svg.y := titleY,
        ^.svg.width := width, ^.svg.height := titleHeight,
        <.div(
          ^.textAlign.center,
          ^.overflow.hidden, ^.textOverflow.ellipsis, ^.whiteSpace.nowrap,
          ^.width := s"${width}px", ^.height := s"${titleHeight}px", ^.minWidth := "0",
          ^.title := titleText,
          titleText
        )
      )

      <.svg.svg(
        css.centerBlock,
        ^.svg.viewBox := s"0 0 $width $height",
        ^.svg.width := width,
        ^.svg.height := s"${height}px",
        ^.svg.preserveAspectRatio := "none",
        background,
        axis,
        title,
        EntityDots(p)
      )

    }

  }

  private val EntityDots = ReactComponentB[VisualRankingComponent.Props]("EntityDots")
    .render_P { p =>

      val values = p.model.rankedEntities.map(_.value)
      val min = values.min.getOrElse(Double.NaN)
      val max = values.max.getOrElse(Double.NaN)
      val valueSpan = max - min

      def isVisible(e: GroupedEntity) = p.selectionModel.visible.contains(e.id)
      def isPinned(e: GroupedEntity) = p.selectionModel.pinned.contains(e.id)
      def isHovered(e: GroupedEntity) = p.selectionModel.hovered.contains(e.id)

      val entitiesInPaintingOrder = p.model.rankedEntities.zipWithIndex.sortBy { case (e, index) =>
        (isPinned(e), isHovered(e), isVisible(e), -index) // `-index` assures that elements first in legend are painted last (i.e. in front)
      }.unzip._1

      val entitiesInPaintingOrderGroupedByRank = entitiesInPaintingOrder.groupBy(_.position)

      val dots = for {
        (_, entities) <- entitiesInPaintingOrderGroupedByRank
        (entity, entityIndexWithinRank) <- entities.zipWithIndex
        value <- entity.value
      } yield {

        val xJitterSpan = (entities.length - 1) * xJitterGap
        val x = xMiddle + (entityIndexWithinRank * xJitterGap) - xJitterSpan / 2

        val y = valueSpan match {
          case 0 => axisTop // display circle at very top if all entity values are identical
          case _ => axisBottom - ((value - min) / valueSpan * axisHeight)
        }


        val color =
          if (isVisible(entity)) {
            if (isPinned(entity)) "black"
            else p.colorMap(entity.id).hexValue
          } else "#cccccc"

        val formattedValue = entity.value.map(_.toString).getOrElse("-")

        <.svg.circle(
          ^.svg.cx := x,
          ^.svg.cy := y,
          ^.svg.r := radius,
          ^.svg.fill := color,
          <.svg.title(s"${entity.displayName}: $formattedValue")
        )

      }
      <.svg.g(dots)
    }
    .build

  private final val component = ReactComponentB[Props](VisualRankingComponent.getClass.getSimpleName)
    .renderBackend[Backend]
    .build

  def apply(proxy: ModelProxy[AppModel]): ReactComponentU[Props, Unit, Backend, TopNode] =
    component(Props(proxy.value.qualityModel, proxy.value.entitySelectionModel, proxy.value.colorMap))

}
