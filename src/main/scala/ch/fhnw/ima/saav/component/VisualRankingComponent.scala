package ch.fhnw.ima.saav
package component

import ch.fhnw.ima.saav.circuit.{UpdateEntityHoveringAction, UpdateEntityPinningAction}
import ch.fhnw.ima.saav.model.app.{AppModel, EntitySelectionModel, GroupedEntity, QualityModel}
import ch.fhnw.ima.saav.model.color.WebColor
import ch.fhnw.ima.saav.model.domain.EntityId
import ch.fhnw.ima.saav.model.layout.QualityChartLayout
import diode.Action
import diode.react.ModelProxy
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.ReactTagOf
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom.raw.SVGElement

import scalacss.ScalaCssReact._

object VisualRankingComponent {

  val ElementId: String = "saav-visual-ranking"

  private val width = 100
  private val height = QualityChartLayout.Height

  private val xMiddle = width / 2

  private val axisTop = QualityChartLayout.BoxTopY
  private val axisBottom = QualityChartLayout.BoxBotY
  private val axisHeight = axisBottom - axisTop

  private val titleText = "Ranking"
  private val titleY = QualityChartLayout.Padding / 2
  private val titleHeight = QualityChartLayout.Padding

  private val defaultRadius = 5
  private val boldRadius = 7

  private val xJitterGap = 3

  final case class Props(
    model: QualityModel,
    selectionModel: EntitySelectionModel,
    colorMap: Map[EntityId, WebColor],
    dispatchCB: Action => Callback
  )

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
        ^.id := ElementId,
        ^.svg.viewBox := s"0 0 $width $height",
        ^.svg.width := s"${width}px",
        ^.svg.height := s"${height}px",
        ^.svg.preserveAspectRatio := "none",
        background,
        axis,
        title,
        EntityDots(p)
      )

    }

  }

  class EntityDotsBackend($: BackendScope[Props, Unit]) {

    private def setHoveredEntity(hoveredEntity: Option[EntityId]) =
      $.props >>= { p =>
        p.dispatchCB(UpdateEntityHoveringAction(hoveredEntity))
      }

    private def toggleEntityPinning(entity: EntityId): Callback =
      $.props >>= { p =>
        val isPinned = p.selectionModel.pinned.contains(entity)
        val pinnedOrNone = if (isPinned) None else Some(entity)
        p.dispatchCB(UpdateEntityPinningAction(pinnedOrNone))
      }

    def render(p: Props): ReactTagOf[SVGElement] = {
      val values = p.model.rankedEntities.map(_.value)
      val min = values.min.getOrElse(Double.NaN)
      val max = values.max.getOrElse(Double.NaN)
      val valueSpan = max - min

      def isVisible(e: GroupedEntity) = p.selectionModel.visible.contains(e.id)
      def isPinned(e: GroupedEntity) = p.selectionModel.pinned.contains(e.id)
      def isHovered(e: GroupedEntity) = p.selectionModel.hovered.contains(e.id)

      val entitiesInPaintingOrder = p.model.rankedEntities.zipWithIndex.sortBy { case (e, index) =>
        (isVisible(e), -index) // `-index` assures that elements first in legend are painted last (i.e. in front)
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

        val (color, radius) =
          if (!isVisible(entity)) ("#cccccc", defaultRadius)
          else if (isPinned(entity)) ("black", boldRadius)
          else {
            val r = if (isHovered(entity)) boldRadius else defaultRadius
            (p.colorMap(entity.id).hexValue, r)
          }

        val formattedValue = formatValue(entity.value)
        val tooltip = s"${entity.displayName}: $formattedValue"

        <.svg.circle(
          ^.key := String.valueOf(entity.id),
          ^.svg.cx := x,
          ^.svg.cy := y,
          ^.svg.r := radius,
          ^.svg.fill := color,
          ^.onClick --> toggleEntityPinning(entity.id),
          ^.onMouseOver --> setHoveredEntity(Some(entity.id)),
          ^.onMouseLeave --> setHoveredEntity(None),
          <.svg.title(tooltip)
        )

      }
      <.svg.g(dots)
    }
  }

  private val EntityDots = ReactComponentB[VisualRankingComponent.Props]("EntityDots")
    .renderBackend[EntityDotsBackend]
    .build

  private final val component = ReactComponentB[Props](VisualRankingComponent.getClass.getSimpleName)
    .renderBackend[Backend]
    .build

  def apply(proxy: ModelProxy[AppModel]): ReactComponentU[Props, Unit, Backend, TopNode] =
    component(Props(proxy.value.qualityModel, proxy.value.entitySelectionModel, proxy.value.colorMap, proxy.dispatchCB[Action]))

}
