package ch.fhnw.ima.saav.component

import ch.fhnw.ima.saav.circuit.{UpdateChartWidthAction, UpdateEntityHoveringAction, UpdateEntityPinningAction, UpdateSubCriteriaHoveringAction}
import ch.fhnw.ima.saav.model.app.AppModel
import ch.fhnw.ima.saav.model.domain.{EntityId, SubCriteriaId}
import ch.fhnw.ima.saav.model.layout.ChartLayout
import diode.react.ModelProxy
import japgolly.scalajs.react.{Callback, _}
import org.scalajs.dom.raw.{HTMLElement, SVGSVGElement, SVGTextElement}

object ChartComponent {
  val ElementId: String = "saav-chart"
}

trait ChartComponent {

  case class Props(proxy: ModelProxy[AppModel])

  protected val svgRootRef: RefSimple[SVGSVGElement] = Ref[SVGSVGElement]("svgRootRef")
  protected val svgSubCriteriaLabelRef: RefSimple[SVGTextElement] = Ref[SVGTextElement]("svgSubCriteriaLabelRef")

  class ChartBackend($: BackendScope[Props, Unit]) {

    def onWindowResize(): Callback =
      $.props >>= { p =>
        svgRootRef($).map { svg =>
          val parent = svg.parentNode.asInstanceOf[HTMLElement]
          val width = parent.clientWidth
          p.proxy.dispatchCB(UpdateChartWidthAction(width))
        }.toOption.getOrElse(Callback.empty)
      }

    def clearHovering: Callback = setHoveredEntity(None) >> setHoveredSubCriteria(None)

    def setHoveredEntity(hoveredEntity: Option[EntityId]): Callback =
      $.props >>= { p =>
        if (p.proxy.value.entitySelectionModel.hovered != hoveredEntity) {
          p.proxy.dispatchCB(UpdateEntityHoveringAction(hoveredEntity))
        } else {
          Callback.empty
        }
      }

    def setHoveredSubCriteria(hoveredSubCriteria: Option[SubCriteriaId]): Callback =
      $.props >>= { p =>
        val dispatchAction = p.proxy.dispatchCB(UpdateSubCriteriaHoveringAction(hoveredSubCriteria))
        val width = p.proxy.value.qualityModel.layout.width
        dispatchAction >> alignSubCriteriaLabel(width)
      }

    def alignSubCriteriaLabel(svgWidth: Int): Callback =
      svgSubCriteriaLabelRef($).map { svgText =>
        Callback {
          val padding = ChartLayout.SubCriteriaLabelPadding
          val textWidth = svgText.getBBox().width
          val halfTextWidth = textWidth / 2d
          val currentMiddleX = svgText.getAttribute("x").toDouble

          val leftCutoff = halfTextWidth + padding
          if (currentMiddleX < leftCutoff) {
            svgText.setAttribute("x", s"$leftCutoff")
          }

          val rightCutoff = svgWidth - padding - halfTextWidth
          if (currentMiddleX > rightCutoff) {
            svgText.setAttribute("x", s"$rightCutoff")
          }
        }
      }.getOrElse(Callback.empty)

    def toggleEntityPinning(entity: EntityId): Callback =
      $.props >>= { p =>
        val isPinned = p.proxy.value.entitySelectionModel.pinned.contains(entity)
        val pinnedOrNone = if (isPinned) None else Some(entity)
        p.proxy.dispatchCB(UpdateEntityPinningAction(pinnedOrNone))
      }

  }

}
