package ch.fhnw.ima.saav.component

import ch.fhnw.ima.saav.controller.UpdateChartWidthAction
import ch.fhnw.ima.saav.model.app.{AppModel, GroupedEntity}
import ch.fhnw.ima.saav.model.domain.EntityId
import ch.fhnw.ima.saav.model.layout.{ProfileChartLayout, QualityChartLayout}
import diode.react.ModelProxy
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB, Ref}
import org.scalajs.dom
import org.scalajs.dom.raw.{HTMLElement, SVGSVGElement, SVGTextElement}

object ProfileChartComponent {

  case class Props(proxy: ModelProxy[AppModel])

  case class State(hoveredEntity: Option[EntityId] = None)

  private val svgRef = Ref[SVGSVGElement]("svgRef")
  private val svgRootRef = Ref[SVGSVGElement]("svgRootRef")
  private val svgSubCriteriaLabelRef = Ref[SVGTextElement]("svgSubCriteriaLabelRef")

  class Backend($: BackendScope[Props, State]) {

    def onWindowResize(proxy: ModelProxy[AppModel]) = {
      svgRootRef($).map { svg =>
        val parent = svg.parentNode.asInstanceOf[HTMLElement]
        val width = parent.clientWidth
        proxy.dispatch(UpdateChartWidthAction(width))
      }.getOrElse(Callback.empty)
    }

    /**
      * The render loop.
      *
      * @param p global properties
      * @param s local state
      * @return the virtual DOM to be rendered.
      */
    def render(p: Props, s: State) = {

      dom.window.onresize = (_: dom.Event) => {
        onWindowResize(p.proxy).runNow()
      }

      val model = p.proxy.value

      val background = <.svg.rect(
        ^.svg.fill := "white",
        ^.svg.stroke := "black",
        ^.svg.x := "0", ^.svg.y := "0",
        ^.svg.width := "100%", ^.svg.height := "100%")

      val coordinateSystem = constructCoordinateSystem(model)
      val entities = if (model.profileModel.criteria.isEmpty) Seq.empty else constructEntities(model, s.hoveredEntity)

      // Assemble everything

      val layout = p.proxy.value.qualityModel.layout

      <.svg.svg(
        ^.ref := svgRootRef,
        ^.svg.viewBox := s"0 0 ${layout.width} ${QualityChartLayout.height}",
        ^.svg.width := "100%",
        ^.svg.height := s"${QualityChartLayout.height}px",
/*
        ^.onClick ==> onSvgMouseEvent(p.proxy, isClicked = true),
        ^.onMouseMove ==> onSvgMouseEvent(p.proxy, isClicked = false),
        ^.onMouseLeave --> clearHovering,
*/

        background,
        coordinateSystem,
        entities
      )
    }



    /**
      * Constructs the boxes and the axes for the hierarchical parallel coordinate system.
      *
      * @param appModel the application model
      * @return a group of SVG elements that make up the coordinate system
      */
    private def constructCoordinateSystem(appModel: AppModel) = {

      val model = appModel.profileModel
      val layout = model.layout
      val hoveredSubCriteria = appModel.subCriteriaSelectionModel.hovered

      // create the criteria boxes

      val criteriaBoxesAndStuff = for (criteria <- model.criteria) yield {

        val containsHoveredSubCriteria = criteria.subCriteria.count(sc => hoveredSubCriteria.contains(sc.id)) > 0

        val boxStroke = if (containsHoveredSubCriteria) "#999999" else "#eeeeee"
        val (x, width) = layout.getCriteriaBox(criteria)
        val box = <.svg.rect(
          ^.svg.fill := "#eeeeee",
          ^.svg.stroke := boxStroke,
          ^.svg.x := x, ^.svg.y := layout.boxTopY,
          ^.svg.width := width, ^.svg.height := (layout.boxBotY - layout.boxTopY))

        val stroke = if (containsHoveredSubCriteria) "black" else "#cccccc"

        val label =
          <.svg.foreignObject(
            ^.svg.x := x,
            ^.svg.y := layout.boxTopY - layout.padding,
            ^.svg.width := width, ^.svg.height := layout.padding,
            <.div(
              ^.textAlign.center,
              ^.overflow.hidden, ^.textOverflow.ellipsis, ^.whiteSpace.nowrap,
              ^.width := s"${width}px", ^.height := s"${layout.padding}px", ^.minWidth := "0",
              ^.title := criteria.displayName,
              criteria.displayName
            )
          )

        <.svg.g(box, label)
      }


//      <.svg.g(criteriaBoxesAndStuff, criteriaAxes, subCriteriaAxes, hoveredAxisLabel)
      <.svg.g(criteriaBoxesAndStuff)
    }



    /**
      * Constructs the horizontal lines for the entities.
      *
      * @param appModel the application model
      * @return a group of SVG elements containing the representation of the entities
      */
    private def constructEntities(appModel: AppModel, hoveredEntity: Option[EntityId]) = {

      val model = appModel.profileModel
      val layout = model.layout
      val hoveredSubCriteria = appModel.subCriteriaSelectionModel.hovered

      def isVisible(e: GroupedEntity) = appModel.entitySelectionModel.visible.contains(e.id)
      def isPinned(e: GroupedEntity) = appModel.entitySelectionModel.pinned.contains(e.id)
      def isHovered(e: GroupedEntity) = hoveredEntity.contains(e.id)

      val maxRadius = layout.getMaxRadius(model.sortedEntities.size)

      var entityIndex = 0
      val entities = for (groupedEntity <- model.sortedEntities) yield {

        val (strokeColor, strokeWidth) =
          if (isVisible(groupedEntity))
            if (isPinned(groupedEntity))
              ("black", 4)
            else if (isHovered(groupedEntity))
              (appModel.colorMap(groupedEntity.id).hexValue, 4)
            else
              (appModel.colorMap(groupedEntity.id).hexValue, 2)
          else
            ("#cccccc", 1)

        // Create the circles

        val entityLine = for (criteria <- model.criteria) yield {
          val x = layout.getCriteriaCenterX(criteria)
          val y = layout.getEntityCenterY(entityIndex, model.sortedEntities.size)
          val criteriaCircle = <.svg.circle(
            ^.svg.cx := x, ^.svg.cy := y, ^.svg.r := computeRadius(criteria.groupedValues.get(groupedEntity.id), layout, maxRadius),
            ^.svg.fill := strokeColor,
            ^.svg.strokeWidth := 0
          )

          val subCriteriaCircles = for (subCriteria <- criteria.subCriteria) yield {
            val x = layout.getSubCriteriaCenterX(subCriteria)
            <.svg.circle(
              ^.svg.cx := x, ^.svg.cy := y, ^.svg.r := computeRadius(subCriteria.groupedValues.get(groupedEntity.id), layout, maxRadius),
              ^.svg.fill := "none",
              ^.svg.stroke := strokeColor,
              ^.svg.strokeWidth := strokeWidth
            )
          }

          <.svg.g(criteriaCircle, subCriteriaCircles)
        }

        entityIndex += 1
        entityLine
      }

      entities
    }

    private def computeRadius(value: Option[Double], layout: ProfileChartLayout, maxRadius: Double): Double = {
      value.map { v =>
        v / (layout.maxValue - layout.minValue) * maxRadius
      }.getOrElse(0) // TODO: Handle missing values (Nan will blow up SVG)
    }
  }

  private val component = ReactComponentB[Props](ProfileChartComponent.getClass.getSimpleName)
    .initialState(State())
    .renderBackend[Backend]
    .build

  def apply(proxy: ModelProxy[AppModel]) = component(Props(proxy))

}