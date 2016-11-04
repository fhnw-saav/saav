package ch.fhnw.ima.saav.component

import ch.fhnw.ima.saav.model.app.{AppModel, EntitySelectionModel, GroupedEntity, ProfileModel}
import ch.fhnw.ima.saav.model.domain.{EntityId, SubCriteriaId}
import ch.fhnw.ima.saav.model.layout.{ChartLayout, ProfileChartLayout}
import diode.react.ModelProxy
import japgolly.scalajs.react.vdom.ReactTagOf
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB, ReactComponentU, ReactMouseEvent, TopNode}
import org.scalajs.dom
import org.scalajs.dom.raw.SVGPoint
import org.scalajs.dom.svg.SVG

import scala.collection.mutable.ArrayBuffer

object ProfileChartComponent extends ChartComponent {

  class Backend($: BackendScope[Props, Unit]) extends ChartBackend($) {

    /**
      * Handle mouse events.
      */
    private def onSvgMouseEvent(isClicked: Boolean)(e: ReactMouseEvent) =
      $.props >>= { p =>
        svgRootRef($).map { svg => {

          val pt = svg.createSVGPoint()
          pt.x = e.clientX
          pt.y = e.clientY

          val cursorPt = pt.matrixTransform(svg.getScreenCTM().inverse())

          val model = p.proxy.value
          val hoveredSubCriteria = findClosestSubCriteria(model.profileModel, cursorPt)
          val hoveredEntity = findClosestEntity(model.profileModel, model.entitySelectionModel, cursorPt)

          val togglePinningIfClicked = hoveredEntity match {
            case Some(entity) if isClicked => toggleEntityPinning(entity)
            case _ => Callback.empty
          }

          setHoveredSubCriteria(hoveredSubCriteria) >> setHoveredEntity(hoveredEntity) >> togglePinningIfClicked
        }
        }.toOption.getOrElse(Callback.empty)
      }

    private def findClosestSubCriteria(model: ProfileModel, cursorPt: SVGPoint): Option[SubCriteriaId] = {
      val layout = model.layout
      if ((cursorPt.y > layout.boxTopY) && (cursorPt.y < layout.boxBotY)) {
        var xmin = Double.MaxValue
        var closestSubCriteria: Option[SubCriteriaId] = None
        for (criteria <- model.criteria) {
          for (subCriteria <- criteria.subCriteria) {
            val x = layout.getSubCriteriaCenterX(subCriteria)
            val distance = Math.abs(x.get - cursorPt.x)
            if (distance < xmin) {
              xmin = distance
              closestSubCriteria = Some(subCriteria.id)
            }
          }
        }
        closestSubCriteria
      } else {
        None
      }

    }

    private def findClosestEntity(model: ProfileModel, selectionModel: EntitySelectionModel, cursorPt: SVGPoint): Option[EntityId] = {
      val layout = model.layout
      val entities = model.sortedEntities

      // if vertically inside box
      if ((cursorPt.y > layout.boxTopY) && (cursorPt.y < layout.boxBotY)) {
        val entityIndex = layout.getEntityIndex(cursorPt.y, entities.size)
        // if entity id valid
        if ((entityIndex >= 0) && (entityIndex < entities.size)) {
          val entityId = entities(entityIndex).id
          // if entity visible (not switched off)
          if (selectionModel.visible.contains(entityId)) {
            Option(entities(entityIndex).id)
          } else
            None
        } else
          None
      } else
        None
    }

    /**
      * The render loop.
      *
      * @param p global properties
      * @return the virtual DOM to be rendered.
      */
    def render(p: Props): ReactTagOf[SVG] = {

      dom.window.onresize = (_: dom.Event) => {
        onWindowResize().runNow()
      }

      val model = p.proxy.value

      val background = <.svg.rect(
        ^.svg.fill := "white",
        ^.svg.stroke := "#eeeeee",
        ^.svg.strokeWidth := 4,
        ^.svg.x := "0", ^.svg.y := "0",
        ^.svg.width := p.proxy.value.profileModel.layout.width, ^.svg.height := "100%")

      val coordinateSystem = constructCoordinateSystem(model)
      val entities = if (model.profileModel.criteria.isEmpty) Seq.empty else constructEntities(model)

      // Assemble everything

      val layout = p.proxy.value.qualityModel.layout

      <.svg.svg(
        ^.ref := svgRootRef,
        ^.svg.viewBox := s"0 0 ${layout.width} ${ChartLayout.Height}",
        ^.svg.width := "100%",
        ^.svg.height := s"${ChartLayout.Height}px",
        ^.svg.preserveAspectRatio := "none",
        ^.onClick ==> onSvgMouseEvent(isClicked = true),
        ^.onMouseMove ==> onSvgMouseEvent(isClicked = false),
        ^.onMouseLeave --> clearHovering,
        background,
        coordinateSystem,
        entities
      )
    }


    /**
      * Constructs the boxes that contain the entity circles.
      *
      * @param appModel the application model
      * @return a group of SVG elements that make up the boxes and stuff
      */
    private def constructCoordinateSystem(appModel: AppModel) = {

      val model = appModel.profileModel
      val layout = model.layout
      val hoveredSubCriteria = appModel.subCriteriaSelectionModel.hovered

      // Loop over all the criteria...

      val criteriaBoxesAndStuff = for (criterion <- model.criteria) yield {

        val containsHoveredSubCriteria = criterion.subCriteria.count(sc => hoveredSubCriteria.contains(sc.id)) > 0

        // First, we create a container to collect all the SVG elements as we go along
        val elements = ArrayBuffer[TagMod]()

        // The box
        val boxStroke = if (containsHoveredSubCriteria) "#999999" else "#eeeeee"
        val (boxX, boxWidth) = layout.getCriteriaBox(criterion)
        val box = <.svg.rect(
          ^.svg.fill := "#eeeeee",
          ^.svg.stroke := boxStroke,
          ^.svg.x := boxX, ^.svg.y := layout.boxTopY,
          ^.svg.width := boxWidth, ^.svg.height := (layout.boxBotY - layout.boxTopY))
        elements += box

        // The line that separates the aggregated criteria column from the subcriteria columns
        if (layout.getCriteriaCenterX(criterion).isDefined) {
          val separatorLineX = boxX + (boxWidth / (criterion.subCriteria.size + 1))
          val separatorLine = <.svg.line(
            ^.svg.x1 := separatorLineX, ^.svg.y1 := layout.boxTopY,
            ^.svg.x2 := separatorLineX, ^.svg.y2 := layout.boxBotY,
            ^.svg.stroke := "#ffffff", ^.svg.strokeWidth := "3"
          )
          elements += separatorLine
        }

        // The box for the probed column/subcriteria
        if (containsHoveredSubCriteria) {
          if (hoveredSubCriteria.isDefined) {

            // have to do this conversion because the selection models work with ids, and the painting works with the objects
            val hovc = criterion.subCriteria.filter(sc => hoveredSubCriteria.get == sc.id).head

            val x = layout.getSubCriteriaCenterX(hovc).get
            val columnWidth = boxWidth / (criterion.subCriteria.size + 1)
            val hoveredBox = <.svg.rect(
              ^.svg.fill := "#dddddd",
              ^.svg.stroke := boxStroke,
              ^.svg.x := (x - (columnWidth / 2)), ^.svg.y := layout.boxTopY,
              ^.svg.width := columnWidth, ^.svg.height := (layout.boxBotY - layout.boxTopY))
            elements += hoveredBox

            val hoveredLabel =
              <.svg.text(
                ^.ref := svgSubCriteriaLabelRef,
                ^.svg.textAnchor := "middle",
                ^.svg.x := x,
                ^.svg.y := layout.boxTopY - ChartLayout.SubCriteriaLabelPadding, hovc.displayName
              )
            elements += hoveredLabel

          }
        }

        // The criteria label
        val label =
          <.svg.foreignObject(
            ^.svg.x := boxX,
            ^.svg.y := ChartLayout.SubCriteriaLabelPadding,
            ^.svg.width := boxWidth, ^.svg.height := layout.padding,
            <.div(
              ^.textAlign.center,
              ^.overflow.hidden, ^.textOverflow.ellipsis, ^.whiteSpace.nowrap,
              ^.width := s"${boxWidth}px", ^.height := s"${layout.padding}px", ^.minWidth := "0",
              ^.title := criterion.displayName,
              criterion.displayName
            )
          )
        elements += label

        // Finally, we return the collected elements
        <.svg.g(elements)
      }

      <.svg.g(criteriaBoxesAndStuff)
    }

    /**
      * Constructs the circles for the entities.
      *
      * @param appModel the application model
      * @return a group of SVG elements containing the representation of the entities
      */
    private def constructEntities(appModel: AppModel) = {

      val model = appModel.profileModel
      val layout = model.layout

      def isVisible(e: GroupedEntity) = appModel.entitySelectionModel.visible.contains(e.id)

      def isPinned(e: GroupedEntity) = appModel.entitySelectionModel.pinned.contains(e.id)

      def isHovered(e: GroupedEntity) = appModel.entitySelectionModel.hovered.contains(e.id)

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

        val y = layout.getEntityCenterY(entityIndex, model.sortedEntities.size)

        val entityLine = for (criteria <- model.criteria) yield {
          // Create aggregated criteria circle, if it is defined
          val x = layout.getCriteriaCenterX(criteria)
          val criteriaCircle = if (x.isDefined) {
            val circle = <.svg.circle(
              ^.svg.cx := x.get, ^.svg.cy := y, ^.svg.r := computeRadius(criteria.groupedValues.get(groupedEntity.id), layout, maxRadius),
              ^.svg.fill := strokeColor,
              ^.svg.strokeWidth := 0
            )
            Option(circle)
          } else {
            Option.empty
          }

          // Create subcriteria cricles
          val subCriteriaCircles = for (subCriteria <- criteria.subCriteria) yield {
            val x = layout.getSubCriteriaCenterX(subCriteria)
            <.svg.circle(
              ^.svg.cx := x, ^.svg.cy := y, ^.svg.r := computeRadius(subCriteria.groupedValues.get(groupedEntity.id), layout, maxRadius),
              ^.svg.fill := "none",
              ^.svg.stroke := strokeColor,
              ^.svg.strokeWidth := strokeWidth
            )
          }

          // Combine the circles
          if (criteriaCircle.isDefined) {
            <.svg.g(criteriaCircle.get, subCriteriaCircles)
          } else {
            <.svg.g(subCriteriaCircles)
          }
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
    .renderBackend[Backend]
    .componentDidMount { $ =>
      $.backend.onWindowResize()
    }
    .build

  def apply(proxy: ModelProxy[AppModel]): ReactComponentU[Props, Unit, Backend, TopNode] = component(Props(proxy))

}