package ch.fhnw.ima.saav.component

import ch.fhnw.ima.saav.controller.SaavController.UpdateEntityPinningAction
import ch.fhnw.ima.saav.model.app.{DataModel, GroupedEntity, GroupedSubCriteria}
import ch.fhnw.ima.saav.model.domain.{Entity, SubCriteria}
import diode.react.ModelProxy
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB, ReactMouseEvent, Ref}
import org.scalajs.dom.raw.SVGSVGElement

object SvgPlotComponent {

  case class Props(proxy: ModelProxy[DataModel])

  case class State(hoveredSubCriteria: Option[SubCriteria] = None, hoveredEntity: Option[Entity] = None)

  val svgRef = Ref[SVGSVGElement]("svgRef")

  class Backend($: BackendScope[Props, State]) {

    def setHoveredSubCriteria(hoveredSubCriteria: Option[SubCriteria]) =
      $.state >>= { s =>
        if (s.hoveredSubCriteria != hoveredSubCriteria) {
          $.setState(s.copy(hoveredSubCriteria = hoveredSubCriteria))
        } else {
          Callback.empty
        }
      }

    def clearHoveredSubCriteria() =
      $.state >>= { s =>
        if (s.hoveredSubCriteria.isDefined) {
          $.setState(s.copy(hoveredSubCriteria = None))
        } else {
          Callback.empty
        }
      }


    // TODO: optimization, only fire the event if there is actually a change
    def setHoveredThings(hoveredSubCriteria: Option[SubCriteria], hoveredEntity: Option[Entity]) =
      $.modState(s => s.copy(hoveredSubCriteria = hoveredSubCriteria, hoveredEntity = hoveredEntity))



    def toggleEntityPinning(groupedEntity: GroupedEntity) =
      $.props >>= { p =>
        val isPinned = p.proxy.value.selectionModel.pinned.contains(groupedEntity.id)
        val pinnedOrNone = if (isPinned) None else Some(groupedEntity.id)
        p.proxy.dispatch(UpdateEntityPinningAction(pinnedOrNone))
      }


    /**
      * The global mouse handler.
      */
    def onSvgMouseEvent(proxy: ModelProxy[DataModel], layout: QualityLayout, isClicked: Boolean)(e: ReactMouseEvent) =
    svgRef($).map { svg =>

      val pt = svg.createSVGPoint()
      pt.x = e.clientX
      pt.y = e.clientY

      val cursorPt = pt.matrixTransform(svg.getScreenCTM().inverse())

/*
        val clicked = if (isClicked) ", Clicked" else ""
        println(s"SVG: ${cursorPt.x}/${cursorPt.y}$clicked")
*/

        //----------------
/*
        val model = proxy.value
        if ((cursorPt.y > layout.getBoxTopY) && (cursorPt.y < layout.getBoxBotY)) {
          var xmin = Double.MaxValue
          var closestSubCriteria: GroupedSubCriteria = null
          for (criteria <- model.criteria) {
            for (subCriteria <- criteria.subCriteria) {
              val x = layout.getSubCriteriaAxisX(subCriteria)
              val distance = Math.abs(x - cursorPt.x)
              if (distance < xmin) {
                xmin = distance
                closestSubCriteria = subCriteria
              }
            }
          }

          Callback.lazily(setHoveredSubCriteria(Some(closestSubCriteria.id)))
        } else {
          Callback.empty
        }
*/
        //----------------

        val model = proxy.value

        val probedSubCriteria = {
          if ((cursorPt.y > layout.getBoxTopY) && (cursorPt.y < layout.getBoxBotY)) {
            var xmin = Double.MaxValue
            var closestSubCriteria: GroupedSubCriteria = null
            for (criteria <- model.criteria) {
              for (subCriteria <- criteria.subCriteria) {
                val x = layout.getSubCriteriaAxisX(subCriteria)
                val distance = Math.abs(x - cursorPt.x)
                if (distance < xmin) {
                  xmin = distance
                  closestSubCriteria = subCriteria
                }
              }
            }
            Some(closestSubCriteria.id)
          } else {
            None
          }
        }

        val probedEntity = {
          if ((cursorPt.y > layout.getCriteriaAxisTopY) && (cursorPt.y < layout.getCriteriaAxisBotY) ||
            (cursorPt.y > layout.getSubCriteriaAxisTopY) && (cursorPt.y < layout.getSubCriteriaAxisBotY)) {

            if ((cursorPt.y > layout.getCriteriaAxisTopY) && (cursorPt.y < layout.getCriteriaAxisBotY)) {

              var minDistance = Double.MaxValue
              var closestEntity: Option[Entity] = None

              for (i <- 1 until model.criteria.size) {
                val x1 = layout.getCriteriaAxisX(model.criteria(i-1))
                val x2 = layout.getCriteriaAxisX(model.criteria(i))
                if ((cursorPt.x > x1) && (cursorPt.x < x2)) {
                  for (entity <- model.selectionModel.selected) {
                    val val1 = computeAxisValue(model.criteria(i-1).groupedValues(entity).get, layout, AxisType.Criteria)
                    val val2 = computeAxisValue(model.criteria(i).groupedValues(entity).get, layout, AxisType.Criteria)
                    val y = layout.getCriteriaAxisBotY - cursorPt.y

                    val interpolatedValue = val1 + ((cursorPt.x- x1) / (x2 - x1) * (val2 - val1))
                    val distance = Math.abs(interpolatedValue - y)

                    if (distance < minDistance) {
                      minDistance = distance
                      closestEntity = Some(entity)
                    }
                  }
                }
              }

              closestEntity
            } else if ((cursorPt.y > layout.getSubCriteriaAxisTopY) && (cursorPt.y < layout.getSubCriteriaAxisBotY)) {
/*
              var r = Random
              var index = r.nextInt(model.rankedEntities.size)
              Some(model.rankedEntities(index).id)
*/
              None
            } else {
              None
            }

          } else {
            None
          }
        }


        if (probedSubCriteria.isDefined || probedEntity.isDefined) {
          Callback.lazily(setHoveredThings(probedSubCriteria, probedEntity))
        } else {
          Callback.empty
        }
        //----------------

        // TODO: Replace with actually desired effect
//        Callback.empty

        // (1) Change the global model (e.g. pinning or selection)
        //     --> dispatch an action to the controller via proxy.dispatch
        //     Details: http://ochrons.github.io/diode/UsageWithReact.html
        //
        // AND/OR
        //
        // (2) Change state local to component (e.g. hovering state)
        //     --> modify state via $.modState
        //     Details: https://github.com/japgolly/scalajs-react/blob/master/doc/USAGE.md#callbacks

    }.getOrElse(Callback.empty)


    /**
      * The render loop.
      *
      * @param p global properties
      * @param s local state
      * @return the virtual DOM to be rendered.
      */
    def render(p: Props, s: State) = {

      println("render")

      val model = p.proxy.value

      // only relevant for aspect ratio and e.g. relative stroke width (svg will be scaled within parent element)
      val plotWidth = 1000
      val plotHeight = 500

      val background = <.svg.rect(
        ^.svg.fill := "white",
        ^.svg.stroke := "black",
        ^.svg.x := "0", ^.svg.y := "0",
        ^.svg.width := "100%", ^.svg.height := "100%")

      val layout = new QualityLayout(model, plotWidth, plotHeight)

      val coordinateSystem = constructCoordinateSystem(model, layout, s.hoveredSubCriteria)
      val entities = constructEntities(model, layout, s.hoveredSubCriteria, s.hoveredEntity)

      // Assemble everything

      <.svg.svg(
        ^.ref := svgRef,
        ^.svg.viewBox := s"0 0 $plotWidth $plotHeight",
        ^.onClick ==> onSvgMouseEvent(p.proxy, layout, isClicked = true),
        ^.onMouseMove ==> onSvgMouseEvent(p.proxy, layout, isClicked = false),
        background,
        coordinateSystem,
        entities
      )

    }

    /**
      * Constructs the boxes and the axes for the hierarchical parallel coordinate system.
      *
      * @param model the data model
      * @param hoveredSubCriteria the sub criteria that is currently probed
      * @param layout the layout parameters for the components
      * @return a group of SVG elements that make up the coordinate system
      */
    private def constructCoordinateSystem(model: DataModel, layout: QualityLayout, hoveredSubCriteria: Option[SubCriteria]) = {

      // create the criteria boxes

      val criteriaBoxesAndStuff = for (criteria <- model.criteria) yield {

        val containsHoveredSubCriteria = criteria.subCriteria.count(sc => hoveredSubCriteria.contains(sc.id)) > 0

        val boxStroke = if (containsHoveredSubCriteria) "#999999" else "#eeeeee"
        val (x, width) = layout.getCriteriaBox(criteria)
        val box = <.svg.rect(
          ^.svg.fill := "#eeeeee",
          ^.svg.stroke := boxStroke,
          ^.svg.x := x, ^.svg.y := layout.getBoxTopY,
          ^.svg.width := width, ^.svg.height := (layout.getBoxBotY - layout.getBoxTopY))

        val stroke = if (containsHoveredSubCriteria) "black" else "#cccccc"

        val axis = <.svg.line(
          ^.svg.x1 := layout.getCriteriaAxisX(criteria), ^.svg.y1 := layout.getCriteriaAxisTopY,
          ^.svg.x2 := layout.getCriteriaAxisX(criteria), ^.svg.y2 := layout.getCriteriaAxisBotY,
          ^.svg.stroke := stroke, ^.svg.strokeWidth := "1"
        )

        val label =
          <.svg.text(
            ^.svg.textAnchor := "middle",
            ^.svg.x := layout.getCriteriaAxisX(criteria),
            ^.svg.y := layout.getBoxTopY - layout.PADDING, criteria.name
          )

        <.svg.g(box, axis, label)
      }

      // create the criteria axes

      val criteriaAxes = for (criteria <- model.criteria) yield {
        val containsHoveredSubCriteria = criteria.subCriteria.count(sc => hoveredSubCriteria.contains(sc.id)) > 0
        val stroke = if (containsHoveredSubCriteria) "black" else "#cccccc"
        <.svg.line(
          ^.svg.x1 := layout.getCriteriaAxisX(criteria), ^.svg.y1 := layout.getCriteriaAxisTopY,
          ^.svg.x2 := layout.getCriteriaAxisX(criteria), ^.svg.y2 := layout.getCriteriaAxisBotY,
          ^.svg.stroke := stroke, ^.svg.strokeWidth := "1"
        )
      }


      // create the subcriteria axes

      // Have to do it like this, because we somehow can not get from domain to app space
      var hoveredAxisX = 0
      var hoveredAxisName = ""

      val subCriteriaAxes = for (criteria <- model.criteria) yield {
        val axes = for (subCriteria <- criteria.subCriteria) yield {

          if (hoveredSubCriteria.contains(subCriteria.id)) {
            hoveredAxisX = layout.getSubCriteriaAxisX(subCriteria)
            hoveredAxisName = hoveredSubCriteria.get.name
          }

          val stroke = if (hoveredSubCriteria.contains(subCriteria.id)) "black" else "#cccccc"
          <.svg.line(
            ^.svg.x1 := layout.getSubCriteriaAxisX(subCriteria), ^.svg.y1 := layout.getSubCriteriaAxisTopY,
            ^.svg.x2 := layout.getSubCriteriaAxisX(subCriteria), ^.svg.y2 := layout.getSubCriteriaAxisBotY,
            ^.svg.stroke := stroke, ^.svg.strokeWidth := "1",
            ^.cursor.pointer
          )
        }
        <.svg.g(axes)
      }

      // TODO: alignment, how do we get the bounding box of an SVG text element?
      val hoveredAxisLabel =
      <.svg.text(
        ^.svg.textAnchor := "middle",
        ^.svg.x := hoveredAxisX,
        ^.svg.y := layout.getSubCriteriaAxisTopY - layout.PADDING + 5, hoveredAxisName
      )

      <.svg.g(criteriaBoxesAndStuff, criteriaAxes, subCriteriaAxes, hoveredAxisLabel)
    }


    /**
      * Constructs the horizontal lines for the entities.
      *
      * @param model  the data model
      * @param layout the layout of the components
      * @return a group of SVG elements containing the representation of the entities
      */
    private def constructEntities(model: DataModel, layout: QualityLayout, hoveredSubCriteria: Option[SubCriteria], hoveredEntity: Option[Entity]) = {

      def isSelected(e: GroupedEntity) = model.selectionModel.selected.contains(e.id)
      def isPinned(e: GroupedEntity) = model.selectionModel.pinned.contains(e.id)
      def isHovered(e: GroupedEntity) = hoveredEntity.contains(e.id)

      val entitiesInPaintingOrder = model.rankedEntities.sortBy(e => (isPinned(e), isSelected(e)))
      val entities = for (groupedEntity <- entitiesInPaintingOrder) yield {

        val (strokeColor, strokeWidth, cursor) =
          if (isSelected(groupedEntity))
            if (isPinned(groupedEntity))
              ("black", 4, ^.cursor.pointer)
            else
            if (isHovered(groupedEntity))
              (model.colorMap(groupedEntity.id).hexValue, 4, ^.cursor.pointer)
            else
              (model.colorMap(groupedEntity.id).hexValue, 2, ^.cursor.pointer)
          else
            ("#cccccc", 1, ^.cursor.default)

        // create the criteria values lines

        var coordString = "M"
        var index = 0

        var valueCoordinates =
          for (criteria <- model.criteria) yield {
            val x = layout.getCriteriaAxisX(criteria)
            val value = computeAxisValue(criteria.groupedValues(groupedEntity.id).get, layout, AxisType.Criteria)
            val y = layout.getCriteriaAxisBotY - value

            if (index == 1) coordString += " L"
            coordString += " " + x + " " + y

            index += 1

            (x, y)
          }

        val criteriaValuesLine =
          <.svg.path(^.svg.d := coordString,
            ^.svg.stroke := strokeColor, ^.svg.strokeWidth := strokeWidth, ^.svg.fill := "none",
            ^.onClick --> toggleEntityPinning(groupedEntity),
            cursor
          )

        // create the subcriteria values lines

        coordString = "M"
        index = 0

        for (criteria <- model.criteria) {
          val valueSubCoordinates =
            for (subCriteria <- criteria.subCriteria) yield {
              val x = layout.getSubCriteriaAxisX(subCriteria)
              val value = computeAxisValue(subCriteria.groupedValues(groupedEntity.id).get, layout, AxisType.Subcriteria)
              val y = layout.getSubCriteriaAxisBotY - value

              if (index == 1) coordString += " L"
              coordString += " " + x + " " + y

              index += 1

              (x, y)
            }

          valueCoordinates = valueCoordinates ++ valueSubCoordinates
        }

        val subCriteriaValuesLine =
          <.svg.path(^.svg.d := coordString,
            ^.svg.stroke := strokeColor, ^.svg.strokeWidth := strokeWidth, ^.svg.fill := "none",
            ^.onClick --> toggleEntityPinning(groupedEntity),
            cursor
          )

        // Create the circles if entity is pinned

        val circles = if (isPinned(groupedEntity)) {
          val c = for ((x, y) <- valueCoordinates) yield {
            <.svg.circle(
              ^.svg.cx := x, ^.svg.cy := y, ^.svg.r := 5,
              ^.svg.fill := "black",
              ^.svg.strokeWidth := 0,
              ^.cursor.pointer
            )
          }
          <.svg.g(c)
        } else {
          <.svg.g
        }

        // Create the value labels if entity is pinned

        val valueLabels = if (isPinned(groupedEntity)) {

          var criteriaValueLabel = <.svg.text
          var subCriteriaValueLabel = <.svg.text

          for (criteria <- model.criteria) {
            val containsHoveredSubCriteria = criteria.subCriteria.count(sc => hoveredSubCriteria.contains(sc.id)) > 0
            if (containsHoveredSubCriteria) {
              criteriaValueLabel =
                <.svg.text(
                  ^.svg.textAnchor := "middle",
                  ^.svg.x := layout.getCriteriaAxisX(criteria),
                  ^.svg.y := layout.getCriteriaAxisBotY + layout.PADDING - 5,
                  criteria.groupedValues(groupedEntity.id).get
                )
            }

            for (subCriteria <- criteria.subCriteria) {
              if (hoveredSubCriteria.contains(subCriteria.id)) {
                subCriteriaValueLabel =
                  <.svg.text(
                    ^.svg.textAnchor := "middle",
                    ^.svg.x := layout.getSubCriteriaAxisX(subCriteria),
                    ^.svg.y := layout.getSubCriteriaAxisBotY + layout.PADDING - 5,
                    subCriteria.groupedValues(groupedEntity.id).get)
              }

            }
          }

          <.svg.g(criteriaValueLabel, subCriteriaValueLabel)
        } else {
          <.svg.g
        }

        // Assemble everything

        if (isPinned(groupedEntity)) {
          <.svg.g(criteriaValuesLine, subCriteriaValuesLine, circles, valueLabels)
        } else {
          <.svg.g(criteriaValuesLine, subCriteriaValuesLine)
        }

      }

      entities
    }

    object AxisType extends Enumeration {
      val Criteria, Subcriteria = Value
    }

    private def computeAxisValue(value: Double, layout: QualityLayout, axisType: AxisType.Value): Double = {
      val (topY, botY) = axisType match {
        case AxisType.Criteria => (layout.getCriteriaAxisTopY, layout.getCriteriaAxisBotY)
        case AxisType.Subcriteria => (layout.getSubCriteriaAxisTopY, layout.getSubCriteriaAxisBotY)
      }

      value / (layout.getMaxValue - layout.getMinValue) * (botY - topY)
    }

  }

  private val component = ReactComponentB[Props](SvgPlotComponent.getClass.getSimpleName)
    .initialState(State())
    .renderBackend[Backend]
    .build

  def apply(proxy: ModelProxy[DataModel]) = component(Props(proxy))

}