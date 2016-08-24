package ch.fhnw.ima.saav.component

import ch.fhnw.ima.saav.controller.SaavController.UpdateEntityPinningAction
import ch.fhnw.ima.saav.model.app.{DataModel, GroupedEntity}
import ch.fhnw.ima.saav.model.domain.SubCriteria
import diode.react.ModelProxy
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB, ReactMouseEvent}

object SvgPlotComponent {

  case class Props(proxy: ModelProxy[DataModel])

  case class State(hoveredSubCriteria: Option[SubCriteria] = None)

  class Backend($: BackendScope[Props, State]) {

    def setHoveredSubCriteria(hoveredSubCriteria: Option[SubCriteria]) =
      $.modState(s => State(hoveredSubCriteria))

    def clearHoveredSubCriteria() = $.modState(s => State(None))

    def toggleEntityPinning(groupedEntity: GroupedEntity) =
      $.props >>= { p =>
        val isPinned = p.proxy.value.selectionModel.pinned.contains(groupedEntity.id)
        val pinnedOrNone = if (isPinned) None else Some(groupedEntity.id)
        p.proxy.dispatch(UpdateEntityPinningAction(pinnedOrNone))
      }

    def onSvgMouseEvent(isClicked: Boolean)(e: ReactMouseEvent) = {

      val clicked = if (isClicked) ", Clicked" else ""
      println(s"SVG: ${e.clientX}/${e.clientY}$clicked")

      Callback.empty // TODO: Replace with actually desired effect

      // (1) Change the global model (e.g. pinning or selection)
      //     --> dispatch an action to the controller via proxy.dispatch
      //     Details: http://ochrons.github.io/diode/UsageWithReact.html
      //
      // AND/OR
      //
      // (2) Change state local to component (e.g. hovering state)
      //     --> modify state via $.modState
      //     Details: https://github.com/japgolly/scalajs-react/blob/master/doc/USAGE.md#callbacks

    }

    def render(p: Props, s: State) = {

      val model = p.proxy.value

      // only relevant for aspect ratio and e.g. relative stroke width (svg will be scaled within parent element)
      val plotWidth = 1000
      val plotHeight = 500

      val background = <.svg.rect(
        ^.svg.fill := "white",
        ^.svg.x := "0", ^.svg.y := "0",
        ^.svg.width := "100%", ^.svg.height := "100%")

      val layout = new QualityLayout(model, plotWidth, plotHeight)

      val coordinateSystem = constructCoordinateSystem(model, s.hoveredSubCriteria, plotHeight, layout)
      val entities = constructEntities(model, layout)

      // Assemble everything

      <.svg.svg(
        ^.svg.viewBox := s"0 0 $plotWidth $plotHeight",
        ^.onClick ==> onSvgMouseEvent(isClicked = true),
        ^.onMouseOver ==> onSvgMouseEvent(isClicked = false),
        ^.onMouseOut ==> onSvgMouseEvent(isClicked = false),
        background,
        coordinateSystem,
        entities
      )

    }


    private def constructCoordinateSystem(model: DataModel, hoveredSubCriteria: Option[SubCriteria], plotHeight: Int, layout: QualityLayout) = {

      // create the criteria boxes
      val boxes = for (criteria <- model.criteria) yield {
        val (x, width) = layout.getCriteriaBox(criteria)
        <.svg.rect(
          ^.svg.fill := "#eeeeee",
          ^.svg.x := x, ^.svg.y := layout.MARGIN,
          ^.svg.width := width, ^.svg.height := (plotHeight - 2 * layout.MARGIN))
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
      val subCriteriaAxes = for (criteria <- model.criteria) yield {
        val axes = for (subCriteria <- criteria.subCriteria) yield {
          val stroke = if (hoveredSubCriteria.contains(subCriteria.id)) "black" else "#cccccc"
          <.svg.line(
            ^.svg.x1 := layout.getSubCriteriaAxisX(subCriteria), ^.svg.y1 := layout.getSubCriteriaAxisTopY,
            ^.svg.x2 := layout.getSubCriteriaAxisX(subCriteria), ^.svg.y2 := layout.getSubCriteriaAxisBotY,
            ^.svg.stroke := stroke, ^.svg.strokeWidth := "1",
            ^.onMouseOver --> setHoveredSubCriteria(Some(subCriteria.id)),
            ^.onMouseOut --> clearHoveredSubCriteria(),
            ^.cursor.pointer
          )
        }
        <.svg.g(axes)
      }

      <.svg.g(boxes, criteriaAxes, subCriteriaAxes)
    }

    private def constructEntities(model: DataModel, layout: QualityLayout) = {

      def isSelected(e: GroupedEntity) = model.selectionModel.selected.contains(e.id)
      def isPinned(e: GroupedEntity) = model.selectionModel.pinned.contains(e.id)

      val entitiesInPaintingOrder = model.rankedEntities.sortBy(e => (isPinned(e), isSelected(e)))
      val entities = for (groupedEntity <- entitiesInPaintingOrder) yield {

        val (strokeColor, strokeWidth, cursor) =
          if (isSelected(groupedEntity))
            if (isPinned(groupedEntity))
              ("black", 4, ^.cursor.pointer)
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
            val value = criteria.groupedValues(groupedEntity.id).get * (layout.getCriteriaAxisBotY - layout.getCriteriaAxisTopY) / 100.0
            val y = layout.getCriteriaAxisBotY - value

            if (index == 1) coordString += " L"
            coordString += " " + x + " " + y

            index += 1

            (x, y)
          }

        val criteriaValuesLine = <.svg.path(^.svg.d := coordString,
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
              val value = subCriteria.groupedValues(groupedEntity.id).get * (layout.getSubCriteriaAxisBotY - layout.getSubCriteriaAxisTopY) / 100.0
              val y = layout.getSubCriteriaAxisBotY - value

              if (index == 1) coordString += " L"
              coordString += " " + x + " " + y

              index += 1

              (x, y)
            }

          valueCoordinates = valueCoordinates ++ valueSubCoordinates
        }

        val subCriteriaValuesLine = <.svg.path(^.svg.d := coordString,
          ^.svg.stroke := strokeColor, ^.svg.strokeWidth := strokeWidth, ^.svg.fill := "none",
          ^.onClick --> toggleEntityPinning(groupedEntity),
          cursor
        )

        // Create the circles if entity is pinned

        if (isPinned(groupedEntity)) {
          val circles = for ((x, y) <- valueCoordinates) yield {
            <.svg.circle(
              ^.svg.cx := x, ^.svg.cy := y, ^.svg.r := 5,
              ^.svg.fill := "black",
              ^.svg.strokeWidth := 0,
              ^.cursor.pointer
            )
          }
          <.svg.g(criteriaValuesLine, subCriteriaValuesLine, circles)
        } else {
          <.svg.g(criteriaValuesLine, subCriteriaValuesLine)
        }


      }

      entities
    }

  }

  private val component = ReactComponentB[Props](SvgPlotComponent.getClass.getSimpleName)
    .initialState(State())
    .renderBackend[Backend]
    .build

  def apply(proxy: ModelProxy[DataModel]) = component(Props(proxy))

}