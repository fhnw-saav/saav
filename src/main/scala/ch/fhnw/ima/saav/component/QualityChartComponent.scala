package ch.fhnw.ima.saav.component

import ch.fhnw.ima.saav.controller.{UpdateChartWidthAction, UpdateEntityPinningAction, UpdateSubCriteriaHoveringAction}
import ch.fhnw.ima.saav.model.app._
import ch.fhnw.ima.saav.model.domain.{Entity, SubCriteria, SubCriteriaId}
import ch.fhnw.ima.saav.model.layout.QualityChartLayout
import diode.react.ModelProxy
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB, ReactMouseEvent, Ref}
import org.scalajs.dom
import org.scalajs.dom.raw._

// noinspection TypeAnnotation
object QualityChartComponent {

  case class Props(proxy: ModelProxy[AppModel])

  case class State(hoveredEntity: Option[Entity] = None)

  private val svgRootRef = Ref[SVGSVGElement]("svgRootRef")
  private val svgSubCriteriaLabelRef = Ref[SVGTextElement]("svgSubCriteriaLabelRef")

  class Backend($: BackendScope[Props, State]) {

    def setHoveredSubCriteria(hoveredSubCriteria: Option[SubCriteriaId]) =
      $.props >>= { p =>
        val dispatchAction = p.proxy.dispatch(UpdateSubCriteriaHoveringAction(hoveredSubCriteria))
        val width = p.proxy.value.qualityModel.layout.width
        dispatchAction >> alignSubCriteriaLabel(width)
      }

    def setHoveredEntity(hoveredEntity: Option[Entity]) =
      $.state >>= { s =>
        if (s.hoveredEntity != hoveredEntity) {
          $.setState(s.copy(hoveredEntity = hoveredEntity))
        } else {
          Callback.empty
        }
      }

    def toggleEntityPinning(entity: Entity) =
      $.props >>= { p =>
        val isPinned = p.proxy.value.entitySelectionModel.pinned.contains(entity)
        val pinnedOrNone = if (isPinned) None else Some(entity)
        p.proxy.dispatch(UpdateEntityPinningAction(pinnedOrNone))
      }

    /**
      * The global mouse handler.
      */
    def onSvgMouseEvent(proxy: ModelProxy[AppModel], isClicked: Boolean)(e: ReactMouseEvent) =
      svgRootRef($).map { svg =>

        val pt = svg.createSVGPoint()
        pt.x = e.clientX
        pt.y = e.clientY

        val cursorPt = pt.matrixTransform(svg.getScreenCTM().inverse())
        val model = proxy.value
        val hoveredSubCriteria = findClosestSubCriteria(model.qualityModel, cursorPt)
        val hoveredEntity = findClosestEntity(model.qualityModel, model.entitySelectionModel, cursorPt)
        val togglePinningIfClicked = hoveredEntity match {
          case Some(entity) if isClicked => toggleEntityPinning(entity)
          case _ => Callback.empty
        }

        setHoveredSubCriteria(hoveredSubCriteria) >> setHoveredEntity(hoveredEntity) >> togglePinningIfClicked

      }.getOrElse(Callback.empty)

    def onWindowResize(proxy: ModelProxy[AppModel]) = {
      svgRootRef($).map { svg =>
        val parent = svg.parentNode.asInstanceOf[HTMLElement]
        val width = parent.clientWidth
        proxy.dispatch(UpdateChartWidthAction(width))
      }.getOrElse(Callback.empty)
    }

    private def alignSubCriteriaLabel(svgWidth: Int) =
      svgSubCriteriaLabelRef($).map { svgText =>
        Callback {
          val padding = QualityChartLayout.subCriteriaLabelPadding
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

    private def findClosestSubCriteria(model: QualityModel, cursorPt: SVGPoint): Option[SubCriteriaId] = {
      val layout = model.layout
      if ((cursorPt.y > layout.boxTopY) && (cursorPt.y < layout.boxBotY)) {
        var xmin = Double.MaxValue
        var closestSubCriteria: Option[SubCriteriaId] = None
        for (criteria <- model.criteria) {
          for (subCriteria <- criteria.subCriteria) {
            val x = layout.getSubCriteriaAxisX(subCriteria)
            val distance = Math.abs(x - cursorPt.x)
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

    def clearHovering = setHoveredEntity(None) >> setHoveredSubCriteria(None)

    private def findClosestEntity(model: QualityModel, selectionModel: EntitySelectionModel, cursorPt: SVGPoint): Option[Entity] = {
      val layout = model.layout
      if ((cursorPt.y > layout.criteriaAxisTopY) && (cursorPt.y < layout.criteriaAxisBotY))
        findClosestEntityViaCriteria(model.criteria, layout, selectionModel, cursorPt)
      else if ((cursorPt.y > layout.subCriteriaAxisTopY) && (cursorPt.y < layout.subCriteriaAxisBotY))
        findClosestEntityViaSubCriteria(model.criteria.flatMap(_.subCriteria), layout, selectionModel, cursorPt)
      else
        None
    }

    private def findClosestEntityViaCriteria(criteria: Seq[GroupedCriteria], layout: QualityChartLayout, selectionModel: EntitySelectionModel, cursorPt: SVGPoint) = {

      var minDistance = Double.MaxValue
      var closestEntity: Option[Entity] = None

      for (i <- 1 until criteria.size) {
        val criteria1 = criteria(i - 1)
        val criteria2 = criteria(i)
        val x1 = layout.getCriteriaAxisX(criteria1)
        val x2 = layout.getCriteriaAxisX(criteria2)
        if ((cursorPt.x > x1) && (cursorPt.x < x2)) {
          for (entity <- selectionModel.visible) {
            val axisValue1 = computeAxisValue(criteria1.groupedValues(entity).get, layout, AxisType.Criteria)
            val axisValue2 = computeAxisValue(criteria2.groupedValues(entity).get, layout, AxisType.Criteria)
            val y = layout.criteriaAxisBotY - cursorPt.y

            val interpolatedValue = axisValue1 + ((cursorPt.x - x1) / (x2 - x1) * (axisValue2 - axisValue1))
            val distance = Math.abs(interpolatedValue - y)

            if (distance < minDistance) {
              minDistance = distance
              closestEntity = Some(entity)
            }
          }
        }
      }

      closestEntity
    }

    private def findClosestEntityViaSubCriteria(subCriteria: Seq[GroupedSubCriteria], layout: QualityChartLayout, selectionModel: EntitySelectionModel, cursorPt: SVGPoint) = {

      var minDistance = Double.MaxValue
      var closestEntity: Option[Entity] = None

      for (i <- 1 until subCriteria.size) {
        val subCriteria1 = subCriteria(i - 1)
        val subCriteria2 = subCriteria(i)
        val x1 = layout.getSubCriteriaAxisX(subCriteria1)
        val x2 = layout.getSubCriteriaAxisX(subCriteria2)
        if ((cursorPt.x > x1) && (cursorPt.x < x2)) {
          for (entity <- selectionModel.visible) {
            val axisValue1 = computeAxisValue(subCriteria1.groupedValues(entity).get, layout, AxisType.Subcriteria)
            val axisValue2 = computeAxisValue(subCriteria2.groupedValues(entity).get, layout, AxisType.Subcriteria)
            val y = layout.subCriteriaAxisBotY - cursorPt.y

            val interpolatedValue = axisValue1 + ((cursorPt.x - x1) / (x2 - x1) * (axisValue2 - axisValue1))
            val distance = Math.abs(interpolatedValue - y)

            if (distance < minDistance) {
              minDistance = distance
              closestEntity = Some(entity)
            }
          }
        }
      }

      closestEntity
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
      val entities = if (model.qualityModel.criteria.isEmpty) Seq.empty else constructEntities(model, s.hoveredEntity)

      // Assemble everything

      val layout = p.proxy.value.qualityModel.layout

      <.svg.svg(
        ^.ref := svgRootRef,
        ^.svg.viewBox := s"0 0 ${layout.width} ${QualityChartLayout.height}",
        ^.svg.width := "100%",
        ^.svg.height := s"${QualityChartLayout.height}px",
        ^.onClick ==> onSvgMouseEvent(p.proxy, isClicked = true),
        ^.onMouseMove ==> onSvgMouseEvent(p.proxy, isClicked = false),
        ^.onMouseLeave --> clearHovering,
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

      val model = appModel.qualityModel
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

        val axis = <.svg.line(
          ^.svg.x1 := layout.getCriteriaAxisX(criteria), ^.svg.y1 := layout.criteriaAxisTopY,
          ^.svg.x2 := layout.getCriteriaAxisX(criteria), ^.svg.y2 := layout.criteriaAxisBotY,
          ^.svg.stroke := stroke, ^.svg.strokeWidth := "1"
        )

        val label =
          <.svg.foreignObject(
            ^.svg.x := layout.getCriteriaAxisX(criteria) - (width / 2f),
            ^.svg.y := layout.boxTopY - layout.padding,
            ^.svg.width := width, ^.svg.height := layout.padding,
            <.div(
              ^.textAlign.center,
              ^.overflow.hidden, ^.textOverflow.ellipsis, ^.whiteSpace.nowrap,
              ^.width := s"${width}px", ^.height := s"${layout.padding}px", ^.minWidth := "0",
              ^.title := criteria.name,
              criteria.name
            )
          )

        <.svg.g(box, axis, label)
      }

      // create the criteria axes

      val criteriaAxes = for (criteria <- model.criteria) yield {
        val containsHoveredSubCriteria = criteria.subCriteria.count(sc => hoveredSubCriteria.contains(sc.id)) > 0
        val stroke = if (containsHoveredSubCriteria) "black" else "#cccccc"
        <.svg.line(
          ^.svg.x1 := layout.getCriteriaAxisX(criteria), ^.svg.y1 := layout.criteriaAxisTopY,
          ^.svg.x2 := layout.getCriteriaAxisX(criteria), ^.svg.y2 := layout.criteriaAxisBotY,
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
            hoveredAxisName = subCriteria.displayName
          }

          val stroke = if (hoveredSubCriteria.contains(subCriteria.id)) "black" else "#cccccc"
          <.svg.line(
            ^.svg.x1 := layout.getSubCriteriaAxisX(subCriteria), ^.svg.y1 := layout.subCriteriaAxisTopY,
            ^.svg.x2 := layout.getSubCriteriaAxisX(subCriteria), ^.svg.y2 := layout.subCriteriaAxisBotY,
            ^.svg.stroke := stroke, ^.svg.strokeWidth := "1"
          )
        }
        <.svg.g(axes)
      }

      val hoveredAxisLabel =
        <.svg.text(
          ^.ref := svgSubCriteriaLabelRef,
          ^.svg.textAnchor := "middle",
          ^.svg.x := hoveredAxisX,
          ^.svg.y := layout.subCriteriaAxisTopY - layout.padding + 5, hoveredAxisName
        )

      <.svg.g(criteriaBoxesAndStuff, criteriaAxes, subCriteriaAxes, hoveredAxisLabel)
    }


    /**
      * Constructs the horizontal lines for the entities.
      *
      * @param model the application model
      * @return a group of SVG elements containing the representation of the entities
      */
    private def constructEntities(model: AppModel, hoveredEntity: Option[Entity]) = {

      val layout = model.qualityModel.layout
      val hoveredSubCriteria = model.subCriteriaSelectionModel.hovered

      def isVisible(e: GroupedEntity) = model.entitySelectionModel.visible.contains(e.id)

      def isPinned(e: GroupedEntity) = model.entitySelectionModel.pinned.contains(e.id)

      def isHovered(e: GroupedEntity) = hoveredEntity.contains(e.id)

      val entitiesInPaintingOrder = model.qualityModel.rankedEntities.sortBy { e =>
        (isPinned(e), isVisible(e), e.sortingPosition) // higher ranks should be painted last (i.e. in front)
      }
      val entities = for (groupedEntity <- entitiesInPaintingOrder) yield {

        val (strokeColor, strokeWidth) =
          if (isVisible(groupedEntity))
            if (isPinned(groupedEntity))
              ("black", 4)
            else if (isHovered(groupedEntity))
              (model.colorMap(groupedEntity.id).hexValue, 4)
            else
              (model.colorMap(groupedEntity.id).hexValue, 2)
          else
            ("#cccccc", 1)

        // create the criteria values lines

        var coordString = "M"
        var index = 0

        var valueCoordinates =
          for (criteria <- model.qualityModel.criteria) yield {
            val x = layout.getCriteriaAxisX(criteria)
            val value = computeAxisValue(criteria.groupedValues(groupedEntity.id).get, layout, AxisType.Criteria)
            val y = layout.criteriaAxisBotY - value

            if (index == 1) coordString += " L"
            coordString += " " + x + " " + y

            index += 1

            (x, y)
          }

        val criteriaValuesLine =
          <.svg.path(^.svg.d := coordString,
            ^.svg.stroke := strokeColor, ^.svg.strokeWidth := strokeWidth, ^.svg.fill := "none",
            ^.onClick --> toggleEntityPinning(groupedEntity.id)
          )

        // create the subcriteria values lines

        coordString = "M"
        index = 0

        for (criteria <- model.qualityModel.criteria) {
          val valueSubCoordinates =
            for (subCriteria <- criteria.subCriteria) yield {
              val x = layout.getSubCriteriaAxisX(subCriteria)
              val value = computeAxisValue(subCriteria.groupedValues(groupedEntity.id).get, layout, AxisType.Subcriteria)
              val y = layout.subCriteriaAxisBotY - value

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
            ^.onClick --> toggleEntityPinning(groupedEntity.id)
          )

        // Create the circles if entity is pinned

        val circles = if (isPinned(groupedEntity)) {
          val c = for ((x, y) <- valueCoordinates) yield {
            <.svg.circle(
              ^.svg.cx := x, ^.svg.cy := y, ^.svg.r := 5,
              ^.svg.fill := "black",
              ^.svg.strokeWidth := 0
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

          for (criteria <- model.qualityModel.criteria) {
            val containsHoveredSubCriteria = criteria.subCriteria.count(sc => hoveredSubCriteria.contains(sc.id)) > 0
            if (containsHoveredSubCriteria) {
              criteriaValueLabel =
                <.svg.text(
                  ^.svg.textAnchor := "middle",
                  ^.svg.x := layout.getCriteriaAxisX(criteria),
                  ^.svg.y := layout.criteriaAxisBotY + layout.padding - 5,
                  criteria.groupedValues(groupedEntity.id).get
                )
            }

            for (subCriteria <- criteria.subCriteria) {
              if (hoveredSubCriteria.contains(subCriteria.id)) {
                subCriteriaValueLabel =
                  <.svg.text(
                    ^.svg.textAnchor := "middle",
                    ^.svg.x := layout.getSubCriteriaAxisX(subCriteria),
                    ^.svg.y := layout.subCriteriaAxisBotY + layout.padding - 5,
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

    private def computeAxisValue(value: Double, layout: QualityChartLayout, axisType: AxisType.Value): Double = {
      val (topY, botY) = axisType match {
        case AxisType.Criteria => (layout.criteriaAxisTopY, layout.criteriaAxisBotY)
        case AxisType.Subcriteria => (layout.subCriteriaAxisTopY, layout.subCriteriaAxisBotY)
      }

      value / (layout.maxValue - layout.minValue) * (botY - topY)
    }

  }

  private val component = ReactComponentB[Props](QualityChartComponent.getClass.getSimpleName)
    .initialState(State())
    .renderBackend[Backend]
    .componentDidMount { $ =>
      $.backend.onWindowResize($.props.proxy)
    }
    .build

  def apply(proxy: ModelProxy[AppModel]) = component(Props(proxy))

}