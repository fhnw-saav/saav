package ch.fhnw.ima.saav.component

import ch.fhnw.ima.saav.model.app._
import ch.fhnw.ima.saav.model.domain.{EntityId, SubCriteriaId}
import ch.fhnw.ima.saav.model.layout.QualityChartLayout
import diode.react.ModelProxy
import japgolly.scalajs.react.vdom.ReactTagOf
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB, ReactComponentU, ReactMouseEvent, TopNode}
import org.scalajs.dom
import org.scalajs.dom.raw._
import org.scalajs.dom.svg.{G, SVG}

import scala.collection.immutable.IndexedSeq
import scala.collection.mutable.ListBuffer

object QualityChartComponent extends ChartComponent {

  class Backend($: BackendScope[Props, Unit]) extends ChartBackend($) {

    /**
      * The global mouse handler.
      */
    def onSvgMouseEvent(isClicked: Boolean)(e: ReactMouseEvent): Callback =
      $.props >>= { p =>
        svgRootRef($).map { svg => {
          val pt = svg.createSVGPoint()
          pt.x = e.clientX
          pt.y = e.clientY

          val cursorPt = pt.matrixTransform(svg.getScreenCTM().inverse())
          val model = p.proxy.value
          val hoveredSubCriteria = findClosestSubCriteria(model.qualityModel, cursorPt)
          val hoveredEntity = findClosestEntity(model.qualityModel, model.entitySelectionModel, cursorPt)
          val togglePinningIfClicked = hoveredEntity match {
            case Some(entity) if isClicked => toggleEntityPinning(entity)
            case _ => Callback.empty
          }

          setHoveredSubCriteria(hoveredSubCriteria) >> setHoveredEntity(hoveredEntity) >> togglePinningIfClicked
        }
        }.toOption.getOrElse(Callback.empty)
      }

    private def findClosestSubCriteria(model: QualityModel, cursorPt: SVGPoint): Option[SubCriteriaId] = {
      val layout = model.layout
      if ((cursorPt.y > QualityChartLayout.BoxTopY) && (cursorPt.y < QualityChartLayout.BoxBotY)) {
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

    private def findClosestEntity(model: QualityModel, selectionModel: EntitySelectionModel, cursorPt: SVGPoint): Option[EntityId] = {
      val layout = model.layout
      if ((cursorPt.y > QualityChartLayout.CriteriaAxisTopY) && (cursorPt.y < QualityChartLayout.CriteriaAxisBotY))
        findClosestEntityViaCriteria(model.criteria, layout, selectionModel, cursorPt)
      else if ((cursorPt.y > QualityChartLayout.SubCriteriaAxisTopY) && (cursorPt.y < QualityChartLayout.SubCriteriaAxisBotY))
        findClosestEntityViaSubCriteria(model.criteria.flatMap(_.subCriteria), layout, selectionModel, cursorPt)
      else
        None
    }

    private def findClosestEntityViaCriteria(criteria: Seq[GroupedCriteria], layout: QualityChartLayout, selectionModel: EntitySelectionModel, cursorPt: SVGPoint) = {

      var minDistance = Double.MaxValue
      var closestEntity: Option[EntityId] = None

      for (i <- 1 until criteria.size) {
        val criteria1 = criteria(i - 1)
        val criteria2 = criteria(i)
        val x1 = layout.getCriteriaAxisX(criteria1)
        val x2 = layout.getCriteriaAxisX(criteria2)
        if ((cursorPt.x > x1) && (cursorPt.x < x2)) {
          for (entity <- selectionModel.visible) {
            val axisValue1 = computeAxisValue(criteria1.groupedValues.get(entity), layout, AxisType.Criteria)
            val axisValue2 = computeAxisValue(criteria2.groupedValues.get(entity), layout, AxisType.Criteria)

            if (axisValue1.isDefined && axisValue2.isDefined) {
              val y = QualityChartLayout.CriteriaAxisBotY - cursorPt.y

              val interpolatedValue = axisValue1.get + ((cursorPt.x - x1) / (x2 - x1) * (axisValue2.get - axisValue1.get))
              val distance = Math.abs(interpolatedValue - y)

              if (distance < minDistance) {
                minDistance = distance
                closestEntity = Some(entity)
              }
            }
          }
        }
      }

      closestEntity
    }

    private def findClosestEntityViaSubCriteria(subCriteria: Seq[GroupedSubCriteria], layout: QualityChartLayout, selectionModel: EntitySelectionModel, cursorPt: SVGPoint) = {

      var minDistance = Double.MaxValue
      var closestEntity: Option[EntityId] = None

      for (i <- 1 until subCriteria.size) {
        val subCriteria1 = subCriteria(i - 1)
        val subCriteria2 = subCriteria(i)
        val x1 = layout.getSubCriteriaAxisX(subCriteria1)
        val x2 = layout.getSubCriteriaAxisX(subCriteria2)
        if ((cursorPt.x > x1) && (cursorPt.x < x2)) {
          for (entity <- selectionModel.visible) {
            val axisValue1 = computeAxisValue(subCriteria1.groupedValues.get(entity), layout, AxisType.Subcriteria)
            val axisValue2 = computeAxisValue(subCriteria2.groupedValues.get(entity), layout, AxisType.Subcriteria)
            if (axisValue1.isDefined && axisValue2.isDefined) {
              val y = QualityChartLayout.SubCriteriaAxisBotY - cursorPt.y

              val interpolatedValue = axisValue1.get + ((cursorPt.x - x1) / (x2 - x1) * (axisValue2.get - axisValue1.get))
              val distance = Math.abs(interpolatedValue - y)

              if (distance < minDistance) {
                minDistance = distance
                closestEntity = Some(entity)
              }
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
        ^.svg.width := p.proxy.value.qualityModel.layout.width, ^.svg.height := "100%")

      val coordinateSystem = constructCoordinateSystem(model)
      val entities = if (model.qualityModel.criteria.isEmpty) Seq.empty else constructEntities(model)

      // Assemble everything

      val layout = p.proxy.value.qualityModel.layout

      <.svg.svg(
        ^.ref := svgRootRef,
        ^.id := ChartComponent.ElementId,
        ^.svg.viewBox := s"0 0 ${layout.width} ${QualityChartLayout.Height}",
        ^.svg.width := "100%",
        ^.svg.height := s"${QualityChartLayout.Height}px",
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
          ^.svg.x := x, ^.svg.y := QualityChartLayout.BoxTopY,
          ^.svg.width := width, ^.svg.height := (QualityChartLayout.BoxBotY - QualityChartLayout.BoxTopY))

        val stroke = if (containsHoveredSubCriteria) "black" else "#cccccc"

        val axis = <.svg.line(
          ^.svg.x1 := layout.getCriteriaAxisX(criteria), ^.svg.y1 := QualityChartLayout.CriteriaAxisTopY,
          ^.svg.x2 := layout.getCriteriaAxisX(criteria), ^.svg.y2 := QualityChartLayout.CriteriaAxisBotY,
          ^.svg.stroke := stroke, ^.svg.strokeWidth := "1"
        )

        val label =
          <.svg.foreignObject(
            ^.svg.x := layout.getCriteriaAxisX(criteria) - (width / 2f),
            ^.svg.y := QualityChartLayout.Padding / 2,
            ^.svg.width := width, ^.svg.height := QualityChartLayout.Padding,
            <.div(
              ^.textAlign.center,
              ^.overflow.hidden, ^.textOverflow.ellipsis, ^.whiteSpace.nowrap,
              ^.width := s"${width}px", ^.height := s"${QualityChartLayout.Padding}px", ^.minWidth := "0",
              ^.title := criteria.displayName,
              criteria.displayName
            )
          )

        <.svg.g(box, axis, label)
      }

      // create the criteria axes

      val criteriaAxes = for (criteria <- model.criteria) yield {
        val containsHoveredSubCriteria = criteria.subCriteria.count(sc => hoveredSubCriteria.contains(sc.id)) > 0
        val stroke = if (containsHoveredSubCriteria) "black" else "#cccccc"
        <.svg.line(
          ^.svg.x1 := layout.getCriteriaAxisX(criteria), ^.svg.y1 := QualityChartLayout.CriteriaAxisTopY,
          ^.svg.x2 := layout.getCriteriaAxisX(criteria), ^.svg.y2 := QualityChartLayout.CriteriaAxisBotY,
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
            ^.svg.x1 := layout.getSubCriteriaAxisX(subCriteria), ^.svg.y1 := QualityChartLayout.SubCriteriaAxisTopY,
            ^.svg.x2 := layout.getSubCriteriaAxisX(subCriteria), ^.svg.y2 := QualityChartLayout.SubCriteriaAxisBotY,
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
          ^.svg.y := QualityChartLayout.SubCriteriaAxisTopY - QualityChartLayout.Padding + 5, hoveredAxisName
        )

      <.svg.g(criteriaBoxesAndStuff, criteriaAxes, subCriteriaAxes, hoveredAxisLabel)
    }


    /**
      * Constructs the horizontal lines for the entities.
      *
      * @param model the application model
      * @return a group of SVG elements containing the representation of the entities
      */
    private def constructEntities(model: AppModel) = {

      val layout = model.qualityModel.layout
      val hoveredSubCriteria = model.subCriteriaSelectionModel.hovered

      def isVisible(e: GroupedEntity) = model.entitySelectionModel.visible.contains(e.id)

      def isPinned(e: GroupedEntity) = model.entitySelectionModel.pinned.contains(e.id)

      def isHovered(e: GroupedEntity) = model.entitySelectionModel.hovered.contains(e.id)

      val entitiesInPaintingOrder = model.qualityModel.rankedEntities.zipWithIndex.sortBy { case (e, index) =>
        (isPinned(e), isHovered(e), isVisible(e), -index) // `-index` assures that elements first in legend are painted last (i.e. in front)
      }.unzip._1

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

        // create the criteria values line

        def createLine(groupedEntity: GroupedEntity, strokeColor: String, strokeWidth: Int, valueCoordinates: ListBuffer[Option[(Double, Double)]]): IndexedSeq[ReactTagOf[G]] = {
          var isPreviousPresent = false
          val criteriaValuesLine =
            for (i <- valueCoordinates.indices) yield {
              var lineSegment = <.svg.g

              var isThisPresent = valueCoordinates(i).isDefined
              if (isThisPresent) {

                var isNextPresent = i < (valueCoordinates.size - 1) && valueCoordinates(i + 1).isDefined
                if (isNextPresent) {
                  lineSegment = <.svg.g(
                    <.svg.line(
                      ^.svg.x1 := valueCoordinates(i).get._1, ^.svg.y1 := valueCoordinates(i).get._2,
                      ^.svg.x2 := valueCoordinates(i + 1).get._1, ^.svg.y2 := valueCoordinates(i + 1).get._2,
                      ^.svg.stroke := strokeColor, ^.svg.strokeWidth := strokeWidth,
                      ^.onClick --> toggleEntityPinning(groupedEntity.id)
                    )
                  )
                } else {
                  if (!isPreviousPresent) {
                    lineSegment = <.svg.g(
                      <.svg.line(
                        ^.svg.x1 := valueCoordinates(i).get._1 - 10, ^.svg.y1 := valueCoordinates(i).get._2,
                        ^.svg.x2 := valueCoordinates(i).get._1 + 10, ^.svg.y2 := valueCoordinates(i).get._2,
                        ^.svg.stroke := strokeColor, ^.svg.strokeWidth := strokeWidth,
                        ^.onClick --> toggleEntityPinning(groupedEntity.id)
                      )
                    )
                  }
                }

              }
              isPreviousPresent = isThisPresent

              lineSegment
            }
          criteriaValuesLine
        }

        val criteria = model.qualityModel.criteria

        var criteriaCoordinates = new ListBuffer[Option[(Double, Double)]]()
        for (criterion <- criteria) {
          var value = computeAxisValue(criterion.groupedValues.get(groupedEntity.id), layout, AxisType.Criteria)

          if (value.isDefined) {
            val x1 = layout.getCriteriaAxisX(criterion)
            val y1 = QualityChartLayout.CriteriaAxisBotY - computeAxisValue(criterion.groupedValues.get(groupedEntity.id), layout, AxisType.Criteria).get
            criteriaCoordinates += Some(Tuple2(x1, y1))
          } else {
            criteriaCoordinates += None
          }
        }

        val criteriaValuesLine = createLine(groupedEntity, strokeColor, strokeWidth, criteriaCoordinates)

        // create the subcriteria values lines

        var subCriteriaCoordinates = new ListBuffer[Option[(Double, Double)]]()
        for (criterion <- criteria) {
          for (subCriterion <- criterion.subCriteria) {
            var value = computeAxisValue(subCriterion.groupedValues.get(groupedEntity.id), layout, AxisType.Subcriteria)

            if (value.isDefined) {
              val x1 = layout.getSubCriteriaAxisX(subCriterion)
              val y1 = QualityChartLayout.SubCriteriaAxisBotY - computeAxisValue(subCriterion.groupedValues.get(groupedEntity.id), layout, AxisType.Subcriteria).get
              subCriteriaCoordinates += Some(Tuple2(x1, y1))
            } else {
              subCriteriaCoordinates += None
            }
          }
        }

        val subCriteriaValuesLine = createLine(groupedEntity, strokeColor, strokeWidth, subCriteriaCoordinates)


        // Create the circles if entity is pinned

        val circles = if (isPinned(groupedEntity)) {
          val c = for (coords <- criteriaCoordinates ++ subCriteriaCoordinates) yield {
            if (coords.isDefined) {
              <.svg.circle(
                ^.svg.cx := coords.get._1, ^.svg.cy := coords.get._2, ^.svg.r := 5,
                ^.svg.fill := "black",
                ^.svg.strokeWidth := 0
              )
            } else {
              <.svg.g
            }
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
              val label = formatValue(criteria.groupedValues.get(groupedEntity.id))
              criteriaValueLabel =
                <.svg.text(
                  ^.svg.textAnchor := "middle",
                  ^.svg.x := layout.getCriteriaAxisX(criteria),
                  ^.svg.y := QualityChartLayout.CriteriaAxisBotY + QualityChartLayout.Padding - 5,
                  label
                )
            }

            for (subCriteria <- criteria.subCriteria) {
              if (hoveredSubCriteria.contains(subCriteria.id)) {
                val label = formatValue(subCriteria.groupedValues.get(groupedEntity.id))
                subCriteriaValueLabel =
                  <.svg.text(
                    ^.svg.textAnchor := "middle",
                    ^.svg.x := layout.getSubCriteriaAxisX(subCriteria),
                    ^.svg.y := QualityChartLayout.SubCriteriaAxisBotY + QualityChartLayout.Padding - 5,
                    label
                  )
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

    private def computeAxisValue(value: Option[Double], layout: QualityChartLayout, axisType: AxisType.Value): Option[Double] = {
      val (topY, botY) = axisType match {
        case AxisType.Criteria => (QualityChartLayout.CriteriaAxisTopY, QualityChartLayout.CriteriaAxisBotY)
        case AxisType.Subcriteria => (QualityChartLayout.SubCriteriaAxisTopY, QualityChartLayout.SubCriteriaAxisBotY)
      }

      value.map { v => v / (layout.maxValue - layout.minValue) * (botY - topY) }
    }
  }

  private val component = ReactComponentB[Props](QualityChartComponent.getClass.getSimpleName)
    .renderBackend[Backend]
    .componentDidMount { $ =>
      $.backend.onWindowResize()
    }
    .build

  def apply(proxy: ModelProxy[AppModel]): ReactComponentU[_root_.ch.fhnw.ima.saav.component.QualityChartComponent.Props, Unit, Backend, TopNode] = component(Props(proxy))

}