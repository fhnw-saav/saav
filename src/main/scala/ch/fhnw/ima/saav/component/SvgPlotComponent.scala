package ch.fhnw.ima.saav.component

import ch.fhnw.ima.saav.model.app.{PlottableCategory, PlottableEntity, PlottableQualityDataModel}
import diode.react.ModelProxy
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{BackendScope, ReactComponentB}

object SvgPlotComponent {

  case class Props(proxy: ModelProxy[PlottableQualityDataModel])

  type HoveredPoint = Option[(PlottableCategory, PlottableEntity)]

  case class State(hoveredPoint: HoveredPoint = None)

  class Backend($: BackendScope[Props, State]) {

    def setHoveredPoint(hoveredPoint: HoveredPoint) =
      $.modState(s => State(hoveredPoint))

    def clearHoveredPoint() = $.modState(s => State(None))

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

      val coordinateSystem = constructCoordinateSystem(model, plotHeight, layout)
      val entities = constructEntities(model, layout)

      // Assemble everything

      <.svg.svg(^.svg.viewBox := s"0 0 $plotWidth $plotHeight",
        background,
        coordinateSystem,
        entities
      )

    }


    private def constructCoordinateSystem(model: PlottableQualityDataModel, plotHeight: Int, layout: QualityLayout) = {

      // create the criteria boxes
      val boxes = for (category <- model.categories) yield {
        val (x, width) = layout.getCriteriaBox(category)
        <.svg.rect(
          ^.svg.fill := "#eeeeee",
          ^.svg.x := x, ^.svg.y := layout.MARGIN,
          ^.svg.width := width, ^.svg.height := (plotHeight - 2 * layout.MARGIN))
      }

      // create the criteria axes
      val criteriaAxes = for (category <- model.categories) yield {
        <.svg.line(
          ^.svg.x1 := layout.getCriteriaAxisX(category), ^.svg.y1 := layout.getCategoryAxisTopY,
          ^.svg.x2 := layout.getCriteriaAxisX(category), ^.svg.y2 := layout.getCategoryAxisBotY,
          ^.svg.stroke := "#cccccc", ^.svg.strokeWidth := "1"
        )
      }

      // create the subcriteria axes
      val subCriteriaAxes = for (category <- model.categories) yield {
        val axes = for (subCategory <- category.subCategories) yield {
          <.svg.line(
            ^.svg.x1 := layout.getSubCriteriaAxisX(subCategory), ^.svg.y1 := layout.getSubCategoryAxisTopY,
            ^.svg.x2 := layout.getSubCriteriaAxisX(subCategory), ^.svg.y2 := layout.getSubCategoryAxisBotY,
            ^.svg.stroke := "#cccccc", ^.svg.strokeWidth := "1"
          )
        }
        <.svg.g(axes)
      }

      <.svg.g(boxes, criteriaAxes, subCriteriaAxes)
    }
  }


  private def constructEntities(model: PlottableQualityDataModel, layout: QualityLayout) = {
    val entities = for (plottableEntity <- model.rankedEntities) yield {

      var stroke = if (plottableEntity.isSelected) plottableEntity.color.hexValue else "#cccccc"
      var strokeWidth = if (plottableEntity.isSelected) 2 else 1

      // create the criteria values lines

      var coordString = "M"
      var index = 0
      for (category <- model.categories) {
        val x = layout.getCriteriaAxisX(category)
        val value = category.groupedValues(plottableEntity.id).get * (layout.getCategoryAxisBotY - layout.getCategoryAxisTopY) / 100.0
        val y = layout.getCategoryAxisBotY - value

        if (index == 1) coordString += " L"
        coordString += " " + x + " " + y

        index += 1
      }
      val criteriaValuesLine = <.svg.path(^.svg.d := coordString,
        ^.svg.stroke := stroke, ^.svg.strokeWidth := strokeWidth, ^.svg.fill := "none"
      )

      // create the subcriteria values lines

      coordString = "M"
      index = 0
      for (category <- model.categories) {
        for (subCategory <- category.subCategories) {
          val x = layout.getSubCriteriaAxisX(subCategory)
          val value = subCategory.groupedValues(plottableEntity.id).get * (layout.getSubCategoryAxisBotY - layout.getSubCategoryAxisTopY) / 100.0
          val y = layout.getSubCategoryAxisBotY - value

          if (index == 1) coordString += " L"
          coordString += " " + x + " " + y

          index += 1
        }
      }
      val subCriteriaValuesLine = <.svg.path(^.svg.d := coordString,
        ^.svg.stroke := stroke, ^.svg.strokeWidth := strokeWidth, ^.svg.fill := "none"
      )

      <.svg.g(criteriaValuesLine, subCriteriaValuesLine)

    }

    entities
  }


  /*
      def render(p: Props, s: State) = {

        val m = p.proxy.value

        // only relevant for aspect ratio and e.g. relative stroke width (svg will be scaled within parent element)
        val plotWidth = 1000
        val plotHeight = 200

        val padding = 10

        val background = <.svg.rect(
          ^.svg.fill := "lightgrey",
          ^.svg.x := "0", ^.svg.y := "0",
          ^.svg.width := "100%", ^.svg.height := "100%")

        val plot = {

          val lineHeight = plotHeight - 2 * padding
          val categoryLineDistance = (plotWidth - 2 * padding) / m.categories.length

          for {
            (category, categoryIndex) <- m.categories.zipWithIndex
            x = (categoryIndex * categoryLineDistance) + (categoryLineDistance / 2.0) + padding
          } yield {
            val lineStartY = padding
            val categoryLines = <.svg.line(
              ^.svg.x1 := x, ^.svg.y1 := lineStartY,
              ^.svg.x2 := x, ^.svg.y2 := lineStartY + lineHeight,
              ^.svg.stroke := "black", ^.svg.strokeWidth := "1"
            )

            val entities = m.rankedEntities.filter(_.isSelected)
            val entityPointDistance = lineHeight / entities.length

            val entityPoints = for {
              (entity, entityIndex) <- entities.zipWithIndex
              y = (entityIndex * entityPointDistance) + lineStartY + (entityPointDistance / 2)
            } yield {
              val isHovered = s.hoveredPoint.contains((category, entity))
              val pinnedOrHovered = if (entity.isPinned || isHovered) "black" else "transparent"
              <.svg.circle(
                ^.svg.cx := x, ^.svg.cy := y, ^.svg.r := 5,
                ^.svg.fill := entity.color.hexValue,
                ^.svg.strokeWidth := 2,
                ^.svg.stroke := pinnedOrHovered,
                ^.onMouseOver --> setHoveredPoint(Some((category, entity))),
                ^.onMouseOut --> clearHoveredPoint()
              )
            }

            <.svg.g(
              categoryLines,
              entityPoints
            )
          }

        }

        <.svg.svg(^.svg.viewBox := s"0 0 $plotWidth $plotHeight",
          background,
          plot
        )

      }
  */




  private val component = ReactComponentB[Props](SvgPlotComponent.getClass.getSimpleName)
    .initialState(State())
    .renderBackend[Backend]
    .build

  def apply(proxy: ModelProxy[PlottableQualityDataModel]) = component(Props(proxy))

}