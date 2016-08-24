package ch.fhnw.ima.saav.component

import ch.fhnw.ima.saav.controller.SaavController.UpdateEntityPinningAction
import ch.fhnw.ima.saav.model.app.{DataModel, PlottableEntity}
import ch.fhnw.ima.saav.model.domain.SubCategory
import diode.react.ModelProxy
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{BackendScope, ReactComponentB}

object SvgPlotComponent {

  case class Props(proxy: ModelProxy[DataModel])

  case class State(hoveredSubCategory: Option[SubCategory] = None)

  class Backend($: BackendScope[Props, State]) {

    def setHoveredSubCategory(hoveredSubCategory: Option[SubCategory]) =
      $.modState(s => State(hoveredSubCategory))

    def clearHoveredSubCategory() = $.modState(s => State(None))

    def toggleEntityPinning(plottableEntity: PlottableEntity) =
      $.props >>= { p =>
        val pinnedOrNone = if (plottableEntity.isPinned) None else Some(plottableEntity.id)
        p.proxy.dispatch(UpdateEntityPinningAction(pinnedOrNone))
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

      val coordinateSystem = constructCoordinateSystem(model, s.hoveredSubCategory, plotHeight, layout)
      val entities = constructEntities(model, layout)

      // Assemble everything

      <.svg.svg(^.svg.viewBox := s"0 0 $plotWidth $plotHeight",
        background,
        coordinateSystem,
        entities
      )

    }


    private def constructCoordinateSystem(model: DataModel, hoveredSubCategory: Option[SubCategory], plotHeight: Int, layout: QualityLayout) = {

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
        val containsHoveredSubCategory = category.subCategories.count(sc => hoveredSubCategory.contains(sc.id)) > 0
        val stroke = if (containsHoveredSubCategory) "black" else "#cccccc"
        <.svg.line(
          ^.svg.x1 := layout.getCriteriaAxisX(category), ^.svg.y1 := layout.getCategoryAxisTopY,
          ^.svg.x2 := layout.getCriteriaAxisX(category), ^.svg.y2 := layout.getCategoryAxisBotY,
          ^.svg.stroke := stroke, ^.svg.strokeWidth := "1"
        )
      }

      // create the subcriteria axes
      val subCriteriaAxes = for (category <- model.categories) yield {
        val axes = for (subCategory <- category.subCategories) yield {
          val stroke = if (hoveredSubCategory.contains(subCategory.id)) "black" else "#cccccc"
          <.svg.line(
            ^.svg.x1 := layout.getSubCriteriaAxisX(subCategory), ^.svg.y1 := layout.getSubCategoryAxisTopY,
            ^.svg.x2 := layout.getSubCriteriaAxisX(subCategory), ^.svg.y2 := layout.getSubCategoryAxisBotY,
            ^.svg.stroke := stroke, ^.svg.strokeWidth := "1",
            ^.onMouseOver --> setHoveredSubCategory(Some(subCategory.id)),
            ^.onMouseOut --> clearHoveredSubCategory(),
            ^.cursor.pointer
          )
        }
        <.svg.g(axes)
      }

      <.svg.g(boxes, criteriaAxes, subCriteriaAxes)
    }

    private def constructEntities(model: DataModel, layout: QualityLayout) = {
      val entitiesInPaintingOrder = model.rankedEntities.sortBy(e => (e.isPinned, e.isSelected))
      val entities = for (plottableEntity <- entitiesInPaintingOrder) yield {

        val (strokeColor, strokeWidth, cursor) =
          if (plottableEntity.isSelected)
            if (plottableEntity.isPinned)
              ("black", 4, ^.cursor.pointer)
            else
              (plottableEntity.color.hexValue, 2, ^.cursor.pointer)
          else
            ("#cccccc", 1, ^.cursor.default)

        // create the criteria values lines

        var coordString = "M"
        var index = 0

        var valueCoordinates =
          for (category <- model.categories) yield {
            val x = layout.getCriteriaAxisX(category)
            val value = category.groupedValues(plottableEntity.id).get * (layout.getCategoryAxisBotY - layout.getCategoryAxisTopY) / 100.0
            val y = layout.getCategoryAxisBotY - value

            if (index == 1) coordString += " L"
            coordString += " " + x + " " + y

            index += 1

            (x, y)
          }

        val criteriaValuesLine = <.svg.path(^.svg.d := coordString,
          ^.svg.stroke := strokeColor, ^.svg.strokeWidth := strokeWidth, ^.svg.fill := "none",
          ^.onClick --> toggleEntityPinning(plottableEntity),
          cursor
        )

        // create the subcriteria values lines

        coordString = "M"
        index = 0

        for (category <- model.categories) {
          val valueSubCoordinates =
            for (subCategory <- category.subCategories) yield {
              val x = layout.getSubCriteriaAxisX(subCategory)
              val value = subCategory.groupedValues(plottableEntity.id).get * (layout.getSubCategoryAxisBotY - layout.getSubCategoryAxisTopY) / 100.0
              val y = layout.getSubCategoryAxisBotY - value

              if (index == 1) coordString += " L"
              coordString += " " + x + " " + y

              index += 1

              (x, y)
            }

          valueCoordinates = valueCoordinates ++ valueSubCoordinates
        }

        val subCriteriaValuesLine = <.svg.path(^.svg.d := coordString,
          ^.svg.stroke := strokeColor, ^.svg.strokeWidth := strokeWidth, ^.svg.fill := "none",
          ^.onClick --> toggleEntityPinning(plottableEntity),
          cursor
        )

        // Create the circles if entity is pinned

        if (plottableEntity.isPinned) {
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