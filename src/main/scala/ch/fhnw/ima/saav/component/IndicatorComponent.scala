package ch.fhnw.ima.saav.component

import ch.fhnw.ima.saav.model.app.AppModel
import diode.react.ModelProxy
import japgolly.scalajs.react.ReactComponentB
import japgolly.scalajs.react.vdom.prefix_<^._

import scalacss.ScalaCssReact._

object IndicatorComponent {

  case class Props(proxy: ModelProxy[AppModel])

  private val component = ReactComponentB[Props](IndicatorComponent.getClass.getSimpleName)
    .render_P { p =>

      val model = p.proxy.value

      val indicators = for {
        pinnedEntity <- model.entitySelectionModel.pinned.toSeq
        hoveredSubCriteria <- model.subCriteriaSelectionModel.hovered.toSeq
        focusSubCriteria <- model.qualityModel.criteria.flatMap(_.subCriteria).find(_.id == hoveredSubCriteria).toSeq
        indicator <- focusSubCriteria.indicators
      } yield {
        val value = indicator.groupedValues(pinnedEntity)
        <.div(
          <.div(css.colXs10, indicator.name),
          <.div(css.colXs2, value)
        )
      }

      <.div(
        <.h2("Indicators"),
        <.div(css.row, indicators.toSeq)
      )

    }
    .build

  def apply(proxy: ModelProxy[AppModel]) = component(Props(proxy))

}
