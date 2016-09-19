package ch.fhnw.ima.saav
package component

import ch.fhnw.ima.saav.model.app._
import ch.fhnw.ima.saav.model.domain.Entity
import diode.react.ModelProxy
import japgolly.scalajs.react.ReactComponentB
import japgolly.scalajs.react.vdom.prefix_<^._

import scalacss.ScalaCssReact._

object IndicatorComponent {

  case class Props(pinnedEntity: Option[Entity], indicators: Seq[GroupedIndicator])

  private val component = ReactComponentB[Props](IndicatorComponent.getClass.getSimpleName)
    .render_P { p =>

      val indicators = for {
        pinnedEntity <- p.pinnedEntity.toSeq
        indicator <- p.indicators
      } yield {
        val value = indicator.groupedValues(pinnedEntity)
        <.tr(
          <.td(css.colXs10, css.overflowHidden, ^.textOverflow.ellipsis, ^.title := indicator.name, indicator.name),
          <.td(css.colXs2, value)
        )
      }

      <.div(
        <.h2("Indicators"),
        <.table(css.table,
          <.thead(<.tr(<.th(css.colXs10, "Indicator"), <.th(css.colXs2, "Median"))),
          <.tbody(
            if (indicators.isEmpty) {
              <.tr(<.td(^.colSpan := 2, <.i("Pin a row and move your mouse over an axis to see something here")))
            } else {
              indicators.toSeq
            }))
      )

    }
    .shouldComponentUpdate { $ =>
      $.currentProps != $.nextProps
    }
    .build

  def apply(proxy: ModelProxy[AppModel]) = {
    val model = proxy.value
    val pinnedEntity = model.entitySelectionModel.pinned
    val allCriteria = model.qualityModel.criteria ++ model.profileModel.criteria
    val indicators = for {
      hoveredSubCriteria <- model.subCriteriaSelectionModel.hovered.toSeq
      focusSubCriteria <- allCriteria.flatMap(_.subCriteria).find(_.id == hoveredSubCriteria).toSeq
      indicator <- focusSubCriteria.indicators
    } yield {
      indicator
    }
    component(Props(pinnedEntity, indicators))
  }

}
