package ch.fhnw.ima.saav
package component

import ch.fhnw.ima.saav.model.domain.Entity
import diode.react.ModelProxy
import japgolly.scalajs.react.ReactComponentB
import japgolly.scalajs.react.vdom.prefix_<^._

import scalacss.ScalaCssReact._

object LegendComponent {

  case class Props(proxy: ModelProxy[Seq[Entity]])

  private final val component = ReactComponentB[Props](LegendComponent.getClass.getSimpleName)
    .render_P(p => {
      val entities = p.proxy.value
      val rows = for ((e, i) <- entities.zipWithIndex)
        yield <.tr(
          <.th(^.scope := "row", i + 1), <.td(e.name)
        )
      <.table(css.legendTable,
        <.thead(<.tr(<.th("#"), <.th("Name"))),
        <.tbody(rows))
    })
    .build

  def apply(proxy: ModelProxy[Seq[Entity]]) = component(Props(proxy))

}
