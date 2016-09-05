package ch.fhnw.ima.saav.component

import ch.fhnw.ima.saav.model.app.Weights
import ch.fhnw.ima.saav.model.domain.{Analysis, Criteria}
import diode.react.ModelProxy
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{BackendScope, ReactComponentB}

import scalacss.ScalaCssReact._

object ExpertConfigComponent {

  private val rightGlyph = <.i(css.glyph.right)
  private val downGlyph = <.i(css.glyph.down)

  case class Props(proxy: ModelProxy[(Analysis, Weights)])

  case class State(
    criteriaToggleStates: Map[Criteria, ToggleState] = Map.empty[Criteria, ToggleState].withDefaultValue(Collapsed),
    subCriteriaToggleStates: Map[Criteria, ToggleState] = Map.empty[Criteria, ToggleState].withDefaultValue(Collapsed)
  )

  sealed trait ToggleState

  case object Collapsed extends ToggleState

  case object Expanded extends ToggleState

  class Backend($: BackendScope[Props, State]) {

    def expandCriteria(criteria: Criteria) = updateCriteriaToggleState(criteria, Expanded)

    def collapseCriteria(criteria: Criteria) = updateCriteriaToggleState(criteria, Collapsed)

    def updateCriteriaToggleState(criteria: Criteria, newToggleState: ToggleState) =
      $.modState { s =>
        val newCriteriaToggleStates = s.criteriaToggleStates.updated(criteria, newToggleState)
        s.copy(criteriaToggleStates = newCriteriaToggleStates)
      }

    def render(p: Props, s: State) = {

      val (analysis, weights) = p.proxy.value

      val criteriaItems = for (criteria <- analysis.criteria) yield {
        <.div(css.row, createCriteriaItem(criteria, s))
      }

      <.div(
        <.h2("Expert Configuration"),
        <.ul(criteriaItems)
      )

    }

    def createCriteriaItem(criteria: Criteria, s: State) = {
      s.criteriaToggleStates(criteria) match {
        case Collapsed =>
          <.div(css.row, rightGlyph, ^.onClick --> expandCriteria(criteria), criteria.name)
        case Expanded =>
          <.div(
            <.div(css.row, downGlyph, ^.onClick --> collapseCriteria(criteria), criteria.name),
            <.ul(css.row,
              for (subCriteria <- criteria.subCriteria) yield {
                <.li(subCriteria.name)
              })
          )
      }
    }

  }

  private val component = ReactComponentB[Props](ExpertConfigComponent.getClass.getSimpleName)
    .initialState(State())
    .renderBackend[Backend]
    .build

  def apply(proxy: ModelProxy[(Analysis, Weights)]) = component(Props(proxy))

}
