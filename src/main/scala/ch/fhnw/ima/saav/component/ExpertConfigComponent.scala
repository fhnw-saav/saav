package ch.fhnw.ima.saav.component

import ch.fhnw.ima.saav.model.app.Weights
import ch.fhnw.ima.saav.model.domain.{Analysis, Criteria, SubCriteria}
import diode.react.ModelProxy
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{BackendScope, ReactComponentB}

import scalacss.ScalaCssReact._

object ExpertConfigComponent {

  private val rightGlyph = <.i(css.glyph.right, ^.cursor.pointer)
  private val downGlyph = <.i(css.glyph.down, ^.cursor.pointer)

  case class Props(proxy: ModelProxy[(Analysis, Weights)])

  case class State(
    criteriaToggleStates: Map[Criteria, ToggleState] = Map.empty[Criteria, ToggleState].withDefaultValue(Collapsed),
    subCriteriaToggleStates: Map[SubCriteria, ToggleState] = Map.empty[SubCriteria, ToggleState].withDefaultValue(Collapsed)
  )

  sealed trait ToggleState

  case object Collapsed extends ToggleState

  case object Expanded extends ToggleState

  class Backend($: BackendScope[Props, State]) {

    def expandCriteria(criteria: Criteria) = updateCriteriaToggleState(criteria, Expanded)

    def collapseCriteria(criteria: Criteria) = updateCriteriaToggleState(criteria, Collapsed)

    def updateCriteriaToggleState(criteria: Criteria, newToggleState: ToggleState) =
      $.modState { s =>
        val newStates = s.criteriaToggleStates.updated(criteria, newToggleState)
        s.copy(criteriaToggleStates = newStates)
      }

    def expandSubCriteria(subCriteria: SubCriteria) = updateSubCriteriaToggleState(subCriteria, Expanded)

    def collapseSubCriteria(subCriteria: SubCriteria) = updateSubCriteriaToggleState(subCriteria, Collapsed)

    def updateSubCriteriaToggleState(subCriteria: SubCriteria, newToggleState: ToggleState) =
      $.modState { s =>
        val newStates = s.subCriteriaToggleStates.updated(subCriteria, newToggleState)
        s.copy(subCriteriaToggleStates = newStates)
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
          <.div(rightGlyph, ^.onClick --> expandCriteria(criteria), criteria.name)
        case Expanded =>
          <.div(
            <.div(downGlyph, ^.onClick --> collapseCriteria(criteria), criteria.name),
            <.ul(css.expertConfigListStyle,
              for (subCriteria <- criteria.subCriteria) yield {
                <.li(createSubCriteriaItem(subCriteria, s))
              })
          )
      }
    }

    def createSubCriteriaItem(subCriteria: SubCriteria, s: State) = {
      s.subCriteriaToggleStates(subCriteria) match {
        case Collapsed =>
          <.div(rightGlyph, ^.onClick --> expandSubCriteria(subCriteria), subCriteria.name)
        case Expanded =>
          <.div(
            <.div(downGlyph, ^.onClick --> collapseSubCriteria(subCriteria), subCriteria.name),
            <.ul(css.expertConfigListStyle,
              for (indicator <- subCriteria.indicators) yield {
                <.li(<.input.checkbox(), " " + indicator.name)
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
