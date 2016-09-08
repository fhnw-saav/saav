package ch.fhnw.ima.saav.component

import ch.fhnw.ima.saav.controller.SaavController.{UpdateIndicatorWeightAction, UpdateSubCriteriaWeightAction}
import ch.fhnw.ima.saav.model.app.{Profile, Quality, Weight, Weights}
import ch.fhnw.ima.saav.model.domain.{Analysis, Criteria, Indicator, SubCriteria}
import diode.react.ModelProxy
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB}

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

    def toggleIndicatorWeight(indicator: Indicator) =
      $.props >>= { p =>
        val weights = p.proxy.value._2
        val isCurrentlyEnabled = weights.enabledIndicators.contains(indicator)
        val toggled = !isCurrentlyEnabled
        p.proxy.dispatch(UpdateIndicatorWeightAction(indicator, toggled))
      }

    def updateSubCriteriaWeight(subCriteria: SubCriteria, weight: Weight) =
      $.props >>= (_.proxy.dispatch(UpdateSubCriteriaWeightAction(subCriteria, weight)))

    def render(p: Props, s: State) = {

      val (analysis, weights) = p.proxy.value

      val criteriaItems = for (criteria <- analysis.criteria) yield {
        <.div(css.row, createCriteriaItem(criteria, s, weights))
      }

      <.div(
        <.h2("Expert Configuration"),
        <.ul(criteriaItems)
      )

    }

    def createCriteriaItem(criteria: Criteria, s: State, weights: Weights) = {
      s.criteriaToggleStates(criteria) match {
        case Collapsed =>
          <.div(rightGlyph, ^.onClick --> expandCriteria(criteria), criteria.name)
        case Expanded =>
          <.div(
            <.div(downGlyph, ^.onClick --> collapseCriteria(criteria), criteria.name),
            <.ul(css.expertConfigListStyle,
              for (subCriteria <- criteria.subCriteria) yield {
                <.li(createSubCriteriaItem(subCriteria, s, weights))
              })
          )
      }
    }

    def createSubCriteriaItem(subCriteria: SubCriteria, s: State, weights: Weights) = {

      def header(glyph: ReactTag, onClick: => Callback) =
        <.div(^.display.inline,
          <.div(^.display.inline, glyph, ^.onClick --> onClick),
          <.div(^.display.inline, subCriteria.name + " "),
          createQualityVsProfileSelector(subCriteria, weights.subCriteriaWeights)
        )

      s.subCriteriaToggleStates(subCriteria) match {
        case Collapsed =>
          header(rightGlyph, expandSubCriteria(subCriteria))
        case Expanded =>
          <.div(
            header(downGlyph, collapseSubCriteria(subCriteria)),
            <.ul(css.expertConfigListStyle,
              for (indicator <- subCriteria.indicators) yield {
                val isChecked = weights.enabledIndicators.contains(indicator)
                <.li(<.input.checkbox(^.checked := isChecked, ^.onChange --> toggleIndicatorWeight(indicator)), " " + indicator.name)
              })
          )
      }
    }

    def createQualityVsProfileSelector(subCriteria: SubCriteria, subCriteriaWeights: Map[SubCriteria, Weight]) = {
      val radioButtonGroupName = "profileVsQuality-" + subCriteria.name
      val isProfile = subCriteriaWeights(subCriteria) == Profile
      <.div(^.display.inline,
        <.input.radio(
          ^.name := radioButtonGroupName,
          ^.checked := !isProfile,
          ^.onChange --> updateSubCriteriaWeight(subCriteria, Quality(1.0))),
        " Q ",
        <.input.radio(
          ^.name := radioButtonGroupName,
          ^.checked := isProfile,
          ^.onChange --> updateSubCriteriaWeight(subCriteria, Profile)),
        " P "
      )
    }

  }

  private val component = ReactComponentB[Props](ExpertConfigComponent.getClass.getSimpleName)
    .initialState(State())
    .renderBackend[Backend]
    .build

  def apply(proxy: ModelProxy[(Analysis, Weights)]) = component(Props(proxy))

}
