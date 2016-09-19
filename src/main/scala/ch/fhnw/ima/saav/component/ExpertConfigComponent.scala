package ch.fhnw.ima.saav.component

import java.util.UUID

import ch.fhnw.ima.saav.controller._
import ch.fhnw.ima.saav.model.app.{Profile, Quality, Weight, Weights}
import ch.fhnw.ima.saav.model.config.Config
import ch.fhnw.ima.saav.model.domain.{Analysis, Criteria, Indicator, SubCriteria}
import diode.Action
import diode.react.ModelProxy
import japgolly.scalajs.react.vdom.ReactTagOf
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB, ReactComponentU, ReactEventI, TopNode}
import org.scalajs.dom.html.Div

import scalacss.ScalaCssReact._

// TODO: Move all CSS to GlobalStyles
object ExpertConfigComponent {

  private val rightGlyph = <.i(css.glyph.right, ^.cursor.pointer)
  private val downGlyph = <.i(css.glyph.down, ^.cursor.pointer)
  private val resetGlyph = <.i(css.glyph.reset, ^.cursor.pointer)

  case class Props(analysis: Analysis, defaultWeights: Weights, weights: Weights, dispatch: Action => Callback)

  case class State(
    criteriaToggleStates: Map[Criteria, ToggleState] = Map.empty[Criteria, ToggleState].withDefaultValue(Collapsed),
    subCriteriaToggleStates: Map[SubCriteria, ToggleState] = Map.empty[SubCriteria, ToggleState].withDefaultValue(Collapsed)
  )

  sealed trait ToggleState

  case object Collapsed extends ToggleState

  case object Expanded extends ToggleState

  class Backend($: BackendScope[Props, State]) {

    private def expandCriteria(criteria: Criteria) = updateCriteriaToggleState(criteria, Expanded)

    private def collapseCriteria(criteria: Criteria) = updateCriteriaToggleState(criteria, Collapsed)

    private def updateCriteriaToggleState(criteria: Criteria, newToggleState: ToggleState) =
      $.modState { s =>
        val newStates = s.criteriaToggleStates.updated(criteria, newToggleState)
        s.copy(criteriaToggleStates = newStates)
      }

    private def expandSubCriteria(subCriteria: SubCriteria) = updateSubCriteriaToggleState(subCriteria, Expanded)

    private def collapseSubCriteria(subCriteria: SubCriteria) = updateSubCriteriaToggleState(subCriteria, Collapsed)

    private def updateSubCriteriaToggleState(subCriteria: SubCriteria, newToggleState: ToggleState) =
      $.modState { s =>
        val newStates = s.subCriteriaToggleStates.updated(subCriteria, newToggleState)
        s.copy(subCriteriaToggleStates = newStates)
      }

    private def toggleIndicatorWeight(indicator: Indicator) =
      $.props >>= { p =>
        val isCurrentlyEnabled = p.weights.enabledIndicators.contains(indicator)
        val toggled = !isCurrentlyEnabled
        p.dispatch(UpdateIndicatorWeightAction(indicator, toggled))
      }

    private def updateSubCriteria(subCriteria: SubCriteria, weight: Weight) =
      $.props >>= (_.dispatch(UpdateSubCriteriaWeightAction(subCriteria, weight)))

    private def updateSubCriteriaQualityWeightValue(subCriteria: SubCriteria)(e: ReactEventI) =
      updateSubCriteria(subCriteria, Quality(e.target.value.toDouble))

    private def reset =
      $.props >>= (p => p.dispatch(UpdateWeightsAction(p.defaultWeights)))

    def render(p: Props, s: State): ReactTagOf[Div] = {

      val resetWidget = <.span(
        css.expertConfigChangedWarning,
        ^.onClick --> reset,
        ^.cursor.pointer,
        ^.title := "Reset",
        <.span(css.expertConfigChangedWarningLabel, "Changed"),
        resetGlyph
      )

      <.div(
        <.h2("Expert Configuration", (p.defaultWeights != p.weights) ?= resetWidget),
        CriteriaTable(CriteriaTableProps(p.analysis.criteria, s.criteriaToggleStates, s.subCriteriaToggleStates, p.weights))
      )

    }

    private final case class CriteriaTableProps(
      criteria: Seq[Criteria],
      criteriaToggleStates: Map[Criteria, ToggleState],
      subCriteriaToggleStates: Map[SubCriteria, ToggleState],
      weights: Weights)

    private val CriteriaTable = ReactComponentB[CriteriaTableProps]("CriteriaTable")
      .render_P { p =>
        <.table(css.table,
          <.thead(
            <.tr(
              <.th(css.colXs8),
              <.th(css.colXs1, ^.textAlign.center, "Q"),
              <.th(css.colXs2, ^.textAlign.left, "Weight"),
              <.th(css.colXs1, ^.textAlign.center, "P")
            )
          ),
          <.tbody(
            for (criteria <- p.criteria) yield {
              CriteriaRow(CriteriaRowProps(criteria, p.criteriaToggleStates(criteria), p.subCriteriaToggleStates, p.weights))
            }
          )
        )
      }
      .build


    private final case class CriteriaRowProps(
      criteria: Criteria,
      criteriaToggleState: ToggleState,
      subCriteriaToggleStates: Map[SubCriteria, ToggleState],
      weights: Weights
    )

    private lazy val CriteriaRow = ReactComponentB[CriteriaRowProps]("CriteriaRow")
      .render_P { p =>
        <.tr(^.backgroundColor := "transparent",
          p.criteriaToggleState match {
            case Collapsed =>
              <.td(css.overflowHidden, ^.paddingLeft := 0, ^.paddingRight := 0, ^.backgroundColor := "transparent", ^.textOverflow.ellipsis, ^.colSpan := 4,
                <.div(^.display.inline, ^.onClick --> expandCriteria(p.criteria), rightGlyph),
                p.criteria.name
              )
            case Expanded =>
              <.td(css.overflowHidden, ^.paddingLeft := 0, ^.paddingRight := 0, ^.backgroundColor := "transparent", ^.textOverflow.ellipsis, ^.colSpan := 4,
                <.div(^.display.inline, ^.onClick --> collapseCriteria(p.criteria), downGlyph),
                p.criteria.name,
                SubCriteriaTable(SubCriteriaTableProps(p.criteria.subCriteria, p.subCriteriaToggleStates, p.weights))
              )
          })
      }
      .build

    private final case class SubCriteriaTableProps(
      subCriteria: Seq[SubCriteria],
      subCriteriaToggleStates: Map[SubCriteria, ToggleState],
      weights: Weights
    )

    private lazy val SubCriteriaTable = ReactComponentB[SubCriteriaTableProps]("SubCriteriaTable")
      .render_P { p =>
        <.table(css.table,
          <.tbody(
            for (subCriteria <- p.subCriteria) yield {
              SubCriteriaRow(SubCriteriaRowProps(subCriteria, p.subCriteriaToggleStates(subCriteria), p.weights))
            }
          )
        )
      }
      .build

    private final case class SubCriteriaRowProps(
      subCriteria: SubCriteria,
      subCriteriaToggleState: ToggleState,
      weights: Weights
    )

    private val SubCriteriaRow = ReactComponentB[SubCriteriaRowProps]("SubCriteriaRow")
      .render_P { p =>

        // TODO: Is this fast enough? Optionally introduce an ID in SubCriteria
        val radioButtonGroupName = String.valueOf(UUID.randomUUID())
        val subCriteriaWeight = p.weights.subCriteriaWeights(p.subCriteria)
        val isProfile = subCriteriaWeight == Profile

        val qualityRadioButton = <.td(css.colXs1, ^.textAlign.center,
          <.input.radio(
            ^.name := radioButtonGroupName,
            ^.checked := !isProfile,
            ^.onChange --> updateSubCriteria(p.subCriteria, Quality(1.0)))
        )

        val profileRadioButton = <.td(css.colXs1, ^.textAlign.center,
          <.input.radio(
            ^.name := radioButtonGroupName,
            ^.checked := isProfile,
            ^.onChange --> updateSubCriteria(p.subCriteria, Profile))
        )

        val weightValue: Double = subCriteriaWeight match {
          case Profile => 1.0
          case Quality(w) => w.toDouble
        }

        val formattedWeightValue = f"$weightValue%1.1f"

        val weightSlider = <.td(css.colXs2,
          <.div(^.`class` := "form-group form-inline input-group", ^.marginTop := "-8px", ^.width := "100%",
            <.input.range(
              css.expertFormControl, ^.boxShadow := "0 0 0 transparent",
              ^.min := 0,
              ^.max := 1,
              ^.step := 0.1,
              ^.disabled := isProfile,
              ^.value := weightValue,
              ^.onChange ==> updateSubCriteriaQualityWeightValue(p.subCriteria)
            ),
            <.div(css.expertInputGroupAddon, formattedWeightValue)
          )
        )

        val nameCollapsedOrNameExpandedWithIndicators = p.subCriteriaToggleState match {
          case Collapsed =>
            <.td(css.overflowHidden, ^.textOverflow.ellipsis, ^.paddingLeft := "20px", ^.colSpan := 4,
              <.div(^.display.inline, ^.onClick --> expandSubCriteria(p.subCriteria), rightGlyph),
              p.subCriteria.name
            )
          case Expanded =>
            <.td(css.overflowHidden, ^.textOverflow.ellipsis, ^.paddingLeft := "20px", ^.colSpan := 4,
              <.div(^.display.inline, ^.onClick --> collapseSubCriteria(p.subCriteria), downGlyph),
              p.subCriteria.name,
              IndicatorList(IndicatorListProps(p.subCriteria.indicators, p.weights.enabledIndicators))
            )
        }
        <.tr(
          nameCollapsedOrNameExpandedWithIndicators,
          qualityRadioButton,
          weightSlider,
          profileRadioButton
        )
      }
      .build

    private final case class IndicatorListProps(
      indicators: Seq[Indicator],
      enabledIndicators: Set[Indicator]
    )

    private lazy val IndicatorList =
      ReactComponentB[IndicatorListProps]("IndicatorList")
        .render_P { p =>
          <.ul(css.expertIndicatorList,
            for (indicator <- p.indicators) yield {
              val isChecked = p.enabledIndicators.contains(indicator)
              <.li(^.whiteSpace.nowrap, ^.overflow.hidden, ^.textOverflow.ellipsis,
                <.input.checkbox(^.checked := isChecked, ^.onChange --> toggleIndicatorWeight(indicator)),
                " " + indicator.name
              )
            })
        }
        .build
  }

  private val component = ReactComponentB[Props](ExpertConfigComponent.getClass.getSimpleName)
    .initialState(State())
    .renderBackend[Backend]
    .shouldComponentUpdate { $ =>
      // compare all fields of props except for `dispatch` (a function which would never be ==)
      val propsChanged = $.currentProps.analysis != $.nextProps.analysis || $.currentProps.weights != $.nextProps.weights
      val stateChanged = $.currentState != $.nextState
      propsChanged || stateChanged
    }
    .build

  def apply(proxy: ModelProxy[(Analysis, Config, Weights)]): ReactComponentU[Props, State, Backend, TopNode] = {
    val (analysis, config, weights) = proxy.value
    val dispatch = proxy.theDispatch
    component(Props(analysis, config.defaultWeights, weights, dispatch))
  }

}
