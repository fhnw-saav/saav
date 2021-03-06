package ch.fhnw.ima.saav
package view

import java.util.UUID

import ch.fhnw.ima.saav.circuit._
import ch.fhnw.ima.saav.model.app.ExpertConfig
import ch.fhnw.ima.saav.model.domain._
import ch.fhnw.ima.saav.model.weight.{Profile, Quality, Weight, Weights}
import diode.Action
import diode.react.ModelProxy
import japgolly.scalajs.react.vdom.ReactTagOf
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB, ReactComponentU, ReactEventI, TopNode}
import org.scalajs.dom.html.Div

import scalacss.ScalaCssReact._

// TODO: Move all CSS to GlobalStyles
object ExpertConfigComponent {

  val Title = "Expert Configuration"

  private val rightGlyph = <.i(css.glyph.right, ^.cursor.pointer)
  private val downGlyph = <.i(css.glyph.down, ^.cursor.pointer)

  case class Props(analysis: Analysis, expertConfig: ExpertConfig, dispatchCB: Action => Callback)

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

    private def toggleIndicatorWeight(indicatorId: IndicatorId) =
      $.props >>= { p =>
        val isCurrentlyEnabled = p.expertConfig.actualWeights.enabledIndicators.contains(indicatorId)
        val toggled = !isCurrentlyEnabled
        p.dispatchCB(UpdateIndicatorWeightAction(indicatorId, toggled))
      }

    private def updateSubCriteria(subCriteriaId: SubCriteriaId, weight: Weight) =
      $.props >>= (_.dispatchCB(UpdateSubCriteriaWeightAction(subCriteriaId, weight)))

    private def updateSubCriteriaQualityWeightValue(subCriteriaId: SubCriteriaId)(e: ReactEventI) =
      updateSubCriteria(subCriteriaId, Quality(e.target.value.toDouble))

    def render(p: Props, s: State): ReactTagOf[Div] = {

      <.div(css.boxed,
        <.h3(^.display.`inline-block`, Title),
        <.div(css.expertConfigToolbar,
          ExpertConfigResetComponent(p.expertConfig.isModified, p.expertConfig.defaultWeights, p.dispatchCB)
        ),
        CriteriaTable(CriteriaTableProps(p.analysis.criteria, s.criteriaToggleStates, s.subCriteriaToggleStates, p.expertConfig.actualWeights))
      )
    }

    private case class CriteriaTableProps(
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


    private case class CriteriaRowProps(
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
                p.criteria.displayName
              )
            case Expanded =>
              <.td(css.overflowHidden, ^.paddingLeft := 0, ^.paddingRight := 0, ^.backgroundColor := "transparent", ^.textOverflow.ellipsis, ^.colSpan := 4,
                <.div(^.display.inline, ^.onClick --> collapseCriteria(p.criteria), downGlyph),
                p.criteria.displayName,
                SubCriteriaTable(SubCriteriaTableProps(p.criteria.subCriteria, p.subCriteriaToggleStates, p.weights))
              )
          })
      }
      .build

    private case class SubCriteriaTableProps(
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

    private case class SubCriteriaRowProps(
      subCriteria: SubCriteria,
      subCriteriaToggleState: ToggleState,
      weights: Weights
    )

    private val SubCriteriaRow = ReactComponentB[SubCriteriaRowProps]("SubCriteriaRow")
      .render_P { p =>

        // TODO: Is this fast enough? Optionally introduce an ID in SubCriteria
        val radioButtonGroupName = String.valueOf(UUID.randomUUID())
        val subCriteriaWeight = p.weights.subCriteriaWeights(p.subCriteria.id)
        val isProfile = subCriteriaWeight == Profile

        val qualityRadioButton = <.td(css.colXs1, ^.textAlign.center,
          <.input.radio(
            ^.name := radioButtonGroupName,
            ^.checked := !isProfile,
            ^.onChange --> updateSubCriteria(p.subCriteria.id, Quality(1.0)))
        )

        val profileRadioButton = <.td(css.colXs1, ^.textAlign.center,
          <.input.radio(
            ^.name := radioButtonGroupName,
            ^.checked := isProfile,
            ^.onChange --> updateSubCriteria(p.subCriteria.id, Profile))
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
              ^.onChange ==> updateSubCriteriaQualityWeightValue(p.subCriteria.id)
            ),
            <.div(css.expertInputGroupAddon, formattedWeightValue)
          )
        )

        val nameCollapsedOrNameExpandedWithIndicators = p.subCriteriaToggleState match {
          case Collapsed =>
            <.td(css.overflowHidden, ^.textOverflow.ellipsis, ^.paddingLeft := "20px", ^.colSpan := 4,
              <.div(^.display.inline, ^.onClick --> expandSubCriteria(p.subCriteria), rightGlyph),
              p.subCriteria.displayName
            )
          case Expanded =>
            <.td(css.overflowHidden, ^.textOverflow.ellipsis, ^.paddingLeft := "20px", ^.colSpan := 4,
              <.div(^.display.inline, ^.onClick --> collapseSubCriteria(p.subCriteria), downGlyph),
              p.subCriteria.displayName,
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

    private case class IndicatorListProps(
      indicators: Seq[Indicator],
      enabledIndicators: Set[IndicatorId]
    )

    private lazy val IndicatorList =
      ReactComponentB[IndicatorListProps]("IndicatorList")
        .render_P { p =>
          <.ul(css.expertIndicatorList,
            for (indicator <- p.indicators) yield {
              val isChecked = p.enabledIndicators.contains(indicator.id)
              <.li(^.whiteSpace.nowrap, ^.overflow.hidden, ^.textOverflow.ellipsis,
                <.input.checkbox(^.checked := isChecked, ^.onChange --> toggleIndicatorWeight(indicator.id)),
                " " + indicator.displayName
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
      val propsChanged = $.currentProps.analysis != $.nextProps.analysis || $.currentProps.expertConfig != $.nextProps.expertConfig
      val stateChanged = $.currentState != $.nextState
      propsChanged || stateChanged
    }
    .build

  def apply(proxy: ModelProxy[(Analysis, ExpertConfig)]): ReactComponentU[Props, State, Backend, TopNode] = {
    val (analysis, expertConfig) = proxy.value
    val dispatchCB = proxy.dispatchCB[Action] _
    component(Props(analysis, expertConfig, dispatchCB))
  }

}
