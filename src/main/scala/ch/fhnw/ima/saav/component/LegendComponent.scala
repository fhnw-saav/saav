package ch.fhnw.ima.saav
package component

import ch.fhnw.ima.saav.controller.SaavController.{AutoColorizeAction, UpdateEntityColorAction, UpdateEntityPinningAction, UpdateEntitySelectionAction}
import ch.fhnw.ima.saav.model.app.PlottableEntity
import ch.fhnw.ima.saav.model.color._
import diode.react.ModelProxy
import japgolly.scalajs.react.extra.components.TriStateCheckbox
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{ReactComponentB, _}
import org.scalajs.dom.raw.HTMLInputElement

import scala.language.postfixOps
import scalacss.ScalaCssReact._

object LegendComponent {

  case class Props(proxy: ModelProxy[Seq[PlottableEntity]], allSelectionState: TriStateCheckbox.State)

  class Backend($: BackendScope[Props, Unit]) {

    def autoColorize = {
      $.props >>= (_.proxy.dispatch(AutoColorizeAction()))
    }

    def updateEntityColor(plottableEntity: PlottableEntity)(e: SyntheticEvent[HTMLInputElement]) = {
      val newColor = WebColor(e.target.value)
      $.props >>= (_.proxy.dispatch(UpdateEntityColorAction(plottableEntity.id, newColor)))
    }

    def toggleEntitySelection(plottableEntity: PlottableEntity) = {
      $.props >>= (_.proxy.dispatch(UpdateEntitySelectionAction(Set(plottableEntity.id), !plottableEntity.isSelected)))
    }

    def updateAllEntitySelections() = {
      $.props >>= { p =>
        val newSelected = p.allSelectionState match {
          case TriStateCheckbox.Checked => false
          case _ => true
        }
        p.proxy.dispatch(UpdateEntitySelectionAction(p.proxy.value.map(_.id).toSet, isSelected = newSelected))
      }
    }

    def toggleEntityPinning(plottableEntity: PlottableEntity)(e: ReactEvent) = {
      // only control pinning if the click happens in a blank table row area (i.e NOT on the checkbox, color widget)
      if (e.target.isInstanceOf[HTMLInputElement]) {
        Callback.empty
      } else {
        $.props >>= { p =>
          val pinnedOrNone = if (plottableEntity.isPinned) None else Some(plottableEntity.id)
          p.proxy.dispatch(UpdateEntityPinningAction(pinnedOrNone))
        }
      }
    }

    val pinGlyph = <.i(css.glyph.pin, ^.title := "Pin")

    def header(entities: Seq[PlottableEntity], allSelectionState: TriStateCheckbox.State) = {

      val autoColorizeGlyph = <.i(
        css.glyph.magic,
        ^.cursor.pointer,
        ^.onClick --> autoColorize,
        ^.title := "Auto-Colorize")

      <.tr(
        <.th("#"),
        <.th("Name"),
        <.th(allCheckbox(allSelectionState)),
        <.th(^.textAlign.center, autoColorizeGlyph),
        <.th(^.textAlign.center, pinGlyph)
      )
    }

    def createRow(entity: PlottableEntity, index: Int) = {

      val selectionStyle = if (entity.isSelected) css.empty else css.textMuted
      val pinStyle = if (entity.isPinned) css.active else css.empty
      val cursor = if (entity.isSelected) ^.cursor.pointer else EmptyTag
      val togglePinOnClick = if (entity.isSelected) ^.onClick ==> toggleEntityPinning(entity) else EmptyTag

      <.tr(selectionStyle, pinStyle, cursor, togglePinOnClick,
        <.th(^.scope := "row", index + 1),
        <.td(entity.name),
        <.td(checkbox(entity)),
        <.td(^.textAlign.center, colorPicker(entity)),
        <.td(^.textAlign.center, if (entity.isPinned) pinGlyph else EmptyTag)
      )
    }

    def allCheckbox(allSelection: TriStateCheckbox.State) = {
      TriStateCheckbox.Component(TriStateCheckbox.Props(allSelection, updateAllEntitySelections()))
    }

    def checkbox(entity: PlottableEntity) = {
      <.input.checkbox(^.checked := entity.isSelected, ^.onChange --> toggleEntitySelection(entity))
    }

    def colorPicker(entity: PlottableEntity) = {
      <.input.color(
        ^.value := (if (entity.isSelected) entity.color else DisabledColor).hexValue,
        ^.disabled := !entity.isSelected,
        ^.onChange ==> updateEntityColor(entity))
    }

    def render(p: Props) = {

      val entities = p.proxy.value
      val rows = entities.zipWithIndex.map {
        case (e, i) => createRow(e, i)
      }

      <.table(css.legendTable,
        <.thead(header(entities, p.allSelectionState)),
        <.tbody(rows))
    }

  }

  private final val component = ReactComponentB[Props](LegendComponent.getClass.getSimpleName)
    .renderBackend[Backend]
    .build

  def apply(proxy: ModelProxy[Seq[PlottableEntity]]) = {

    val allSelectionState = proxy.value.map(_.isSelected).distinct match {
      case Seq(true) => TriStateCheckbox.Checked
      case Seq(false) => TriStateCheckbox.Unchecked
      case _ => TriStateCheckbox.Indeterminate
    }

    val props = Props(proxy, allSelectionState)
    component(props)
  }

}
