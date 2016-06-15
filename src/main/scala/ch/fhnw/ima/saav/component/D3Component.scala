package ch.fhnw.ima.saav.component

import ch.fhnw.ima.saav.model.model.Analysis
import ch.fhnw.ima.saav.model.model.Entity.Project
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{BackendScope, ReactComponentB}
import org.scalajs.dom.raw.HTMLDivElement
import org.singlespaced.d3js.d3

import scala.scalajs.js

/**
  * A component which uses D3.js to append content to a DOM element.
  *
  * Accessing the DOM element with React is a bit problematic, since React builds a virtual DOM and
  * updates the real DOM behind the scenes. Therefore the real DOM element does not exist at the time of the `render`
  * function call. All D3 magic thus needs to be in the `componentDidMount` function, which is called after the real
  * DOM has been updated.
  */
object D3Component {

  case class State(node: Option[HTMLDivElement])

  case class Props(model: Option[Analysis[Project]])

  class Backend($: BackendScope[Props, State]) {
    def render() = <.div()
  }

  private val component = ReactComponentB[Props](D3Component.getClass.getSimpleName)
    .initialState(State(None))
    .renderBackend[Backend]
    .domType[HTMLDivElement]
    .componentDidMount(scope => {
      val div = scope.getDOMNode()
      scope.modState(s => State(Some(div)))
    })
    .shouldComponentUpdate(scope => {
      scope.nextState.node match {
        case Some(node) => scope.nextProps.model.foreach(appendContents(node, _))
        case _ =>
      }
      false
    })
    .build

  def appendContents(node: HTMLDivElement, analysis: Analysis[Project]): Unit = {
    val date = new js.Date()
    val now = Seq(
      date.getHours(),
      date.getMinutes(),
      date.getSeconds()
    ).mkString(":")

    d3.select(node).text(s"Loaded ${analysis.entities.size} project(s) @ $now")
  }

  def apply(model: Option[Analysis[Project]]) = component(Props(model))

}
