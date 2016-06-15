package ch.fhnw.ima.saav.component

import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{Callback, ReactComponentB}
import org.scalajs.dom.raw.HTMLDivElement
import org.singlespaced.d3js.Ops._
import org.singlespaced.d3js.d3

import scala.scalajs.js

/**
  * A component which uses D3.js to append an SVG tree to a DOM element.
  *
  * Accessing the DOM element with React is a bit problematic, since React builds a virtual DOM and
  * updates the real DOM behind the scenes. Therefore the real DOM element does not exist at the time of the `render`
  * function call. All D3 magic thus needs to be in the `componentDidMount` function, which is called after the real
  * DOM has been updated.
  */
object D3Component {

  private val component = ReactComponentB[Unit](D3Component.getClass.getSimpleName)
    .render($ => <.div())
    .domType[HTMLDivElement]
    .componentDidMount(scope => Callback {
      val div = scope.getDOMNode()
      appendBarChartTo(div)
    })
    .build

  // https://github.com/spaced/scala-js-d3-example-app/blob/master/src/main/scala/example/ScalaJSExample.scala
  def appendBarChartTo(node: HTMLDivElement): Unit = {
    val graphHeight = 450
    val barWidth = 80
    val barSeparation = 10
    val maxData = 50
    val horizontalBarDistance = barWidth + barSeparation
    val barHeightMultiplier = graphHeight / maxData
    val c = d3.rgb("DarkSlateBlue")

    val rectXFun = (d: Int, i: Int) => i * horizontalBarDistance
    val rectYFun = (d: Int) => graphHeight - d * barHeightMultiplier
    val rectHeightFun = (d: Int) => d * barHeightMultiplier
    val rectColorFun = (d: Int, i: Int) => c.brighter(i * 0.5).toString

    val svg = d3.select(node).append("svg").attr("width", "100%").attr("height", "450px")
    val sel = svg.selectAll("rect").data(js.Array(8, 22, 31, 36, 48, 17, 25))
    sel.enter()
      .append("rect")
      .attr("x", rectXFun)
      .attr("y", rectYFun)
      .attr("width", barWidth)
      .attr("height", rectHeightFun)
      .style("fill", rectColorFun)
  }

  def apply() = component()

}
