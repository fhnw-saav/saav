package ch.fhnw.ima.saav
package component

import ch.fhnw.ima.saav.model.DataModel
import ch.fhnw.ima.saav.model.colors.WebColor
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{BackendScope, ReactComponentB}
import org.scalajs.dom.raw.HTMLDivElement
import org.singlespaced.d3js.Ops._
import org.singlespaced.d3js.d3

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scalacss.ScalaCssReact._

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

  case class Props(dataModel: DataModel)

  class Backend($: BackendScope[Props, State]) {
    def render() = <.div(css.svgContainer)
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
        case Some(node) =>
          // delete complete svg if already present
          d3.select(node).select("svg").remove()
          // render d3 (will append new svg)
          val model = scope.nextProps.dataModel
          if (model.selectedEntities.nonEmpty) {
            appendContents(node, model)
          }
        case _ =>
      }
      false // never need to re-render (directly manipulating DOM via D3)
    })
    .build

  def appendContents(node: HTMLDivElement, dataModel: DataModel): Unit = {

    val svgWidth = 1000
    val svgHeight = 400

    val paddingTop = 50
    val paddingBottom = 30

    val barPaddingFraction = 0.1
    val valueLabelOffsetY = 20d
    val pinningIndicatorOffsetY = 20

    val maxWidth = svgWidth
    val maxHeight = svgHeight - paddingTop - paddingBottom

    // an individual data item -> defines one bar
    case class Datum(name: String, median: Double, color: WebColor, isPinned: Boolean)

    // create data items from analysis model
    val data = dataModel.selectedEntities.map { entity =>
      val median = dataModel.analysis.groupedValue(entity)
      Datum(entity.name, median.getOrElse(Double.NaN), dataModel.colors(entity), dataModel.pinnedEntity.contains(entity))
    }.toJSArray

    // how data items map to pixel coordinates
    val scaleX = d3.scale.ordinal().domain(data.map(_.name)).rangeRoundBands((0d, maxWidth.toDouble), barPaddingFraction, 0)
    val scaleY = d3.scale.linear().domain(js.Array(0, data.map(_.median).max)).range(js.Array(maxHeight, 0))

    val axisX = d3.svg.axis().orient("bottom").scale(scaleX)

    def barTranslateX(offset: Double) = (d: Datum) => s"translate(${offset + scaleX(d.name)}, 0)"
    val barWidth: Double = scaleX.rangeBand()

    val svg = d3
      .select(node)
      .append("svg")
      .attr("class", css.svgContentResponsive.htmlClass)
      .attr("preserveAspectRatio", "xMinYMin meet")
      .attr("viewBox", s"0 0 $svgWidth $svgHeight")

    val chart = svg.append("g")
      .attr("transform", s"translate(0, $paddingTop)")

    // x axis
    chart.append("g")
      .attr("class", css.barChartAxis.htmlClass)
      .attr("transform", s"translate (0, $maxHeight)")
      .call(axisX)

    // one bar for each data item
    val bars = chart
      .selectAll("g.bars")
      .data(data)
      .enter()

    // bar rectangles
    bars
      .append("g")
      .attr("transform", barTranslateX(0))
      .append("rect")
      .attr("fill", (d: Datum) => d.color.hexValue)
      .attr("width", barWidth)
      .attr("y", (d: Datum) => scaleY(d.median))
      .attr("height", (d: Datum) => maxHeight - scaleY(d.median))

    // bar value labels
    bars
      .append("text")
      .attr("class", css.barChartValueLabel.htmlClass)
      .text((d: Datum) => s"${d.median}")
      .attr("transform", barTranslateX(barWidth / 2))
      .attr("text-anchor", "middle")
      .attr("y", (d: Datum) => scaleY(d.median) + valueLabelOffsetY)

    // pinning
    bars
      .append("text")
      .attr("class", css.barChartValueLabel.htmlClass)
      .text((d: Datum) => if (d.isPinned) "*" else "")
      .attr("transform", barTranslateX(barWidth / 2))
      .attr("text-anchor", "middle")
      .attr("y", (d: Datum) => scaleY(d.median) + valueLabelOffsetY + pinningIndicatorOffsetY)

  }

  def apply(dataModel: DataModel) = component(Props(dataModel))

}
