package ch.fhnw.ima.saav.style

import scalacss.Defaults._
import scala.language.postfixOps

/**
  * Defines CSS styles that can be referenced from Scala.js React tags.
  * The actual CSS will be inlined into the index.html document.
  * Motivation: If we ever switch from Bootstrap to e.g. Materialize, this should be
  * the only place that requires CSS changes...
  */
object GlobalStyles extends StyleSheet.Inline {

  import dsl._

  // a bootstrap container with customizations
  val container = style(
    addClassNames("container"),
    paddingTop(20 px)
  )

  // bootstrap's button-style anchors
  val mainLinkButton = style(
    addClassNames("btn", "btn-primary", "btn-lg", "btn-block", "active")
  )

  val defaultButton = style(
    addClassName("btn btn-default")
  )

  // bootstrap's info-style alert box
  val infoBox = style(
    addClassNames("alert", "alert-info")
  )

  val pullRight = style(
    addClassNames("pull-right")
  )

  // SVG responsiveness (http://stackoverflow.com/questions/16265123/resize-svg-when-window-is-resized-in-d3-js)
  val svgContainer = style(
    display.inlineBlock,
    position.relative,
    width(100 %%),
    paddingBottom(100 %%),
    verticalAlign.top,
    overflow.hidden
  )

  val svgContentResponsive = style(
    display.inlineBlock,
    position.absolute,
    top(10 px),
    left(0 px)
  )

  val barChartBarRect = style(
    svgFill := c"#286090" // bootstrap blue
  )

  val barChartAxis = style {
    unsafeChild("path.domain")(
      svgFill := none,
      svgStroke(c"#fff")
    )
  }

  val barChartValueLabel = style {
    svgFill := "white"
  }

  // our own custom styling for the file upload drop zone
  val fileDropZone = style(
    border(2 px, dashed, c"#bbb"),
    borderRadius(5 px),
    marginTop(20 px),
    padding(25 px),
    textAlign.center,
    color(c"#bbb")
  )

  val hidden = style(
    display.none
  )

}
