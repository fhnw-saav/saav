package ch.fhnw.ima.saav
package style

import scala.language.postfixOps
import scalacss.Defaults._

/**
  * Defines CSS styles that can be referenced from Scala.js React tags.
  * The actual CSS will be inlined into the index.html document.
  * Motivation: If we ever switch from Bootstrap to e.g. Materialize, this should be
  * the only place that requires CSS changes...
  */
object GlobalStyles extends StyleSheet.Inline {

  import dsl._

  def styleWrap(classNames: String*) = style(addClassNames(classNames: _*))

  // a bootstrap container with customizations
  val container = style(
    addClassNames("container"),
    paddingTop(20 px)
  )

  // bootstrap's button-style anchors
  val mainLinkButton = styleWrap("btn", "btn-primary", "btn-lg", "btn-block", "active")

  val defaultButton = styleWrap("btn btn-default")

  // bootstrap's info-style alert box
  val infoBox = styleWrap("alert", "alert-info")

  val pullRight = styleWrap("pull-right")

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

  // wrap styles in a namespace, assign to val to prevent lazy initialization
  object modal {
    val modal = styleWrap("modal")
    val fade = styleWrap("fade")
    val dialog = styleWrap("modal-dialog")
    val content = styleWrap("modal-content")
    val header = styleWrap("modal-header")
    val body = styleWrap("modal-body")
    val footer = styleWrap("modal-footer")
  }

  val _modal = modal

  object form {
    val group = styleWrap("form-group")
    val control = styleWrap("form-control")
  }

  val _form = form

}
