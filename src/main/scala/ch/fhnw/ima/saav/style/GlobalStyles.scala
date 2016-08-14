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
  val saavContainer = style(
    addClassNames("container-fluid"),
    paddingTop(20 px)
  )

  // bootstrap's button-style anchors
  val mainLinkButton = styleWrap("btn", "btn-primary", "btn-lg", "btn-block", "active")

  val defaultButton = styleWrap("btn btn-default")

  // bootstrap's info-style alert box
  val infoBox = styleWrap("alert", "alert-info")

  val pullRight = styleWrap("pull-right")

  val row = styleWrap("row")

  val colXs2 = styleWrap("col-xs-2")
  val colXs3 = styleWrap("col-xs-3")
  val colXs9 = styleWrap("col-xs-9")
  val colXs10 = styleWrap("col-xs-10")
  val colXs12 = styleWrap("col-xs-12")

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

  object modal {
    val modal = styleWrap("modal")
    val fade = styleWrap("fade")
    val dialog = styleWrap("modal-dialog")
    val content = styleWrap("modal-content")
    val header = styleWrap("modal-header")
    val body = styleWrap("modal-body")
    val footer = styleWrap("modal-footer")
  }

  val _modal = modal // prevent lazy initialization

  object form {
    val group = styleWrap("form-group")
    val control = styleWrap("form-control")
  }

  val _form = form

  val legendTable = styleWrap("table table-hover")

  object glyph {
    val magic = styleWrap("fa fa-fw fa-magic")
  }

  val _glyph = glyph // prevent lazy initialization

  val vSpaced = style(
    marginTop(10 px),
    marginBottom(10 px)
  )

}
