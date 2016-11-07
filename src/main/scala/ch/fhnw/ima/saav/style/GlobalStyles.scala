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

  val activeTab = style(
    addClassName("btn"),
    addClassName("btn-primary"),
    marginRight(5 px)
  )

  val inactiveTab = style(
    addClassName("btn"),
    addClassName("btn-default"),
    marginRight(5 px)
  )

  // bootstrap's button-style anchors
  val mainLinkButton = styleWrap("btn", "btn-primary", "btn-lg", "btn-block", "active")

  val defaultButton = styleWrap("btn btn-default")

  // bootstrap's info-style alert box
  val infoBox = styleWrap("alert", "alert-info")

  val pullRight = styleWrap("pull-right")

  val row = styleWrap("row")

  val colXs1 = styleWrap("col-xs-1")
  val colXs2 = styleWrap("col-xs-2")
  val colXs3 = styleWrap("col-xs-3")
  val colXs4 = styleWrap("col-xs-4")
  val colXs5 = styleWrap("col-xs-5")
  val colXs6 = styleWrap("col-xs-6")
  val colXs7 = styleWrap("col-xs-7")
  val colXs8 = styleWrap("col-xs-8")
  val colXs9 = styleWrap("col-xs-9")
  val colXs10 = styleWrap("col-xs-10")
  val colXs11 = styleWrap("col-xs-11")
  val colXs12 = styleWrap("col-xs-12")

  val centerBlock = styleWrap("center-block")

  val boxed = style(
    border(2 px, c"#eee", solid),
    padding(10 px)
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

  val legendTable = style(
    addClassName("table"),
    width(100 %%),
    paddingRight(`0`),
    marginTop(10 px),
    unsafeChild("thead tr th")( // `All` / `None` actions

      // align with chart labels/boxes
      height(30 px),
      paddingTop(`0`),
      paddingRight(`0`),
      paddingBottom(10 px),
      paddingLeft(`0`),

      fontWeight.normal
    ),
    unsafeChild("tbody tr td")( // checkbox and color
      width(1 px),
      paddingTop(`0`),
      paddingRight(5 px),
      paddingBottom(`0`),
      paddingLeft(`0`)
    ),
    unsafeChild("tbody tr th")( // rank column
      textAlign.right,
      width(1 px),
      paddingLeft(5 px),
      paddingLeft(5 px),
      paddingTop(`0`),
      paddingBottom(`0`)
    )
  )

  val colorCell = style(
    width(20 px),
    height(20 px),
    backgroundColor.transparent,
    verticalAlign.middle,
    marginTop(`0`),
    marginRight(`0`),
    marginBottom(1 px),
    marginLeft(`0`),
    padding(`0`),
    border.none
  )

  val table = style(
    addClassName("table"),
    width(100 %%),
    paddingRight(`0`),
    marginTop(10 px)
  )

  val expertConfigToolbar = style(
    addClassName("pull-right"),
    paddingTop(10 px),
    paddingBottom(10 px)
  )

  val expertConfigReset = style(
    display.inlineBlock,
    color(c"#d9534f")
  )

  val expertIndicatorList = style(
    addClassName("list-unstyled"),
    marginLeft(18 px) // align with font awesome fixed-width glyph
  )

  val expertFormControl = style(
    addClassName("form-control"),
    borderColor.transparent,
    paddingLeft.`0`,
    paddingRight.`0`
  )

  val expertInputGroupAddon = style(
    addClassName("input-group-addon"),
    backgroundColor.transparent,
    border.none
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

  var overflowHidden = style(
    maxWidth.`0`,
    overflow.hidden,
    whiteSpace.nowrap
  )

  object glyph {
    val right = styleWrap("fa fa-fw fa-chevron-right")
    val down = styleWrap("fa fa-fw fa-chevron-down")
  }

  val _glyph = glyph // prevent lazy initialization

  val vSpaced = style(
    marginTop(10 px),
    marginBottom(10 px)
  )

  val hSpaced = style(
    marginLeft(5 px)
  )

  val textMuted = styleWrap("text-muted")

  val empty = styleWrap("")

  val active = styleWrap("active")
  val nonActive = empty

  val bgPrimary = styleWrap("bg-primary")
  val bgInfo = styleWrap("bg-info")


}
