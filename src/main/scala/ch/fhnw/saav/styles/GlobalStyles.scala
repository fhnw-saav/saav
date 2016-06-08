package ch.fhnw.saav.styles

import ch.fhnw.saav.styles.GlobalStyles.cssClassNames.CssClassName

import scalacss.Defaults._
import scalacss.DslBase.ToStyle

object GlobalStyles extends StyleSheet.Inline {

  // simple constants for all used CSS classes
  object cssClassNames {

    type CssClassName = String

    // bootstrap-specific
    val container = "container"
    val tabs = "nav nav-tabs"
    val active = "active"
    val alert = "alert"
    val alertInfo = "alert-info"
    val alertInfoBox = s"$alert $alertInfo"

    // saav-specific
    val tabContents = "tab-contents"
  }

  // programmatically create a CSS (will be inlined into index.html document)

  import dsl._

  selector(cssClassNames.container)(
    paddingTop(20.px)
  )

  selector(cssClassNames.tabContents)(
    paddingTop(20.px)
  )

  def selector(cssClassName: CssClassName)(t: ToStyle*) = style(unsafeRoot("." + cssClassName)(t: _*))

}
