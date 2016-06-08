package ch.fhnw.saav.styles

import scalacss.Defaults._

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
    paddingTop(20.px)
  )

  // bootstrap's tabbed navigation
  val tabbedNavigation = style(
    addClassNames("nav nav-tabs")
  )

  // our own custom styling for the main tabs
  val mainTab = style(
    paddingTop(20.px)
  )

  // bootstrap's info-style alert box
  val infoBox = style(
    addClassNames("alert", "alert-info")
  )

  // Simple constants to avoid literal CSS classes in code
  // TODO: Investigate whether scalacss DSL supports conditional classes
  object className {
    val active = "active"
  }

}
