package ch.fhnw.ima.saav.style

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

  // our own custom styling for the file upload drop zone
  val fileDropZone = style(
    border(2.px, dashed, c"#bbb"),
    borderRadius(5.px),
    padding(25.px),
    textAlign.center,
    color(c"#bbb")
  )

  // Simple constants to avoid literal CSS classes in code
  // TODO: Investigate whether scalacss DSL supports conditional classes
  object className {
    val active = "active"
  }

}
