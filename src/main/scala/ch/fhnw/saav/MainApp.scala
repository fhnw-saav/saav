package ch.fhnw.saav

import ch.fhnw.saav.component.{CChart, CTodo}
import ch.fhnw.saav.style.GlobalStyles
import japgolly.scalajs.react.extra.router.{Resolution, Router, _}
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{ReactDOM, _}
import org.scalajs.dom
import org.scalajs.dom._

import scalacss.Defaults._
import scalacss.ScalaCssReact._
import scala.scalajs.js

object MainApp extends js.JSApp {

  // the part of the URL that comes before the # separator
  val baseUrl = BaseUrl(dom.window.location.href.takeWhile(_ != '#'))

  // abstracts a URL hash (i.e. the part that comes after the # separator)
  sealed abstract class Location(val displayName: String, val link: String)

  // currently supported URL hashes
  object Location {

    object ProjectLocation extends Location("Project Application", "project")
    object AppointmentLocation extends Location("Academic Appointment", "appointment")
    object ResearchLocation extends Location("Research Group/Institution", "research")

    def values = List[Location](ProjectLocation, AppointmentLocation, ResearchLocation)
  }

  // configures how URLs map to components
  val routerConfig: RouterConfig[Location] = RouterConfigDsl[Location].buildConfig { dsl =>
    import dsl._

    // defines how a location is mapped to a rendered component
    def filterRoute(loc: Location): Rule = staticRoute("#/" + loc.link, loc) ~> renderR(ctl => {
      // create a placeholder component until we have useful UIs
      val c = ReactComponentB[Unit]("All")
        .render($ => <.div(
          CTodo(loc.displayName),
          CChart()
        ))
        .build
      c()
    })

    // establish filter routes for all known locations
    val filterRoutes: Rule = Location.values.map(filterRoute).reduce(_ | _)
    // register a fall-back location
    filterRoutes.notFound(redirectToPage(Location.ProjectLocation)(Redirect.Replace))

  }.renderWith(layout)

  // base layout for all pages
  def layout(ctl: RouterCtl[Location], r: Resolution[Location]) = {

    // globally defined CSS styles
    val css = GlobalStyles

    // helper method to control activity of bootstrap tabs
    def activeIf(loc: Location) = if (r.page == loc) css.className.active else ""

    // create one bootstrap tab for each location
    val tabs = for (loc <- Location.values) yield
      <.li(^.className := activeIf(loc),
        <.a(^.href := "#/" + loc.link, loc.displayName))

    // our main container, containing...
    <.div(css.container,

      // a tab navigation...
      <.ul(css.tabbedNavigation, tabs),

      // .. and the contents of the currently active tab (as determined by the router)
      <.div(css.mainTab, r.render())
    )
  }

  def main() = {
    // create stylesheet
    GlobalStyles.addToDocument()
    // create the router component (which knows what to render based on defined rules > see routerConfig above)
    val router = Router(baseUrl, routerConfig.logToConsole)()
    // actually render the router component
    ReactDOM.render(router, document.getElementById("root"))
  }

}
