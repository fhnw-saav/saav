package ch.fhnw.saav

import ch.fhnw.saav.components.CTodo
import japgolly.scalajs.react.{ReactDOM, _}
import japgolly.scalajs.react.extra.router.{Resolution, Router, _}
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom
import org.scalajs.dom._

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
      CTodo(loc.displayName)
    })

    // establish filter routes for all known locations
    val filterRoutes: Rule = Location.values.map(filterRoute).reduce(_ | _)
    // register a fall-back location
    filterRoutes.notFound(redirectToPage(Location.ProjectLocation)(Redirect.Replace))

  }.renderWith(layout)

  // base layout for all pages
  def layout(ctl: RouterCtl[Location], r: Resolution[Location]) = {

    // helper method to control activity of bootstrap tabs
    def activeIf(loc: Location) = if (r.page == loc) "active" else ""

    // create one bootstrap tab for each location
    val tabs = for (loc <- Location.values) yield
      <.li(^.role := "presentation", ^.className := activeIf(loc), <.a(^.href := "#/" + loc.link, loc.displayName))

    // our main container, containing...
    <.div(^.className := "container",

      // a navigation...
      <.ul(^.className := "nav nav-tabs",
        tabs
      ),

      // .. and the contents of the tab (as determined by the router)
      <.div(^.className := "container tab-contents",r.render())
    )
  }

  // the router is itself a component -> it knows what to render based on defined rules (see routerConfig above)
  val router: ReactComponentU[Unit, Resolution[Location], Any, TopNode] =
    Router(baseUrl, routerConfig.logToConsole)()

  def main() = {
    // actually render the router component
    ReactDOM.render(router, document.getElementById("root"))
  }

}
