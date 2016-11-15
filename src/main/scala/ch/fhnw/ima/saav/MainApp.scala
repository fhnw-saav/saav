package ch.fhnw.ima.saav

import ch.fhnw.ima.saav.component.pages.Page.HomePage
import ch.fhnw.ima.saav.component.pages._
import ch.fhnw.ima.saav.controller.SaavCircuit
import ch.fhnw.ima.saav.style.GlobalStyles
import japgolly.scalajs.react.ReactDOM
import japgolly.scalajs.react.extra.router.{Resolution, Router, _}
import japgolly.scalajs.react.vdom.ReactTagOf
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.html.Div

import scala.scalajs.js
import scalacss.Defaults._
import scalacss.ScalaCssReact._

object MainApp extends js.JSApp {

  // the part of the URL that comes before the # separator
  val baseUrl = BaseUrl(dom.window.location.href.takeWhile(_ != '#'))

  // configures how URLs map to components
  val routerConfig: RouterConfig[Page] = RouterConfigDsl[Page].buildConfig { dsl =>
    import dsl._

    // re-usable connections to controller
    val modelConnection = new SaavCircuit().connect(m => m)

    // defines how hash-prefixed locations are mapped to a rendered component
    def routeSubPage(subPage: SubPage): Rule = staticRoute("#/" + subPage.hashLink, subPage) ~> renderR(_ => {
      subPage match {
        case _ => modelConnection(proxy => AnalysisPageComponent(subPage.displayName, subPage.configFileUrl, proxy))
      }
    })

    val homePageRoute = staticRoute(root, HomePage) ~> renderR(_ => HomePageComponent())
    val subPageRoutes = Page.subPages.map(routeSubPage).reduce(_ | _)

    val defaultRedirect = redirectToPage(Page.HomePage)(Redirect.Replace)
    (homePageRoute | subPageRoutes).notFound(defaultRedirect)

  }.renderWith(layout)

  // base layout for all pages
  private def layout(ctl: RouterCtl[Page], r: Resolution[Page]): ReactTagOf[Div] = {

    // globally defined CSS styles
    val css = GlobalStyles

    // our main container
    <.div(css.saavContainer, r.render())
  }

  def main(): Unit = {
    // create stylesheet
    GlobalStyles.addToDocument()
    // create the router component (which knows what to render based on defined rules > see routerConfig above)
    val router = Router(baseUrl, routerConfig.logToConsole)()
    // actually render the router component
    ReactDOM.render(router, document.getElementById("root"))
  }

}
