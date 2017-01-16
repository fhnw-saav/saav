package ch.fhnw.ima.saav

import ch.fhnw.ima.saav.circuit.SaavCircuit
import ch.fhnw.ima.saav.style.GlobalStyles
import ch.fhnw.ima.saav.view.css
import ch.fhnw.ima.saav.view.pages._
import japgolly.scalajs.react.ReactDOM
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom._

import scala.scalajs.js
import scalacss.Defaults._
import scalacss.ScalaCssReact._

object MainApp extends js.JSApp {

  def main(): Unit = {

    // add CSS
    GlobalStyles.addToDocument()

    // create page
    val modelConnection = new SaavCircuit().connect(m => m)
    val page = modelConnection(proxy => AnalysisPageComponent("Example", getConfigFileUrl, proxy))

    // embed page in bootstrap container
    val container = <.div(css.saavContainer, page)

    // render into DOM
    ReactDOM.render(container, document.getElementById("root"))

  }

}
