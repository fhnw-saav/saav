package ch.fhnw.ima.saav

import ch.fhnw.ima.saav.style.GlobalStyles
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.annotation.JSName

package object component {

  // expose familiar/simple names
  val jQuery = JQueryStatic
  val css = GlobalStyles

  @JSName("URL")
  @js.native
  object URL extends dom.URL

}
