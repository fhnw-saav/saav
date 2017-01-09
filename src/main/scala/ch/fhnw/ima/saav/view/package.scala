package ch.fhnw.ima.saav

import ch.fhnw.ima.saav.style.GlobalStyles
import japgolly.scalajs.react.Callback

package object view {

  // expose familiar/simple names
  val jQuery = JQueryStatic
  val css = GlobalStyles

  def formatValue(value: Option[Double]): String =
    value.map(v => "%.2f".format(v)).getOrElse("-")

}
