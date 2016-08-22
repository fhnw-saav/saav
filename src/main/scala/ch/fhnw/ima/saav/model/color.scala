package ch.fhnw.ima.saav
package model

import ch.fhnw.ima.saav.model.app.PlottableEntity

object color {

  // org.scalajs.dom.ext.Color doesn't support hex value access
  final case class WebColor(hexValue: String)

  val SolarizedPalette = Seq(
    WebColor("#b58900"),
    WebColor("#cb4b16"),
    WebColor("#dc322f"),
    WebColor("#d33682"),
    WebColor("#6c71c4"),
    WebColor("#268bd2"),
    WebColor("#2aa198"),
    WebColor("#859900")
  )

  val DefaultColor = WebColor("#000000")

  val DisabledColor = WebColor("#777777")

  def autoColorMap(entities: Seq[PlottableEntity]): Map[PlottableEntity, WebColor] =
    entities.zipWithIndex.map {
      case (e, i) => (e, SolarizedPalette(i % SolarizedPalette.size))
    }.toMap.withDefaultValue(color.DefaultColor)

}
