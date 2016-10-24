package ch.fhnw.ima.saav
package model

object color {

  // TODO: Replace when https://github.com/scala-js/scala-js-dom/pull/243 is released
  final case class WebColor(hexValue: String)

  val ColorPalette = Seq(
    WebColor("#a6cee3"),
    WebColor("#1f78b4"),
    WebColor("#b2df8a"),
    WebColor("#33a02c"),
    WebColor("#fb9a99"),
    WebColor("#e31a1c"),
    WebColor("#fdbf6f"),
    WebColor("#ff7f00"),
    WebColor("#cab2d6"),
    WebColor("#6a3d9a"),
    WebColor("#ffff99"),
    WebColor("#b15928")
  )

  val DisabledColor = WebColor("#777777")

}