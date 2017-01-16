package ch.fhnw.ima.saav
package view

import ch.fhnw.ima.saav.view.bootstrap.DismissibleWarning
import ch.fhnw.ima.saav.circuit.UpdateVisibility
import ch.fhnw.ima.saav.model.app._
import ch.fhnw.ima.saav.model.config.ConfigMismatch
import diode.react.ModelProxy
import japgolly.scalajs.react.vdom.ReactTagOf
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{BackendScope, ReactComponentB, ReactComponentU, TopNode}
import org.scalajs.dom.html.Div

import scalacss.ScalaCssReact._

object pages {

  object AnalysisPageComponent {

    case class Props(configFileUrl: Option[String], proxy: ModelProxy[SaavModel])

    private val component = ReactComponentB[Props](AnalysisPageComponent.getClass.getSimpleName)
      .render_P(p => {

        val (showWarning, title, content): (Boolean, String, TagMod) = p.proxy.value.model match {
          case Left(noData) => (
            false,
            s"Import",
            FileImportComponent(p.configFileUrl, p.proxy.zoom(_ => noData))
          )
          case Right(data) => (
            data.config.mismatch.missingIndicators.nonEmpty || data.config.mismatch.unexpectedIndicators.nonEmpty,
            data.config.title,
            PageWithDataComponent(data.config.title, p.proxy.zoom(_ => data))
          )
        }

        if (showWarning) {
          val warning = DismissibleWarning("Imported data does not match expected analysis structure (console log for details)")
          <.div(warning, <.h3(title), content)
        } else {
          <.div(<.h3(title), content)
        }

      })
      .build

    def apply(title: String, configFileUrl: Option[String], proxy: ModelProxy[SaavModel]): ReactComponentU[Props, Unit, Unit, TopNode] = component(Props(configFileUrl, proxy))

  }

  object PageWithDataComponent {

    case class Props(title: String, proxy: ModelProxy[AppModel])

    case class State(activeTab: Tab)

    sealed trait Tab {
      def name: String
    }

    case object QualityTab extends Tab {
      val name = "Quality"
    }

    case object ProfileTab extends Tab {
      val name = "Profile"
    }

    class Backend($: BackendScope[Props, State]) {

      private def updateExpertConfigVisibility(visibility: ExpertConfigVisibility) =
        $.props >>= { p =>
          p.proxy.dispatchCB(UpdateVisibility(visibility))
        }

      def render(p: Props, s: State): ReactTagOf[Div] = {
        <.div(
          for (tab <- Seq(QualityTab, ProfileTab)) yield {
            val style = if (s.activeTab == tab) css.activeTab else css.inactiveTab
            <.div(style, ^.cursor.pointer, ^.onClick --> $.modState(s => s.copy(activeTab = tab)), tab.name)
          },
          <.div(css.pullRight,
            ExpertConfigResetComponent(p.proxy.zoom(_.expertConfig)), {
              val defaultTitle = if (p.title.isEmpty) s.activeTab.name else p.title + " | " + s.activeTab.name
              <.div(css.hSpaced, ^.display.`inline-block`, PdfExportComponent(ChartComponent.ElementId, defaultTitle, s.activeTab, p.proxy))
            }
          ),
          <.div(
            s.activeTab match {
              case QualityTab => <.div(css.row, css.vSpaced,
                <.div(css.colXs2, LegendComponent(p.proxy, _.qualityModel.rankedEntities)),
                <.div(css.colXs1, VisualRankingComponent(p.proxy)),
                <.div(css.colXs9, QualityChartComponent(p.proxy))
              )
              case ProfileTab => <.div(css.row, css.vSpaced,
                <.div(css.colXs3, LegendComponent(p.proxy, _.profileModel.sortedEntities, showRank = false)),
                <.div(css.colXs9, ProfileChartComponent(p.proxy))
              )
            },
            <.div(css.row,
              <.div(css.colXs12, IndicatorComponent(p.proxy))
            ),
            <.div(css.row, {
              val expertConfigVisibility = p.proxy.value.expertConfig.visibility

              def switchButton(newVisibility: ExpertConfigVisibility, prefix: String) = {
                val displayName = s"$prefix ${ExpertConfigComponent.Title}"
                <.div(css.colXs12, css.vSpaced,
                  <.div(css.defaultButton, ^.onClick --> updateExpertConfigVisibility(newVisibility), displayName)
                )
              }

              expertConfigVisibility match {
                case ExpertConfigHidden => switchButton(ExpertConfigVisible, "Show")
                case ExpertConfigVisible => Seq(
                  switchButton(ExpertConfigHidden, "Hide"),
                  <.div(css.colXs12, ExpertConfigComponent(p.proxy.zoom(m => (m.analysis, m.expertConfig))))
                )
              }

            }
            )
          )
        )
      }
    }

    private val component = ReactComponentB[Props](PageWithDataComponent.getClass.getSimpleName)
      .initialState(State(activeTab = QualityTab))
      .renderBackend[Backend]
      .build

    def apply(title: String, proxy: ModelProxy[AppModel]): ReactComponentU[Props, State, Backend, TopNode] = component(Props(title, proxy))

  }

}
