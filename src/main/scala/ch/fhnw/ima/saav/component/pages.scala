package ch.fhnw.ima.saav
package component

import ch.fhnw.ima.saav.controller.UpdateVisibility
import ch.fhnw.ima.saav.model.app._
import diode.react.ModelProxy
import japgolly.scalajs.react.vdom.ReactTagOf
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{BackendScope, ReactComponentB, ReactComponentU, TopNode}
import org.scalajs.dom.html.Div

import scalacss.ScalaCssReact._

object pages {

  // abstracts a page (at least conceptually -> this is a SPA)
  trait Page {
    def displayName: String
  }

  // abstracts a sub-page identifiable via URL hash (i.e. the part that comes after the # separator)
  trait SubPage extends Page {
    def hashLink: String
  }

  object Page {

    case object HomePage extends Page {
      override def displayName = "Home"
    }

    case object ProjectAnalysisPage extends SubPage {

      def displayName = "Projects"

      def hashLink = "projects"

    }

    case object PersonAnalysisPage extends SubPage {

      def displayName = "Persons"

      def hashLink = "persons"

    }

    case object OrganisationAnalysisPage extends SubPage {

      def displayName = "Organisations"

      def hashLink = "organisations"

    }

    def subPages: List[SubPage] = List[SubPage](ProjectAnalysisPage, PersonAnalysisPage, OrganisationAnalysisPage)
  }

  object HomePageComponent {

    private val component = ReactComponentB[Unit](HomePageComponent.getClass.getSimpleName)
      .render(_ => {

        def createButton(subPage: SubPage) =
          <.a(css.mainLinkButton, ^.role := "button", ^.href := "#/" + subPage.hashLink, subPage.displayName)

        <.div(Page.subPages.map(createButton))
      })
      .build

    def apply(): ReactComponentU[Unit, Unit, Unit, TopNode] = component()

  }

  object AnalysisPageComponent {

    case class Props(title: String, proxy: ModelProxy[SaavModel])

    private val component = ReactComponentB[Props](AnalysisPageComponent.getClass.getSimpleName)
      .render_P(p => {

        val (title, content): (String, TagMod) = p.proxy.value.model match {
          case Left(noData) => (s"Import ${p.title}", FileImportComponent(p.proxy.zoom(_ => noData)))
          case Right(data) => (p.title, PageWithDataComponent(p.proxy.zoom(_ => data)))
        }

        <.div(<.h3(title), content)

      })
      .build

    def apply(title: String, proxy: ModelProxy[SaavModel]): ReactComponentU[Props, Unit, Unit, TopNode] = component(Props(title, proxy))

  }

  object PageWithDataComponent {

    case class Props(proxy: ModelProxy[AppModel])

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
        $.props.flatMap(_.proxy.dispatch(UpdateVisibility(visibility)))

      def render(p: Props, s: State): ReactTagOf[Div] = {
        <.div(
          for (tab <- Seq(QualityTab, ProfileTab)) yield {
            val style = if (s.activeTab == tab) css.activeTab else css.inactiveTab
            <.div(style, ^.cursor.pointer, ^.onClick --> $.modState(s => s.copy(activeTab = tab)), tab.name)
          },
          <.div(css.pullRight,
            ExpertConfigResetComponent(p.proxy.zoom(_.expertConfig)),
            <.div(css.hSpaced, ^.display.`inline-block`, PdfExportComponent(ChartComponent.ElementId))
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

    def apply(proxy: ModelProxy[AppModel]): ReactComponentU[Props, State, Backend, TopNode] = component(Props(proxy))

  }

}
