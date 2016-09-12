package ch.fhnw.ima.saav
package component

import ch.fhnw.ima.saav.component.pages.Page.ProjectAnalysisPage
import ch.fhnw.ima.saav.model.app.{AppModel, SaavModel}
import diode.react.ModelProxy
import japgolly.scalajs.react.{BackendScope, ReactComponentB}
import japgolly.scalajs.react.vdom.prefix_<^._

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

      def displayName = "Project Application"

      def hashLink = "project"

    }

    case object PersonAnalysisPage extends SubPage {

      def displayName = "Academic Appointment"

      def hashLink = "person"

    }

    case object OrganisationAnalysisPage extends SubPage {

      def displayName = "Research Organisation"

      def hashLink = "organisation"

    }

    def subPages = List[SubPage](ProjectAnalysisPage, PersonAnalysisPage, OrganisationAnalysisPage)
  }

  object HomePageComponent {

    private val component = ReactComponentB[Unit](HomePageComponent.getClass.getSimpleName)
      .render(_ => {

        def createButton(subPage: SubPage) =
          <.a(css.mainLinkButton, ^.role := "button", ^.href := "#/" + subPage.hashLink, subPage.displayName)

        <.div(Page.subPages.map(createButton))
      })
      .build

    def apply() = component()

  }

  object ProjectAnalysisPageComponent {

    case class Props(proxy: ModelProxy[SaavModel])

    private val component = ReactComponentB[Props](ProjectAnalysisPageComponent.getClass.getSimpleName)
      .render_P(p => {

        val content: TagMod = p.proxy.value.model match {
          case Left(noData) => FileImportComponent(p.proxy.zoom(_ => noData))
          case Right(data) => PageWithDataComponent(p.proxy.zoom(_ => data))
        }

        <.div(<.h1(ProjectAnalysisPage.displayName), content)
      })
      .build

    def apply(proxy: ModelProxy[SaavModel]) = component(Props(proxy))

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
      def render(p: Props, s: State) = {
        <.div(
          <.ul(css.navTabs,
            for (tab <- Seq(QualityTab, ProfileTab)) yield {
              val style = if (s.activeTab == tab) css.active else css.nonActive
              <.li(style, ^.cursor.pointer, <.a(^.onClick --> $.modState(s => s.copy(activeTab = tab)), tab.name))
            }
          ),
          <.div(
            s.activeTab match {
              case QualityTab => <.div(css.row, css.vSpaced,
                <.div(css.colXs3, LegendComponent(p.proxy, _.qualityModel.rankedEntities)),
                <.div(css.colXs9, QualityChartComponent(p.proxy))
              )
              case ProfileTab => <.div(css.row, css.vSpaced,
                <.div(css.colXs3, LegendComponent(p.proxy, _.profileModel.sortedEntities)),
                <.div(css.colXs9, ProfileChartComponent(p.proxy))
              )
            },
            <.div(css.row,
              <.div(css.colXs3, IndicatorComponent(p.proxy)),
              <.div(css.colXs9, ExpertConfigComponent(p.proxy.zoom(m => (m.analysis, m.weights))))
            )
          )
        )
      }
    }

    private val component = ReactComponentB[Props](PageWithDataComponent.getClass.getSimpleName)
      .initialState(State(activeTab = QualityTab))
      .renderBackend[Backend]
      .build

    def apply(proxy: ModelProxy[AppModel]) = component(Props(proxy))

  }

}
