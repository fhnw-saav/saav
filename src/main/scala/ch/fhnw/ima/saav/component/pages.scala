package ch.fhnw.ima.saav
package component

import ch.fhnw.ima.saav.component.pages.Page.ProjectAnalysisPage
import ch.fhnw.ima.saav.model.SaavModel
import ch.fhnw.ima.saav.model.domain.{Analysis, Entity}
import diode.react.ModelProxy
import japgolly.scalajs.react.ReactComponentB
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

        val content: TagMod = p.proxy.value.analysis match {
          case Left(importProgress) => FileImportComponent(p.proxy)
          case Right(analysisModel) => AnalysisReadyComponent(p.proxy, analysisModel)
        }

        <.div(<.h1(ProjectAnalysisPage.displayName), content)
      })
      .build

    def apply(proxy: ModelProxy[SaavModel]) = component(Props(proxy))

  }

  object AnalysisReadyComponent {

    case class Props(proxy: ModelProxy[SaavModel], analysis: Analysis[Entity])

    private val component = ReactComponentB[Props](AnalysisReadyComponent.getClass.getSimpleName)
      .render_P(p => {
        <.div(
          <.div(css.row,
            <.div(css.colXs12,
              <.span(css.pullRight, PdfExportComponent(p.analysis))
            )
          ),
          <.div(css.row,
            <.div(css.colXs2, LegendComponent(p.proxy.zoom(_ => p.analysis.entities))),
            <.div(css.colXs10, D3Component(p.analysis))
          )
        )
      })
      .build

    def apply(proxy: ModelProxy[SaavModel], analysis: Analysis[Entity]) = component(Props(proxy, analysis))

  }

}
