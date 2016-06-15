package ch.fhnw.ima.saav.component

import ch.fhnw.ima.saav.component.Pages.Page.ProjectAnalysisPage
import ch.fhnw.ima.saav.model.model.Analysis
import ch.fhnw.ima.saav.model.model.Entity.Project
import ch.fhnw.ima.saav.style.GlobalStyles
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{BackendScope, ReactComponentB}

import scalacss.ScalaCssReact._

object Pages {

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

  val css = GlobalStyles

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

    case class State(model: Option[Analysis[Project]])

    class Backend($: BackendScope[Unit, State]) {

      def update = (model: Analysis[Project]) =>
        $.setState(State(Some(model))).runNow()

      def render(s: State) = {
        <.div(<.h1(ProjectAnalysisPage.displayName), FileImportComponent(update), D3Component(s.model))
      }

    }

    private val component = ReactComponentB[Unit](ProjectAnalysisPageComponent.getClass.getSimpleName)
      .initialState(State(None))
      .renderBackend[Backend]
      .build

    def apply() = component()

  }

}