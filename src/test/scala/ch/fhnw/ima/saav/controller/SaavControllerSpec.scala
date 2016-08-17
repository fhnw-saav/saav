package ch.fhnw.ima.saav.controller

import ch.fhnw.ima.saav.controller.SaavController.{AnalysisHandler, _}
import ch.fhnw.ima.saav.model.app._
import ch.fhnw.ima.saav.model.domain.Entity.Person
import ch.fhnw.ima.saav.model.domain._
import diode.RootModelRW
import org.scalatest._

import scala.collection.immutable.ListSet

class SaavControllerSpec extends FunSpec with Matchers {

  def analysisHandler = new AnalysisHandler(new RootModelRW(SaavModel().model))

  def selectionAndPinningHandler(initialModel: SelectionAndPinning) = new SelectionAndPinningHandler(new RootModelRW(initialModel))

  def failOnUnexpectedAction = fail("Unexpected action result")

  describe(s"Handling ${AnalysisImportInProgressAction.getClass.getSimpleName}") {
    it("should wire a progress value") {
      val result = analysisHandler.handle(AnalysisImportInProgressAction(0.9f))
      result.newModelOpt match {
        case Some(Left(NoDataModel(ImportInProgress(progress)))) =>
          progress shouldBe 0.9f
        case _ => failOnUnexpectedAction
      }
    }
  }

  describe(s"Handling ${AnalysisImportFailedAction.getClass.getSimpleName}") {
    it("should wire a throwable") {
      val throwable = new Throwable("test")
      val result = analysisHandler.handle(AnalysisImportFailedAction(throwable, logToConsole = false))
      result.newModelOpt match {
        case Some(Left(NoDataModel(ImportFailed(t)))) =>
          t shouldBe throwable
        case _ => failOnUnexpectedAction
      }
    }
  }

  describe(s"Handling ${AnalysisReadyAction.getClass.getSimpleName}") {
    it("should wire a data model") {
      val analysis = AnalysisBuilder.projectAnalysisBuilder.build
      val result = analysisHandler.handle(AnalysisReadyAction(analysis))
      result.newModelOpt match {
        case Some(Right(DataModel(actualAnalysis, _, selectedEntities, None))) =>
          actualAnalysis shouldBe analysis
          selectedEntities.toSeq == analysis.entities
        case _ => failOnUnexpectedAction
      }
    }
  }

  describe(s"Handling ${UpdateEntitySelectionAction.getClass.getSimpleName}") {
    it("should wire selected entities") {
      val handler = selectionAndPinningHandler(SelectionAndPinning())
      val selectedEntities = Seq(PlottableEntity(Person("x"), value = None), PlottableEntity(Person("y"), value = None))
      val result = handler.handle(UpdateEntitySelectionAction(selectedEntities, isSelected = true))
      result.newModelOpt match {
        case Some(SelectionAndPinning(actualSelectedEntities, actualPinned)) =>
          actualSelectedEntities.toSeq == selectedEntities.map(_.entity)
          actualPinned shouldBe empty
        case _ => failOnUnexpectedAction
      }
    }
    it("should clear pinning if entity is no longer selected") {
      val anEntity = Person("x")
      val handler = selectionAndPinningHandler(SelectionAndPinning(pinned = Some(anEntity)))
      val result = handler.handle(UpdateEntitySelectionAction(Seq(), isSelected = false))
      result.newModelOpt match {
        case Some(SelectionAndPinning(actualSelectedEntities, actualPinned)) =>
          actualSelectedEntities shouldBe empty
          actualPinned shouldBe empty
        case _ => failOnUnexpectedAction
      }
    }
    it("should not touch pinning if entity is still selected") {
      val anEntity = PlottableEntity(Person("x"), value = None)
      val handler = selectionAndPinningHandler(SelectionAndPinning(pinned = Some(anEntity.entity)))
      val result = handler.handle(UpdateEntitySelectionAction(Seq(anEntity), isSelected = true))
      result.newModelOpt match {
        case Some(SelectionAndPinning(actualSelectedEntities, actualPinned)) =>
          actualSelectedEntities shouldBe ListSet(anEntity.entity)
          actualPinned shouldBe Some(anEntity.entity)
        case _ => failOnUnexpectedAction
      }
    }
  }

  describe(s"Handling ${UpdateEntityPinningAction.getClass.getSimpleName}") {
    it("should wire a pinned entity") {
      val handler = selectionAndPinningHandler(SelectionAndPinning())
      val pinnedEntity = Some(PlottableEntity(Person("x"), value = None))
      val result = handler.handle(UpdateEntityPinningAction(pinnedEntity))
      result.newModelOpt match {
        case Some(SelectionAndPinning(_, actualPinnedEntity)) =>
          actualPinnedEntity shouldBe pinnedEntity.map(_.entity)
        case _ => failOnUnexpectedAction
      }
    }
  }

}
