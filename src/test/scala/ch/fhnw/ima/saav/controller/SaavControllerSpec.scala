package ch.fhnw.ima.saav.controller

import ch.fhnw.ima.saav.controller.SaavController.{AnalysisHandler, _}
import ch.fhnw.ima.saav.model.app._
import ch.fhnw.ima.saav.model.domain._
import diode.RootModelRW
import org.scalatest._

class SaavControllerSpec extends FunSpec with Matchers {

  def analysisHandler = new AnalysisHandler(new RootModelRW(SaavModel().model))

  def selectionAndPinningHandler(initialModel: EntitySelectionModel) = new SelectionAndPinningHandler(new RootModelRW(initialModel))

  def failOnUnexpectedAction = fail("Unexpected action result")

  describe(s"Handling ${AnalysisImportInProgressAction.getClass.getSimpleName}") {

    it("should wire a progress value") {
      val result = analysisHandler.handle(AnalysisImportInProgressAction(0.9f))
      result.newModelOpt match {
        case Some(Left(NoDataAppModel(ImportInProgress(progress)))) =>
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
        case Some(Left(NoDataAppModel(ImportFailed(t)))) =>
          t shouldBe throwable
        case _ => failOnUnexpectedAction
      }
    }

  }

  describe(s"Handling ${AnalysisReadyAction.getClass.getSimpleName}") {

    it("should wire a model") {
      val analysis = AnalysisBuilder().build
      val result = analysisHandler.handle(AnalysisReadyAction(analysis))
      result.newModelOpt match {
        case Some(Right(appModel)) =>
          appModel.qualityModel.rankedEntities.size shouldBe analysis.entities.size
        case _ => failOnUnexpectedAction
      }
    }

  }

  describe(s"Handling ${UpdateEntitySelectionAction.getClass.getSimpleName}") {

    it("should wire selected entities") {
      val handler = selectionAndPinningHandler(EntitySelectionModel())
      val selectedEntities = Set(Entity("x"), Entity("y"))
      val result = handler.handle(UpdateEntitySelectionAction(selectedEntities, isSelected = true))
      result.newModelOpt match {
        case Some(EntitySelectionModel(actualSelectedEntities, actualPinned)) =>
          actualSelectedEntities should contain theSameElementsAs selectedEntities
          actualPinned shouldBe empty
        case _ => failOnUnexpectedAction
      }
    }

    it("should clear pinning if entity is no longer selected") {
      val anEntity = Entity("x")
      val handler = selectionAndPinningHandler(EntitySelectionModel(pinned = Some(anEntity)))
      val result = handler.handle(UpdateEntitySelectionAction(Set.empty, isSelected = false))
      result.newModelOpt match {
        case Some(EntitySelectionModel(actualSelectedEntities, actualPinned)) =>
          actualSelectedEntities shouldBe empty
          actualPinned shouldBe empty
        case _ => failOnUnexpectedAction
      }
    }

    it("should not touch pinning if entity is still selected") {
      val anEntity = Entity("x")
      val handler = selectionAndPinningHandler(EntitySelectionModel(pinned = Some(anEntity)))
      val result = handler.handle(UpdateEntitySelectionAction(Set(anEntity), isSelected = true))
      result.newModelOpt match {
        case Some(EntitySelectionModel(actualSelectedEntities, actualPinned)) =>
          actualSelectedEntities shouldBe Set(anEntity)
          actualPinned shouldBe Some(anEntity)
        case _ => failOnUnexpectedAction
      }
    }
  }

  describe(s"Handling ${UpdateEntityPinningAction.getClass.getSimpleName}") {

    it("should wire a pinned entity") {
      val handler = selectionAndPinningHandler(EntitySelectionModel())
      val pinnedEntity = Some(Entity("x"))
      val result = handler.handle(UpdateEntityPinningAction(pinnedEntity))
      result.newModelOpt match {
        case Some(EntitySelectionModel(_, actualPinnedEntity)) =>
          actualPinnedEntity shouldBe pinnedEntity
        case _ => failOnUnexpectedAction
      }
    }

    it("should clear pinning") {
      val handler = selectionAndPinningHandler(EntitySelectionModel(pinned = Some(Entity("x"))))
      val result = handler.handle(UpdateEntityPinningAction(None))
      result.newModelOpt match {
        case Some(EntitySelectionModel(_, actualPinnedEntity)) =>
          actualPinnedEntity shouldBe None
        case _ => failOnUnexpectedAction
      }
    }

  }

}
