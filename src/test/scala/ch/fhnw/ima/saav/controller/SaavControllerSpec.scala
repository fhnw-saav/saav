package ch.fhnw.ima.saav.controller

import ch.fhnw.ima.saav.controller.SaavController.{AnalysisHandler, _}
import ch.fhnw.ima.saav.model.app._
import ch.fhnw.ima.saav.model.domain.Entity.Person
import ch.fhnw.ima.saav.model.domain._
import diode.RootModelRW
import org.scalatest._

class SaavControllerSpec extends FunSpec with Matchers {

  def analysisHandler = new AnalysisHandler(new RootModelRW(SaavModel().model))

  def entityHandler(entities: Seq[PlottableEntity]) = new EntityHandler(new RootModelRW(entities))

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
        case Some(Right(PlottableQualityDataModel(rankedEntities, categories))) =>
          rankedEntities.size shouldBe analysis.entities.size
        case _ => failOnUnexpectedAction
      }
    }
  }

  describe(s"Handling ${UpdateEntitySelectionAction.getClass.getSimpleName}") {

    it("should wire selected entities") {
      val allEntities = Seq(PlottableEntity(Person("x")), PlottableEntity(Person("y")), PlottableEntity(Person("z")))
      val handler = entityHandler(allEntities)
      val selectedEntities = allEntities.map(_.id).toSet
      val result = handler.handle(UpdateEntitySelectionAction(selectedEntities, isSelected = true))
      result.newModelOpt match {
        case Some(entities) =>
          entities.count(_.isSelected) shouldBe allEntities.size
          entities.count(_.isPinned) shouldBe 0
        case _ => failOnUnexpectedAction
      }
    }

    it("should clear pinning if entity is no longer selected") {
      val allEntities = Seq(PlottableEntity(Person("x"), isPinned = true), PlottableEntity(Person("y")), PlottableEntity(Person("z")))
      val handler = entityHandler(allEntities)
      val result = handler.handle(UpdateEntitySelectionAction(allEntities.map(_.id).toSet, isSelected = false))
      result.newModelOpt match {
        case Some(entities) =>
          entities.count(_.isSelected) shouldBe 0
          entities.count(_.isPinned) shouldBe 0
        case _ => failOnUnexpectedAction
      }
    }

    it("should not touch pinning if entity is still selected") {
      val anEntity = Person("x")
      val allEntities = Seq(
        PlottableEntity(anEntity, isPinned = true, isSelected = true),
        PlottableEntity(Person("y"), isSelected = false),
        PlottableEntity(Person("z"), isSelected = false)
      )
      val handler = entityHandler(allEntities)
      val result = handler.handle(UpdateEntitySelectionAction(Set(anEntity), isSelected = true))
      result.newModelOpt match {
        case Some(entities) =>
          entities.filter(_.isSelected).map(_.id) shouldBe Seq(anEntity)
          entities.filter(_.isPinned).map(_.id) shouldBe Seq(anEntity)
        case _ => failOnUnexpectedAction
      }
    }

  }

  describe(s"Handling ${UpdateEntityPinningAction.getClass.getSimpleName}") {

    it("should wire a pinned entity") {
      val anEntity = Person("x")
      val allEntities: Seq[PlottableEntity] = Seq(PlottableEntity(anEntity), PlottableEntity(Person("y")), PlottableEntity(Person("z")))
      val handler = entityHandler(allEntities)
      val result = handler.handle(UpdateEntityPinningAction(Some(anEntity)))
      result.newModelOpt match {
        case Some(entities) =>
          entities.filter(_.isPinned).map(_.id) shouldBe Seq(anEntity)
        case _ => failOnUnexpectedAction
      }
    }

    it("should clear pinning") {
      val allEntities = Seq(PlottableEntity(Person("x"), isPinned = true), PlottableEntity(Person("y")), PlottableEntity(Person("z")))
      val handler = entityHandler(allEntities)
      val result = handler.handle(UpdateEntityPinningAction(None))
      result.newModelOpt match {
        case Some(entities) =>
          entities.count(_.isPinned) shouldBe 0
        case _ => failOnUnexpectedAction
      }
    }
  }

}
