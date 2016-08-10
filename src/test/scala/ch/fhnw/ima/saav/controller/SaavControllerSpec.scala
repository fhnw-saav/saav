package ch.fhnw.ima.saav.controller

import ch.fhnw.ima.saav.controller.SaavController.{AnalysisHandler, _}
import ch.fhnw.ima.saav.model._
import ch.fhnw.ima.saav.model.domain.AnalysisBuilder
import diode.RootModelRW
import org.scalatest.{FlatSpec, _}

class SaavControllerSpec extends FlatSpec with Matchers {

  val model = SaavModel().model

  def createHandler = new AnalysisHandler(new RootModelRW(model))

  it should s"handle ${AnalysisImportInProgressAction.getClass.getSimpleName}" in {
    val result = createHandler.handle(AnalysisImportInProgressAction(0.9f))
    result.newModelOpt match {
      case Some(Left(NoDataModel(ImportInProgress(progress)))) =>
        progress shouldBe 0.9f
      case _ =>
        fail("Unexpected action result")
    }
  }

  it should s"handle ${AnalysisImportFailedAction.getClass.getSimpleName}" in {
    val throwable = new Throwable("test")
    val result = createHandler.handle(AnalysisImportFailedAction(throwable, logToConsole = false))
    result.newModelOpt match {
      case Some(Left(NoDataModel(ImportFailed(t)))) =>
        t shouldBe throwable
      case _ =>
        fail("Unexpected action result")
    }
  }

  it should s"handle ${AnalysisReadyAction.getClass.getSimpleName}" in {
    val analysis = AnalysisBuilder.personAnalysisBuilder.build
    val result = createHandler.handle(AnalysisReadyAction(analysis))
    result.newModelOpt match {
      case Some(Right(DataModel(actualAnalysis, _))) =>
        actualAnalysis shouldBe analysis
      case _ =>
        fail("Unexpected action result")
    }
  }

}
