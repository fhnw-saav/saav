package ch.fhnw.ima.saav.controller

import ch.fhnw.ima.saav.controller.SaavController.{AnalysisHandler, _}
import ch.fhnw.ima.saav.model.model.AnalysisBuilder
import ch.fhnw.ima.saav.model.{ImportFailed, ImportInProgress, SaavModel}
import diode.ActionResult.ModelUpdate
import diode.RootModelRW
import org.scalatest.{FlatSpec, _}

class SaavControllerSpec extends FlatSpec with Matchers {

  val model = SaavModel().analysis

  def createHandler = new AnalysisHandler(new RootModelRW(model))

  it should s"handle ${AnalysisImportInProgressAction.getClass.getSimpleName}" in {
    val result = createHandler.handle(AnalysisImportInProgressAction(0.9f))
    result match {
      case ModelUpdate(Left(ImportInProgress(progress))) =>
        progress shouldBe 0.9f
      case _ =>
        fail("Unexpected action result")
    }
  }

  it should s"handle ${AnalysisImportFailedAction.getClass.getSimpleName}" in {
    val throwable = new Throwable("test")
    val result = createHandler.handle(AnalysisImportFailedAction(throwable, logToConsole = false))
    result match {
      case ModelUpdate(Left(ImportFailed(t))) =>
        t shouldBe throwable
      case _ =>
        fail("Unexpected action result")
    }
  }

  it should s"handle ${AnalysisReadyAction.getClass.getSimpleName}" in {
    val analysis = AnalysisBuilder.personAnalysisBuilder.build
    val result = createHandler.handle(AnalysisReadyAction(analysis))
    result match {
      case ModelUpdate(Right(actualAnalysis)) =>
        actualAnalysis shouldBe analysis
      case _ =>
        fail("Unexpected action result")
    }
  }

}
