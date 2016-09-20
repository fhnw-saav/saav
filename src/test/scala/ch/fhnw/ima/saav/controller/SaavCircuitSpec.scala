package ch.fhnw.ima.saav.controller

import ch.fhnw.ima.saav.model
import ch.fhnw.ima.saav.model.app._
import ch.fhnw.ima.saav.model.color._
import ch.fhnw.ima.saav.model.domain._
import ch.fhnw.ima.saav.model.weight.{Profile, Weights}
import org.scalatest._

class SaavCircuitSpec extends FunSpec with Matchers {

  private val analysis = AnalysisBuilder().build

  private def circuitWithAnalysis() = {
    val circuit = new SaavCircuit()
    circuit.dispatch(AnalysisReadyAction(analysis))
    circuit
  }

  private def failOnUnexpectedAction = fail("Unexpected action result")

  describe(s"${AnalysisImportHandler.getClass.getSimpleName}") {

    it("should wire a progress value") {
      val circuit = new SaavCircuit()
      circuit.dispatch(AnalysisImportInProgressAction(0.9f))
      val model = circuit.zoom(AnalysisImportHandler.modelGet).value
      model match {
        case Left(NoDataAppModel(ImportInProgress(progress))) =>
          progress shouldBe 0.9f
        case _ => failOnUnexpectedAction
      }
    }

    it("should wire a throwable") {
      val circuit = new SaavCircuit()
      val throwable = new Throwable("test")
      circuit.dispatch(AnalysisImportFailedAction(throwable, logToConsole = false))
      val model = circuit.zoom(AnalysisImportHandler.modelGet).value
      model match {
        case Left(NoDataAppModel(ImportFailed(t))) =>
          t shouldBe throwable
        case _ => failOnUnexpectedAction
      }
    }

    it("should wire a model") {
      val model = circuitWithAnalysis().zoom(AnalysisImportHandler.modelGet).value
      model match {
        case Right(appModel) =>
          appModel.qualityModel.rankedEntities.size shouldBe analysis.entities.size
        case _ => failOnUnexpectedAction
      }
    }

  }

  describe(s"${EntitySelectionHandler.getClass.getSimpleName}") {

    it("should wire visible entities") {
      val visibleEntities = Set(Entity("x"), Entity("y"))
      val circuit = circuitWithAnalysis()
      circuit.dispatch(UpdateEntityVisibilityAction(visibleEntities, visible = true))
      val model = circuit.zoom(EntitySelectionHandler.modelGet).value
      model match {
        case EntitySelectionModel(actualVisibleEntities, actualPinned) =>
          actualVisibleEntities should contain theSameElementsAs visibleEntities
          actualPinned shouldBe empty
        case _ => failOnUnexpectedAction
      }
    }

    it("should clear pinning if entity is no longer visible") {
      val anEntity = Entity("x")
      val circuit = circuitWithAnalysis()
      circuit.dispatch(UpdateEntityPinningAction(Some(anEntity)))
      circuit.dispatch(UpdateEntityVisibilityAction(Set.empty, visible = false))
      val model = circuit.zoom(EntitySelectionHandler.modelGet).value
      model match {
        case EntitySelectionModel(actualVisibleEntities, actualPinned) =>
          actualVisibleEntities shouldBe empty
          actualPinned shouldBe empty
        case _ => failOnUnexpectedAction
      }
    }

    it("should not touch pinning if entity is still visible") {
      val anEntity = Entity("x")
      val circuit = circuitWithAnalysis()
      circuit.dispatch(UpdateEntityPinningAction(Some(anEntity)))
      circuit.dispatch(UpdateEntityVisibilityAction(Set(anEntity), visible = true))
      val model = circuit.zoom(EntitySelectionHandler.modelGet).value
      model match {
        case EntitySelectionModel(actualVisibleEntities, actualPinned) =>
          actualVisibleEntities shouldBe Set(anEntity)
          actualPinned shouldBe Some(anEntity)
        case _ => failOnUnexpectedAction
      }
    }

    it("should clear pinning") {
      val anEntity = Entity("x")
      val circuit = circuitWithAnalysis()

      circuit.dispatch(UpdateEntityPinningAction(Some(anEntity)))
      circuit.zoom(EntitySelectionHandler.modelGet).value match {
        case EntitySelectionModel(_, actualPinned) =>
          actualPinned shouldBe Some(anEntity)
        case _ => failOnUnexpectedAction
      }

      circuit.dispatch(UpdateEntityPinningAction(None))
      circuit.zoom(EntitySelectionHandler.modelGet).value match {
        case EntitySelectionModel(_, actualPinned) =>
          actualPinned shouldBe empty
        case _ => failOnUnexpectedAction
      }

    }

  }

  describe(s"${SubCriteriaSelectionHandler.getClass.getSimpleName}") {

    it("should wire/clear a hovered sub-criteria") {
      val hoveredSubCriteria = Some(SubCriteria("x", Seq.empty))
      val circuit = circuitWithAnalysis()

      circuit.dispatch(UpdateSubCriteriaHoveringAction(hoveredSubCriteria))
      circuit.zoom(SubCriteriaSelectionHandler.modelGet).value match {
        case SubCriteriaSelectionModel(actualHoveredSubCriteria) =>
          actualHoveredSubCriteria shouldBe hoveredSubCriteria
        case _ => failOnUnexpectedAction
      }

      circuit.dispatch(UpdateSubCriteriaHoveringAction(None))
      circuit.zoom(SubCriteriaSelectionHandler.modelGet).value match {
        case SubCriteriaSelectionModel(actualHoveredSubCriteria) =>
          actualHoveredSubCriteria shouldBe empty
        case _ => failOnUnexpectedAction
      }

    }

  }

  describe(s"${ColorHandler.getClass.getSimpleName}") {

    it("should update and auto-colorize colors") {
      val anEntity = Entity("x")
      val aColor = WebColor("#999999")
      val circuit = circuitWithAnalysis()

      circuit.dispatch(UpdateEntityColorAction(anEntity, aColor))
      val updatedColors = circuit.zoom(ColorHandler.modelGet).value
      updatedColors(anEntity) shouldBe aColor

      circuit.dispatch(AutoColorizeAction(Seq(anEntity)))
      val autoColors = circuit.zoom(ColorHandler.modelGet).value
      autoColors(anEntity) shouldBe model.color.SolarizedPalette(0)
    }

  }

  describe(s"${WeightsHandler.getClass.getSimpleName}") {

    it("should update weights") {
      val circuit = circuitWithAnalysis()
      val newWeights = Weights(Map.empty, Set.empty)
      circuit.dispatch(UpdateWeightsAction(newWeights))
      val weights = circuit.zoom(WeightsHandler.modelGet).value
      weights shouldBe newWeights
    }

    it("should control enabled indicators") {
      val indicator = Indicator("x", Map.empty)
      val circuit = circuitWithAnalysis()
      circuit.dispatch(UpdateIndicatorWeightAction(indicator, isEnabled = false))
      val weights = circuit.zoom(WeightsHandler.modelGet).value
      weights.enabledIndicators shouldBe empty
    }

    it("should control sub-criteria weights") {
      val subCriteria = SubCriteria("foo", Seq.empty)
      val circuit = circuitWithAnalysis()
      circuit.dispatch(UpdateSubCriteriaWeightAction(subCriteria, Profile))
      val weights = circuit.zoom(WeightsHandler.modelGet).value
      weights.subCriteriaWeights(subCriteria) shouldBe Profile
    }

  }

  describe(s"${ChartLayoutHandler.getClass.getSimpleName}") {
    it("should update layout in both chart models") {
      val circuit = circuitWithAnalysis()
      circuit.dispatch(UpdateChartWidthAction(42))
      val model = circuit.zoom(ChartLayoutHandler.modelGet).value
      model._1.width shouldBe 42
      model._2.width shouldBe 42
    }
  }

}