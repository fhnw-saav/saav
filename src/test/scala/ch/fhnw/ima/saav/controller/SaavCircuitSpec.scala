package ch.fhnw.ima.saav.controller

import ch.fhnw.ima.saav.TestUtil
import ch.fhnw.ima.saav.controller.io.AnalysisDataImporter.ImportState
import ch.fhnw.ima.saav.model.app._
import ch.fhnw.ima.saav.model.color._
import ch.fhnw.ima.saav.model.config.AnalysisConfig
import ch.fhnw.ima.saav.model.domain._
import ch.fhnw.ima.saav.model.weight.{Profile, Weights}
import org.scalatest.{FunSpec, Matchers}

class SaavCircuitSpec extends FunSpec with Matchers with TestUtil {

  private def circuitWithAnalysis() = {
    val circuit = new SaavCircuit()
    circuit.dispatch(AnalysisReadyAction(analysis = analysis))
    circuit
  }

  private def failOnUnexpectedAction = fail("Unexpected action result")

  describe(s"${AnalysisImportHandler.getClass.getSimpleName}") {

    it("should calculate a progress value") {
      val circuit = new SaavCircuit()
      val rows = TestUtil.createTestRows(20)
      circuit.dispatch(AnalysisDataImportInProgressAction(ImportState(AnalysisConfig.empty, AnalysisBuilder(), rows, 1)))
      val model = circuit.zoom(AnalysisImportHandler.modelGet).value
      model match {
        case Left(NoDataAppModel(ImportInProgress(progress))) =>
          progress shouldBe 0.5f
        case _ => failOnUnexpectedAction
      }
    }

    it("should wire a throwable") {
      val circuit = new SaavCircuit()
      val throwable = new Throwable("test")
      circuit.dispatch(ImportFailedAction(throwable, logToConsole = false))
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
      val visibleEntities = Set(entityOne.id, entityTwo.id)
      val circuit = circuitWithAnalysis()
      circuit.dispatch(UpdateEntityVisibilityAction(allEntityIds, visible = false))
      circuit.dispatch(UpdateEntityVisibilityAction(visibleEntities, visible = true))
      val model = circuit.zoom(EntitySelectionHandler.modelGet).value
      model match {
        case EntitySelectionModel(actualVisibleEntities, actualPinned, _) =>
          actualVisibleEntities should contain theSameElementsAs visibleEntities
          actualPinned shouldBe empty
        case _ => failOnUnexpectedAction
      }
    }

    it("should clear pinning if entity is no longer visible") {
      val anEntity = EntityId("x")
      val circuit = circuitWithAnalysis()
      circuit.dispatch(UpdateEntityPinningAction(Some(anEntity)))
      circuit.dispatch(UpdateEntityVisibilityAction(allEntityIds, visible = false))
      val model = circuit.zoom(EntitySelectionHandler.modelGet).value
      model match {
        case EntitySelectionModel(actualVisibleEntities, actualPinned, _) =>
          actualVisibleEntities shouldBe empty
          actualPinned shouldBe empty
        case _ => failOnUnexpectedAction
      }
    }

    it("should not touch pinning if entity is still visible") {
      val anEntity = entityOne.id
      val circuit = circuitWithAnalysis()
      circuit.dispatch(UpdateEntityVisibilityAction(allEntityIds, visible = false))
      circuit.dispatch(UpdateEntityPinningAction(Some(anEntity)))
      circuit.dispatch(UpdateEntityVisibilityAction(Set(anEntity), visible = true))
      val model = circuit.zoom(EntitySelectionHandler.modelGet).value
      model match {
        case EntitySelectionModel(actualVisibleEntities, actualPinned, _) =>
          actualVisibleEntities shouldBe Set(anEntity)
          actualPinned shouldBe Some(anEntity)
        case _ => failOnUnexpectedAction
      }
    }

    it("should control pinning") {
      val anEntity = EntityId("x")
      val circuit = circuitWithAnalysis()

      circuit.dispatch(UpdateEntityPinningAction(Some(anEntity)))
      circuit.zoom(EntitySelectionHandler.modelGet).value match {
        case EntitySelectionModel(_, actualPinned, _) =>
          actualPinned shouldBe Some(anEntity)
        case _ => failOnUnexpectedAction
      }

      circuit.dispatch(UpdateEntityPinningAction(None))
      circuit.zoom(EntitySelectionHandler.modelGet).value match {
        case EntitySelectionModel(_, actualPinned, _) =>
          actualPinned shouldBe empty
        case _ => failOnUnexpectedAction
      }

    }

    it("should control hovering") {
      val anEntity = EntityId("x")
      val circuit = circuitWithAnalysis()

      circuit.dispatch(UpdateEntityHoveringAction(Some(anEntity)))
      circuit.zoom(EntitySelectionHandler.modelGet).value match {
        case EntitySelectionModel(_, _, actualHovered) =>
          actualHovered shouldBe Some(anEntity)
        case _ => failOnUnexpectedAction
      }

      circuit.dispatch(UpdateEntityHoveringAction(None))
      circuit.zoom(EntitySelectionHandler.modelGet).value match {
        case EntitySelectionModel(_, _, actualHovered) =>
          actualHovered shouldBe empty
        case _ => failOnUnexpectedAction
      }

    }

  }

  describe(s"${SubCriteriaSelectionHandler.getClass.getSimpleName}") {

    it("should wire/clear a hovered sub-criteria") {
      val hoveredSubCriteria = Some(subCriteriaId)
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

    it("should update colors") {
      val anEntity = EntityId("x")
      val aColor = WebColor("#999999")
      val circuit = circuitWithAnalysis()

      circuit.dispatch(UpdateEntityColorAction(anEntity, aColor))
      val updatedColors = circuit.zoom(ColorHandler.modelGet).value
      updatedColors(anEntity) shouldBe aColor
    }

  }

  describe(s"${ExpertConfigHandler.getClass.getSimpleName}") {

    it("should update weights") {
      val circuit = circuitWithAnalysis()
      val defaultWeights = circuit.zoom(ExpertConfigHandler.modelGet).value.defaultWeights
      val newWeights = Weights(Map.empty, Set.empty)
      circuit.dispatch(UpdateWeightsAction(newWeights))
      val expertConfig = circuit.zoom(ExpertConfigHandler.modelGet).value
      expertConfig.actualWeights shouldBe newWeights
      expertConfig.defaultWeights shouldBe defaultWeights
    }

    it("should control enabled indicators") {
      val circuit = circuitWithAnalysis()
      allIndicatorIds.foreach { i =>
        circuit.dispatch(UpdateIndicatorWeightAction(i, isEnabled = false))
      }
      circuit.dispatch(UpdateIndicatorWeightAction(indicatorId, isEnabled = true))
      val expertConfig = circuit.zoom(ExpertConfigHandler.modelGet).value
      expertConfig.actualWeights.enabledIndicators.size shouldBe 1
    }

    it("should control sub-criteria weights") {
      val circuit = circuitWithAnalysis()
      circuit.dispatch(UpdateSubCriteriaWeightAction(subCriteriaId, Profile))
      val expertConfig = circuit.zoom(ExpertConfigHandler.modelGet).value
      expertConfig.actualWeights.subCriteriaWeights(subCriteriaId) shouldBe Profile
    }

    it("should control visibility") {
      val circuit = circuitWithAnalysis()

      {
        circuit.dispatch(UpdateVisibility(ExpertConfigVisible))
        val expertConfig = circuit.zoom(ExpertConfigHandler.modelGet).value
        expertConfig.visibility shouldBe ExpertConfigVisible
      }

      {
        circuit.dispatch(UpdateVisibility(ExpertConfigHidden))
        val expertConfig = circuit.zoom(ExpertConfigHandler.modelGet).value
        expertConfig.visibility shouldBe ExpertConfigHidden
      }

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

    it("should preserve sub-criteria assignment when updating layout in both chart models") {
      val circuit = circuitWithAnalysis()

      // move everything to profile chart
      allSubCriteriaIds.foreach { id =>
        circuit.dispatch(UpdateSubCriteriaWeightAction(id, Profile))
      }
      val modelWithQualityAndProfileCategories = circuit.zoom(ChartLayoutHandler.modelGet).value
      modelWithQualityAndProfileCategories._1.criteria.size shouldBe 0
      modelWithQualityAndProfileCategories._2.criteria.size shouldBe 2

      // assert that the quality vs. profile assignment is not lost after updating chart layouts
      circuit.dispatch(UpdateChartWidthAction(42))
      val model = circuit.zoom(ChartLayoutHandler.modelGet).value
      model._1.criteria.size shouldBe 0
      model._2.criteria.size shouldBe 2
    }

  }

}