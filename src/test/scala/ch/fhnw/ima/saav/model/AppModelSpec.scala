package ch.fhnw.ima.saav.model

import ch.fhnw.ima.saav.TestUtil
import ch.fhnw.ima.saav.controller.logic.AppModelFactory
import ch.fhnw.ima.saav.model.app._
import ch.fhnw.ima.saav.model.config.AnalysisConfig
import ch.fhnw.ima.saav.model.domain.{AnalysisBuilder, Entity, EntityId, ReviewId}
import ch.fhnw.ima.saav.model.weight.{Quality, Weights}
import org.scalatest.{FunSpec, Matchers}

class AppModelSpec extends FunSpec with Matchers with TestUtil {

  describe(s"A ${AppModel.getClass.getSimpleName}") {

    val model = AppModelFactory.createAppModel(analysisConfig, analysis)

    it("should aggregate medians across all categories and sub-categories") {

      val qualityModel = model.qualityModel

      // ranking (highest global median first)
      qualityModel.rankedEntities.size shouldBe 3
      qualityModel.rankedEntities(0).id shouldBe entityTwo.id
      qualityModel.rankedEntities(0).value shouldBe Some(100)

      qualityModel.rankedEntities(1).id shouldBe entityOne.id
      qualityModel.rankedEntities(1).value shouldBe Some(22.25)

      qualityModel.rankedEntities(2).id shouldBe entityThree.id
      qualityModel.rankedEntities(2).value shouldBe Some(0)

      qualityModel.criteria.size shouldBe 2

      val criteriaOne = qualityModel.criteria(0)
      criteriaOne.groupedValues(entityTwo.id) shouldBe 101
      criteriaOne.groupedValues(entityThree.id) shouldBe 0
      criteriaOne.groupedValues(entityOne.id) shouldBe 2
      criteriaOne.subCriteria.size shouldBe 1
      criteriaOne.subCriteria(0).groupedValues(entityOne.id) shouldBe 2
      criteriaOne.subCriteria(0).groupedValues(entityTwo.id) shouldBe 101
      criteriaOne.subCriteria(0).groupedValues(entityThree.id) shouldBe 0


      val criteriaTwo = qualityModel.criteria(1)
      criteriaTwo.groupedValues(entityOne.id) shouldBe 42.5
      criteriaTwo.groupedValues(entityTwo.id) shouldBe 99
      criteriaTwo.groupedValues.get(entityThree.id) shouldBe None
      criteriaTwo.subCriteria.size shouldBe 1
      criteriaTwo.subCriteria(0).groupedValues(entityOne.id) shouldBe 42.5
      criteriaTwo.subCriteria(0).groupedValues(entityTwo.id) shouldBe 99
      criteriaTwo.subCriteria(0).groupedValues.get(entityThree.id) shouldBe None

    }

    it("should only include enabled indicators when aggregating medians") {

      // enable all indicators below criteria 1
      val someIndicators = analysis.criteria(1).subCriteria(0).indicators.map(_.id).toSet
      val weights = Weights(subCriteriaWeights = Map().withDefaultValue(Quality(1.0)), enabledIndicators = someIndicators)

      val qualityModel = QualityModel(model.analysis, weights, 1000)

      // ranking (highest global median first)
      qualityModel.rankedEntities.size shouldBe 3
      qualityModel.rankedEntities(0).id shouldBe entityTwo.id
      qualityModel.rankedEntities(0).value shouldBe Some(99)

      qualityModel.rankedEntities(1).id shouldBe entityOne.id
      qualityModel.rankedEntities(1).value shouldBe Some(42.5)

      qualityModel.rankedEntities(2).id shouldBe entityThree.id
      qualityModel.rankedEntities(2).value shouldBe None

      qualityModel.criteria.size shouldBe 1

      // weights unchanged --> same expectations as with defaults
      val criteriaTwo = qualityModel.criteria(0)
      criteriaTwo.groupedValues(entityOne.id) shouldBe 42.5
      criteriaTwo.groupedValues(entityTwo.id) shouldBe 99
      criteriaTwo.groupedValues.get(entityThree.id) shouldBe None
      criteriaTwo.subCriteria.size shouldBe 1
      criteriaTwo.subCriteria(0).groupedValues(entityOne.id) shouldBe 42.5
      criteriaTwo.subCriteria(0).groupedValues(entityTwo.id) shouldBe 99
      criteriaTwo.subCriteria(0).groupedValues.get(entityThree.id) shouldBe None

    }

    it("should wire criteria aggregation") {
      model.config.nonAggregatableCriteria.size shouldBe 1
      model.profileModel.criteria.head.aggregated shouldBe false
    }

    it("https://github.com/fhnw-saav/saav/issues/72") {

      val e1 = EntityId("E1")
      val e2 = EntityId("E2")

      val analysis = AnalysisBuilder().criteria("C").subCriteria("SC").indicator("I")

        .addValue(Entity(e1), ReviewId("R1"), 1)
        .addValue(Entity(e1), ReviewId("R2"), 1)
        .addValue(Entity(e1), ReviewId("R3"), 2)

        .addValue(Entity(e2), ReviewId("R1"), 2)
        .addValue(Entity(e2), ReviewId("R2"), 2)
        .addValue(Entity(e2), ReviewId("R3"), 3)

        .build.build.build.build

      val model = AppModelFactory.createAppModel(AnalysisConfig.empty, analysis)
      val indicator = model.qualityModel.criteria(0).subCriteria(0).indicators(0)

      // median of 1,1,2
      indicator.groupedValues.get(e1) shouldBe Some(1)

      // median of 2,2,3
      indicator.groupedValues.get(e2) shouldBe Some(2)
    }

  }

}
