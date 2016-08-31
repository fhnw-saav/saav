package ch.fhnw.ima.saav.model

import ch.fhnw.ima.saav.Seq
import ch.fhnw.ima.saav.model.app._
import ch.fhnw.ima.saav.model.domain._
import org.scalatest.{FunSpec, Matchers}

class AppModelSpec extends FunSpec with Matchers {

  describe(s"A ${AppModel.getClass.getSimpleName}") {

    val entityOne = Entity("P1")
    val entityTwo = Entity("P2")
    val entityThree = Entity("P3")

    val review = Review("Review")

    val analysis = AnalysisBuilder()
      .criteria("Cat 1")
        .subCriteria("Sub-Cat 11")
          .indicator("Indicator 111")
            .addValue(entityOne, review, 1)
            .addValue(entityTwo, review, 101)
          .build
          .indicator("Indicator 112")
            .addValue(entityOne, review, 3)
            .addValue(entityTwo, review, 101)
            .addValue(entityThree, review, 0)
          .build
        .build
      .build
      .criteria("Cat 2")
        .subCriteria("Sub-Cat 21")
          .indicator("Indicator 211")
            .addValue(entityOne, review, 42)
            .addValue(entityTwo, review, 99)
          .build
          .indicator("Indicator 212")
            .addValue(entityOne, review, 43)
          .build
        .build
      .build
    .build

    val model = AppModel(analysis)

    it("should aggregate medians across all categories and sub-categories") {

      val qualityModel = model.qualityModel

      // ranking (highest global median first)
      qualityModel.rankedEntities.size shouldBe 3
      qualityModel.rankedEntities(0).id shouldBe entityTwo
      qualityModel.rankedEntities(0).value shouldBe Some(100)

      qualityModel.rankedEntities(1).id shouldBe entityOne
      qualityModel.rankedEntities(1).value shouldBe Some(22.25)

      qualityModel.rankedEntities(2).id shouldBe entityThree
      qualityModel.rankedEntities(2).value shouldBe Some(0)

      qualityModel.criteria.size shouldBe 2

      val criteriaOne = qualityModel.criteria(0)
      criteriaOne.groupedValues(entityTwo) shouldBe Some(101)
      criteriaOne.groupedValues(entityThree) shouldBe Some(0)
      criteriaOne.groupedValues(entityOne) shouldBe Some(2)
      criteriaOne.subCriteria.size shouldBe 1
      criteriaOne.subCriteria(0).groupedValues(entityOne) shouldBe Some(2)
      criteriaOne.subCriteria(0).groupedValues(entityTwo) shouldBe Some(101)
      criteriaOne.subCriteria(0).groupedValues(entityThree) shouldBe Some(0)


      val criteriaTwo = qualityModel.criteria(1)
      criteriaTwo.groupedValues(entityOne) shouldBe Some(42.5)
      criteriaTwo.groupedValues(entityTwo) shouldBe Some(99)
      criteriaTwo.groupedValues(entityThree) shouldBe None
      criteriaTwo.subCriteria.size shouldBe 1
      criteriaTwo.subCriteria(0).groupedValues(entityOne) shouldBe Some(42.5)
      criteriaTwo.subCriteria(0).groupedValues(entityTwo) shouldBe Some(99)
      criteriaTwo.subCriteria(0).groupedValues(entityThree) shouldBe None

    }

    it("should re-calculate aggregated medians when weights are updated") {

      // disable all indicators below criteria 0
      val someIndicators = analysis.criteria(0).subCriteria(0).indicators.toSet
      val weights = Weights(disabledIndicators = someIndicators)

      val newModel = model.updateWeights(analysis, weights).qualityModel

      // ranking (highest global median first)
      newModel.rankedEntities.size shouldBe 3
      newModel.rankedEntities(0).id shouldBe entityTwo
      newModel.rankedEntities(0).value shouldBe Some(99)

      newModel.rankedEntities(1).id shouldBe entityOne
      newModel.rankedEntities(1).value shouldBe Some(42.5)

      newModel.rankedEntities(2).id shouldBe entityThree
      newModel.rankedEntities(2).value shouldBe None

      newModel.criteria.size shouldBe 2

      // all indicators below disabled --> all values None
      val criteriaOne = newModel.criteria(0)
      criteriaOne.groupedValues(entityTwo) shouldBe None
      criteriaOne.groupedValues(entityThree) shouldBe None
      criteriaOne.groupedValues(entityOne) shouldBe None
      criteriaOne.subCriteria.size shouldBe 1
      criteriaOne.subCriteria(0).groupedValues(entityOne) shouldBe None
      criteriaOne.subCriteria(0).groupedValues(entityTwo) shouldBe None
      criteriaOne.subCriteria(0).groupedValues(entityThree) shouldBe None

      // weights unchanged --> same expectations as with defaults
      val criteriaTwo = newModel.criteria(1)
      criteriaTwo.groupedValues(entityOne) shouldBe Some(42.5)
      criteriaTwo.groupedValues(entityTwo) shouldBe Some(99)
      criteriaTwo.groupedValues(entityThree) shouldBe None
      criteriaTwo.subCriteria.size shouldBe 1
      criteriaTwo.subCriteria(0).groupedValues(entityOne) shouldBe Some(42.5)
      criteriaTwo.subCriteria(0).groupedValues(entityTwo) shouldBe Some(99)
      criteriaTwo.subCriteria(0).groupedValues(entityThree) shouldBe None

    }

    it("should calculate weighted medians") {

      {
        val valuesWithWeights = Seq((13d, 0.1), (23d, 0.03), (54d, 0.04))
        val weightedMedian = app.weightedMedian(valuesWithWeights)
        assert(weightedMedian === Some(13))
      }

      {
        val valuesWithWeights = Seq((1d, 0.613), (2d, 0.001), (3d, 0.613))
        val weightedMedian = app.weightedMedian(valuesWithWeights)
        assert(weightedMedian === Some(2))
      }

    }
  }

}
