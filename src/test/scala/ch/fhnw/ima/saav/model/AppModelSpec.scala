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

    val reviewOne = Review("Review1")
    val reviewTwo = Review("Review2")
    val reviewThree = Review("Review3")

    val analysis = AnalysisBuilder()
      .criteria("Cat 1")
        .subCriteria("Sub-Cat 11")
          .indicator("Indicator 111")
            .addValue(entityOne, reviewOne, 1)
            .addValue(entityOne, reviewTwo, 0.1)
            .addValue(entityOne, reviewThree, 2)
            .addValue(entityTwo, reviewOne, 101)
            .addValue(entityTwo, reviewTwo, 100)
            .addValue(entityTwo, reviewThree, 102)
          .build
          .indicator("Indicator 112")
            .addValue(entityOne, reviewOne, 3)
            .addValue(entityTwo, reviewOne, 101)
            .addValue(entityThree, reviewOne, 0)
          .build
        .build
      .build
      .criteria("Cat 2")
        .subCriteria("Sub-Cat 21")
          .indicator("Indicator 211")
            .addValue(entityOne, reviewOne, 41)
            .addValue(entityOne, reviewTwo, 43)
            .addValue(entityTwo, reviewOne, 99)
          .build
          .indicator("Indicator 212")
            .addValue(entityOne, reviewOne, 43)
          .build
        .build
      .build
    .build

    val indicators = analysis.criteria.flatMap(_.subCriteria.flatMap(_.indicators))
    val weights = Weights(enabledIndicators = indicators.toSet)
    val model = AppModel(analysis, weights)

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

    it("should only include enabled indicators when aggregating medians") {

      // enable all indicators below criteria 1
      val someIndicators = analysis.criteria(1).subCriteria(0).indicators.toSet
      val weights = Weights(enabledIndicators = someIndicators)

      val qualityModel = QualityModel(model.analysis, weights)

      // ranking (highest global median first)
      qualityModel.rankedEntities.size shouldBe 3
      qualityModel.rankedEntities(0).id shouldBe entityTwo
      qualityModel.rankedEntities(0).value shouldBe Some(99)

      qualityModel.rankedEntities(1).id shouldBe entityOne
      qualityModel.rankedEntities(1).value shouldBe Some(42.5)

      qualityModel.rankedEntities(2).id shouldBe entityThree
      qualityModel.rankedEntities(2).value shouldBe None

      qualityModel.criteria.size shouldBe 1

      // weights unchanged --> same expectations as with defaults
      val criteriaTwo = qualityModel.criteria(0)
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
