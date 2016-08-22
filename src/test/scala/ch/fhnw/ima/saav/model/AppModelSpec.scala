package ch.fhnw.ima.saav.model

import ch.fhnw.ima.saav.Seq
import ch.fhnw.ima.saav.model.app._
import ch.fhnw.ima.saav.model.domain.Entity.Project
import ch.fhnw.ima.saav.model.domain._
import org.scalatest.{FunSpec, Matchers}

class AppModelSpec extends FunSpec with Matchers {

  describe(s"A PlottableQualityDataModel") {

    val entityOne = Project("P1")
    val entityTwo = Project("P2")
    val entityThree = Project("P3")

    val review = Review("Review")

    val analysis = AnalysisBuilder.projectAnalysisBuilder
      .category("Cat 1")
        .subCategory("Sub-Cat 11")
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
      .category("Cat 2")
        .subCategory("Sub-Cat 21")
          .indicator("Indicator 211")
            .addValue(entityOne, review, 42)
            .addValue(entityTwo, review, 99)
          .build
          .indicator("Indicator 212")
            .addValue(entityOne, review, 43)
          .build
        .build
      .build
    .build.asInstanceOf[Analysis[Entity]] // TODO: Fix co-variance

    val model = PlottableQualityDataModel(analysis)

    it("should aggregate medians across all categories and sub-categories") {

      // ranking (highest global median first)
      model.rankedEntities.size shouldBe 3
      model.rankedEntities(0).entity shouldBe entityTwo
      model.rankedEntities(0).value shouldBe Some(100)

      model.rankedEntities(1).entity shouldBe entityOne
      model.rankedEntities(1).value shouldBe Some(22.25)

      model.rankedEntities(2).entity shouldBe entityThree
      model.rankedEntities(2).value shouldBe Some(0)

      model.categories.size shouldBe 2

      val categoryOne = model.categories(0)
      categoryOne.groupedValues(entityTwo) shouldBe Some(101)
      categoryOne.groupedValues(entityThree) shouldBe Some(0)
      categoryOne.groupedValues(entityOne) shouldBe Some(2)
      categoryOne.subCategories.size shouldBe 1
      categoryOne.subCategories(0).groupedValues(entityOne) shouldBe Some(2)
      categoryOne.subCategories(0).groupedValues(entityTwo) shouldBe Some(101)
      categoryOne.subCategories(0).groupedValues(entityThree) shouldBe Some(0)


      val categoryTwo = model.categories(1)
      categoryTwo.groupedValues(entityOne) shouldBe Some(42.5)
      categoryTwo.groupedValues(entityTwo) shouldBe Some(99)
      categoryTwo.groupedValues(entityThree) shouldBe None
      categoryTwo.subCategories.size shouldBe 1
      categoryTwo.subCategories(0).groupedValues(entityOne) shouldBe Some(42.5)
      categoryTwo.subCategories(0).groupedValues(entityTwo) shouldBe Some(99)
      categoryTwo.subCategories(0).groupedValues(entityThree) shouldBe None

    }

    it("should re-calculate aggregated medians when weights are updated") {

      // disable all indicators below category 0
      val someIndicators = analysis.categories(0).subCategories(0).indicators.toSet
      val weights = Weights(disabledIndicators = someIndicators)

      val newModel = model.updateWeights(analysis, weights)

      // ranking (highest global median first)
      newModel.rankedEntities.size shouldBe 3
      newModel.rankedEntities(0).entity shouldBe entityTwo
      newModel.rankedEntities(0).value shouldBe Some(99)

      newModel.rankedEntities(1).entity shouldBe entityOne
      newModel.rankedEntities(1).value shouldBe Some(42.5)

      newModel.rankedEntities(2).entity shouldBe entityThree
      newModel.rankedEntities(2).value shouldBe None

      newModel.categories.size shouldBe 2

      // all indicators below disabled --> all values None
      val categoryOne = newModel.categories(0)
      categoryOne.groupedValues(entityTwo) shouldBe None
      categoryOne.groupedValues(entityThree) shouldBe None
      categoryOne.groupedValues(entityOne) shouldBe None
      categoryOne.subCategories.size shouldBe 1
      categoryOne.subCategories(0).groupedValues(entityOne) shouldBe None
      categoryOne.subCategories(0).groupedValues(entityTwo) shouldBe None
      categoryOne.subCategories(0).groupedValues(entityThree) shouldBe None

      // weights unchanged --> same expectations as with defaults
      val categoryTwo = newModel.categories(1)
      categoryTwo.groupedValues(entityOne) shouldBe Some(42.5)
      categoryTwo.groupedValues(entityTwo) shouldBe Some(99)
      categoryTwo.groupedValues(entityThree) shouldBe None
      categoryTwo.subCategories.size shouldBe 1
      categoryTwo.subCategories(0).groupedValues(entityOne) shouldBe Some(42.5)
      categoryTwo.subCategories(0).groupedValues(entityTwo) shouldBe Some(99)
      categoryTwo.subCategories(0).groupedValues(entityThree) shouldBe None

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
