package ch.fhnw.ima.saav.model

import ch.fhnw.ima.saav.TestUtil
import ch.fhnw.ima.saav.model.app._
import ch.fhnw.ima.saav.model.config.Config
import ch.fhnw.ima.saav.model.domain._
import ch.fhnw.ima.saav.model.weight.{Quality, Weights}
import org.scalatest.{FunSpec, Matchers}

class AppModelSpec extends FunSpec with Matchers with TestUtil {

  describe(s"A ${AppModel.getClass.getSimpleName}") {

    val model = AppModel(analysis, createConfig(analysis))

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
      val weights = createWeights(someIndicators)

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
  }

  private def createWeights(indicators: Set[IndicatorId]) = {
    Weights(subCriteriaWeights = Map().withDefaultValue(Quality(1.0)), enabledIndicators = indicators)
  }

  private def createConfig(a: Analysis) = {
    val indicators = a.criteria.flatMap(_.subCriteria.flatMap(_.indicators).map(_.id))
    new Config {
      val defaultWeights: Weights = createWeights(indicators.toSet)
    }
  }

}
