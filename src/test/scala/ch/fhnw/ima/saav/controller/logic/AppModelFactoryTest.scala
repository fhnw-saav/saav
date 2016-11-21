package ch.fhnw.ima.saav.controller.logic

import ch.fhnw.ima.saav.TestUtil
import ch.fhnw.ima.saav.model.domain.{CriteriaId, SubCriteriaId}
import ch.fhnw.ima.saav.model.weight.{Profile, Quality}
import org.scalatest.{FunSpec, Matchers}

class AppModelFactoryTest extends FunSpec with Matchers with TestUtil {

  describe(s"An ${AppModelFactory.getClass.getSimpleName}") {

    val appModel = AppModelFactory.createAppModel(analysisConfig, analysis)

    it("should initialize sub-criteria weights") {
      appModel.config.defaultWeights.subCriteriaWeights shouldEqual Map(
        SubCriteriaId(CriteriaId("C_1"), "SC_11") -> Quality(1.0),
        SubCriteriaId(CriteriaId("C_2"), "SC_21") -> Quality(0.42),
        SubCriteriaId(CriteriaId("C_3"), "SC_31") -> Profile
      )
    }

    it("should initialize enabled indicators") {
      appModel.config.defaultWeights.enabledIndicators.size shouldEqual 5
    }

  }

}