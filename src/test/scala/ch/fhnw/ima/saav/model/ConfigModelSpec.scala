package ch.fhnw.ima.saav.model

import ch.fhnw.ima.saav.model.config.{AnalysisConfig, CriteriaConfig, IndicatorConfig, SubCriteriaConfig}
import ch.fhnw.ima.saav.model.weight.{Profile, Quality}
import org.scalatest.{FunSpec, Matchers}

class ConfigModelSpec extends FunSpec with Matchers {

  val json =
    """{
        "criteria" : [
          {
            "name" : "Criteria A",
            "subCriteria" : [
              {
                "name" : "Sub-Criteria A1",
                "weight" : { "Profile" : { } },
                "indicators" : [
                  {
                    "name" : "Indicator Foo",
                    "enabled" : true
                  },
                  {
                    "name" : "Indicator Bar",
                    "enabled" : false
                  }
                ]
              }
            ]
          },
          {
            "name" : "Criteria B",
            "subCriteria" : [
              {
                "name" : "Sub-Criteria B1",
                "weight" : { "Quality" : { "weight" : 0.42 } },
                "indicators" : [
                  {
                    "name" : "Indicator Foo",
                    "enabled" : false
                  }
                ]
              }
            ]
          }
        ]
  }"""

  it("should parse an analysis config") {

    val expectedAnalysisConfig = AnalysisConfig(
      Seq(
        CriteriaConfig(
          "Criteria A",
          Seq(
            SubCriteriaConfig(
              "Sub-Criteria A1",
              Profile,
              Seq(
                IndicatorConfig("Indicator Foo", enabled = true),
                IndicatorConfig("Indicator Bar", enabled = false)
              )
            )
          )
        ),
        CriteriaConfig(
          name = "Criteria B",
          Seq(
            SubCriteriaConfig(
              "Sub-Criteria B1",
              Quality(0.42d),
              Seq(IndicatorConfig("Indicator Foo", enabled = false))
            )
          )
        )
      )
    )

    val actualAnalysisConfig = AnalysisConfig.fromJson(json)
    actualAnalysisConfig shouldBe Right(expectedAnalysisConfig)
  }

}
