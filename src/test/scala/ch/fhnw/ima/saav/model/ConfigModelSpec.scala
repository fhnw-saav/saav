package ch.fhnw.ima.saav.model

import cats.data.Xor
import ch.fhnw.ima.saav.model.config.{AnalysisConfig, CriteriaConfig, IndicatorConfig, SubCriteriaConfig}
import ch.fhnw.ima.saav.model.weight.{Profile, Quality}
import io.circe.generic.auto._
import io.circe.parser._
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
                "indicators" : [
                  {
                    "name" : "Indicator Foo",
                    "weight" : { "Profile" : { } }
                  },
                  {
                    "name" : "Indicator Bar",
                    "weight" : { "Quality" : { "weight" : 0.42 }
                    }
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
                "indicators" : [
                  {
                    "name" : "Indicator Foo",
                    "weight" : { "Profile" : { } }
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
              Seq(
                IndicatorConfig("Indicator Foo", Profile),
                IndicatorConfig("Indicator Bar", Quality(0.42d))
              )
            )
          )
        ),
        CriteriaConfig(
          name = "Criteria B",
          Seq(
            SubCriteriaConfig(
              "Sub-Criteria B1",
              Seq(IndicatorConfig("Indicator Foo", Profile))
            )
          )
        )
      )
    )

    val actualAnalysisConfig = decode[AnalysisConfig](json)
    actualAnalysisConfig shouldBe Xor.right(expectedAnalysisConfig)
  }

}
