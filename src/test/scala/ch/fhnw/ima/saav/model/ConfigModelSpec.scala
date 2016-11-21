package ch.fhnw.ima.saav.model

import ch.fhnw.ima.saav.TestUtil
import ch.fhnw.ima.saav.model.config.AnalysisConfig
import org.scalatest.{FunSpec, Matchers}

class ConfigModelSpec extends FunSpec with Matchers with TestUtil {

  val json =
    """{
        "criteria" : [
          {
            "name" : "C_1",
            "aggregatable" : true,
            "subCriteria" : [
              {
                "name" : "SC_11",
                "weight" : {
                  "Quality" : {
                    "weight" : 1
                  }
                },
                "indicators" : [
                  {
                    "name" : "I_111",
                    "enabled" : true
                  },
                  {
                    "name" : "I_112",
                    "enabled" : true
                  }
                ]
              }
            ]
          },
          {
            "name" : "C_2",
            "aggregatable" : true,
            "subCriteria" : [
              {
                "name" : "SC_21",
                "weight" : {
                  "Quality" : {
                    "weight" : 0.42
                  }
                },
                "indicators" : [
                  {
                    "name" : "I_211",
                    "enabled" : true
                  },
                  {
                    "name" : "I_212",
                    "enabled" : true
                  }
                ]
              }
            ]
          },
          {
            "name" : "C_3",
            "aggregatable" : false,
            "subCriteria" : [
              {
                "name" : "SC_31",
                "weight" : {
                  "Profile" : {

                  }
                },
                "indicators" : [
                  {
                    "name" : "I_311",
                    "enabled" : false
                  },
                  {
                    "name" : "I_312",
                    "enabled" : true
                  }
                ]
              }
            ]
          }
        ]
      }"""

  it("should parse an analysis config") {
    val actualAnalysisConfig = AnalysisConfig.fromJson(json)
    actualAnalysisConfig shouldBe Right(analysisConfig)
  }

}
