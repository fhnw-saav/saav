package ch.fhnw.ima.saav

import ch.fhnw.ima.saav.controller._
import ch.fhnw.ima.saav.controller.io.AnalysisDataImporter
import ch.fhnw.ima.saav.model.config.AnalysisConfig
import ch.fhnw.ima.saav.model.domain.AnalysisBuilder
import org.scalactic.TolerantNumerics
import org.scalatest.{FunSpec, Matchers}

class SaavIntegrationTestSpec extends FunSpec with Matchers {

  // Scala.js does not allow programmatic loading of files from the local resources folder
  private val csv =
    "Entity,Hierarchy,Review,Value\n" +
      "A,1:::11:::111,1,2.43\n" +
      "A,1:::11:::111,2,4.53\n" +
      "A,1:::11:::111,3,0.15\n" +
      "B,1:::11:::111,1,3.55\n" +
      "B,1:::11:::111,2,3.63\n" +
      "B,1:::11:::111,3,2.62\n" +
      "C,1:::11:::111,1,0.51\n" +
      "C,1:::11:::111,2,4.99\n" +
      "C,1:::11:::111,3,3.81\n" +
      "A,1:::11:::112,1,0.33\n" +
      "A,1:::11:::112,2,1.04\n" +
      "A,1:::11:::112,3,0.55\n" +
      "B,1:::11:::112,1,4.72\n" +
      "B,1:::11:::112,2,1.34\n" +
      "B,1:::11:::112,3,4.74\n" +
      "C,1:::11:::112,1,0.65\n" +
      "C,1:::11:::112,2,4.99\n" +
      "C,1:::11:::112,3,4.24\n" +
      "A,1:::12:::121,1,1.51\n" +
      "A,1:::12:::121,2,1.93\n" +
      "A,1:::12:::121,3,2.09\n" +
      "B,1:::12:::121,1,4.39\n" +
      "B,1:::12:::121,2,3.77\n" +
      "B,1:::12:::121,3,4.95\n" +
      "C,1:::12:::121,1,3.74\n" +
      "C,1:::12:::121,2,2.9\n" +
      "C,1:::12:::121,3,3.67\n" +
      "A,1:::12:::122,1,1.5\n" +
      "A,1:::12:::122,2,0.6\n" +
      "A,1:::12:::122,3,4.14\n" +
      "B,1:::12:::122,1,3.12\n" +
      "B,1:::12:::122,2,1.75\n" +
      "B,1:::12:::122,3,2.69\n" +
      "C,1:::12:::122,1,0.77\n" +
      "C,1:::12:::122,2,1.46\n" +
      "C,1:::12:::122,3,3.71\n" +
      "A,2:::21:::211,1,2.46\n" +
      "A,2:::21:::211,2,3.59\n" +
      "A,2:::21:::211,3,4.51\n" +
      "B,2:::21:::211,1,3.43\n" +
      "B,2:::21:::211,2,1.08\n" +
      "B,2:::21:::211,3,3.33\n" +
      "C,2:::21:::211,1,4.72\n" +
      "C,2:::21:::211,2,0.04\n" +
      "C,2:::21:::211,3,1.12\n" +
      "A,2:::21:::212,1,3.72\n" +
      "A,2:::21:::212,2,1.35\n" +
      "A,2:::21:::212,3,0.68\n" +
      "B,2:::21:::212,1,4.93\n" +
      "B,2:::21:::212,2,0.37\n" +
      "B,2:::21:::212,3,1.7\n" +
      "C,2:::21:::212,1,2.13\n" +
      "C,2:::21:::212,2,4.1\n" +
      "C,2:::21:::212,3,4.08\n" +
      "A,2:::22:::221,1,3.08\n" +
      "A,2:::22:::221,2,0.21\n" +
      "A,2:::22:::221,3,4.99\n" +
      "B,2:::22:::221,1,4.02\n" +
      "B,2:::22:::221,2,3.12\n" +
      "B,2:::22:::221,3,4.38\n" +
      "C,2:::22:::221,1,0.48\n" +
      "C,2:::22:::221,2,4.27\n" +
      "C,2:::22:::221,3,4.67\n" +
      "A,2:::22:::222,1,1.79\n" +
      "A,2:::22:::222,2,0.88\n" +
      "A,2:::22:::222,3,4.61\n" +
      "B,2:::22:::222,1,3.31\n" +
      "B,2:::22:::222,2,1.81\n" +
      "B,2:::22:::222,3,0.29\n" +
      "C,2:::22:::222,1,0.26\n" +
      "C,2:::22:::222,2,4.17\n" +
      "C,2:::22:::222,3,3.28"

  describe("SAAV") {
    it("should correctly import and aggregate data") {

      val rows = AnalysisDataImporter.splitContentsIntoRows(csv)
      val builder = AnalysisBuilder()
      for ((row, rowIndex) <- rows.zipWithIndex) {
        AnalysisDataImporter.parseRow(builder, rowIndex, row, allowValuesOutsideRange = true)
      }
      val analysis = builder.build

      val circuit = new SaavCircuit()
      circuit.dispatch(AnalysisReadyAction(AnalysisConfig.empty, analysis))

      val model = circuit.zoom(AnalysisImportHandler.modelGet).value
      model match {
        case Right(appModel) =>

          implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(0.01)

          val entities = appModel.analysis.entities
          entities.size shouldBe 3

          val a = entities(0)
          val b = entities(1)
          val c = entities(2)

          val first = appModel.qualityModel.rankedEntities(0)
          first.displayName shouldBe "B"
          assert(first.value.get === 3.04)

          val second = appModel.qualityModel.rankedEntities(1)
          second.displayName shouldBe "C"
          assert(second.value.get === 2.87)

          val third = appModel.qualityModel.rankedEntities(2)
          third.displayName shouldBe "A"
          assert(third.value.get === 2.19)

          val c1 = appModel.qualityModel.criteria(0)

          assert(c1.groupedValues(a.id) === 1.73)
          assert(c1.groupedValues(b.id) === 3.44)
          assert(c1.groupedValues(c.id) === 2.95)

          val c2 = appModel.qualityModel.criteria(1)

          assert(c2.groupedValues(a.id) === 2.66)
          assert(c2.groupedValues(b.id) === 2.65)
          assert(c2.groupedValues(c.id) === 2.78)

        case a@_ => fail("Unexpected action result: " + a)
      }
    }
  }

}