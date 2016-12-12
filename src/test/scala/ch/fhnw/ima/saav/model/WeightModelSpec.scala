package ch.fhnw.ima.saav.model

import ch.fhnw.ima.saav._
import org.scalactic.TolerantNumerics
import org.scalatest.{FunSpec, Matchers}

class WeightModelSpec extends FunSpec with Matchers {

  it("should calculate weighted means") {
      val valuesWithWeights = Seq((13d, 0.1), (23d, 0.03), (54d, 0.04))
      val weightedMean = weight.weightedMean(valuesWithWeights)
      implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(0.01)
      weightedMean match {
        case Some(mean) => assert(mean === 24.418)
        case wrongValue @ _ => fail(s"Unexpected mean value $wrongValue")
      }
  }

  it("should handle empty value collection") {
    weight.weightedMean(Seq.empty) shouldBe None
  }

}
