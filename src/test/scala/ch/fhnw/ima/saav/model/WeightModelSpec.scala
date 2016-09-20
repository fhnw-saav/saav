package ch.fhnw.ima.saav.model

import ch.fhnw.ima.saav._
import org.scalatest.{FunSpec, Matchers}

class WeightModelSpec extends FunSpec with Matchers {

  it("should calculate weighted medians") {

    {
      val valuesWithWeights = Seq((13d, 0.1), (23d, 0.03), (54d, 0.04))
      val weightedMedian = weight.weightedMedian(valuesWithWeights)
      assert(weightedMedian === Some(13))
    }

    {
      val valuesWithWeights = Seq((1d, 0.613), (2d, 0.001), (3d, 0.613))
      val weightedMedian = weight.weightedMedian(valuesWithWeights)
      assert(weightedMedian === Some(2))
    }

  }

}
