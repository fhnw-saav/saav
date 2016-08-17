package ch.fhnw.ima.saav.model

import ch.fhnw.ima.saav.model.app._
import org.scalatest.FlatSpec

class AppModelSpec extends FlatSpec {

  ignore should "provide a plotting-friendly API" in {

    val model: PlottableQualityDataModel = ???

    for (category <- model.categories) {

      println(category.name)

      model.rankedEntities.foreach { plottableEntity =>
        category.groupedValue(plottableEntity.entity)
      }

      for (subCategory <- category.subCategories) {

        println(subCategory.name)

        model.rankedEntities.foreach { plottableEntity =>
          subCategory.groupedValue(plottableEntity.entity)
        }

      }

    }

  }

  it should "calculate weighted medians" in {

    {
      val valuesWithWeights = Seq((13d, 0.1), (23d, 0.03), (54d, 0.04))
      val weightedMedian = app.weightedMedian(valuesWithWeights)
      weightedMedian === 13
    }

    {
      val valuesWithWeights = Seq((1d, 0.613), (2d, 0.001), (3d, 0.613))
      val weightedMedian = app.weightedMedian(valuesWithWeights)
      weightedMedian === 2
    }

  }

}
