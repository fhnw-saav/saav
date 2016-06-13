package ch.fhnw.saav.model

import ch.fhnw.saav.model.model.{AnalysisBuilder, Review}
import ch.fhnw.saav.model.model.Entity.Project
import org.scalatest.FlatSpec

class ModelSpec extends FlatSpec {

  "An analysis builder" should "add categories" in {
    val builder = new AnalysisBuilder[Project]
    builder.category("Category A")
    builder.category("Category B")
    val analysis = builder.build()

    assert(analysis.categories.size == 2)
  }

  it should "re-use existing categories" in {
    val builder = new AnalysisBuilder[Project]
    val name = "Category"
    val first = builder.category(name)
    val second = builder.category(name)
    val analysis = builder.build()

    assert(first === second)
    assert(analysis.categories.size == 1)
  }

  it should "add sub-categories" in {
    val builder = new AnalysisBuilder[Project]
    val categoryScope = builder.category("Category")
    categoryScope.subCategory("Sub-Category A")
    categoryScope.subCategory("Sub-Category B")
    val analysis = builder.build()

    assert(analysis.categories.head.subCategories.size == 2)
  }

  it should "re-use existing sub-categories" in {
    val builder = new AnalysisBuilder[Project]
    val categoryScope = builder.category("Category")
    val name = "Sub-Category"
    val first = categoryScope.subCategory(name)
    val second = categoryScope.subCategory(name)
    val analysis = builder.build()

    assert(first === second)
    assert(analysis.categories.head.subCategories.size == 1)
  }

  it should "add indicators" in {
    val builder = new AnalysisBuilder[Project]
    val subCategoryScope = builder.category("Category").subCategory("Sub-Category")
    subCategoryScope.indicator("Indicator 1")
    subCategoryScope.indicator("Indicator 2")
    val analysis = builder.build()

    assert(analysis.categories.head.subCategories.head.indicators.size == 2)
  }

  it should "re-use existing indicators" in {
    val builder = new AnalysisBuilder[Project]
    val subCategoryScope = builder.category("Category").subCategory("Sub-Category")
    val name = "Indicator"
    val first = subCategoryScope.indicator(name)
    val second = subCategoryScope.indicator(name)
    val analysis = builder.build()

    assert(first === second)
    assert(analysis.categories.head.subCategories.head.indicators.size == 1)
  }

  it should "add values" in {
    val builder = new AnalysisBuilder[Project]
    val indicator = builder.category("Category").subCategory("Sub-Category").indicator("Indicator")

    val entityOne = Project("Project 1")
    val entityTwo = Project("Project 2")
    val entityThree = Project("Project 3")

    val reviewOne = Review("Review 1")
    val reviewTwo = Review("Review 2")

    indicator.addValue(entityOne, reviewOne, 11)
    indicator.addValue(entityOne, reviewTwo, 12)
    indicator.addValue(entityThree, reviewOne, 31)
    indicator.addValue(entityTwo, reviewOne, 21)
    indicator.addValue(entityThree, reviewTwo, 32)

    val analysis = builder.build()

    assert(analysis.entities.size == 3)
    assert(analysis.reviews.size == 2)

    assert(analysis.value(entityOne, indicator, reviewOne) == Option(11))
    assert(analysis.value(entityOne, indicator, reviewTwo) == Option(12))
    assert(analysis.value(entityThree, indicator, reviewOne) == Option(31))
    assert(analysis.value(entityTwo, indicator, reviewOne) == Option(21))
    assert(analysis.value(entityThree, indicator, reviewTwo) == Option(32))

  }

  it should "be easy to build and query" in {

    val builder = AnalysisBuilder.projectAnalysisBuilder

    // setting up hierarchical structure

    val animals = builder.category("Animals")
    val animalsMammals = animals.subCategory("Mammals")
    val animalsInsects = animals.subCategory("Insects")
    val animalsMammalsLegCount = animalsMammals.indicator("Leg Count")
    val animalsInsectsWingLength = animalsInsects.indicator("Wing Length")

    // actual entities to be compared

    val mutantProject = Project("Mutants")
    val normativeProject = Project("Heteronormatives")

    // identify different readings

    val biologistReview = Review("Biologist Review")
    val mathematicianReview = Review("Mathematician Review")

    // actual values are added on level of indicators

    animalsMammalsLegCount.addValue(normativeProject, biologistReview, 4)
    animalsMammalsLegCount.addValue(normativeProject, mathematicianReview, 4.5)
    animalsMammalsLegCount.addValue(mutantProject, biologistReview, 42)
    animalsMammalsLegCount.addValue(mutantProject, mathematicianReview, 42.5)

    animalsInsectsWingLength.addValue(normativeProject, biologistReview, 2.2)
    animalsInsectsWingLength.addValue(normativeProject, mathematicianReview, 2.5)
    animalsInsectsWingLength.addValue(mutantProject, biologistReview, 99)
    animalsInsectsWingLength.addValue(mutantProject, mathematicianReview, 99.5)

    val analysis = builder.build()

    // accessing values (e.g. when plotting)

    for {
      category <- analysis.categories
      subCategory <- category.subCategories
      indicator <- subCategory.indicators
      entity <- analysis.entities
      review <- analysis.reviews
      value <- analysis.value(entity, indicator, review)
    } {
      println(s"${category.name}, ${subCategory.name}, ${indicator.name}, ${entity.name}, ${review.name}, $value")
    }

  }

}