package ch.fhnw.ima.saav.model

import ch.fhnw.ima.saav.model.model.{AnalysisBuilder, Review}
import ch.fhnw.ima.saav.model.model.Entity.Project
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

  it should "group by indicator/sub-category/category using median" in {
    val builder = new AnalysisBuilder[Project]

    val category = builder.category("Category")
    val subCategoryOne = category.subCategory("Sub-Category 1")
    val indicatorOneOne = subCategoryOne.indicator("Indicator 11")

    val project = Project("Project")

    val reviewOne = Review("Review 1")
    val reviewTwo = Review("Review 2")

    // grouping by indicator

    assert(builder.build().groupedValue(project, indicatorOneOne) == Option.empty, "Grouping by indicator: No values")

    indicatorOneOne.addValue(project, reviewTwo, 2)
    indicatorOneOne.addValue(project, reviewOne, 1)
    assert(builder.build().groupedValue(project, indicatorOneOne) == Option(1.5), "Grouping by indicator: Even number of values")

    indicatorOneOne.addValue(project, reviewTwo, 3)
    assert(builder.build().groupedValue(project, indicatorOneOne) == Option(2), "Grouping by indicator: Odd number of values")

    // grouping by sub-category

    assert(builder.build().groupedValue(project, subCategoryOne) == Option(2), "Grouping by sub-category (single indicator)")

    val indicatorOneTwo = subCategoryOne.indicator("Indicator 12")

    indicatorOneTwo.addValue(project, reviewOne, 3)
    indicatorOneTwo.addValue(project, reviewTwo, 3)

    assert(builder.build().groupedValue(project, subCategoryOne) == Option(2.5), "Grouping by sub-category (multiple indicators)")

    // grouping by category

    assert(builder.build().groupedValue(project, category) == Option(2.5), "Grouping by sub-category (single sub-category)")

    category.subCategory("Sub-Category 2").indicator("Indicator 21")
      .addValue(project, reviewOne, 5)
      .addValue(project, reviewTwo, 7)
    assert(builder.build().groupedValue(project, category) == Option(4.25), "Grouping by sub-category (multiple sub-categories)")
  }

}