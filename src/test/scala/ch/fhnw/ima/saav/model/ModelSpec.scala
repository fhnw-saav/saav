package ch.fhnw.ima.saav.model

import ch.fhnw.ima.saav.model.model.Entity.Project
import ch.fhnw.ima.saav.model.model.{AnalysisBuilder, Review}
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
    val indicatorScope = builder.category("Category").subCategory("Sub-Category").indicator("Indicator")

    val entityOne = Project("Project 1")
    val entityTwo = Project("Project 2")
    val entityThree = Project("Project 3")

    val reviewOne = Review("Review 1")
    val reviewTwo = Review("Review 2")

    indicatorScope.addValue(entityOne, reviewOne, 11)
    indicatorScope.addValue(entityOne, reviewTwo, 12)
    indicatorScope.addValue(entityThree, reviewOne, 31)
    indicatorScope.addValue(entityTwo, reviewOne, 21)
    indicatorScope.addValue(entityThree, reviewTwo, 32)

    val analysis = builder.build()

    assert(analysis.entities.size == 3)
    assert(analysis.reviews.size == 2)

    val indicator = indicatorScope.toIndicator

    assert(analysis.value(entityOne, indicator, reviewOne) == Option(11))
    assert(analysis.value(entityOne, indicator, reviewTwo) == Option(12))
    assert(analysis.value(entityThree, indicator, reviewOne) == Option(31))
    assert(analysis.value(entityTwo, indicator, reviewOne) == Option(21))
    assert(analysis.value(entityThree, indicator, reviewTwo) == Option(32))

  }

  "An analysis" should "group values by indicator/sub-category/category using median" in {

    // build a hierarchy with some test values

    val builder = new AnalysisBuilder[Project]

    val project = Project("Project")

    val reviewOne = Review("Review 1")
    val reviewTwo = Review("Review 2")

    val categoryScope1 = builder.category("Category 1")
    val categoryScope2 = builder.category("Category 2")

    val subCategoryScope1 = categoryScope1.subCategory("Sub-Category 1")
    val indicatorScope11 = subCategoryScope1.indicator("Indicator 11")
    indicatorScope11.addValue(project, reviewOne, 1)
    indicatorScope11.addValue(project, reviewTwo, 2)

    val indicatorScope12 = subCategoryScope1.indicator("Indicator 12")
    indicatorScope12.addValue(project, reviewTwo, 3)

    // actually create analysis

    val analysis = builder.build()

    val category1 = analysis.categories(0)
    val category2 = analysis.categories(1)

    val subCategory1 = category1.subCategories(0)
    val indicator11 = subCategory1.indicators(0)
    val indicator12 = subCategory1.indicators(1)

    // grouping by indicator
    assert(analysis.groupedValue(project, indicator11) == Option(1.5), "Grouping by indicator: Even number of values")
    assert(analysis.groupedValue(project, indicator12) == Option(3), "Grouping by indicator: Odd number of values")

    // grouping by sub-category
    assert(analysis.groupedValue(project, subCategory1) == Option(2.25), "Grouping by sub-category")

    // grouping by category
    assert(analysis.groupedValue(project, category1) == Option(2.25), "Grouping by category")
    assert(analysis.groupedValue(project, category2) == Option.empty, "Grouping by category with no values")
  }

  it should "be truly immutable" in {
    val builder = AnalysisBuilder.projectAnalysisBuilder
    val analysis = builder.build()

    builder.category("Category")

    assert(analysis.categories.isEmpty)
  }

  it should "contain entities in insertion order" in {

    val project1 = Project("Project 1")
    val project2 = Project("Project 2")
    val project3 = Project("Project 3")

    val review = Review("Review")

    val builder = AnalysisBuilder.projectAnalysisBuilder
    builder.category("Category").subCategory("Sub-Category").indicator("Indicator")
      .addValue(project3, review, 3)
      .addValue(project1, review, 1)
      .addValue(project2, review, 3)

    assert(builder.build().entities == Seq(project3, project1, project2))
  }

}