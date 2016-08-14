package ch.fhnw.ima.saav.model

import ch.fhnw.ima.saav.model.domain.Entity.Project
import ch.fhnw.ima.saav.model.domain.{AnalysisBuilder, Review}
import org.scalatest.FlatSpec

class DomainModelSpec extends FlatSpec {

  "An analysis builder" should "add categories" in {
    val analysis = AnalysisBuilder.projectAnalysisBuilder
        .category("Category A").build
        .category("Category B").build
        .build

    assert(analysis.categories.size == 2)
  }

  it should "re-use existing categories" in {
    val analysis = AnalysisBuilder.projectAnalysisBuilder
        .category("Category").build
        .category("Category").build
        .build

    assert(analysis.categories.size == 1)
  }

  it should "add sub-categories" in {
    val analysis = AnalysisBuilder.projectAnalysisBuilder
        .category("Category")
          .subCategory("Sub-Category A").build
          .subCategory("Sub-Category B").build
          .build
        .build

    assert(analysis.categories.head.subCategories.size == 2)
  }

  it should "re-use existing sub-categories" in {
    val analysis = AnalysisBuilder.projectAnalysisBuilder
        .category("Category")
          .subCategory("Sub-Category").build
          .subCategory("Sub-Category").build
          .build
        .build

    assert(analysis.categories.head.subCategories.size == 1)
  }

  it should "add indicators" in {
    val analysis = AnalysisBuilder.projectAnalysisBuilder
        .category("Category")
          .subCategory("Sub-Category")
              .indicator("Indicator A").build
              .indicator("Indicator B").build
              .build
          .build
      .build

    assert(analysis.categories.head.subCategories.head.indicators.size == 2)
  }

  it should "re-use existing indicators" in {
    val analysis = AnalysisBuilder.projectAnalysisBuilder
        .category("Category")
          .subCategory("Sub-Category")
            .indicator("Indicator").build
            .indicator("Indicator").build
            .build
          .build
        .build

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

    val analysis = builder.build

    assert(analysis.entities.size == 3)
    assert(analysis.reviews.size == 2)

    val indicator = analysis.categories(0).subCategories(0).indicators(0)

    assert(indicator.values((entityOne, reviewOne)) == 11)
    assert(indicator.values((entityOne, reviewTwo)) == 12)
    assert(indicator.values((entityTwo, reviewOne)) == 21)
    assert(indicator.values((entityThree, reviewOne)) == 31)
    assert(indicator.values((entityThree, reviewTwo)) == 32)
  }

  "An analysis" should "group values by indicator/sub-category/category using median" in {

    val project = Project("Project")

    val reviewOne = Review("Review 1")
    val reviewTwo = Review("Review 2")

    // build a hierarchy with some test values
    val analysis = AnalysisBuilder.projectAnalysisBuilder
        .category("Category 1")
            .subCategory("Sub-Category 11")
              .indicator("Indicator 111")
                .addValue(project, reviewOne, 1)
                .addValue(project, reviewTwo, 2)
                .build
              .indicator("Indicator 112")
                .addValue(project, reviewTwo, 3)
                .build
              .indicator("Indicator 113")
                .build
              .build
            .build
        .category("Category 2").build
        .build

    // retrieve entity references to look-up values
    val category1 = analysis.categories(0)
    val category2 = analysis.categories(1)

    val subCategory11 = category1.subCategories(0)
    val indicator111 = subCategory11.indicators(0)
    val indicator112 = subCategory11.indicators(1)
    val indicator113 = subCategory11.indicators(2)

    // grouping by indicator
    assert(analysis.groupedValue(project, indicator111) == Option(1.5), "Grouping by indicator: Even number of values")
    assert(analysis.groupedValue(project, indicator112) == Option(3), "Grouping by indicator: Odd number of values")
    assert(analysis.groupedValue(project, indicator113).isEmpty, "Grouping by indicator: No values")

    // grouping by sub-category
    assert(analysis.groupedValue(project, subCategory11) == Option(2.25), "Grouping by sub-category")

    // grouping by category
    assert(analysis.groupedValue(project, category1) == Option(2.25), "Grouping by category")
    assert(analysis.groupedValue(project, category2) == Option.empty, "Grouping by category with no values")
  }

  it should "be truly immutable" in {
    val builder = AnalysisBuilder.projectAnalysisBuilder
    val analysis = builder.build

    builder.category("Category")

    assert(analysis.categories.isEmpty)
  }

  it should "contain entities in insertion order" in {

    val project1 = Project("Project 1")
    val project2 = Project("Project 2")
    val project3 = Project("Project 3")

    val review = Review("Review")

    val analysis = AnalysisBuilder.projectAnalysisBuilder
      .category("Category")
        .subCategory("Sub-Category")
          .indicator("Indicator")
            .addValue(project3, review, 3)
            .addValue(project1, review, 1)
            .addValue(project2, review, 3)
            .build
          .build
        .build
      .build

    assert(analysis.entities == Seq(project3, project1, project2))
  }

}