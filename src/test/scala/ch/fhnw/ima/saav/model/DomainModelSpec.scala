package ch.fhnw.ima.saav.model

import ch.fhnw.ima.saav.model.domain.{AnalysisBuilder, Entity, EntityId, ReviewId}
import org.scalatest.FlatSpec

class DomainModelSpec extends FlatSpec {

  "An analysis builder" should "add criteria" in {
    val analysis = AnalysisBuilder()
        .criteria("Criteria A").build
        .criteria("Criteria B").build
        .build

    assert(analysis.criteria.size == 2)
  }

  it should "re-use existing criteria" in {
    val analysis = AnalysisBuilder()
        .criteria("Criteria").build
        .criteria("Criteria").build
        .build

    assert(analysis.criteria.size == 1)
  }

  it should "add sub-criteria" in {
    val analysis = AnalysisBuilder()
        .criteria("Criteria")
          .subCriteria("Sub-Criteria A").build
          .subCriteria("Sub-Criteria B").build
          .build
        .build

    assert(analysis.criteria.head.subCriteria.size == 2)
  }

  it should "re-use existing sub-criteria" in {
    val analysis = AnalysisBuilder()
        .criteria("Criteria")
          .subCriteria("Sub-Criteria").build
          .subCriteria("Sub-Criteria").build
          .build
        .build

    assert(analysis.criteria.head.subCriteria.size == 1)
  }

  it should "add indicators" in {
    val analysis = AnalysisBuilder()
        .criteria("Criteria")
          .subCriteria("Sub-Criteria")
              .indicator("Indicator A").build
              .indicator("Indicator B").build
              .build
          .build
      .build

    assert(analysis.criteria.head.subCriteria.head.indicators.size == 2)
  }

  it should "re-use existing indicators" in {
    val analysis = AnalysisBuilder()
        .criteria("Criteria")
          .subCriteria("Sub-Criteria")
            .indicator("Indicator").build
            .indicator("Indicator").build
            .build
          .build
        .build

    assert(analysis.criteria.head.subCriteria.head.indicators.size == 1)
  }

  it should "add values" in {
    val builder = AnalysisBuilder()
    val indicatorScope = builder.criteria("Criteria").subCriteria("Sub-Criteria").indicator("Indicator")

    val entityOne = Entity(EntityId("Project 1"))
    val entityTwo = Entity(EntityId("Project 2"))
    val entityThree = Entity(EntityId("Project 3"))

    val reviewOne = ReviewId("Review 1")
    val reviewTwo = ReviewId("Review 2")

    indicatorScope.addValue(entityOne, reviewOne, 11)
    indicatorScope.addValue(entityOne, reviewTwo, 12)
    indicatorScope.addValue(entityThree, reviewOne, 31)
    indicatorScope.addValue(entityTwo, reviewOne, 21)
    indicatorScope.addValue(entityThree, reviewTwo, 32)

    val analysis = builder.build

    assert(analysis.entities.size == 3)
    assert(analysis.reviews.size == 2)

    val indicator = analysis.criteria(0).subCriteria(0).indicators(0)

    assert(indicator.values((entityOne.id, reviewOne)) == 11)
    assert(indicator.values((entityOne.id, reviewTwo)) == 12)
    assert(indicator.values((entityTwo.id, reviewOne)) == 21)
    assert(indicator.values((entityThree.id, reviewOne)) == 31)
    assert(indicator.values((entityThree.id, reviewTwo)) == 32)
  }

  it should "be truly immutable" in {
    val builder = AnalysisBuilder()
    val analysis = builder.build

    builder.criteria("Criteria")

    assert(analysis.criteria.isEmpty)
  }

  it should "contain entities in insertion order" in {

    val project1 = Entity(EntityId("Project 1"))
    val project2 = Entity(EntityId("Project 2"))
    val project3 = Entity(EntityId("Project 3"))

    val review = ReviewId("Review")

    val analysis = AnalysisBuilder()
      .criteria("Criteria")
        .subCriteria("Sub-Criteria")
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