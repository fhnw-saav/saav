package ch.fhnw.ima.saav.model

import ch.fhnw.ima.saav.model.app.{GroupedCriteria, GroupedSubCriteria}
import ch.fhnw.ima.saav.model.domain.{Criteria, SubCriteria}

/**
  * Computes layout parameters for presenting quality aspects of given criteria.
  */
class QualityLayout(criteria: Seq[GroupedCriteria], minValueOption: Option[Double], maxValueOption: Option[Double]) {

  // only relevant for aspect ratio and e.g. relative stroke width (svg will be scaled within parent element)
  val width = 1000
  val height = 500

  private val margin = 20
  val padding = 20
  private val verticalAxisGap = 70
  private val headerTextGap = 40

  val minValue = Math.min(0, minValueOption.getOrElse(0d))
  val maxValue = Math.max(0, maxValueOption.getOrElse(0d))

  private val criteriaBoxesMap = new scala.collection.mutable.HashMap[Criteria, (Int, Int)]
  private val criteriaAxesMap = new scala.collection.mutable.HashMap[Criteria, Int]
  private val subCriteriaAxesMap = new scala.collection.mutable.HashMap[SubCriteria, Int]
  private val subCriteriaDomainMap = new scala.collection.mutable.HashMap[SubCriteria, GroupedSubCriteria]

  // Compute general parameters

  val criteriaCount = criteria.size
  val axisCount = criteria.foldLeft(0)((count, c) => count + c.subCriteria.size)
  val axisSpacing = Math.max((width - ((criteriaCount + 1) * margin) - (criteriaCount * 2 * padding)) / (axisCount - criteriaCount), 0)
  val axisHeight = (height - headerTextGap - 2 * padding - verticalAxisGap - margin) / 2

  // Compute boxes y positions

  val boxTopY = headerTextGap
  val boxBotY = height - margin

  // Compute axes y positions

  val criteriaAxisTopY = headerTextGap + padding
  val criteriaAxisBotY = criteriaAxisTopY + axisHeight

  val subCriteriaAxisTopY = height - margin - padding - axisHeight
  val subCriteriaAxisBotY = subCriteriaAxisTopY + axisHeight

  // Compute boxes and axes x positions

  private var index = 0
  private var x = 0
  for (criterion <- criteria) {

    x = x + margin
    val criteriaWidth = 2 * padding + ((criterion.subCriteria.size - 1) * axisSpacing)

    criteriaBoxesMap(criterion.id) = (x, criteriaWidth)
    criteriaAxesMap(criterion.id) = x + (criteriaWidth / 2)

    var subIndex = 0
    for (subCriteria <- criterion.subCriteria) {
      subCriteriaAxesMap(subCriteria.id) = x + padding + (subIndex * axisSpacing)
      subIndex += 1

      subCriteriaDomainMap(subCriteria.id) = subCriteria
    }

    index += 1
    x = x + criteriaWidth
  }

  // And now for the getters or something...

  def getCriteriaBox(criteria: GroupedCriteria): (Int, Int) = criteriaBoxesMap(criteria.id)

  def getCriteriaAxisX(criteria: GroupedCriteria): Int = criteriaAxesMap(criteria.id)

  def getSubCriteriaAxisX(subCriteria: GroupedSubCriteria): Int = subCriteriaAxesMap(subCriteria.id)

  // TODO: does not work if called from text element in component
  def getAppSubCriteria(hoveredSubCriteria: Option[SubCriteria]): GroupedSubCriteria = {
    subCriteriaDomainMap(hoveredSubCriteria.get)
  }

}
