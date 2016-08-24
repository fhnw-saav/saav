package ch.fhnw.ima.saav.component

import ch.fhnw.ima.saav.model.app.{DataModel, GroupedCriteria, GroupedSubCriteria}

/**
  * This class computes all the relevant layout parameters.
  */

class QualityLayout {
  val MARGIN = 10
  val PADDING = 20
  val VERTICAL_AXIS_GAP = 40

  private var criteriaAxisTopY = 0
  private var criteriaAxisBotY = 0
  private var subCriteriaAxisTopY = 0
  private var subCriteriaAxisBotY = 0

  private val criteriaBoxesMap = new scala.collection.mutable.HashMap[GroupedCriteria, (Int, Int)]
  private val criteriaAxesMap = new scala.collection.mutable.HashMap[GroupedCriteria, Int]
  private val subCriteriaAxesMap = new scala.collection.mutable.HashMap[GroupedSubCriteria, Int]

  def this(model: DataModel, width: Int, height: Int) {
    this()

    // Compute general parameters

    val criteriaCount = computeCriteriaCount(model)
    val axisCount = computeAxisCount(model)

    val axisSpacing = (width - ((criteriaCount+1) * MARGIN) - (criteriaCount * 2 * PADDING)) / (axisCount - criteriaCount)

    val axisHeight = (height - 2*PADDING - VERTICAL_AXIS_GAP) / 2

    // Compute axes y positions

    criteriaAxisTopY = PADDING
    criteriaAxisBotY = PADDING + axisHeight

    subCriteriaAxisTopY = height - PADDING - axisHeight
    subCriteriaAxisBotY = height - PADDING

    // Compute boxes and axes x positions

    var index = 0
    var x = 0
    for (criteria <- model.criteria) {

      x = x + MARGIN
      val criteriaWidth = 2*PADDING + ((criteria.subCriteria.size-1) * axisSpacing)

      criteriaBoxesMap(criteria) = (x, criteriaWidth)
      criteriaAxesMap(criteria) = x + (criteriaWidth / 2)

      var subIndex = 0
      for (subCriteria <- criteria.subCriteria) {
        subCriteriaAxesMap(subCriteria) = x + PADDING + (subIndex * axisSpacing)
        subIndex += 1
      }

      index += 1
      x = x + criteriaWidth
    }
  }

  def getCriteriaAxisTopY = criteriaAxisTopY
  def getCriteriaAxisBotY = criteriaAxisBotY
  def getSubCriteriaAxisTopY = subCriteriaAxisTopY
  def getSubCriteriaAxisBotY = subCriteriaAxisBotY

  def getCriteriaBox(criteria: GroupedCriteria): (Int, Int) = criteriaBoxesMap(criteria)
  def getCriteriaAxisX(criteria: GroupedCriteria): Int = criteriaAxesMap(criteria)
  def getSubCriteriaAxisX(subCriteria: GroupedSubCriteria): Int = subCriteriaAxesMap(subCriteria)


  private def computeCriteriaCount(model: DataModel) = {
    model.criteria.size
  }

  private def computeAxisCount(model: DataModel) = {
    var count = 0
    for (criteria <- model.criteria) {
      count += criteria.subCriteria.size
    }
    count
  }

}
