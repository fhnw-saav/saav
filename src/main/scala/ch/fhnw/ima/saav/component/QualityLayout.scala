package ch.fhnw.ima.saav.component

import ch.fhnw.ima.saav.model.app.{DataModel, GroupedCriteria, GroupedSubCriteria}
import ch.fhnw.ima.saav.model.domain.{Criteria, SubCriteria}

/**
  * This class computes all the relevant layout parameters.
  */

class QualityLayout {
  val MARGIN = 20
  val PADDING = 20
  val VERTICAL_AXIS_GAP = 70
  val HEADER_TEXT_GAP = 40

  // TODO: these do not really belong here, should be part of the data model, probably be defined in the data itself
  private var minValue: Double = 0
  private var maxValue: Double = 0

  private var boxTopY = 0
  private var boxBotY = 0

  private var criteriaAxisTopY = 0
  private var criteriaAxisBotY = 0
  private var subCriteriaAxisTopY = 0
  private var subCriteriaAxisBotY = 0

  private val criteriaBoxesMap = new scala.collection.mutable.HashMap[Criteria, (Int, Int)]
  private val criteriaAxesMap = new scala.collection.mutable.HashMap[Criteria, Int]
  private val subCriteriaAxesMap = new scala.collection.mutable.HashMap[SubCriteria, Int]

  private val subCriteriaDomainMap = new scala.collection.mutable.HashMap[SubCriteria, GroupedSubCriteria]

  def this(model: DataModel, width: Int, height: Int) {
    this()

    // Compute general parameters

    val criteriaCount = computeCriteriaCount(model)
    val axisCount = computeAxisCount(model)

    val axisSpacing = Math.max((width - ((criteriaCount+1) * MARGIN) - (criteriaCount * 2 * PADDING)) / (axisCount - criteriaCount), 0)

    val axisHeight = (height - HEADER_TEXT_GAP - 2*PADDING - VERTICAL_AXIS_GAP - MARGIN) / 2

    // Compute boxes y positions

    boxTopY = HEADER_TEXT_GAP
    boxBotY = height - MARGIN

    // Compute axes y positions

    criteriaAxisTopY = HEADER_TEXT_GAP + PADDING
    criteriaAxisBotY = criteriaAxisTopY + axisHeight

    subCriteriaAxisTopY = height - MARGIN - PADDING - axisHeight
    subCriteriaAxisBotY = subCriteriaAxisTopY + axisHeight

    // Compute boxes and axes x positions

    var index = 0
    var x = 0
    for (criteria <- model.criteria) {

      x = x + MARGIN
      val criteriaWidth = 2*PADDING + ((criteria.subCriteria.size-1) * axisSpacing)

      criteriaBoxesMap(criteria.id) = (x, criteriaWidth)
      criteriaAxesMap(criteria.id) = x + (criteriaWidth / 2)

      var subIndex = 0
      for (subCriteria <- criteria.subCriteria) {
        subCriteriaAxesMap(subCriteria.id) = x + PADDING + (subIndex * axisSpacing)
        subIndex += 1

        subCriteriaDomainMap(subCriteria.id) = subCriteria
      }

      index += 1
      x = x + criteriaWidth
    }

    // Compute boxes and axes x positions

    for (criteria <- model.criteria) {
     for (subCriteria <- criteria.subCriteria) {
       for (entity <- model.rankedEntities) {
         val value = subCriteria.groupedValues(entity.id).get
         minValue = Math.min(minValue, value)
         maxValue = Math.max(maxValue, value)
       }
      }
    }

  }


  // And now for the getters or something...

  def getMinValue = minValue
  def getMaxValue = maxValue

  def getBoxTopY = boxTopY
  def getBoxBotY = boxBotY

  def getCriteriaAxisTopY = criteriaAxisTopY
  def getCriteriaAxisBotY = criteriaAxisBotY
  def getSubCriteriaAxisTopY = subCriteriaAxisTopY
  def getSubCriteriaAxisBotY = subCriteriaAxisBotY

  def getCriteriaBox(criteria: GroupedCriteria): (Int, Int) = criteriaBoxesMap(criteria.id)
  def getCriteriaAxisX(criteria: GroupedCriteria): Int = criteriaAxesMap(criteria.id)
  def getSubCriteriaAxisX(subCriteria: GroupedSubCriteria): Int = subCriteriaAxesMap(subCriteria.id)


  // TODO: does not work if called from text element in component
  def getAppSubCriteria(hoveredSubCriteria: Option[SubCriteria]): GroupedSubCriteria = {
    subCriteriaDomainMap(hoveredSubCriteria.get)
  }

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
