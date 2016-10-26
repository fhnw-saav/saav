package ch.fhnw.ima.saav
package model

import ch.fhnw.ima.saav.model.app.{GroupedCriteria, GroupedSubCriteria}
import ch.fhnw.ima.saav.model.domain.{CriteriaId, SubCriteriaId}

object layout {

  object QualityChartLayout {

    val Height: Int = 500
    val Padding: Int = 20
    val SubCriteriaLabelPadding: Int = 5
    val Margin: Int = 20
    val VerticalAxisGap: Int = 70
    val HeaderTextGap: Int = 40
    val AxisHeight: Int = (Height - HeaderTextGap - 2 * Padding - VerticalAxisGap - Margin) / 2

    // Boxes y positions

    val BoxTopY: Int = HeaderTextGap
    val BoxBotY: Int = Height - Margin
    val SubCriteriaAxisTopY: Int = Height - Margin - Padding - AxisHeight
    val SubCriteriaAxisBotY: Int = SubCriteriaAxisTopY + AxisHeight

    // Axes y positions

    val CriteriaAxisTopY: Int = HeaderTextGap + Padding
    val CriteriaAxisBotY: Int = CriteriaAxisTopY + AxisHeight

  }

  /**
    * Computes layout parameters for presenting quality aspects of given criteria.
    */
  class QualityChartLayout(val width: Int, val criteria: Seq[GroupedCriteria], minValueOption: Option[Double], maxValueOption: Option[Double]) {

    import QualityChartLayout._

    val minValue: Double = Math.min(0, minValueOption.getOrElse(0d))
    val maxValue: Double = Math.max(0, maxValueOption.getOrElse(0d))

    private val criteriaBoxesMap = new scala.collection.mutable.HashMap[CriteriaId, (Int, Int)]
    private val criteriaAxesMap = new scala.collection.mutable.HashMap[CriteriaId, Int]
    private val subCriteriaAxesMap = new scala.collection.mutable.HashMap[SubCriteriaId, Int]
    private val subCriteriaDomainMap = new scala.collection.mutable.HashMap[SubCriteriaId, GroupedSubCriteria]

    // Compute general parameters

    private val criteriaCount = criteria.size
    private val axisCount = criteria.foldLeft(0)((count, c) => count + c.subCriteria.size)

    // Calculate axis spacing
    // In the special case of each criterion having exactly one sub-criterion, axis spacing should be 0
    // but we still need to make use of available space > tracked as extraPadding
    private val (axisSpacing, extraPadding) = {
      val axisGapCount = axisCount - criteriaCount
      val availableWidth = width - ((criteriaCount + 1) * Margin) - (criteriaCount * 2 * Padding)
      if (criteriaCount == 0) {
        (0, availableWidth)
      } else if (axisGapCount == 0) {
        (0, availableWidth / criteriaCount)
      } else {
        (Math.max(availableWidth / axisGapCount, 0), 0)
      }
    }

    // Compute boxes and axes x positions

    private var index = 0
    private var x = 0
    for (criterion <- criteria) {

      x = x + Margin
      val criteriaWidth = 2 * Padding + ((criterion.subCriteria.size - 1) * axisSpacing) + extraPadding

      criteriaBoxesMap(criterion.id) = (x, criteriaWidth)
      criteriaAxesMap(criterion.id) = x + (criteriaWidth / 2)

      var subIndex = 0
      for (subCriteria <- criterion.subCriteria) {
        subCriteriaAxesMap(subCriteria.id) = x + Padding + (extraPadding / 2) + (subIndex * axisSpacing)
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

  }


  // Why do we do this? And why not like the other constants in the class below?
  object ProfileChartLayout {
    val height = 500
    val subCriteriaLabelPadding = 5
  }

  /**
    * Computes layout parameters for presenting profile aspects of given criteria.
    */
  final class ProfileChartLayout(val width: Int, val criteria: Seq[GroupedCriteria], minValueOption: Option[Double], maxValueOption: Option[Double]) {

    import ProfileChartLayout._

    // TODO: share/connect with quality chart layout?
    val margin = 20
    val padding = 20
    val headerTextGap = 40

    val minValue: Double = Math.min(0, minValueOption.getOrElse(0d))
    val maxValue: Double = Math.max(0, maxValueOption.getOrElse(0d))

    private val criteriaBoxesMap = new scala.collection.mutable.HashMap[CriteriaId, (Int, Int)]
    private val criteriaCenterMap = new scala.collection.mutable.HashMap[CriteriaId, Int]
    private val subCriteriaCenterMap = new scala.collection.mutable.HashMap[SubCriteriaId, Int]

    private val subCriteriaCount = criteria.foldLeft(0)((count, c) => count + c.subCriteria.size)

    private val aggregatedCriteriaCount = getAggregatedCriteriaCount(criteria)

    val boxTopY: Int = headerTextGap
    val boxBotY: Int = height - margin

    private val circleCount = aggregatedCriteriaCount + subCriteriaCount
    private val columnWidth = (width - margin - (criteria.size * margin)) / circleCount

    private var index = 0
    private var x = margin
    for (criterion <- criteria) {
      val columnCount = criterion.subCriteria.size + (if (isCriteriaAggregated(criterion)) 1 else 0)
      val criteriaWidth = columnCount * columnWidth
      criteriaBoxesMap(criterion.id) = (x, criteriaWidth)

      if (isCriteriaAggregated(criterion)) {
        val criteriaX = x + columnWidth/2
        criteriaCenterMap(criterion.id) = criteriaX
        x = x + columnWidth
      }

      var subIndex = 0
      for (subCriteria <- criterion.subCriteria) {
        val subCriteriaX = x + columnWidth/2
        subCriteriaCenterMap(subCriteria.id) = subCriteriaX

        x = x + columnWidth
        subIndex += 1
      }

      x = x + margin
      index += 1
    }



    // Prepare for when/If we have criteria that do not get aggregated...

    def getAggregatedCriteriaCount(criteria: Seq[GroupedCriteria]): Int = criteria.size
    def isCriteriaAggregated(criteria: GroupedCriteria): Boolean = true

    // And now for the getters or something...

    def getCriteriaBox(criteria: GroupedCriteria): (Int, Int) = criteriaBoxesMap(criteria.id)

    def getCriteriaCenterX(criteria: GroupedCriteria): Option[Int] = criteriaCenterMap.get(criteria.id)
    def getSubCriteriaCenterX(subCriteria: GroupedSubCriteria): Option[Int] = subCriteriaCenterMap.get(subCriteria.id)

    def getRowHeight(entityCount: Int): Int = {
      val availableHeight = height - headerTextGap - margin
      availableHeight / entityCount
    }

    def getEntityCenterY(entityIndex: Int, entityCount: Int): Int = {
      val rowHeight = getRowHeight(entityCount)
      headerTextGap + (entityIndex * rowHeight) + rowHeight/2
    }

    def getEntityIndex(y: Double, entityCount: Int): Int = {
      ((y - headerTextGap) / getRowHeight(entityCount)).toInt
    }

    def getMaxRadius(entityCount: Int): Double = {
      Math.min(columnWidth, getRowHeight(entityCount)) * 0.9 / 2
    }

  }

}
