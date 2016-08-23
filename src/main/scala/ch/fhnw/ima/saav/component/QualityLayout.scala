package ch.fhnw.ima.saav.component

import ch.fhnw.ima.saav.model.app.{DataModel, PlottableCategory, PlottableSubCategory}

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

  private val criteriaBoxesMap = new scala.collection.mutable.HashMap[PlottableCategory, (Int, Int)]
  private val criteriaAxesMap = new scala.collection.mutable.HashMap[PlottableCategory, Int]
  private val subCriteriaAxesMap = new scala.collection.mutable.HashMap[PlottableSubCategory, Int]

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
    for (category <- model.categories) {

      x = x + MARGIN
      val criteriaWidth = 2*PADDING + ((category.subCategories.size-1) * axisSpacing)

      criteriaBoxesMap(category) = (x, criteriaWidth)
      criteriaAxesMap(category) = x + (criteriaWidth / 2)

      var subIndex = 0
      // intellij does not catch it if we forget the .subCategories
      for (subCategory <- category.subCategories) {
        subCriteriaAxesMap(subCategory) = x + PADDING + (subIndex * axisSpacing)
        subIndex += 1
      }

      index += 1
      x = x + criteriaWidth
    }
  }

  def getCategoryAxisTopY = criteriaAxisTopY
  def getCategoryAxisBotY = criteriaAxisBotY
  def getSubCategoryAxisTopY = subCriteriaAxisTopY
  def getSubCategoryAxisBotY = subCriteriaAxisBotY

  def getCriteriaBox(category: PlottableCategory): (Int, Int) = criteriaBoxesMap(category)
  def getCriteriaAxisX(category: PlottableCategory): Int = criteriaAxesMap(category)
  def getSubCriteriaAxisX(subCategory: PlottableSubCategory): Int = subCriteriaAxesMap(subCategory)


  private def computeCriteriaCount(model: DataModel) = {
    model.categories.size
  }

  private def computeAxisCount(model: DataModel) = {
    var count = 0
    for (category <- model.categories) {
      count += category.subCategories.size
    }
    count
  }

}
