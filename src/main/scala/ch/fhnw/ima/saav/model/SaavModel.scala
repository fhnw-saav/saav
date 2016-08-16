package ch.fhnw.ima.saav
package model

import ch.fhnw.ima.saav.model.colors._
import ch.fhnw.ima.saav.model.domain.{Analysis, Entity}

import scala.collection.immutable.ListSet

/** Application model (incl. presentation state). */
case class SaavModel(model: Either[NoDataModel, DataModel] = Left(NoDataModel(ImportNotStarted())))

case class NoDataModel(importState: ImportState)

sealed trait ImportState

final case class ImportNotStarted() extends ImportState

final case class ImportInProgress(progress: Float) extends ImportState

final case class ImportFailed(throwable: Throwable) extends ImportState

case class DataModel(
  analysis: Analysis[Entity],
  colors: Map[Entity, WebColor] = Map().withDefaultValue(DefaultColor),
  selectedEntities: ListSet[Entity],
  pinnedEntity: Option[Entity] = None
)