package ch.fhnw.ima.saav
package model

import ch.fhnw.ima.saav.model.model.{Analysis, Entity}

case class SaavModel(analysis: Either[ImportState, Analysis[Entity]] = Left(ImportNotStarted()))

sealed trait ImportState

final case class ImportNotStarted() extends ImportState

final case class ImportInProgress(progress: Float) extends ImportState

final case class ImportFailed(throwable: Throwable) extends ImportState
