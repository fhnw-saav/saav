package ch.fhnw.ima.saav
package model

import ch.fhnw.ima.saav.model.colors.WebColor
import ch.fhnw.ima.saav.model.domain.{Analysis, Entity}

// TODO: Split into case classes (SaavModelImportPending, SaavModelAnalysisReady)

/** Application model (incl. presentation state). */
case class SaavModel(analysis: Either[ImportState, Analysis[Entity]] = Left(ImportNotStarted()), colors: Map[Entity, WebColor] = Map().withDefaultValue(WebColor("#000000")))

sealed trait ImportState

final case class ImportNotStarted() extends ImportState

final case class ImportInProgress(progress: Float) extends ImportState

final case class ImportFailed(throwable: Throwable) extends ImportState