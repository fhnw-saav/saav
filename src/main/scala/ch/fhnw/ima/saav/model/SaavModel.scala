package ch.fhnw.ima.saav
package model

import ch.fhnw.ima.saav.model.model.Analysis
import ch.fhnw.ima.saav.model.model.Entity.Project

case class SaavModel(projectAnalysis: Either[ImportState, Analysis[Project]] = Left(ImportNotStarted()))

sealed trait ImportState

final case class ImportNotStarted() extends ImportState

final case class ImportInProgress(progress: Float) extends ImportState

final case class ImportFailed(throwable: Throwable) extends ImportState
