package ch.fhnw.ima.saav
package model

import ch.fhnw.ima.saav.model.model.Analysis
import ch.fhnw.ima.saav.model.model.Entity.Project

case class SaavModel(projectAnalysis: Either[ImportProgress, Analysis[Project]])

sealed trait ImportProgress

final case class NotStarted() extends ImportProgress

final case class InProgress(progress: Float) extends ImportProgress

final case class Failed(throwable: Throwable) extends ImportProgress
