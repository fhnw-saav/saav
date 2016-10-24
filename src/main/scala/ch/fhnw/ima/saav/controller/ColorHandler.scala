package ch.fhnw.ima.saav.controller

import ch.fhnw.ima.saav.model.app.SaavModel
import ch.fhnw.ima.saav.model.color.WebColor
import ch.fhnw.ima.saav.model.domain.EntityId
import diode.{Action, ActionHandler, ActionResult, ModelRW}

final case class UpdateEntityColorAction(entity: EntityId, webColor: WebColor) extends Action

class ColorHandler[M](modelRW: ModelRW[M, Map[EntityId, WebColor]]) extends ActionHandler(modelRW) {

  override def handle: PartialFunction[Any, ActionResult[M]] = {
    case UpdateEntityColorAction(entity, color) => updated(value + (entity -> color))
  }

}

object ColorHandler {

  def modelGet: (SaavModel) => Map[EntityId, WebColor] =
    _.model.right.toOption.map(_.colorMap).getOrElse(Map())

  def modelSet: (SaavModel, Map[EntityId, WebColor]) => SaavModel = (m, v) =>
    m.copy(model = m.model.right.map(_.copy(colorMap = v)))

}