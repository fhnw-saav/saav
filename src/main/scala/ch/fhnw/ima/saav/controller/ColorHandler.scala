package ch.fhnw.ima.saav.controller

import ch.fhnw.ima.saav._
import ch.fhnw.ima.saav.model.app.SaavModel
import ch.fhnw.ima.saav.model.color.{WebColor, _}
import ch.fhnw.ima.saav.model.domain.Entity
import diode.{Action, ActionHandler, ActionResult, ModelRW}

final case class AutoColorizeAction(entities: Seq[Entity]) extends Action

final case class UpdateEntityColorAction(entity: Entity, webColor: WebColor) extends Action

class ColorHandler[M](modelRW: ModelRW[M, Map[Entity, WebColor]]) extends ActionHandler(modelRW) {

  override def handle: PartialFunction[Any, ActionResult[M]] = {
    case AutoColorizeAction(entities) => updated(autoColorMap(entities))
    case UpdateEntityColorAction(entity, color) => updated(value + (entity -> color))
  }

}

object ColorHandler {

  def modelGet: (SaavModel) => Map[Entity, WebColor] =
    _.model.right.toOption.map(_.colorMap).getOrElse(Map())

  def modelSet: (SaavModel, Map[Entity, WebColor]) => SaavModel = (m, v) =>
    m.copy(model = m.model.right.map(_.copy(colorMap = v)))

}