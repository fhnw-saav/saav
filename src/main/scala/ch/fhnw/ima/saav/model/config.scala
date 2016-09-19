package ch.fhnw.ima.saav.model

import ch.fhnw.ima.saav.model.app.Weights

object config {

  trait Config {

    def defaultWeights: Weights

  }

}
