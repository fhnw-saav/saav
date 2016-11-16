package ch.fhnw.ima.saav.controller.io

import ch.fhnw.ima.saav.model.config.AnalysisConfig
import org.scalajs.dom._
import org.scalajs.dom.raw.XMLHttpRequest

import scala.concurrent.{Future, Promise}

object AnalysisConfigImporter {

  def importConfig(configFileUrl: String): Future[AnalysisConfig] = {
    val xhr = new XMLHttpRequest()
    xhr.open("GET", configFileUrl)
    val resultPromise = Promise[AnalysisConfig]()
    xhr.onload = { (_: Event) =>
      if (xhr.status == 200) {
        val json = xhr.responseText
        println(s"[${getClass.getSimpleName}] Fetched $configFileUrl:\n$json")
        val eitherConfigOrError = AnalysisConfig.fromJson(json)
        eitherConfigOrError match {
          case Right(analysisConfig: AnalysisConfig) =>
            resultPromise.success(analysisConfig)
          case Left(error: io.circe.Error) =>
            resultPromise.failure(error.fillInStackTrace())
        }
      } else {
        resultPromise.failure(new IllegalStateException(s"Failed to retrieve configuration '$configFileUrl'"))
      }
    }
    xhr.send()
    resultPromise.future
  }

}
