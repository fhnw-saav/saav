package ch.fhnw.ima

import org.scalajs.dom

package object saav {

  // http://hseeberger.github.io/blog/2013/10/25/attention-seq-is-not-immutable

  type Seq[+A] = scala.collection.Seq[A]
  val Seq = scala.collection.Seq

  def getUrlParameter(param: String): Option[String] = {
    val searchUrl = dom.window.location.search.substring(1)
    val params = searchUrl.split('&')
    params.flatMap(p => {
      val s = p.split('=')
      if (s.length == 2) {
        Some((s(0), s(1)))
      } else {
        None
      }
    }).find((kv: (String, String)) => kv._1 == param).map(_._2)
  }


  // Optional URL parameter referencing analysis configuration (aka catalog)
  def getConfigFileUrl: Option[String] = getUrlParameter("configFileUrl")

  // Optional URL parameter to auto-load data
  def getDataFileUrl: Option[String] = getUrlParameter("dataFileUrl")

}
