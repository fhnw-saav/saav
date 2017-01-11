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


  // allows provision of custom catalogs
  // e.g. http://fhnw-saav.github.io/saav?configFileUrl=http://127.0.0.1:3000/config/projects.json#/projects
  def getCustomConfigUrl: Option[String] = getUrlParameter("configFileUrl")

  // allows provision of custom data
  // e.g. http://fhnw-saav.github.io/saav?dataFileUrl=http://127.0.0.1:3000/data.csv#/projects
  def getCustomDataUrl: Option[String] = getUrlParameter("dataFileUrl")

}
