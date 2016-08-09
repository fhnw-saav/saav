package ch.fhnw.ima

package object saav {

  // http://hseeberger.github.io/blog/2013/10/25/attention-seq-is-not-immutable

  type Seq[+A] = scala.collection.Seq[A]
  val Seq = scala.collection.Seq

}
