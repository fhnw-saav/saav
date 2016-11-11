addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.13")
addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.0-M14-7")
addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.3")
addSbtPlugin("com.typesafe.sbt" % "sbt-site" % "1.0.0")
addSbtPlugin("org.clapper" % "sbt-editsource" % "0.7.0")

addSbtPlugin("com.typesafe.sbt" % "sbt-ghpages" % "0.5.4")
// A dependency of the sbt-ghpages plugin (as opposed to a project dependency)
libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.21"