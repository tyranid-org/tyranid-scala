
import sbt._

class Plugins( info:ProjectInfo ) extends PluginDefinition( info ) {
  val akkaRepo   = "Akka Repo" at "http://akka.io/repository"
  val akkaPlugin = "se.scalablesolutions.akka" % "akka-sbt-plugin" % "1.1.3"

  val untypedRepo = "Untyped Repo" at "http://repo.untyped.com"
  val lessCompiler = "com.untyped" % "sbt-less" % "0.1"
}

