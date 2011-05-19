
import sbt._

class Plugins( info:ProjectInfo ) extends PluginDefinition( info ) {
  val untypedRepo = "Untyped Repo" at "http://repo.untyped.com"
  val lessCompiler = "com.untyped" % "sbt-less" % "0.1"
}

