
import sbt._

import com.untyped.LessCssPlugin


class Tyranid( info: ProjectInfo ) extends DefaultProject( info ) {
	override def compileOptions = super.compileOptions ++ Seq(Deprecation, Unchecked)

  // Dependencies
  val scalaToolsRepo   = "scala-tools.org" at "http://scala-tools.org/repo-releases"
  val centralMavenRepo = "central-maven"   at "http://repo1.maven.org/maven2/"

  val liftVersion = "2.3"

  override def libraryDependencies = Set(
    "net.liftweb"              %% "lift-webkit"              % liftVersion     % "compile->default",
    "net.liftweb"              %% "lift-mapper"              % liftVersion     % "compile->default",
    "net.liftweb"              %% "lift-util"                % liftVersion     % "compile->default",
    "net.liftweb"              %% "lift-widgets"             % liftVersion     % "compile->default",

    "net.databinder"           %% "dispatch-http"            % "0.7.8"         % "compile->default",
    "net.databinder"           %% "dispatch-lift-json"       % "0.7.8"         % "compile->default",

    "org.codehaus.jackson"      % "jackson-core-asl"         % "1.7.2"         % "compile->default",
    "org.codehaus.jackson"      % "jackson-mapper-asl"       % "1.7.2"         % "compile->default",

    "org.mortbay.jetty"         % "jetty"                    % "6.1.22"        % "test->default",

    "org.mongodb"               % "mongo-java-driver"        % "2.5.3"         % "compile->default",

    //"postgresql"              % "postgresql"               % "8.4-701.jdbc4" % "compile->default",
    //"xom"                       % "xom"                      % "1.1"           % "compile->default",

    "org.slf4j"                 % "slf4j-log4j12"            % "1.6.1",

    "javax.servlet"             % "servlet-api"              % "2.5",

    "org.scalatest"             % "scalatest"                % "1.3"           % "test->default"
  ) ++ super.libraryDependencies

  // Publishing
  override def managedStyle = ManagedStyle.Maven
  val publishTo = "Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/releases/"
  Credentials( Path.userHome / ".nexus" / ".scala-tools-credentials", log )
}

