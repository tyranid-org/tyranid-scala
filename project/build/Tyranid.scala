
import sbt._

class Tyranid( info: ProjectInfo ) extends DefaultProject( info ) {
  // required because Ivy doesn't pull repositories from poms
  val scalaToolsRepo   = "scala-tools.org" at "http://scala-tools.org/repo-releases"
  val centralMavenRepo = "central-maven"   at "http://repo1.maven.org/maven2/"

  val liftVersion = "2.3"

	override def compileOptions = super.compileOptions ++ Seq(Unchecked)

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

    "com.mongodb.casbah"       %% "casbah"                   % "2.1.1"         % "compile->default",

    //"postgresql"              % "postgresql"               % "8.4-701.jdbc4" % "compile->default",
    //"xom"                       % "xom"                      % "1.1"           % "compile->default",

    "org.slf4j"                 % "slf4j-log4j12"            % "1.6.1",

    "javax.servlet"             % "servlet-api"              % "2.5",

    "org.scalatest"             % "scalatest"                % "1.3"           % "test->default"
  ) ++ super.libraryDependencies
}

