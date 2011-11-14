
import sbt._
import Keys._


object Tyranid extends Build {
  import less.Plugin.LessKeys
  import coffeescript.Plugin.CoffeeKeys

  val liftVersion = "2.4-M4"

  val commonLibs = Seq(
    "net.liftweb"              %% "lift-webkit"              % liftVersion       % "compile->default", // Lift, Scala Web Framework
    "net.liftweb"              %% "lift-mapper"              % liftVersion       % "compile->default",
    "net.liftweb"              %% "lift-util"                % liftVersion       % "compile->default",
    "net.liftweb"              %% "lift-widgets"             % liftVersion       % "compile->default",

    "org.scala-tools.time"     %% "time"                     % "0.5"             % "compile->default",

    "net.databinder"            %% "dispatch-http"            % "0.8.5"         % "compile->default",
    "net.databinder"            %% "dispatch-lift-json"       % "0.8.5"         % "compile->default",

    "org.codehaus.jackson"      % "jackson-core-asl"         % "1.9.0"           % "compile->default", // JSON Parsing Library
    "org.codehaus.jackson"      % "jackson-mapper-asl"       % "1.9.0"           % "compile->default", // adds ObjectMapper and TreeMapper capability to the above 

    "org.mongodb"               % "mongo-java-driver"        % "2.6.3"           % "compile->default",

    "com.amazonaws"             % "aws-java-sdk"             % "1.1.9"           % "compile->default", // Amazon AWS Java API

    //"postgresql"                % "postgresql"               % "8.4-701.jdbc4"   % "compile->default", // Java API to PostgreSQL
    //"xom"                       % "xom"                      % "1.1"             % "compile->default", // mutable Java XML API

    "org.slf4j"                 % "slf4j-log4j12"            % "1.6.1"           % "compile",          // Logging Wrapper

    "javax.servlet"             % "servlet-api"              % "2.5"             % "provided->default",// Java Servlet API

    "se.scalablesolutions.akka" % "akka-actor"               % "1.2",

    "org.scalatest"             % "scalatest"                % "1.3"             % "test->default"     // ScalaTest which is compatible with TestNG  (for Scala testing)
  )

  val commonSettings = Defaults.defaultSettings ++ Seq(
    organization         := "org.tyranid",
    version              := "0.2.16",
    scalaVersion         := "2.9.1",
    libraryDependencies ++= commonLibs,
    resolvers            += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
    publishTo            := Some("Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/releases/"),
    credentials          += Credentials( Path.userHome / ".nexus" / ".scala-tools-credentials" ),
    publishMavenStyle    := true
  ) ++
    less.Plugin.lessSettings ++
    coffeescript.Plugin.coffeeSettings ++
    Seq( (resourceManaged in (Compile, LessKeys.less) ) <<= ( sourceDirectory in Compile )( _ / "webapp" / "lcss" ) ) ++
    Seq( (resourceManaged in (Compile, CoffeeKeys.coffee) ) <<= ( sourceDirectory in Compile )( _ / "webapp" / "cjs" ) )

  lazy val tyranid = Project(
    id       = "tyranid",
    base     = file( "." ),
    settings = commonSettings
  )
}

