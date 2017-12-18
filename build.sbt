name := """vStats"""
organization := "com.vp"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

// scalaVersion := "2.12.4"
scalaVersion := "2.12.3"

libraryDependencies += guice
libraryDependencies += ws
libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.2" % Test
libraryDependencies += "org.neo4j.driver" % "neo4j-java-driver" % "1.5.0-beta03"
