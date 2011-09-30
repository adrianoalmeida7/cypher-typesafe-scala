name := "Cypher type safe"

version := "1.0"

organization := "Caelum"

scalaVersion := "2.9.0"

resolvers += "repo1"     at "http://repo1.maven.org/maven2/"


libraryDependencies <+= scalaVersion((v:String) => "org.scalatest" % ("scalatest_" + v) % "1.6.1" % "test")

libraryDependencies += "org.neo4j" % "neo4j-cypher" % "1.5.M01" % "compile"