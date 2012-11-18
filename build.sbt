name := "mpconverter"

version := "1.0"

scalaVersion := "2.9.2"

libraryDependencies += "org.opencastproject" % "matterhorn-common" % "1.4-SNAPSHOT"

libraryDependencies += "org.slf4j" % "slf4j-api" % "1.5.8"

libraryDependencies += "org.slf4j" % "slf4j-log4j12" % "1.5.8"

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"
