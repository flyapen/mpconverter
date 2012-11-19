name := "mpconverter"

version := "1.0"

scalaVersion := "2.9.2"

libraryDependencies += "org.opencastproject" % "matterhorn-common" % "1.4-SNAPSHOT"

libraryDependencies += "jline" % "jline" % "0.9.94"

libraryDependencies += "org.slf4j" % "slf4j-api" % "1.5.8"

libraryDependencies += "org.slf4j" % "slf4j-log4j12" % "1.5.8"

// used by the one-jar plugin
seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"
