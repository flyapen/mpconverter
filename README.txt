The MpConverter is a Scala/sbt based project.

In order to build you need:
- Scala and sbt. Go to http://typesafe.com/stack/download to download and install the Typesafe Stack.
- Maven for building Matterhorn artifacts
- The Matterhorn 1.4-SNAPSHOT artifacts in your local maven repository.
  Check out Matterhorn from the Opencast repository (svn co https://opencast.jira.com/svn/MH/branches/1.4.x)
  and run "mvn install" in the base directory.

To build the script run "sbt one-jar" in the projects base dir and copy the resulting jar next to "mpconv.sh".
Run "mpconv.sh" and follow the instructions.

