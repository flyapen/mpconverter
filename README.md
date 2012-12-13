The MediaPackage Converter is a Scala/sbt based project.

# Building Yourself
In order to build you need:

- Java 1.5+
- Scala and sbt. Go to http://typesafe.com/stack/download to download and install the Typesafe Stack.
- Maven for building Matterhorn artifacts

**Steps**

Build Matterhorn 1.4-SNAPSHOT

    $ svn co https://opencast.jira.com/svn/MH/branches/1.4.x matterhorn 
    $ cd matterhorn 
    $ mvn install     

Build MpConverter

    $ sbt one-jar
    $ cp target/scala-2.9.2/mpconverter_2.9.2-1.0-one-jar.jar bin/
    
# Binary Distribution
If you don't want to build MpConverter yourself you can find the binary in the `bin/` directory.

# Use
The only requirement for running the converter is a Java 1.5+ runtime. Run `mpconv.sh <mp.zip|mp.tgz|mp.tar.gz|directory>` and follow the instructions.

The `mpconv.sh` wrapper script does not run on Mac OS X platforms. Please use `java -jar mpconverter_2.9.2-1.0-one-jar.jar <arg> 2>/dev/null` instead.

# Notes           
Running the converter on a directory will rewrite or create the manifest *in place*. Please keep a copy if you need the original package. Passing in a zip or tgz however leaves the original media package untouched.
  
