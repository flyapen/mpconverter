#!/bin/sh
SCRIPT=$(readlink -f $0)
SCRIPTPATH=$(dirname $SCRIPT)
java -jar $SCRIPTPATH/mpconverter_2.9.2-1.0-one-jar.jar $1 2>/dev/null