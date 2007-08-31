
This directory contains the Java version of the Yacas core
interpreter.

To build, type:

	make -f makefile.yacas

The interpreter can then be invoked from the command line
with:

	java -jar yacas.jar

or alternatively it can be invoked as an applet, by opening
yacasconsole.html. 

The binary files that comprise the entire binary release
for the Java version are:

1) yacas.jar - the Java class files in one jar file.
2) yacasconsole.html - the file that launches the applet.
3) hints.txt - the hints that are shown in the applet
   (the grey box with commands that match what you are typing in 
   at that moment).

The Java version has almost all the features the C++ version
has. In fact, there is no reason the Java version should not
have all the same features. At the time of writing (version 1.0.58),
plugins are not available yet, not all command line arguments
are available yet, and the command line prompt does not have the
history yet.


