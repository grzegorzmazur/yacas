
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
2) scripts.zip - all the scripts compressed in to one zip file.
3) yacasconsole.html - the file that launches the applet.
4) hints.txt - the hints that are shown in the applet
   (the grey box with commands that match what you are typing in 
   at that moment).
5) yacas.gif - the logo shown in the applet version.

To get copy-pasting to work in the applet version, you need
to tell the Java virtual machine that you trust this applet
to get or set data on the clipboard of your OS.

On Unix-like systems this means creating a file ~/.java.policy
if it does not already exist, and then adding the following
lines to it:

	grant codeBase "http://www.xs4all.nl/~apinkus/*" {
	permission java.awt.AWTPermission "accessClipboard";
	};

This grants all applets residing at http://www.xs4all.nl/~apinkus/*
access to the clipboard.

This can be done in a similar way on Windows, but the directory
where you need to place the .java.policy file depends on the
version of Windows that is running.

The Java version has almost all the features the C++ version
has. In fact, there is no reason the Java version should not
have all the same features. At the time of writing (version 1.0.58),
plugins are not available yet, not all command line arguments
are available yet, and the command line prompt does not have the
history yet.


