

YACAS, INTRODUCTION
===================

This is the Windows binary release of Yacas. Yacas is a computer algebra
system. 


INSTALLING YACAS
================

You should find the following files in this distribution: 

    	yacas.exe   - the executable which you need to run to start yacas
    	scripts.dat - file containing some extra data needed by the executable
    	readme.txt  - this file
      copying.txt - copyright notice

In addition, the documentation to Yacas can be downloaded from the Yacas
web site, in html, pdf and postscript format.


RUNNING YACAS
=============

To run Yacas, you need to extract the files mentioned above from the zip
file into some directory. Yacas needs to run from the directory the files
were extracted in, or, alternatively, you would need to
pass on extra parameters on the command
line to tell yacas where to find the 'scripts.dat' file. For example, if Yacas
was extracted into c:\myprogs\yacas\, then it can be invoked with :

    	c:\myprogs\yacas\yacas.exe --archive c:\myprogs\yacas\scripts.dat

The windows version currently works as a console application, so you would need to run it from a command prompt.

ONLINE HELP
===========

The documentation for Yacas is not included with the binary release for
Windows. Therefore online help (?, ??) will not work out of the box. The HTML
format documentation can be downloaded from the Yacas site and installed in the
"documentation" subdirectory. Then Yacas should be started with a "-rootdir"
option pointing to that location. For example: if c:\yacas\documentation exists, then one can run

	yacas --rootdir c:\yacas\documentation

To get interactive help, a console-mode HTML browser such as "lynx" should be
installed so that it is on the path (or, again, in the same directory as the
Yacas executable is run from).

If this is too much trouble, read the HTML documentation using any Web browser, or print the PS/PDF files.

PLOTTING WITH GNUPLOT
=====================

Yacas comes with support for the free "gnuplot" program. The Windows version of
gnuplot can be downloaded from http://www.gnuplot.info/ and should be unpacked
either onto the path (C:\WINDOWS\ or such), or into the same directory as the
Yacas executable is run from. The name of the gnuplot executable must be
"wgnupl32.exe". Plotting will fail if the current directory is not writable
(i.e. do not run it all from a CDROM).

Note that if you are running Yacas under the CYGWIN environment, then plotting may not work (it is not tested).


USING THE YACAS DLL FROM WITHIN YOUR OWN C++ PROJECT IN MS DEVELOPER STUDIO
===========================================================================

The yacadll.dll dynamic link library offers the API described in
the cyacas.h header file. In order to use the DLL, one needs to:

1) include the "cyacas.h" header file in the C or C++ file that needs access to the API
2) change the settings so the compiler knows where to find the "cyacas.h" header file
3) Link against the yacasdll.lib file
2) change the settings so the compiler knows where to find the "yacasdll.lib" library file
5) the file yacasdll.dll has to be placed in the right place (placing it in the same directory 
   as the executable will work).

The DLL\ subdirectory contains the DLL, LIB and H files required, as well as a subdirectory
examples\ with some example projects.


TEST SUITE
==========

The Yacas test suite is included with the distribution. Just run test.bat
in the tests\ directory. These files also provide a nice set of example
expressions you might want to play around with.


TERMS OF USE
============

This software package is derived from the Yacas source code.
Yacas is released in source code format, under the General Public License (GPL).
The binary release of this package is bound to that license.

The Yacas source code can be found at:

      http://www.xs4all.nl/~apinkus/yacas.html

The license and terms of use have been included in the file 'copying.txt'
that accompanies this package.
The program comes WITHOUT WARRANTY.

Enjoy!
The Yacas development team




