
Experimental user interface based on the fltk widget set.
To build:
1) Download and install the fltk widget set if it doesn't yet
   exist on your system.
1) Build and install the regular Yacas console application.
2) Go into directory proteus/
3) Type 'make depend -f makefile.linux'
4) Type 'make -f makefile.linux'
5) Type 'make install -f makefile.linux'
6) To run, type './proteusworksheet' or './proteuscalculator'

And you're off! As an example, you might want to try the command:

	Notepad("./testnotes/index");

This shows a small example notepad with some links to other example
notepads.

Ayal Pinkus
