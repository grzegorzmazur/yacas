@ECHO OFF

ECHO Building SIS File ...

if %OS%!==Windows_NT! goto NT

CHDIR p:\yacas-1.0.53\epoc
Goto EndBit

:NT
CHDIR /D p:\yacas-1.0.53\epoc

:EndBit
MAKESIS -v "yacas.pkg"
