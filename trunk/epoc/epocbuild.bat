@ECHO OFF

TITLE Yacas build
CHDIR p:\yacas-1.0.53\src

if %OS%!==Windows_NT! goto NT

rem IF EXIST p:\yacas-1.0.53\src\EPOCYACAS.MARM nmake -f p:\yacas-1.0.53\src\EPOCYACAS.MARM > p:\yacas-1.0.53\src\EPOCYACAS.MARM.log
rem Goto EndBit


:NT
ECHO Searching for script: p:\yacas-1.0.53\epoc\GenRSS
IF EXIST p:\yacas-1.0.53\epoc\GenRSS.bat CALL p:\yacas-1.0.53\epoc\GenRSS.bat > p:\yacas-1.0.53\epoc\GenRSS.log 2>&1



ECHO Searching for makfile: p:\yacas-1.0.53\src\EPOCCLI.MARM
IF EXIST p:\yacas-1.0.53\src\EPOCCLI.MARM nmake -f p:\yacas-1.0.53\src\EPOCCLI.MARM > p:\yacas-1.0.53\src\EPOCCLI.MARM.log 2>&1
ECHO Searching for makfile: p:\yacas-1.0.53\src\EPOCSERV.MARM
IF EXIST p:\yacas-1.0.53\src\EPOCSERV.MARM nmake -f p:\yacas-1.0.53\src\EPOCSERV.MARM > p:\yacas-1.0.53\src\EPOCSERV.MARM.log 2>&1


ECHO Searching for script: p:\yacas-1.0.53\epoc\GenMBM
IF EXIST p:\yacas-1.0.53\epoc\GenMBM.bat CALL p:\yacas-1.0.53\epoc\GenMBM.bat > p:\yacas-1.0.53\epoc\GenMBM.log 2>&1
ECHO Searching for script: p:\yacas-1.0.53\epoc\GenAIF
IF EXIST p:\yacas-1.0.53\epoc\GenAIF.bat CALL p:\yacas-1.0.53\epoc\GenAIF.bat > p:\yacas-1.0.53\epoc\GenAIF.log 2>&1
ECHO Searching for script: p:\yacas-1.0.53\epoc\CopyAIF
rem IF EXIST p:\yacas-1.0.53\epoc\CopyAIF.bat CALL p:\yacas-1.0.53\epoc\CopyAIF.bat > p:\yacas-1.0.53\epoc\CopyAIF.log 2>&1
rem ECHO Searching for script: p:\yacas-1.0.53\help\HelpGen
rem IF EXIST p:\yacas-1.0.53\help\HelpGen.bat CALL p:\yacas-1.0.53\help\HelpGen.bat > rem p:\yacas-1.0.53\help\HelpGen.log 2>&1
rem ECHO Searching for script: p:\yacas-1.0.53\help\HelpCopy
rem IF EXIST p:\yacas-1.0.53\help\HelpCopy.bat CALL p:\yacas-1.0.53\help\HelpCopy.bat > p:\yacas-1.0.53\help\HelpCopy.log 2>&1
ECHO Searching for script: p:\yacas-1.0.53\epoc\GenSIS
IF EXIST p:\yacas-1.0.53\epoc\GenSIS.bat CALL p:\yacas-1.0.53\epoc\GenSIS.bat > p:\yacas-1.0.53\epoc\GenSIS.log 2>&1

:EndBit


REM
REM Start the EPOC Emulator
REM
REM p:\epoc32\release\wins\deb\epoc.exe
CHDIR p:\yacas-1.0.53\epoc
