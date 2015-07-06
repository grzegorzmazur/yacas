@echo off

setlocal enabledelayedexpansion

set ARGC=0
for %%x in (%*) do set /a ARGC+=1

if %ARGC% equ 0 goto help
if %ARGC% gtr 1 goto nohelp

if "%1" == "-h" goto help
if "%1" == "--help" goto help
if "%1" == "/h" goto help
if "%1" == "/help" goto help

goto nohelp

:help

echo usage: %0 ^<cmd^> ^<dir^> ^<script^>...
echo   cmd       Command plus options, needed to run Yacas
echo   dir       Directory in which scripts reside
echo   script... Test scripts to be run
echo Test script may reside in <dir> or in the current directory
echo Exit status is number of tests scripts which fail

exit /b 0

:nohelp

if %ARGC% geq 2 goto enoughparams

echo error: %0: missing parameters
exit /b 255

:enoughparams

set CMD=%~1
set CMD=%CMD:\=/%
set SCRIPTDIR=%2
shift
shift

set "SCRIPTS="

:parse
if "%~1" neq "" (
  set SCRIPTS=%SCRIPTS% %1
  shift
  goto :parse
)
if defined SCRIPTS set SCRIPTS=%SCRIPTS:~1%

set FAILED_TESTS=
set FAILURES=0
set TOTALTESTS=0

set PID=123

set TESTFILE=%TEMP%\test-yacas.%PID%
set TIMEFILE=%TEMP%\time-yacas.%PID%
set VERSIONF=%TEMP%\version-yacas-%PID%.ys
set VERSIONF=%VERSIONF:\=/%
set LOGFILE=yacas-logfile.txt
echo Print(Version()); > %VERSIONF%

for /f %%a in ('%CMD% %VERSIONF%') do (
    if not "%%a" == "Quitting..." set VERSION=%%a
)

set COMMENT=!VERSION!

echo /* >> %LOGFILE%
echo Command = %CMD% >> %LOGFILE%
echo Version = %VERSION% >> %LOGFILE%
echo Comment = %COMMENT% >> %LOGFILE%
date /t >> %LOGFILE%
time /t >> %LOGFILE%
echo Logfile = %LOGFILE% >> %LOGFILE%
echo */ >> %LOGFILE%

echo Command = %CMD%
echo Version = %VERSION%
echo Comment = %COMMENT%
date /t
time /t
echo Logfile = %LOGFILE%

set SCRIPTDIR=%SCRIPTDIR:\=/%

set /a succ=0
set /a fail=0

for %%s in (%SCRIPTS%) do (

    set f=%%s

    if exist "%SCRIPTDIR%/%%s" set f=%SCRIPTDIR%/%%s

    echo Running %%s

    if exist %TESTFILE% del %TESTFILE%

    %CMD% !f! > %TESTFILE% 2>&1
	
	
	for %%A in (%TESTFILE%) do set size=%%~zA

	if !size! equ 0 (
		set /a succ+=1
		echo OK!
	) else (
	
		for /f "delims=:" %%i in ('FINDSTR  /N "Quitting..." %TESTFILE%') do set quittingLine=%%i
		
		if !quittingLine! equ 1 (
			set /a succ+=1
			echo OK
		) else (
			set /a fail+=1
			echo FAILED
			type %TESTFILE% 
		)
	)
)

echo Tests succeeded: %succ%
echo Tests failed: %fail%

exit /b %fail%