@echo off
FOR %%a IN (*.yts) DO call _do_test.bat %%a

rem FOR %%a IN (*.yts) DO call ..\yacas -pc --archive ..\scripts.dat %%a
@echo on
Echo Finished
pause
