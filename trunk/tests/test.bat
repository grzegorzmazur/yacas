
FOR %%a IN (*.yts) DO call ..\yacas -pc --archive ..\scripts.dat %%a
Echo "finished"
pause
