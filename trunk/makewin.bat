
cd src\
call compress.bat
cd ..

mkdir winrel\
copy yacas.exe winrel\
copy src\scripts.dat winrel\
copy readme.bin.win32.txt winrel\readme.txt
copy copying winrel\copying.txt

mkdir winrel\DLL\
copy src\Release\yacasdll.dll winrel\DLL\
copy src\Release\yacasdll.lib winrel\DLL\
copy src\cyacas.h winrel\DLL\

mkdir winrel\DLL\examples\
copy embed\wintest1.* winrel\DLL\examples\

mkdir winrel\tests\
copy tests\*.yts winrel\tests\
copy tests\test.bat winrel\tests\

echo "finished"
pause
