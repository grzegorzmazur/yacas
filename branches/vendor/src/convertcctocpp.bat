@echo off
echo Converting C++ source files in ramscripts\
cd ramscripts
ren *.cc *.cpp
echo Converting C++ source files in server\
cd ..\server
ren *.cc *.cpp
echo Converting C++ source files in src\
cd ..\src
ren *.cc *.cpp