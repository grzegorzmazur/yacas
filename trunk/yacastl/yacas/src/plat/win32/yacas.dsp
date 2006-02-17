# Microsoft Developer Studio Project File - Name="yacas" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=yacas - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "yacas.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "yacas.mak" CFG="yacas - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "yacas - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "yacas - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "yacas - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "yacas - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /I "D:\work\src\YACAS\src" /I "D:\work\src\YACAS\src\plat\win32" /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"D:\work\src\nci\solib\yacas.lib"

!ENDIF 

# Begin Target

# Name "yacas - Win32 Release"
# Name "yacas - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=..\..\..\..\yacas\src\anumber.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\arggetter.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\arrayclass.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\commandline.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\deffile.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\errors.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\genericobject.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\genericstructs.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\grower.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\infixparser.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\lispatom.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\lispcleanupstack.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\lispenvironment.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\lispeval.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\lispevalhash.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\lisphash.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\lispio.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\lispobject.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\lispparser.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\lispplugin.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\lispstring.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\lispuserfunc.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\mathcommands.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\mathcommands2.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\mathcommands3.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\mathenvironment.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\mathuserfunc.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\mathutil.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\patcher.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\patternclass.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\patterns.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\platmath.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\standard.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\stdcommandline.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\stdfileio.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\stdstubs.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\stringio.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\substitute.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\testnum.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\tokenizer.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\win32dll.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\yacasapi.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\yacasnumbers.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=..\..\..\..\yacas\src\anumber.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\anumber.inl
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\arggetter.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\arrayclass.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\arrayclass.inl
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\choices.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\commandline.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\deffile.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\deffile.inl
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\elfdll.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\errors.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\evalfunc.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\genericobject.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\genericstructs.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\grower.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\grower.inl
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\infixparser.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\lispassert.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\lispatom.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\lispcleanupstack.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\lispenvironment.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\lisperror.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\lispeval.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\lispevalhash.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\lispglobals.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\lisphash.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\lisphash.inl
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\lispio.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\lispobject.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\lispobject.inl
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\lispparser.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\lispplugin.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\lispstring.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\lispstring.inl
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\plat\win32\lisptype.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\lispuserfunc.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\plat\win32\List.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\plat\win32\List.inl
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\log.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\mathcommands.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\mathenvironment.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\mathuserfunc.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\mathutil.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\numbers.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\patcher.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\patternclass.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\patterns.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\plat\win32\platfileio.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\platmath.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\ramdisk.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\refcount.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\standard.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\standard.inl
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\stdcommandline.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\stdfileio.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\stringio.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\stubs.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\plat\win32\stubs.inl
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\substitute.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\tokenizer.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\win32dll.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\yacas\src\yacas.h
# End Source File
# End Group
# End Target
# End Project
