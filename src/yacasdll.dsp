# Microsoft Developer Studio Project File - Name="yacasdll" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=yacasdll - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "yacasdll.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "yacasdll.mak" CFG="yacasdll - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "yacasdll - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "yacasdll - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "yacasdll - Win32 Release"

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
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "YACASDLL_EXPORTS" /Yu"stdafx.h" /FD /c
# ADD CPP /nologo /MT /W3 /GX /O2 /I "plat\win32" /I "\prj\yacas\src\\" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "YACASDLL_EXPORTS" /FR /FD /c
# SUBTRACT CPP /YX /Yc /Yu
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386

!ELSEIF  "$(CFG)" == "yacasdll - Win32 Debug"

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
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "YACASDLL_EXPORTS" /Yu"stdafx.h" /FD /GZ /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /I "plat\win32" /I "\prj\yacas\src\\" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "YACASDLL_EXPORTS" /FR /FD /GZ /c
# SUBTRACT CPP /YX /Yc /Yu
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept

!ENDIF 

# Begin Target

# Name "yacasdll - Win32 Release"
# Name "yacasdll - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=.\anumber.cpp
# End Source File
# Begin Source File

SOURCE=.\archiver.cpp
# End Source File
# Begin Source File

SOURCE=.\arggetter.cpp
# End Source File
# Begin Source File

SOURCE=.\arrayclass.cpp
# End Source File
# Begin Source File

SOURCE=.\commandline.cpp
# End Source File
# Begin Source File

SOURCE=.\ctokenizer.cpp
# End Source File
# Begin Source File

SOURCE=.\cyacas.cpp
# End Source File
# Begin Source File

SOURCE=.\deffile.cpp
# End Source File
# Begin Source File

SOURCE=.\errors.cpp
# End Source File
# Begin Source File

SOURCE=.\genericobject.cpp
# End Source File
# Begin Source File

SOURCE=.\genericstructs.cpp
# End Source File
# Begin Source File

SOURCE=.\grower.cpp
# End Source File
# Begin Source File

SOURCE=.\infixparser.cpp
# End Source File
# Begin Source File

SOURCE=.\lispatom.cpp
# End Source File
# Begin Source File

SOURCE=.\lispcleanupstack.cpp
# End Source File
# Begin Source File

SOURCE=.\lispenvironment.cpp
# End Source File
# Begin Source File

SOURCE=.\lispeval.cpp
# End Source File
# Begin Source File

SOURCE=.\lispevalhash.cpp
# End Source File
# Begin Source File

SOURCE=.\lisphash.cpp
# End Source File
# Begin Source File

SOURCE=.\lispio.cpp
# End Source File
# Begin Source File

SOURCE=.\lispobject.cpp
# End Source File
# Begin Source File

SOURCE=.\lispparser.cpp
# End Source File
# Begin Source File

SOURCE=.\lispplugin.cpp
# End Source File
# Begin Source File

SOURCE=.\lispstring.cpp
# End Source File
# Begin Source File

SOURCE=.\lispuserfunc.cpp
# End Source File
# Begin Source File

SOURCE=.\mathcommands.cpp
# End Source File
# Begin Source File

SOURCE=.\mathcommands2.cpp
# End Source File
# Begin Source File

SOURCE=.\mathcommands3.cpp
# End Source File
# Begin Source File

SOURCE=.\mathenvironment.cpp
# End Source File
# Begin Source File

SOURCE=.\mathuserfunc.cpp
# End Source File
# Begin Source File

SOURCE=.\mathutil.cpp
# End Source File
# Begin Source File

SOURCE=.\obmalloc.cpp
# End Source File
# Begin Source File

SOURCE=.\patcher.cpp
# End Source File
# Begin Source File

SOURCE=.\patternclass.cpp
# End Source File
# Begin Source File

SOURCE=.\patterns.cpp
# End Source File
# Begin Source File

SOURCE=.\platmath.cpp
# End Source File
# Begin Source File

SOURCE=.\standard.cpp
# End Source File
# Begin Source File

SOURCE=.\stdfileio.cpp
# End Source File
# Begin Source File

SOURCE=.\stdstubs.cpp
# End Source File
# Begin Source File

SOURCE=.\stringio.cpp
# End Source File
# Begin Source File

SOURCE=.\substitute.cpp
# End Source File
# Begin Source File

SOURCE=.\tokenizer.cpp
# End Source File
# Begin Source File

SOURCE=.\unipoly.cpp
# End Source File
# Begin Source File

SOURCE=.\win32dll.cpp
# End Source File
# Begin Source File

SOURCE=.\win32yacasdll.cpp
# End Source File
# Begin Source File

SOURCE=.\xmltokenizer.cpp
# End Source File
# Begin Source File

SOURCE=.\yacasapi.cpp
# End Source File
# Begin Source File

SOURCE=.\yacasbase.cpp
# End Source File
# Begin Source File

SOURCE=.\yacasnumbers.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\StdAfx.h
# End Source File
# Begin Source File

SOURCE=.\yacasdll.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# Begin Source File

SOURCE=.\cursor1.cur
# End Source File
# Begin Source File

SOURCE=.\yacasdll.rc
# End Source File
# End Group
# Begin Source File

SOURCE=.\ReadMe.txt
# End Source File
# End Target
# End Project
