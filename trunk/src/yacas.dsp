# Microsoft Developer Studio Project File - Name="yacas" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

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
!MESSAGE "yacas - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "yacas - Win32 Debug" (based on "Win32 (x86) Console Application")
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
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /I "\prj\yacas\src\\" /I "\work\yacas\src\\" /I "\work\yacas\src\plat\win32\\" /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D "_MBCS" /D "SUPPORT_SERVER" /YX /FD /c
# ADD BASE RSC /l 0x413 /d "NDEBUG"
# ADD RSC /l 0x413 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib ws2_32.lib /nologo /subsystem:console /machine:I386 /out:"..\yacas.exe"

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
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /I "\work\yacas\src\\" /I "\work\yacas\src\plat\win32\\" /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D "_MBCS" /D "SUPPORT_SERVER" /FR /YX /FD /GZ /c
# ADD BASE RSC /l 0x413 /d "_DEBUG"
# ADD RSC /l 0x413 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib ws2_32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept

!ENDIF 

# Begin Target

# Name "yacas - Win32 Release"
# Name "yacas - Win32 Debug"
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

SOURCE=.\compressedfiles.cpp
# End Source File
# Begin Source File

SOURCE=.\deffile.cpp
# End Source File
# Begin Source File

SOURCE=.\errors.cpp
# End Source File
# Begin Source File

SOURCE=.\exedll.cpp
# End Source File
# Begin Source File

SOURCE=.\filescanner.cpp
# End Source File
# Begin Source File

SOURCE=.\genericobject.cpp
# End Source File
# Begin Source File

SOURCE=.\genericstructs.cpp
# End Source File
# Begin Source File

SOURCE=..\plugins\pcre\get.c
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

SOURCE=..\plugins\pcre\maketables.c
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

SOURCE=.\numbers.cpp
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

SOURCE=..\plugins\pcre\pcre.c
# End Source File
# Begin Source File

SOURCE=..\plugins\pcre\pcreposix.c
# End Source File
# Begin Source File

SOURCE=.\platmath.cpp
# End Source File
# Begin Source File

SOURCE=..\plugins\pcre\plugin.cpp
# End Source File
# Begin Source File

SOURCE=.\standard.cpp
# End Source File
# Begin Source File

SOURCE=.\stdcommandline.cpp
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

SOURCE=..\plugins\pcre\study.c
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

SOURCE=.\vm.cpp
# End Source File
# Begin Source File

SOURCE=..\plugins\filescanner\win32_filescanner_plugin.cpp
# End Source File
# Begin Source File

SOURCE=.\plat\win32\win32commandline.cpp
# End Source File
# Begin Source File

SOURCE=.\win32dll.cpp
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

SOURCE=.\yacasmain.cpp
# End Source File
# Begin Source File

SOURCE=.\yacasnumbers.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\archiver.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# Begin Source File

SOURCE=.\chang.txt
# End Source File
# End Target
# End Project
