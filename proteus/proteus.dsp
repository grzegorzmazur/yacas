# Microsoft Developer Studio Project File - Name="proteus" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

CFG=proteus - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "proteus.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "proteus.mak" CFG="proteus - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "proteus - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "proteus - Win32 Debug" (based on "Win32 (x86) Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "proteus - Win32 Release"

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
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /MD /GX /Os /Ob2 /I "." /I "c:\Projects\fltk-1.1.3\\" /I "..\src" /I "..\src\plat\win32" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "WIN32_LEAN_AND_MEAN" /D "VC_EXTRA_LEAN" /D "WIN32_EXTRA_LEAN" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 fltk.lib wsock32.lib comctl32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:windows /machine:I386 /nodefaultlib:"libcd" /out:"../test/proteus.exe" /libpath:"c:\Projects\fltk-1.1.3\lib"
# SUBTRACT LINK32 /pdb:none /incremental:yes

!ELSEIF  "$(CFG)" == "proteus - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "proteus_"
# PROP BASE Intermediate_Dir "proteus_"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "proteus_"
# PROP Intermediate_Dir "proteus_"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /MDd /Gm /GX /ZI /Od /I "." /I "c:\Projects\fltk-1.1.3\\" /I "..\src" /I "..\src\plat\win32" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "WIN32_LEAN_AND_MEAN" /D "VC_EXTRA_LEAN" /D "WIN32_EXTRA_LEAN" /D "WORKSHEET" /FR /YX /FD /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /debug /machine:I386 /pdbtype:sept
# ADD LINK32 fltkd.lib wsock32.lib comctl32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:windows /debug /machine:I386 /nodefaultlib:"libcd" /out:"..\winrel\proteus.exe" /pdbtype:sept /libpath:"c:\Projects\fltk-1.1.3\lib"
# SUBTRACT LINK32 /pdb:none /incremental:no

!ENDIF 

# Begin Target

# Name "proteus - Win32 Release"
# Name "proteus - Win32 Debug"
# Begin Group "proteus"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\editor.cpp
# End Source File
# Begin Source File

SOURCE=.\FltkConsole.cpp
# End Source File
# Begin Source File

SOURCE=.\FltkHintWindow.cpp
# End Source File
# Begin Source File

SOURCE=.\grapher.cpp
# End Source File
# Begin Source File

SOURCE=.\HelpView.cpp
# End Source File
# Begin Source File

SOURCE=.\tabs.cpp
# End Source File
# End Group
# Begin Group "yacas engine"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\src\anumber.cpp
# End Source File
# Begin Source File

SOURCE=..\src\arggetter.cpp
# End Source File
# Begin Source File

SOURCE=..\src\arrayclass.cpp
# End Source File
# Begin Source File

SOURCE=..\plugins\pcre\chartables.c
# End Source File
# Begin Source File

SOURCE=..\src\commandline.cpp
# End Source File
# Begin Source File

SOURCE=..\src\debugmem.cpp
# End Source File
# Begin Source File

SOURCE=..\src\deffile.cpp
# End Source File
# Begin Source File

SOURCE=..\src\errors.cpp
# End Source File
# Begin Source File

SOURCE=..\src\exedll.cpp
# End Source File
# Begin Source File

SOURCE=..\src\filescanner.cpp
# End Source File
# Begin Source File

SOURCE=..\src\genericobject.cpp
# End Source File
# Begin Source File

SOURCE=..\src\genericstructs.cpp
# End Source File
# Begin Source File

SOURCE=..\src\grower.cpp
# End Source File
# Begin Source File

SOURCE=..\src\infixparser.cpp
# End Source File
# Begin Source File

SOURCE=..\src\lispatom.cpp
# End Source File
# Begin Source File

SOURCE=..\src\lispcleanupstack.cpp
# End Source File
# Begin Source File

SOURCE=..\src\lispenvironment.cpp
# End Source File
# Begin Source File

SOURCE=..\src\lispeval.cpp
# End Source File
# Begin Source File

SOURCE=..\src\lispevalhash.cpp
# End Source File
# Begin Source File

SOURCE=..\src\lisphash.cpp
# End Source File
# Begin Source File

SOURCE=..\src\lispio.cpp
# End Source File
# Begin Source File

SOURCE=..\src\lispobject.cpp
# End Source File
# Begin Source File

SOURCE=..\src\lispparser.cpp
# End Source File
# Begin Source File

SOURCE=..\src\lispplugin.cpp
# End Source File
# Begin Source File

SOURCE=..\src\lispstring.cpp
# End Source File
# Begin Source File

SOURCE=..\src\lispuserfunc.cpp
# End Source File
# Begin Source File

SOURCE=..\src\mathcommands.cpp
# End Source File
# Begin Source File

SOURCE=..\src\mathcommands2.cpp
# End Source File
# Begin Source File

SOURCE=..\src\mathcommands3.cpp
# End Source File
# Begin Source File

SOURCE=..\src\mathenvironment.cpp
# End Source File
# Begin Source File

SOURCE=..\src\mathuserfunc.cpp
# End Source File
# Begin Source File

SOURCE=..\src\mathutil.cpp
# End Source File
# Begin Source File

SOURCE=..\src\obmalloc.cpp
# End Source File
# Begin Source File

SOURCE=..\src\patcher.cpp
# End Source File
# Begin Source File

SOURCE=..\src\patternclass.cpp
# End Source File
# Begin Source File

SOURCE=..\src\patterns.cpp
# End Source File
# Begin Source File

SOURCE=..\plugins\pcre\pcre.c
# End Source File
# Begin Source File

SOURCE=..\plugins\pcre\pcreposix.c
# End Source File
# Begin Source File

SOURCE=..\src\platmath.cpp
# End Source File
# Begin Source File

SOURCE=..\plugins\pcre\plugin.cpp
# End Source File
# Begin Source File

SOURCE=..\src\standard.cpp
# End Source File
# Begin Source File

SOURCE=..\src\stdcommandline.cpp
# End Source File
# Begin Source File

SOURCE=..\src\stdfileio.cpp
# End Source File
# Begin Source File

SOURCE=..\src\stdstubs.cpp
# End Source File
# Begin Source File

SOURCE=..\src\stringio.cpp
# End Source File
# Begin Source File

SOURCE=..\plugins\pcre\study.c
# End Source File
# Begin Source File

SOURCE=..\src\substitute.cpp
# End Source File
# Begin Source File

SOURCE=..\src\tokenizer.cpp
# End Source File
# Begin Source File

SOURCE=..\src\unipoly.cpp
# End Source File
# Begin Source File

SOURCE=..\plugins\filescanner\win32_filescanner_plugin.cpp
# End Source File
# Begin Source File

SOURCE=..\src\win32dll.cpp
# End Source File
# Begin Source File

SOURCE=..\src\yacasapi.cpp
# End Source File
# Begin Source File

SOURCE=..\src\yacasnumbers.cpp
# End Source File
# End Group
# Begin Source File

SOURCE=..\src\compressedfiles.cpp
# End Source File
# Begin Source File

SOURCE=..\src\ctokenizer.cpp
# End Source File
# Begin Source File

SOURCE=..\src\libmath.cpp
# End Source File
# Begin Source File

SOURCE=..\src\minilzo.c
# End Source File
# Begin Source File

SOURCE=..\src\numbers.cpp
# End Source File
# Begin Source File

SOURCE=.\todo.txt
# End Source File
# Begin Source File

SOURCE=.\wintodo.txt
# End Source File
# Begin Source File

SOURCE=..\src\xmltokenizer.cpp
# End Source File
# End Target
# End Project
