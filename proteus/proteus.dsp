# Microsoft Developer Studio Project File - Name="arc" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

CFG=arc - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "proteus.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "proteus.mak" CFG="arc - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "arc - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "arc - Win32 Debug" (based on "Win32 (x86) Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "arc - Win32 Release"

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
# ADD CPP /nologo /MT /GX /Os /Ob2 /I "." /I ".." /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "WIN32_LEAN_AND_MEAN" /D "VC_EXTRA_LEAN" /D "WIN32_EXTRA_LEAN" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 fltk.lib wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:windows /machine:I386 /nodefaultlib:"libcd" /out:"../test/arc.exe" /libpath:"..\lib"
# SUBTRACT LINK32 /pdb:none /incremental:yes

!ELSEIF  "$(CFG)" == "arc - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "arc_"
# PROP BASE Intermediate_Dir "arc_"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "arc_"
# PROP Intermediate_Dir "arc_"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /MTd /Gm /GX /ZI /Od /I "." /I "..\fltk-1.0.10" /I "..\yacas\src" /I "..\yacas\src\plat\win32" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "WIN32_LEAN_AND_MEAN" /D "VC_EXTRA_LEAN" /D "WIN32_EXTRA_LEAN" /D "WORKSHEET" /YX /FD /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /debug /machine:I386 /pdbtype:sept
# ADD LINK32 fltkd.lib wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:windows /pdb:"proteusworksheet.pdb" /debug /machine:I386 /nodefaultlib:"libcd" /out:"E:\cygwin\home\aya\yacas\scripts\proteusworksheet.exe" /pdbtype:sept /libpath:"..\fltk-1.0.10\lib"
# SUBTRACT LINK32 /pdb:none /incremental:no

!ENDIF 

# Begin Target

# Name "arc - Win32 Release"
# Name "arc - Win32 Debug"
# Begin Group "yacas engine"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\yacas\src\anumber.cpp
# End Source File
# Begin Source File

SOURCE=..\yacas\src\arggetter.cpp
# End Source File
# Begin Source File

SOURCE=..\yacas\src\arrayclass.cpp
# End Source File
# Begin Source File

SOURCE=..\yacas\src\commandline.cpp
# End Source File
# Begin Source File

SOURCE=..\yacas\src\deffile.cpp
# End Source File
# Begin Source File

SOURCE=..\yacas\src\errors.cpp
# End Source File
# Begin Source File

SOURCE=..\yacas\src\genericobject.cpp
# End Source File
# Begin Source File

SOURCE=..\yacas\src\genericstructs.cpp
# End Source File
# Begin Source File

SOURCE=..\yacas\src\grower.cpp
# End Source File
# Begin Source File

SOURCE=..\yacas\src\infixparser.cpp
# End Source File
# Begin Source File

SOURCE=..\yacas\src\lispatom.cpp
# End Source File
# Begin Source File

SOURCE=..\yacas\src\lispcleanupstack.cpp
# End Source File
# Begin Source File

SOURCE=..\yacas\src\lispenvironment.cpp
# End Source File
# Begin Source File

SOURCE=..\yacas\src\lispeval.cpp
# End Source File
# Begin Source File

SOURCE=..\yacas\src\lispevalhash.cpp
# End Source File
# Begin Source File

SOURCE=..\yacas\src\lisphash.cpp
# End Source File
# Begin Source File

SOURCE=..\yacas\src\lispio.cpp
# End Source File
# Begin Source File

SOURCE=..\yacas\src\lispobject.cpp
# End Source File
# Begin Source File

SOURCE=..\yacas\src\lispparser.cpp
# End Source File
# Begin Source File

SOURCE=..\yacas\src\lispplugin.cpp
# End Source File
# Begin Source File

SOURCE=..\yacas\src\lispstring.cpp
# End Source File
# Begin Source File

SOURCE=..\yacas\src\lispuserfunc.cpp
# End Source File
# Begin Source File

SOURCE=..\yacas\src\mathcommands.cpp
# End Source File
# Begin Source File

SOURCE=..\yacas\src\mathcommands2.cpp
# End Source File
# Begin Source File

SOURCE=..\yacas\src\mathcommands3.cpp
# End Source File
# Begin Source File

SOURCE=..\yacas\src\mathenvironment.cpp
# End Source File
# Begin Source File

SOURCE=..\yacas\src\mathuserfunc.cpp
# End Source File
# Begin Source File

SOURCE=..\yacas\src\mathutil.cpp
# End Source File
# Begin Source File

SOURCE=..\yacas\src\patcher.cpp
# End Source File
# Begin Source File

SOURCE=..\yacas\src\patternclass.cpp
# End Source File
# Begin Source File

SOURCE=..\yacas\src\patterns.cpp
# End Source File
# Begin Source File

SOURCE=..\yacas\src\platmath.cpp
# End Source File
# Begin Source File

SOURCE=..\yacas\src\standard.cpp
# End Source File
# Begin Source File

SOURCE=..\yacas\src\stdcommandline.cpp
# End Source File
# Begin Source File

SOURCE=..\yacas\src\stdfileio.cpp
# End Source File
# Begin Source File

SOURCE=..\yacas\src\stdstubs.cpp
# End Source File
# Begin Source File

SOURCE=..\yacas\src\stringio.cpp
# End Source File
# Begin Source File

SOURCE=..\yacas\src\substitute.cpp
# End Source File
# Begin Source File

SOURCE=..\yacas\src\tokenizer.cpp
# End Source File
# Begin Source File

SOURCE=..\yacas\src\win32dll.cpp
# End Source File
# Begin Source File

SOURCE=..\yacas\src\yacasapi.cpp
# End Source File
# Begin Source File

SOURCE=..\yacas\src\yacasnumbers.cpp
# End Source File
# End Group
# Begin Group "proteus"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\editor.cpp
# End Source File
# Begin Source File

SOURCE=.\fl_adjustable_file_chooser.cpp
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
# Begin Source File

SOURCE=.\todo.txt
# End Source File
# Begin Source File

SOURCE=.\wintodo.txt
# End Source File
# End Target
# End Project
