# Microsoft Developer Studio Project File - Name="proteusdebugger" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=proteusdebugger - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "proteusdebugger.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "proteusdebugger.mak" CFG="proteusdebugger - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "proteusdebugger - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "proteusdebugger - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

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
# ADD CPP /nologo /W3 /GX /O2 /I "..\src\\" /I "..\src\plat\win32\\" /I "..\..\fltk-1.0.10\\" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "WIN32_LEAN_AND_MEAN" /D "VC_EXTRA_LEAN" /D "WIN32_EXTRA_LEAN" /D "DEBUG_MODE" /YX /FD /c
# ADD BASE RSC /l 0x413 /d "NDEBUG"
# ADD RSC /l 0x413 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 fltkd.lib wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:console /incremental:yes /debug /machine:I386 /nodefaultlib:"libc" /out:"../scripts/proteusdebugger.exe" /libpath:"..\..\fltk-1.0.10\lib\\"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

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
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /I "..\src\\" /I "..\src\plat\win32\\" /I "..\..\fltk-1.0.10\\" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "WIN32_LEAN_AND_MEAN" /D "VC_EXTRA_LEAN" /D "WIN32_EXTRA_LEAN" /D "DEBUG_MODE" /FR /YX /FD /GZ /c
# ADD BASE RSC /l 0x413 /d "_DEBUG"
# ADD RSC /l 0x413 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 fltkd.lib wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:console /debug /machine:I386 /nodefaultlib:"libcd" /pdbtype:sept /libpath:"..\..\fltk-1.0.10\lib\\"

!ENDIF 

# Begin Target

# Name "proteusdebugger - Win32 Release"
# Name "proteusdebugger - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=..\src\anumber.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\src\arggetter.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\src\arrayclass.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\src\commandline.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\debugclass.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\src\deffile.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\editor.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\src\errors.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\fl_adjustable_file_chooser.cpp
# End Source File
# Begin Source File

SOURCE=..\src\genericobject.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\src\genericstructs.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\src\grower.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\HelpView.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\src\infixparser.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\src\lispatom.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\src\lispcleanupstack.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\src\lispenvironment.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\src\lispeval.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\src\lispevalhash.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\src\lisphash.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\src\lispio.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\src\lispobject.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\src\lispparser.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\src\lispplugin.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\src\lispstring.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\src\lispuserfunc.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\src\mathcommands.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\src\mathcommands2.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\src\mathcommands3.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\src\mathenvironment.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\src\mathuserfunc.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\src\mathutil.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\src\patcher.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\src\patternclass.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\src\patterns.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\src\platmath.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\src\standard.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\src\stdcommandline.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\src\stdfileio.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\src\stdstubs.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\src\stringio.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\src\substitute.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\src\tokenizer.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\tracer.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\src\unipoly.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\src\win32dll.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\src\yacasapi.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\src\yacasnumbers.cpp

!IF  "$(CFG)" == "proteusdebugger - Win32 Release"

!ELSEIF  "$(CFG)" == "proteusdebugger - Win32 Debug"

# ADD CPP /I "..\fltk-1.0.10\\"

!ENDIF 

# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
