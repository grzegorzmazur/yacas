!include "MUI2.nsh"

!define APPNAME "Yacas"
!define DESCRIPTION "Yet Another Computer Algebra System"
!define VERSIONMAJOR 1
!define VERSIONMINOR 5
!define VERSIONPATCH 99
!define HELPURL "http://www.yacas.org"
!define UPDATEURL "http://www.yacas.org/downloads"
!define ABOUTURL "http://www.yacas.org"
!define INSTALLSIZE 17500

Name ${APPNAME}
Outfile "yacas-${VERSIONMAJOR}.${VERSIONMINOR}.${VERSIONPATCH}-win64.exe"

InstallDir "$PROGRAMFILES64\yacas\"

!include "WordFunc.nsh"
!insertmacro VersionCompare

Var UNINSTALL_OLD_VERSION

Function .onInit
    SetRegView 64

    ClearErrors
    ReadRegStr $0 HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}" "DisplayVersion"
    IfErrors init.uninst
    ${VersionCompare} $0 ${VERSIONMAJOR}.${VERSIONMINOR}.${VERSIONPATCH} $1
    IntCmp $1 2 init.uninst

    MessageBox MB_YESNO|MB_ICONQUESTION "${APPNAME} version $0 seems to be already installed on your system.$\nWould you like to remove it and proceed with the installation of version ${VERSIONMAJOR}.${VERSIONMINOR}.${VERSIONPATCH}?" IDYES init.uninst
    Quit

init.uninst:
    ClearErrors
    ReadRegStr $0 HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}" "UninstallString"
    IfErrors init.done
    StrCpy $UNINSTALL_OLD_VERSION '$0 /S'

init.done:

FunctionEnd

Function un.onInit
    SetRegView 64
FunctionEnd

!define MUI_ICON "yacas.ico"
!define MUI_UNICON "yacas.ico"

!insertmacro MUI_PAGE_WELCOME
!insertmacro MUI_PAGE_LICENSE "COPYING"
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_COMPONENTS
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_PAGE_FINISH

!insertmacro MUI_UNPAGE_WELCOME
!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES
!insertmacro MUI_UNPAGE_FINISH

!insertmacro MUI_LANGUAGE "English"

Section "Yacas"
    SectionIn RO

    StrCmp $UNINSTALL_OLD_VERSION "" yacas.files
    ExecWait '$UNINSTALL_OLD_VERSION'

yacas.files:

    SetOutPath $INSTDIR
    File /r bin

    SetOutPath $INSTDIR\share\yacas
    File yacas.ico
    File /r share\yacas\scripts
    File /r share\yacas\tests

    WriteUninstaller $INSTDIR\Uninstall.exe

	WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}" "DisplayName" "${APPNAME} - ${DESCRIPTION}"
	WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}" "UninstallString" "$\"$INSTDIR\Uninstall.exe$\""
	WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}" "QuietUninstallString" "$\"$INSTDIR\Uninstall.exe$\" /S"
	WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}" "InstallLocation" "$INSTDIR"
	WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}" "DisplayIcon" "$INSTDIR\share\yacas\yacas.ico"
	WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}" "Publisher" "Yacas Team"
	WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}" "HelpLink" "${HELPURL}"
	#WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}" "URLUpdateInfo" "${UPDATEURL}"
	#WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}" "URLInfoAbout" "${ABOUTURL}"
	WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}" "DisplayVersion" "${VERSIONMAJOR}.${VERSIONMINOR}.${VERSIONPATCH}"
	WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}" "VersionMajor" ${VERSIONMAJOR}
	WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}" "VersionMinor" ${VERSIONMINOR}
	WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}" "NoModify" 1
	WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}" "NoRepair" 1
	WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}" "EstimatedSize" ${INSTALLSIZE}
SectionEnd

Section "Documentation"
    SetOutPath $INSTDIR\share\yacas
    File /r share\yacas\documentation
SectionEnd

Section "Development"
    SetOutPath $INSTDIR
    File /r lib
    File /r include
SectionEnd

Section "Uninstall"
    Delete $INSTDIR\Uninstall.exe
    RMDir /r /REBOOTOK $INSTDIR
    DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}"
SectionEnd
