
#ifndef __win32commandline_h__
#define __win32commandline_h__

#include <conio.h>
#include <stdio.h>
#include <stdlib.h> 
#include <windows.h> 

#include "commandline.h"

class CWin32CommandLine : public CCommandLine
{
public:
    CWin32CommandLine();
    ~CWin32CommandLine();
public:
    virtual LispInt GetKey();
    virtual void NewLine();
    virtual void ShowLine(LispCharPtr prompt,LispInt promptlen,LispInt cursor);
    virtual void Pause();

    // new functionality
    void color_print(LispCharPtr str, WORD text_attrib);
private:
    HANDLE out_console;
};

#endif