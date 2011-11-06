
#ifndef __win32commandline_h__
#define __win32commandline_h__

#include <conio.h>
#include <stdio.h>
#include <stdlib.h>
#include <windows.h>

#include <assert.h>
#include "..\..\commandline.h"

class CWin32CommandLine : public CCommandLine
{
public:
    CWin32CommandLine();
    ~CWin32CommandLine();
public:
    virtual LispInt GetKey();
    void ReadLineSub(LispChar * prompt);
    virtual void NewLine();
    virtual void ShowLine(LispChar * prompt,LispInt promptlen,LispInt cursor);
    virtual void Pause();

    // new functionality
    void color_print(const LispChar * str, WORD text_attrib);
    void color_read(LispChar * str, WORD text_attrib);
protected:
  void ShowLine();
  LispChar * iLastPrompt;
private:
    HANDLE out_console;
    HANDLE in_console;
    bool _is_NT_or_later;
};

#endif
