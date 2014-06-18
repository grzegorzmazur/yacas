#ifndef YACAS_WIN32COMMANDLINE_H
#define YACAS_WIN32COMMANDLINE_H

#include <conio.h>
#include <stdio.h>
#include <stdlib.h>
#include <windows.h>

#include <assert.h>

#include "commandline.h"

class CWin32CommandLine : public CCommandLine
{
public:
    CWin32CommandLine();
    ~CWin32CommandLine();
public:
    virtual LispInt GetKey();
    void ReadLineSub(const std::string& prompt);
    virtual void NewLine();
    virtual void ShowLine(const std::string& prompt, unsigned cursor);
    virtual void Pause();

    // new functionality
    void color_print(const std::string& str, WORD text_attrib);
    void color_read(LispChar * str, WORD text_attrib);
protected:
    void ShowLine();
    std::string last_prompt;
private:
    HANDLE out_console;
    HANDLE in_console;
};

#endif
