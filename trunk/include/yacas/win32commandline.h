#ifndef YACAS_WIN32COMMANDLINE_H
#define YACAS_WIN32COMMANDLINE_H

#include <memory>

#include <windows.h>

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
    static const std::size_t BUF_SIZE = 16384;
    std::unique_ptr<char []> _buf;
};

#endif
