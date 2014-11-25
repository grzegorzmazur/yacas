#ifndef YACAS_WIN32COMMANDLINE_H
#define YACAS_WIN32COMMANDLINE_H

#include <windows.h>

#include "commandline.h"

class CWin32CommandLine : public CCommandLine
{
public:
    CWin32CommandLine();
    ~CWin32CommandLine();

    LispInt GetKey();
    void NewLine();
    void ShowLine(const std::string& prompt, unsigned cursor);
    void Pause();
    void MaxHistoryLinesSaved(std::size_t);

private:
    void color_print(const std::string& str, WORD text_attrib);

    HANDLE out_console;

    SHORT _cursor_line, _last_line;

    std::size_t _max_lines;
};

#endif
