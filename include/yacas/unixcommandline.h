#ifndef YACAS_UNIXCOMMANDLINE_H
#define YACAS_UNIXCOMMANDLINE_H

#include "commandline.h"

#include <termios.h>


/** Unix command line class, using assorted termios functionality
 *  and sending ansi character sequences to the console.
 */
class CUnixCommandLine : public CCommandLine
{
public:
    CUnixCommandLine();
    ~CUnixCommandLine();

    char32_t GetKey() override;
    void NewLine() override;
    void ShowLine(const std::string& prompt, unsigned cursor) override;
    void Pause() override;
    void MaxHistoryLinesSaved(std::size_t) override;

private:
    unsigned char term_chars[NCCS];
    struct termios orig_termio, rl_termio;
    int _cursor_line, _last_line;

    std::size_t _max_lines;
};


#endif

