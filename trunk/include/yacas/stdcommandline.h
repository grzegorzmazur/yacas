#ifndef YACAS_STDCOMMANDLINE_H
#define YACAS_STDCOMMANDLINE_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <signal.h>
#ifndef WIN32
#   include <unistd.h>
#   include <termios.h>
#endif
#include <time.h>

#include "yacasbase.h"
#include "commandline.h"
/** Simple no-frills implementation of CCommandLine, using stdlibc-functions
 *  only, and no ansi characters. No history is supported either.
 */
class CStdCommandLine : public CCommandLine
{
public:
    CStdCommandLine();
    ~CStdCommandLine();
    virtual void ReadLine(const std::string& prompt);
public:
    virtual LispInt GetKey();
    virtual void NewLine();
    virtual void ShowLine(const std::string& prompt, unsigned cursor);
    virtual void Pause();
};


#endif

