
#ifndef __stdcommandline_h__
#define __stdcommandline_h__

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <signal.h>
#include <unistd.h>
#include <termios.h>
#include <time.h>

#include "commandline.h"
/** Simple no-frills implementation of CCommandLine, using stdlibc-functions
 *  only, and no ansi characters. No history is supported either.
 */
class CStdCommandLine : public CCommandLine
{
public:
    CStdCommandLine();
    ~CStdCommandLine();
    virtual void ReadLine(LispCharPtr prompt);
public:
    virtual LispInt GetKey();
    virtual void NewLine();
    virtual void ShowLine(LispCharPtr prompt,LispInt promptlen,LispInt cursor);
    virtual void Pause();
};


#endif

