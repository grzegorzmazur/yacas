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
public:
  virtual LispInt GetKey();
  virtual void NewLine();
  virtual void ShowLine(const std::string& prompt, LispInt cursor);
  virtual void Pause();
  virtual void MaxHistoryLinesSaved(LispInt aNrLines);
private:
  unsigned char term_chars[NCCS];
  struct termios orig_termio, rl_termio;
  LispInt _cursor_line, _last_line;
public:
  LispInt iMaxLines;
};


#endif

