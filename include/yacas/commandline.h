
/** \file commandline.h
 *  Implementation of a platform-independent command line with history.
 *
 *  The class CCommandLine has two roles:
 *  1) It has an outside interface, ReadLine(), that yields to the
 *     system until the user is finished entering an expression at
 *     the command line. The result can be found in iLine.
 *  2) It defines a mini-API needed to implement the command line.
 *     For each platform Yacas is meant to run on there should be a
 *     CCommandLine-derived class if this functionality is to be used.
 *
 */

#ifndef YACAS_COMMANDLINE_H
#define YACAS_COMMANDLINE_H

#include <string>
#include <vector>

#include "lispstring.h"
#include "yacasbase.h"
#include "utf8.h"

enum ESpecialChars
{
    eDelete     = utf8::internal::CODE_POINT_MAX + 1,
    eBackSpace,
    eLeft,
    eRight,
    eUp,
    eDown,
    eHome,
    eEnd,
    eEnter,
    eTab,
    eEscape,
    eKill
};

/**
 *  Implementation of a platform-independent command line with history.
 *
 *  The class CCommandLine has two roles:
 *  1) It has an outside interface, ReadLine(), that yields to the
 *     system until the user is finished entering an expression at
 *     the command line. The result can be found in iLine.
 *  2) It defines a mini-API needed to implement the command line.
 *     For each platform Yacas is meant to run on there should be a
 *     CCommandLine-derived class if this functionality is to be used.
 *
 *  The derived class is responsible for filling the history list,
 *  and for externalizing the history list to disk when the system
 *  shuts down.
 */

/// \class CConsoleHistory, implement history list the user can browse through.
class CConsoleHistory
{
public:
  CConsoleHistory();

  void ResetHistoryPosition();
  void AddLine(const std::string& s);
  void Append(const std::string& s);
  bool ArrowUp(std::string& s, unsigned c);
  bool ArrowDown(std::string& s, unsigned c);
  bool Complete(std::string& s, unsigned& c);
  std::size_t NrLines();
  const std::string& GetLine(std::size_t);

protected:
  std::vector<std::string> iHistory;
  std::size_t history;
};

class CCommandLine : public YacasBase
{
public:
  CCommandLine():
      full_line_dirty(false),
      history_unchanged(false)
  {
  }

  virtual ~CCommandLine() = default;

  /// Call this function if the user needs to enter an expression.
  virtual void ReadLine(const std::string& prompt);
public: //platform stuff
  /** return a key press, which is either an ascii value, or one
   * of the values specified in ESpecialChars
   */
  virtual char32_t GetKey() = 0;
  /// Go to the next line on the console (carriage return/line feed).
  virtual void NewLine()   = 0;
  /** Show the current line (in iSubLine), with the required prompt,
   *  and the cursor position at cursor (starting from the prompt).
   */
  virtual void ShowLine(const std::string& prompt, unsigned cursor) = 0;
  /// Pause for a short while. Used when matching brackets.
  virtual void Pause() = 0;

  /// Maximum number of history lines to be saved
  virtual void MaxHistoryLinesSaved(std::size_t aNrLines);

protected:
  virtual void ReadLineSub(const std::string& prompt);
private:
  void GetHistory(std::size_t aLine);
  void ShowOpen(const std::string& prompt,
                 LispChar aOpen, LispChar aClose,
                 unsigned aCurPos);
protected:
  bool full_line_dirty;
  bool history_unchanged;

public:
  std::string iLine;
  std::string iSubLine;

  CConsoleHistory iHistoryList;
};

#endif
