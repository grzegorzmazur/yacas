
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

#ifndef __commandline_h__
#define __commandline_h__

#include "lispstring.h"
#include "lisptype.h"

enum ESpecialChars
{
    eDelete     = 0x1000,
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
class CCommandLine
{
public:
    CCommandLine() : history(0),iHistoryUnchanged(0), iTraceHistory(0){};
    virtual ~CCommandLine();
    /// Call this function if the user needs to enter an expression.
    virtual void ReadLine(LispCharPtr prompt);
public: //platform stuff
    /** return a key press, which is either an ascii value, or one
     * of the values specified in ESpecialChars
     */
    virtual LispInt GetKey() = 0;
    /// Go to the next line on the console (carriage return/line feed).
    virtual void NewLine()   = 0;
    /** Show the current line (in iSubLine), with the required prompt,
     *  and the cursor position at cursor (starting from the prompt).
     */
    virtual void ShowLine(LispCharPtr prompt,
                          LispInt promptlen,LispInt cursor) = 0;
    /// Pause for a short while. Used when matching brackets.
    virtual void Pause() = 0;

    /// Maximum number of history lines to be saved (-1 is all)
    virtual void MaxHistoryLinesSaved(LispInt aNrLines);

protected:
    virtual void ReadLineSub(LispCharPtr prompt);
private:
    void GetHistory(LispInt aLine);
    void ShowOpen(LispCharPtr prompt,LispInt promptlen,
                  LispChar aOpen, LispChar aClose, LispInt aCurPos);
protected:
    LispInt iFullLineDirty;
    LispInt iHistoryUnchanged;
    LispInt history;
    
public:
    LispString iLine;
    LispString iSubLine;
    LispInt iTraceHistory;
    
    CDeletingArrayGrower<LispStringPtr> iHistory;
};

#endif

