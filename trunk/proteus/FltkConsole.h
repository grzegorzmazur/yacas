// FltkConsole.h

#ifndef __FltkConsole_h__
#define __FltkConsole_h__


#include <FL/Fl_Widget.H>
#include "grower.h"
#include "lispstring.h"


/// \class ConsoleOutBase can draw one line in the output window
class ConsoleOutBase
{
public:
    virtual ~ConsoleOutBase();
    ///. The main method: draw yourself
    virtual void draw(int x, int y, int width,int draw_input=1) = 0;
    /// Return the height of the line
    virtual int height(int draw_input=1) = 0;
    /// input text associated with this block
    virtual LispCharPtr input();
    /// Whether the user can edit the input line of this object
    virtual int IsEditable();
    /// Whether the user can see the input line of this object
    virtual int InputIsVisible();
    /// Saving to file
    virtual void Save(FILE* f) = 0;
};


class ConsoleFlatText : public ConsoleOutBase
{
public:
    ConsoleFlatText(LispCharPtr aText, int aColor, const char* aPrompt,
                   int aFont = FL_HELVETICA,int aFontSize = 12);
    virtual void draw(int x, int y, int width,int draw_input=1);
    virtual int height(int draw_input=1);
    inline LispString& Text() {return iText;};
    virtual LispCharPtr input();
    virtual void Save(FILE* f);
private:
    LispString iText;
    int iColor;
    const char* iPrompt;
    int iFont,iFontSize;
};

class ConsoleGrouped : public ConsoleOutBase
{
public:
    ConsoleGrouped(int aShowInput, int aEnableInput)
        : iShowInput(aShowInput), iEnableInput(aEnableInput) {}
    void Add(ConsoleOutBase* aLine);
    void DeleteAll();
    virtual void draw(int x, int y, int width,int draw_input=1);
    virtual int height(int draw_input=1);
    virtual LispCharPtr input();
    virtual int IsEditable();
    virtual int InputIsVisible();
    virtual void Save(FILE* f);
private:
    CDeletingArrayGrower<ConsoleOutBase*> iConsoleOut;
    int iShowInput;
    int iEnableInput;
};


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

class FltkHintWindow;
class FltkConsole : public Fl_Widget
{
public:
    FltkConsole(int x, int y, int w, int h, int aDefaultFontSize);
    ~FltkConsole();
    void SaveHistory();
    void Restart();
public:
    void draw();
    virtual int handle(int event);
//    virtual void resize(int,int,int,int);
public:
    void AddGroup();
    void AddGroup(int aShowInput, int aEnableInput);
    void AddText(LispCharPtr aText,int color, const char* aPrompt,
                              int aFont = FL_COURIER, int aFontSize = 12);
    void InsertText(const LispCharPtr aText);
    void handle_key(int key);
    void SetInput(LispCharPtr aText, LispInt nr);
    void LoadNotePad(LispCharPtr aFile);
    void SaveNotePad(LispCharPtr aFile);
    inline void ShowInput(int aShowInput)     {iShowInput   = aShowInput;};
    inline void EnableInput(int aEnableInput) {iEnableInput = aEnableInput;};
    void DeleteAll();
private:
    void TryToHint(int ifrom,int ito);
    void CheckForNewHints();
    void DeleteHints();
    void CreateHints();
    void AddHintLine(LispCharPtr aText);
    void AddOutput(ConsoleOutBase* aOutput);
    void CommandLineStartNew();
    void CommandLineEnd();
    void GetHistory(LispInt aLine);
    void SetCurrentHighlighted(int i);
    inline void SetInputDirty()  {iInputDirty = 1;}
    inline void SetOutputDirty() {iOutputDirty = 1;}
    void MakeSureHighlightedVisible();
    void DrawInterEdit();
    void DrawUnderEdit();
    void DrawInputLine(int lowy);
    void DoLine(char* inpline);
    void UpdateHeight(int aDelta);
private:
    CDeletingArrayGrower<ConsoleOutBase*> iConsoleOut;
    ConsoleGrouped *iLast;
    
protected:
    LispInt cursor;
    LispInt iFullLineDirty;
//TODO remove?    LispInt iHistoryUnchanged;
//TODO remove?    LispInt history;
    
public:
    LispString iLine;
    LispString iSubLine;
//TODO remove?    LispInt iTraceHistory;
//TODO remove?    LispInt iMaxLines;

/*TODO remove?     CDeletingArrayGrower<LispStringPtr> iHistory; */

    int iDefaultFontSize;
    FltkHintWindow* hints;
//    int iOutputOffsetX;
//    int iOutputOffsetY;
    int iMouseDownX;
    int iMouseDownY;
    int iMoveBaseX;
    int iMoveBaseY;
    int iMovingOutput;
    int iOutputHeight;

protected:
    LispInt iCurrentHighlighted;

private:
    int iInputDirty;
    int iOutputDirty;
    int iShowInput;
    int iEnableInput;
};


#endif


