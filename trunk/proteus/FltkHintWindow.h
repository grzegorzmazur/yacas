

#ifndef __FltkHintWindow_h__
#define __FltkHintWindow_h__

#include <FL/Fl_Window.H>
#include "lisptype.h"

#define MAX_HINT_LINES 20
class FltkHintWindow
{
public:
    FltkHintWindow(int aTextSize);
    void AddLine(LispCharPtr aText);
    virtual FL_EXPORT void draw(int x, int y);
private:
    LispCharPtr iText[MAX_HINT_LINES];
    int iNrLines;
    int iMaxWidth;
    int iTextSize;
};

#endif

