

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
  void AddDescription(LispCharPtr aText);
  virtual void draw(int x, int y);
  int height();
public:
  LispCharPtr iText[MAX_HINT_LINES];
  int iNrLines;

  LispCharPtr iDescription[MAX_HINT_LINES];
  int iNrDescriptions;

  int iMaxWidth;
  int iTextSize;
};

#endif
