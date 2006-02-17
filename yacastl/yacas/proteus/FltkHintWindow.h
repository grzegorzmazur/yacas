

#ifndef __FltkHintWindow_h__
#define __FltkHintWindow_h__

#include <FL/Fl_Window.H>
#include "lisptype.h"

#define MAX_HINT_LINES 20
class FltkHintWindow
{
public:
  FltkHintWindow(int aTextSize);
  void AddLine(LispChar * aText);
  void AddDescription(LispChar * aText);
  virtual void draw(int x, int y);
  int height();
public:
  LispChar * iText[MAX_HINT_LINES];
  int iNrLines;

  LispChar * iDescription[MAX_HINT_LINES];
  int iNrDescriptions;

  int iMaxWidth;
  int iTextSize;
};

#endif

