
#include <FL/Fl.H>
#include <FL/fl_draw.h>
#include "FltkHintWindow.h"

FltkHintWindow::FltkHintWindow(int aTextSize)
    : iNrLines(0), iMaxWidth(0), iTextSize(aTextSize)
{
}
void FltkHintWindow::AddLine(LispCharPtr aText)
{
    iText[iNrLines] = aText;

    fl_font(FL_HELVETICA,iTextSize);
    int width = (int)fl_width(iText[iNrLines]);
    if (width>iMaxWidth)
        iMaxWidth = width;
    iNrLines++;
}

void FltkHintWindow::draw(int x, int y)
{
    int ix = x;
    int iy = y - iNrLines*fl_height();
    int w = 5+iMaxWidth;
    int h = iNrLines*fl_height()+2;
    
    fl_color(FL_YELLOW);
    fl_rectf(ix,iy,w,h);
    fl_color(FL_BLUE);
    fl_rect(ix,iy,w,h);

    fl_font(FL_HELVETICA,iTextSize);
    int i;
    for (i=0;i<iNrLines;i++)
    {
        fl_draw(iText[i],ix+2,iy+(i+1)*fl_height()-fl_descent());
    }
}

