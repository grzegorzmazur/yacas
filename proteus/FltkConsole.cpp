

/*

//    redraw(); //output changed
//    Fl::flush();

TODO:
x loading a notepad seems to require the first line to be empty?
x save note books
x convert the previous example note pads to the new format.

- links should always keep their text, and not change
- input cells should maintain their font sizes, also when editing
- you should be able to set currentFont size and type and color 
  for the command line also.
- document the graphing capabilities
- prompt changing
- command processing changing: $
- graphing cell:
  - flplot for new grapher, make an flplot2
    - flplot2 should not be using any globals!
  - consoleoutbase-derived class that does the same as Drawer, but in a
cell.
  - FltkGraphStart that adds a graphing cell, like the Proteus'AddCommand
does.
- flplot not installed?
- flplot not loaded by default?

- Notepad(FindFile("")) crashes, empty file name should be trapped
  before fopen is called.
- embedded grapher
- embedded links, text
- pp formulae

- if there is a selection,show the input line above the output, and as
  soon as the input line is modified, don't show the output. only show
  the output again when enter is pressed, or escape, etc.
- MakeSure.... should adjust to the top, not the bottom!


  Worksheet:

  
- multi-line output items that pixel-wrap.
- scrolling the input line.
  - also scrolling arrows on the input line, and handling left, right arrows to
- caching of hints: remember the previously found hint, and
  try to figure out if a new search is warranted.
- hints in the text editor.

- merge the different tabs.cpp files, and parametrize them?
 - word wrapping based on text size in pixels also.
 - when working on the command line, only redraw the command line!
 - bug where you cannot jump to the "startup code" manual section?
 - a few more problems with html help: the examples don't go to
   the next line.
 - quick function reference and, find help on, and clickable examples
   in the help browser.
 - pass user interactions to the console out objects. These objects
   can then be anything.
 - make a helpdialog console out object.
 - menu, with settings etc.
 - mouse selections
 - have restart, ? and ?? etc

 */

#include <stdio.h>
#include <FL/Fl.H>
#include <FL/fl_draw.H>
#include <FL/Fl_Tabs.H>
#include <FL/Fl_Scroll.H>
#include "yacasprivate.h"
#include "FltkConsole.h"
#include "FltkHintWindow.h"
#include "HelpView.h"
#include "yacas.h"

#ifdef WIN32
#  define snprintf _snprintf
#endif

#define SUPPORT_NOTEPAD


const char* inPrompt =  "In>  ";
const char* outPrompt = "Out> ";
const char* printPrompt =  "  ";
const char* errorPrompt = "Error> ";
const char* linkPrompt = "Link: ";
#define BufSz 512

#define SetInputFont() fl_font(FL_HELVETICA,iDefaultFontSize)
#define INPUT_PROMPT "$ "


int LoadHints(char* file);
void DisposeHints();


FltkConsole::~FltkConsole()
{
    DeleteHints();

    // Remove ALL the loaded hints from memory
    DisposeHints();
}

FltkConsole::FltkConsole(int x, int y, int w, int h, int aDefaultFontSize)
: Fl_Widget(x,y,w,h,NULL), iLast(NULL),iDefaultFontSize(aDefaultFontSize),
hints(NULL),
/*iOutputOffsetX(0), iOutputOffsetY(0), */
iMouseDownX(0),iMouseDownY(0),iMovingOutput(0),
iOutputHeight(0),iCurrentHighlighted(-1),
iInputDirty(1),iOutputDirty(1),iShowInput(1),iEnableInput(1)
{
    {
        extern char defdir[128];
        char buf[128];
        sprintf(buf,"%shints",defdir);
        {
          FILE*f=fopen(buf,"r");
          if (!f) 
          {
            strcpy(buf,"hints");
          }
          else fclose(f);
        }
        LoadHints(buf);
    }
    CommandLineStartNew();
    UpdateHeight(0);
}

void FltkConsole::DeleteAll()
{
    int nr = iConsoleOut.NrItems();
    while (nr--)
    {
        delete iConsoleOut[0];
        iConsoleOut.Delete(0);
    }
    iOutputHeight = 0;
    UpdateHeight(0);
    iLast = NULL;
    iCurrentHighlighted = -1;
}


void FltkConsole::SaveNotePad(LispCharPtr aFile)
{
  FILE*f=fopen(aFile,"w");
  if(f)
  {
    fprintf(f,"Proteus'AddLink(\"True\",\"Saved note pad: %s\",False,False);\n",aFile);
    int i;
    for (i=0;i<iConsoleOut.NrItems();i++)
    {
      iConsoleOut[i]->Save(f);
    }
    
    fclose(f);
  }
}

int currentNotepadFontSize=15;
int currentNotepadFontColor=FL_BLACK;
int currentNotepadFontType=FL_HELVETICA;
void FltkConsole::LoadNotePad(LispCharPtr aFile)
{
  currentNotepadFontSize = 15;
  currentNotepadFontColor=FL_BLACK;
  currentNotepadFontType=FL_HELVETICA;

  DeleteAll();
  extern CYacas* yacas;
  char buf[256];//TODO buffer overflow problem
  snprintf(buf,256,"Load(\"%s\");",aFile);
  yacas->Evaluate(buf);

  extern LispString the_out;
  the_out.SetNrItems(0);
  the_out.Append('\0');

  iOutputHeight+=15; // for the cursor, TODO what was the define again for the cursor height?
  UpdateHeight(0);

  SetCurrentHighlighted(0); // to update the initial line value
  //        iOutputOffsetY = 0;
  MakeSureHighlightedVisible();//
  redraw(); //output changed
  Fl::flush();
  return;

    int prevshow = iShowInput;
    int prevenable = iEnableInput;


    FILE*f=fopen(aFile,"r");
    if(f)
    {
        DeleteAll();
        char buff[BufSz];
        while(fgets(buff,BufSz-2,f))
        {
            char* start;
            int i;
            for(i=0;buff[i] && buff[i] != '\n';++i)
                ;
            buff[i++] = '\0';

            int showinput = iShowInput;
            int enableinput = iEnableInput;
            int holdoutput = 0;
            int internal = 0;
            int link = 0;
            extern LispString the_out;

            start = &buff[0];
            if (start[0] == ':')
            {
                start++;
                while (*start != ':')
                {
                    switch(*start)
                    {
                    case 'n':  internal         = 1; break;
                    case 'i':  showinput        = 0; break;
                    case 'e':  enableinput      = 0; break;
                    case 'h':  holdoutput       = 1; break;
                    case 'l':  link             = 1; break;
                    }
                    start++;
                }
                start++;
            }
            if (!internal)
            {
                AddGroup(showinput, enableinput);
                AddText(start, FL_BLACK,inPrompt,FL_HELVETICA,iDefaultFontSize);
            }
            if (link)
            {
            NEXTLINE:
                fgets(buff,BufSz-2,f);
                if (enableinput)
                {
                    AddText(buff, FL_BLUE,linkPrompt,FL_HELVETICA_BOLD,iDefaultFontSize);
                }
                else
                {
                    if (buff[0] != ':')
                    {
                        AddText(buff, FL_BLACK,"",FL_HELVETICA_BOLD,iDefaultFontSize);
                        goto NEXTLINE;
                    }
                }
            }
            else if (!holdoutput)
            {
                extern CYacas* yacas;
                yacas->Evaluate(start);

                if (!internal)
                {
                    if (the_out[0])
                    {
                        AddText(the_out.String(), FL_RED,printPrompt,FL_COURIER,iDefaultFontSize);
                    }

                    if (yacas->Error()[0] != '\0')
                    {
                        AddText(yacas->Error(), FL_RED,errorPrompt,FL_HELVETICA,iDefaultFontSize);
                    }
                    else
                    {
                        AddText(yacas->Result(), FL_BLUE,outPrompt,FL_HELVETICA,iDefaultFontSize);
                    }
                }
            }
            the_out.SetNrItems(0);
            the_out.Append('\0');
        }
        fclose(f);

        iOutputHeight+=15; // for the cursor, TODO what was the define again for the cursor height?
        UpdateHeight(0);

        SetInputDirty();
        SetOutputDirty();
        SetCurrentHighlighted(0); // to update the initial line value
//        iOutputOffsetY = 0;
        MakeSureHighlightedVisible();
        redraw();
    }
    iShowInput = prevshow;
    iEnableInput = prevenable;
    iLast = NULL;
}


void FltkConsole::CommandLineStartNew()
{
    DeleteHints();
    cursor=0;
}

void FltkConsole::SaveHistory()
{
}

void FltkConsole::AddGroup()
{
    AddGroup(iShowInput, iEnableInput);
}
void FltkConsole::AddGroup(int aShowInput, int aEnableInput)
{
    iLast = new ConsoleGrouped(aShowInput, aEnableInput);
    AddOutput(iLast);
}

void FltkConsole::AddText(LispCharPtr aText,int color, const char* aPrompt,
                          int aFont, int aFontSize)
{
    if (iLast == NULL)
        return;
    iOutputHeight-=iLast->height();
    ConsoleOutBase* toadd;

    char buffer[BufSz+1];
    while (aText[0] != '\0')
    {
        int len=0;

        while (aText[0] == '\n' || aText[0] == '\r') aText++;
        if (aText[0] != '\0')
        {
            while (len < BufSz && aText[len] != '\n' && aText[len] != '\r' && aText[len] != '\0')
            {
                len++;
            }
        }
        if (aText[len] == '\0')
        {
            toadd = new ConsoleFlatText(aText, color,aPrompt,aFont,aFontSize);
            iLast->Add(toadd);
            aText+=len;
        }
        else
        {
            memcpy(buffer,aText,len);
            buffer[len] = '\0';
            toadd = new ConsoleFlatText(buffer, color,aPrompt,aFont,aFontSize);
            iLast->Add(toadd);
            if (aText[len] == '\n') aText++;
            aText+=len;
        }
    }
    iOutputHeight+=iLast->height();
    iOutputDirty = 1;
    UpdateHeight(0);
}


void FltkConsole::CommandLineEnd()
{
    DoLine(&iSubLine[0]);
}

void FltkConsole::Restart()
{
  DeleteAll();
  {
    extern LispPtr graph;
    graph.Set(NULL);
  }
  
  extern void RestartYacas();
  RestartYacas();
  extern Fl_Scroll *console_scroll;
  console_scroll->redraw();
}

void FltkConsole::DoLine(char* inpline)
{
    int font_size = 0;

    if(*inpline)
    {
        extern Fl_Tabs* mainTabs;
        extern Fl_Group* helptab;
        if (inpline[0] == '?')
        {
            if (inpline[1] == '?')
            {
                mainTabs->value(helptab);
                goto END;
            }
            else
            {
                if (strlen(&inpline[1]) < 100)
                {
                    char buf[120];
                    extern char defdir[128];

                    sprintf(buf,"%s/documentation/ref.html#%s",defdir,&inpline[1]);
                    extern void HelpGo(char*);
                    HelpGo(buf);
                }
                mainTabs->value(helptab);
                goto END;
            }
        }
        else
        {
            if (!strncmp(inpline,"restart",7))
            {
              Restart();
              goto END;
            }
            else if (!strncmp(inpline,"quit",4))
            {
                exit(0);
            }
        }
    }


    extern CYacas* yacas;
    yacas->Evaluate(inpline);

#ifdef SUPPORT_NOTEPAD
    if (iCurrentHighlighted >= 0)
    {
        if (iConsoleOut[iCurrentHighlighted]->IsEditable())
        {
            iLast = (ConsoleGrouped*)iConsoleOut[iCurrentHighlighted];
            iOutputHeight -= iLast->height();
            UpdateHeight(0);
            font_size = iLast->Fontsize();
            iLast->DeleteAll();
        }
        else
        {
            AddGroup();
        }
    }
    else
    {
        AddGroup();
    }
#else
    AddGroup();
#endif

    if (font_size==0) font_size=iDefaultFontSize;

    AddText(inpline, FL_BLACK,inPrompt,FL_HELVETICA,font_size);
    //SetInputDirty();
    SetOutputDirty();
//    redraw(); //output changed
//    Fl::flush();
    {
//        extern CYacas* yacas;
//        yacas->Evaluate(inpline);

        extern LispString the_out;
        if (the_out[0])
        {
            AddText(the_out.String(), FL_RED,printPrompt,FL_COURIER,font_size);
            the_out.SetNrItems(0);
            the_out.Append('\0');
        }

        if (yacas->Error()[0] != '\0')
        {
            AddText(yacas->Error(), FL_RED,errorPrompt,FL_HELVETICA,font_size);
        }
        else
        {
            AddText(yacas->Result(), FL_BLUE,outPrompt,FL_HELVETICA,font_size);
        }
    }

END:
    LispCharPtr text = "";
    if (iCurrentHighlighted >= 0)
    {
        text = iConsoleOut[iCurrentHighlighted]->input();
    }
    SetInput(text, strlen(text)+1);
}


/*
 void FltkConsole::resize(int x,int y,int w,int h)
{
    // input AND output dirty
    SetInputDirty();
    SetOutputDirty();
    Fl_Widget::resize(x,y,w,h);
}
 */

void FltkConsole::SetInput(LispCharPtr aText, LispInt nr)
{
    iSubLine.SetNrItems(0);
    LispInt i;
    for (i=0;i<nr;i++)
    {
        iSubLine.Append(aText[i]);
    }
    if (cursor >= iSubLine.NrItems())
    	cursor = iSubLine.NrItems()-1;
}
void FltkConsole::GetHistory(LispInt aLine)
{
}

void FltkConsole::MakeSureHighlightedVisible()
{
  extern Fl_Scroll *console_scroll;
  if (iCurrentHighlighted < 0)
  {
    int newy;
    newy=iOutputHeight+20-console_scroll->h();
    if (newy<0) newy=0;
    console_scroll->position(console_scroll->xposition(),newy);
    SetOutputDirty();
//    redraw(); //output changed
//    Fl::flush();
  }
  else
  {
    int i,nr = iConsoleOut.NrItems();
    int iy = 0;
    for (i=0;i<nr && i<iCurrentHighlighted;i++)
    {
      int thisheight = iConsoleOut[i]->height();
      iy+=thisheight;
    }
    extern Fl_Scroll *console_scroll;
    int cons_h = console_scroll->h();
    int hh=h();
    int ypos = console_scroll->yposition();
    int cellh = 0;
    if (iConsoleOut.NrItems()>0)
      cellh = iConsoleOut[iCurrentHighlighted]->height();
    if (iy < ypos || iy+cellh > ypos+cons_h)
    {
      if (iy>iOutputHeight+20-console_scroll->h())
        iy = iOutputHeight+20-console_scroll->h();
      console_scroll->position(console_scroll->xposition(),iy);
      SetOutputDirty();
//      redraw(); //output changed
//      Fl::flush();
    }
  }
  return; //NEWA
/*
#ifdef SUPPORT_NOTEPAD
    if (iCurrentHighlighted < 0)
    {
        iOutputOffsetY = 0;
    }
    else
    {
        int i,nr = iConsoleOut.NrItems();
        int iy = y(), ih = h();
        int lowy = iy+ih-fl_height();
        int thisheight;
        for (i=nr-1;i>=iCurrentHighlighted;i--)
        {
            //        if (lowy+iOutputOffsetY < iy)
            //            break;
            thisheight = iConsoleOut[i]->height();

//            printf("#%d, %d pix\n",i,thisheight);
            lowy = lowy - thisheight;
            if (iCurrentHighlighted == i)
            {
                int top = lowy+iOutputOffsetY;
                int bottom = top + thisheight;

//printf("top %d, bottom %d\n",top,bottom);
                if (top < iy)
                {
//                    printf("adjusttop : %d\n",iOutputOffsetY);
                    iOutputOffsetY = (iy-lowy);
//                    printf("to : %d\n",iOutputOffsetY);
                }
                if (bottom > iy+ih-fl_height()-1)
                {
//                    printf("adjustbottom\n");
                    iOutputOffsetY = iy+ih-fl_height()-1 - lowy - thisheight;
                }
                //lowy+iOutputOffsetY,thisheight
                //fl_rect(ix+iOutputOffsetX, lowy+iOutputOffsetY, iw, thisheight);
                break;
            }
        }
    }
//printf("item %d, offs %d\n",iCurrentHighlighted,iOutputOffsetY);
#endif
*/
}

void FltkConsole::handle_key(int key)
{
    switch (key)
    {
    case eDelete:
        if (cursor<iSubLine.NrItems()-1)
        {
            iSubLine.Delete(cursor);
            iFullLineDirty = 1;
        }
        break;
    case eBackSpace:
        if (cursor>0)
        {
            cursor--;
            iSubLine.Delete(cursor);
            iFullLineDirty = 1;
        }
        break;
    case eLeft:
        if (cursor>0)
            cursor--;
        break;
    case eRight:
        if (cursor<iSubLine.NrItems()-1)
            cursor++;
        break;
    case eUp:

#ifdef SUPPORT_NOTEPAD
        {
            DeleteHints();
            if (iCurrentHighlighted>0)
                SetCurrentHighlighted(iCurrentHighlighted-1);
            else if (iCurrentHighlighted == 0)
            {
            }
            else
                SetCurrentHighlighted(iConsoleOut.NrItems()-1);
            SetOutputDirty();
            MakeSureHighlightedVisible();
            cursor = iSubLine.NrItems()-1;
            redraw(); //output changed
            Fl::flush();
            break;
        }
#endif
        break;
    case eDown:
#ifdef SUPPORT_NOTEPAD
        {
            DeleteHints();
            if (iCurrentHighlighted < 0)
            {
            }
            else if (iCurrentHighlighted < iConsoleOut.NrItems()-1)
                SetCurrentHighlighted(iCurrentHighlighted+1);
            else
                SetCurrentHighlighted(-1);
            SetOutputDirty();
            MakeSureHighlightedVisible();
            redraw(); //output changed
            Fl::flush();
            cursor = iSubLine.NrItems()-1;
            break;
        }
#endif
        break;
    case eTab:
        {
        }
        break;
    case eEscape:
        iSubLine.SetNrItems(1);
        iSubLine[0] = '\0';
        cursor = iSubLine.NrItems()-1;
        iFullLineDirty = 1;
        DeleteHints();
        iCurrentHighlighted = -1;
        iLast = NULL;
        SetOutputDirty();
        break;
    case eHome:
        cursor=0;
        break;
    case eEnd:
        cursor=iSubLine.NrItems()-1;
        break;
    case eEnter:
        DeleteHints();
        if (iSubLine.NrItems()>1)
        {
            CommandLineEnd();
            if (iCurrentHighlighted < 0 )
                CommandLineStartNew();
        }
        else
        {
            if (iCurrentHighlighted>=0)
                if (iConsoleOut[iCurrentHighlighted]->IsEditable())
                {
                    LispCharPtr text = strdup(iConsoleOut[iCurrentHighlighted]->input());
                    DoLine(text);
                    free(text);
                }
        }
        return;
        break;
    default:
        {
            LispChar cc=(LispChar)key;
            iSubLine.Insert(cursor,cc);
            iFullLineDirty = 1;
        }
        cursor++;
        if (hints == NULL)
          CheckForNewHints();
        break;
    }
}

struct HintItem
{
    char* base;
    char* hint;
};


CArrayGrower<HintItem> hintTexts;
char* htex = NULL;
int hoffsets[256];

void DisposeHints()
{
    if (htex)
        free(htex);
    htex = NULL;
}

int LoadHints(char* file)
{
    hintTexts.SetNrItems(0);
    FILE*f = fopen(file,"r");

    if (!f)
    {
        printf("File \"hints\" not found: please type make install -f makefile.linux first\n");
        exit(0);
    }

    {
        fseek(f,0,SEEK_END);
        int n = ftell(f);
        fseek(f,0,SEEK_SET);
        htex = (char*)malloc(n);
        if(htex != NULL)
        {
            fread(htex,1,n,f);
            int i=0;
            HintItem hi;
            for (;;)
            {
                while(htex[i] != ':') i++;
                if (!strncmp(&htex[i],"::",2))
                    break;

                i++;
                hi.base = &htex[i];
                while(htex[i] != ':') i++;
                htex[i] = '\0';
                i++;
                hi.hint = &htex[i];
                while(htex[i] != ':') i++;
                htex[i] = '\0';
                hintTexts.Append(hi);
            }
        }
        fclose(f);
    }
    {
        int i;
        for (i=0;i<256;i++) hoffsets[i] = -1;
        int nr = hintTexts.NrItems();
        if (nr > 0)
        {
            hoffsets[(unsigned char)hintTexts[0].base[0]] = 0;
            for (i=1;i<nr;i++)
            {
                if (hintTexts[i].base[0] != hintTexts[i-1].base[0])
                {
                    hoffsets[(unsigned char)hintTexts[i].base[0]] = i;
                }
            }
        }
    }
    return hintTexts.NrItems();
}



void FltkConsole::TryToHint(int ifrom,int ito)
{
    int nrhints = hintTexts.NrItems();
    int i,start;
    start = hoffsets[(unsigned char)iSubLine[ifrom]];
    if (start<0)
        return;
    for (i = start;i<nrhints;i++)
    {
        if ((unsigned char)iSubLine[ifrom] < (unsigned char)hintTexts[i].base[0])
            break;
        if (!strncmp(&iSubLine[ifrom],hintTexts[i].base,strlen(hintTexts[i].base)))
        {
            if (hints == NULL)
                CreateHints();
            AddHintLine(hintTexts[i].hint);
        }
    }
}

void FltkConsole::CheckForNewHints()
{
	int braces = 1;

	int ifrom,ito;
	ito = cursor;

	while (ito > 0 && braces > 0)
	{
	  ito --;
	  if (iSubLine[ito] == '(')
		braces--;
	  if (iSubLine[ito] == ')')
		braces++;
	}
	if (braces == 0 && ito>0)
	{
	  ifrom = ito-1;
	  while (ifrom>0 && IsAlpha(iSubLine[ifrom])) ifrom--;
	  if (ifrom >= 0)
		if (!IsAlpha(iSubLine[ifrom])) ifrom++;
	  if (ito>ifrom)
		TryToHint(ifrom,ito);
	}
}

void FltkConsole::SetCurrentHighlighted(int i)
{

    if (iCurrentHighlighted != i)
    {
        DeleteHints();

        iCurrentHighlighted = i;

        if (iCurrentHighlighted >= 0 && iConsoleOut[i]->InputIsVisible())
        {
            LispCharPtr text = iConsoleOut[iCurrentHighlighted]->input();
            SetInput(text, strlen(text)+1);
        }
        else
        {
            SetInput("", 1);
        }
    }
}

int FltkConsole::handle(int event)
{
    //printf("event %d\n",event);
    switch (event)
    {
        /*
         case FL_SHOW:
        SetInputDirty();
        SetOutputDirty();
        redraw(); //input and output changed
        break;
        */
    case FL_PUSH: //mouse down
    case FL_DRAG:
        {
            SetInputFont();
            int yy = Fl::event_y();
            if (yy > y() + h()-(int)fl_height())
            {
                int xx = Fl::event_x();
                int i = 0;
                int width = x() + (int)fl_width(INPUT_PROMPT);

                for (i=0;i<iSubLine.NrItems()-1;i++)
                {
                    int added = (int)fl_width(iSubLine[i]);
                    if (width + added > xx)
                        break;
                    width += added;
                }
                cursor = i;
                SetInputDirty();
                //SetOutputDirty();
                redraw(); //input changed
            }
            else
            {
                {
                    iMovingOutput = 1;
                    iMoveBaseX = 0;
                    iMoveBaseY = 0;
                    iMouseDownX = Fl::event_x();
                    iMouseDownY = Fl::event_y();

#ifdef SUPPORT_NOTEPAD
                    {
                        int prevHighlight = iCurrentHighlighted;
                        iCurrentHighlighted = -1;
                        fl_font(FL_HELVETICA,iDefaultFontSize);
                        int iy = y(), ih = h();
                        int lowy = iy+ih-fl_height();
                        int i,nr = iConsoleOut.NrItems();
                        for (i=nr-1;i>=0;i--)
                        {
                            if (lowy+0/*iOutputOffsetY*/ < iy)
                                break;
                            if (lowy+0/*iOutputOffsetY*/>iMouseDownY && lowy+0/*iOutputOffsetY*/ - iConsoleOut[i]->height()<iMouseDownY)
                            {
                                if (prevHighlight == i)
                                {
                                    iCurrentHighlighted = i;
                                }
                                else
                                {
                                    SetCurrentHighlighted(i);
                                    SetInputDirty();
                                    SetOutputDirty();
                                    redraw();//input and output changed
                                }
                                break;
                            }
                            lowy = lowy - iConsoleOut[i]->height();
                        }
                    }
#endif
                }
            }
        }
        break;
    case FL_RELEASE: //mouse up
        iMovingOutput = 0;
        break;
    case FL_KEYBOARD:
        {
            int c = Fl::event_key();
            switch (c)
            {
            case FL_BackSpace    : handle_key(eBackSpace); break;
            case FL_Tab          : handle_key(eTab); break;
            case FL_Enter        : handle_key(eEnter); break;
            case FL_Escape       : handle_key(eEscape); break;
            case FL_Home         : handle_key(eHome); break;
            case FL_Left         : handle_key(eLeft); break;
            case FL_Up           : handle_key(eUp); break;
            case FL_Right        : handle_key(eRight); break;
            case FL_Down         : handle_key(eDown); break;
            case FL_End          : handle_key(eEnd); break;
            case FL_Delete       : handle_key(eDelete); break;
            case FL_Page_Up      :
                {
                    int delta = h()/4;
                    if (Fl::event_shift())
                    {
/*
                         if (iOutputOffsetY+delta < iOutputHeight)
                            iOutputOffsetY += delta;
                        else
                            iOutputOffsetY = iOutputHeight;
*/
                    }
                    else
                    {
                        handle_key(eUp);
                    }
                }
                break;
            case FL_Page_Down    :
                {
                    int delta = h()/4;
                    if (Fl::event_shift())
                    {
/*
                        if (iOutputOffsetY-delta > 0)
                            iOutputOffsetY -= delta;
                        else
                            iOutputOffsetY = 0;
*/
                    }
                    else
                    {
                        handle_key(eDown);
                    }
                }
                break;
            default              :
                c =Fl::event_text()[0];
                if (c>=32 && c<127)
                    handle_key(c);
                break;
            }
            SetInputDirty();
            redraw(); //input changed
        }
        break;
    }
    return 1;
}

void FltkConsole::InsertText(const LispCharPtr aText)
{
   while(*aText) 
   {
    LispChar c = *aText++; 
    iSubLine.Insert(cursor++,c);
   }
   if (hints == NULL)
     CheckForNewHints();
}

//iCurrentHighlighted
void FltkConsole::draw()
{
    fl_font(FL_HELVETICA,iDefaultFontSize);
    fl_clip(x(),y(),w(),h());
    fl_color(FL_WHITE);
    fl_rectf(x(),y(),w(),h());
    if (iCurrentHighlighted >= 0)
        if (iConsoleOut[iCurrentHighlighted]->IsEditable() &&
            iConsoleOut[iCurrentHighlighted]->InputIsVisible())
        {
            DrawInterEdit();
            fl_pop_clip();
            return;
        }
    DrawUnderEdit();
    fl_pop_clip();
    return;
}
void FltkConsole::DrawInterEdit()
{
    fl_clip(x(),y(),w(),h());

    int i,nr = iConsoleOut.NrItems();
    int ix = x(),iy = y(), iw = w(), ih = h();
    int lowy = iy+ih-fl_height();
    int inputlowy = y()+h()-fl_height()-fl_descent();
    for (i=nr-1;i>=0;i--)
    {
        if (lowy/*+iOutputOffsetY*/ < iy)
            break;
        int selected = (iCurrentHighlighted == i);
        int thisheight = iConsoleOut[i]->height(!selected);

        lowy = lowy - thisheight;
        iConsoleOut[i]->draw(ix/*+iOutputOffsetX*/, lowy/*+iOutputOffsetY*/, iw,!selected);
#ifdef SUPPORT_NOTEPAD
        if (iCurrentHighlighted == i)
        {
            SetInputFont();
            lowy -= fl_height();
            inputlowy = lowy/*+iOutputOffsetY*/;
            DrawInputLine(lowy/*+iOutputOffsetY*/);

            fl_color(FL_BLACK);
            fl_rect(ix/*+iOutputOffsetX*/, lowy/*+iOutputOffsetY*/, iw, thisheight+fl_height());
        }
#endif
    }

    if (hints)
    {
        fl_font(FL_HELVETICA,iDefaultFontSize);
        hints->draw(x(),inputlowy-3);
    }

    
    fl_pop_clip();
    iOutputDirty = 0;
    return;
}


void FltkConsole::DrawInputLine(int lowy)
{
    const char* text = &iSubLine[0];
    int iix =  x() + (int)fl_width(INPUT_PROMPT);
    int cur = iix+(int)fl_width(text, cursor);

    int limit = x()+w()-8;
    if (cur > limit)
    {
        iix += (limit - cur);
        cur = limit;
    }

    SetInputFont();
    fl_color(FL_RED);
    fl_draw(INPUT_PROMPT,x(),lowy+fl_height()-fl_descent());

    fl_clip(x() + (int)fl_width(INPUT_PROMPT),lowy,w()-(int)fl_width(INPUT_PROMPT),fl_height());

    fl_draw(text,iix,lowy+fl_height()-fl_descent());

    fl_color(FL_BLACK);
    fl_begin_line();
    fl_vertex(cur,lowy);
    fl_vertex(cur,lowy+fl_height());
    fl_end_line();

    fl_pop_clip();
    iInputDirty = 0;
}

void FltkConsole::DrawUnderEdit()
{
    int i,nr;
    nr = iConsoleOut.NrItems();
    int ix = x(),iy = y(), iw = w(), ih = h();
    int lowy = iy+ih-fl_height();

    DrawInputLine(lowy);


//    if (iOutputDirty)
    {
        fl_clip(x(),y(),w(),h()-fl_height());
        for (i=nr-1;i>=0;i--)
        {
            if (lowy/*+iOutputOffsetY*/ < iy)
                break;
            int thisheight = iConsoleOut[i]->height();
            lowy = lowy - thisheight;
            iConsoleOut[i]->draw(ix/*+iOutputOffsetX*/, lowy/*+iOutputOffsetY*/, iw);
#ifdef SUPPORT_NOTEPAD
            if (iCurrentHighlighted == i)
            {
                fl_color(FL_BLACK);
                fl_rect(ix/*+iOutputOffsetX*/, lowy/*+iOutputOffsetY*/, iw, thisheight);
            }
#endif
        }
        fl_pop_clip();

        if (hints)
        {
            fl_font(FL_HELVETICA,iDefaultFontSize);
            hints->draw(x(),y()+h()-fl_height()-fl_descent()-3);
        }
    }
    iOutputDirty = 0;
    return;
}


void FltkConsole::UpdateHeight(int aDelta)
{
  {
    iOutputHeight+= aDelta;
    extern Fl_Scroll *console_scroll;
    if (iOutputHeight>0 && console_scroll)
    {
      resize(x(),y(),w(),iOutputHeight);
      MakeSureHighlightedVisible();
      console_scroll->redraw();
    }
  }
}
void FltkConsole::AddOutput(ConsoleOutBase* aOutput)
{
  iConsoleOut.Append(aOutput);
  int extraHeight = aOutput->height();
  UpdateHeight(extraHeight);
}

void FltkConsole::DeleteHints()
{
    if (hints != NULL)
    {
        delete hints;
        hints = NULL;
        iOutputDirty=1;
    }
}

void FltkConsole::CreateHints()
{
    DeleteHints();
    hints = new FltkHintWindow(iDefaultFontSize);
    iOutputDirty=1;
}
void FltkConsole::AddHintLine(LispCharPtr aText)
{
    hints->AddLine(aText);
}

ConsoleOutBase::~ConsoleOutBase()
{
}

void ConsoleFlatText::Save(FILE* f)
{
  fprintf(f,"Proteus'AddCommand(\"%s\");\n",iText.String());
}

ConsoleFlatText::ConsoleFlatText(LispCharPtr aText, int aColor, const char* aPrompt,
                                 int aFont,int aFontSize)
{
    iText = aText;
    iColor = aColor;
    iPrompt = aPrompt;
    iFont = aFont;
    iFontSize = aFontSize;
}
void ConsoleFlatText::draw(int x, int y, int width,int draw_input)
{
    fl_font(iFont,iFontSize);

    y += fl_height()-fl_descent();
    int promptWidth = (int)fl_width(iPrompt);
    fl_color(FL_GREEN);
    fl_draw(iPrompt,x,y);

    //    fl_clip(x,y,width,height());
    fl_color(iColor);
    const char* text = &iText[0];
    fl_draw(text,x+promptWidth,y);
}
int ConsoleFlatText::height(int draw_input)
{
    fl_font(iFont,iFontSize);
    return fl_height();
}
LispCharPtr ConsoleFlatText::input()
{
    return &iText[0];
}

int ConsoleFlatText::Fontsize() const
{
  return iFontSize;
}


void ConsoleGrouped::Save(FILE* f)
{
  if (iConsoleOut.NrItems()>0 && iShowInput && iEnableInput)
  {
    fprintf(f,":");
    if (!iShowInput) fprintf(f,"i");
    if (!iEnableInput) fprintf(f,"e");
    fprintf(f,":");
    iConsoleOut[0]->Save(f);
  }
}

int ConsoleGrouped::Fontsize() const
{
  if (iConsoleOut.NrItems()>0)
    return iConsoleOut[0]->Fontsize();
  return 0;
}


LispCharPtr ConsoleGrouped::input()
{
    if (!iEnableInput)
        return "";
    if (iConsoleOut.NrItems()>0)
        return iConsoleOut[0]->input();
    return "";
}

void ConsoleGrouped::Add(ConsoleOutBase* aLine)
{
    iConsoleOut.Append(aLine);
}
void ConsoleGrouped::draw(int x, int y, int width,int draw_input)
{
    int i, nr,from;

    if (iShowInput && draw_input)
        from = 0;
    else
        from = 1;
    nr = iConsoleOut.NrItems();
    for (i=from;i<nr;i++)
    {
        iConsoleOut[i]->draw(x, y, width);
        y+=iConsoleOut[i]->height();
    }
}
int ConsoleGrouped::height(int draw_input)
{
    int i, nr,h,from;
    if (iShowInput && draw_input)
        from = 0;
    else
        from = 1;
        
    nr = iConsoleOut.NrItems();
    h=0;
    for (i=from;i<nr;i++)
    {
        h+=iConsoleOut[i]->height();
    }
    return h;
}
void ConsoleGrouped::DeleteAll()
{
    int i,nr;
    nr = iConsoleOut.NrItems();
    for (i=0;i<nr;i++)
    {
        delete iConsoleOut[0];
        iConsoleOut.Delete(0);
    }
}
int ConsoleGrouped::IsEditable()
{
    return iEnableInput;
}
int ConsoleGrouped::InputIsVisible()
{
    return iShowInput;
}

LispCharPtr ConsoleOutBase::input()
{
    return "";
}

int ConsoleOutBase::IsEditable()
{
    return 0;
}
int ConsoleOutBase::InputIsVisible()
{
    return 0;
}

