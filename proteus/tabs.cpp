#include <stdio.h>
#include <FL/Fl.H>
#include <FL/Fl_Button.H>
#include <FL/Fl_Clock.H>
#include <FL/Fl_Group.H>
#include <FL/Fl_Input.H>
#include <FL/Fl_Multiline_Input.H>
#include <FL/Fl_Return_Button.H>
#include <FL/Fl_Tabs.H>
#include <FL/Fl_Window.H>
#include <FL/Fl_Select_Browser.H>
#include <FL/fl_draw.H>
#include <FL/Fl_Menu_Bar.H>
#include <FL/Fl_Menu_Item.H>
#include <FL/Fl_Scroll.H>
#include <Fl/Fl_File_Chooser.H>
#include <stdlib.h>
#include "yacas.h"
#include "editor.h"
#include "HelpView.h"
#include "FltkConsole.h"
#include "yacdefines.h"
#include "grapher.h"
#include "stringio.h"

#include "standard.h"
#include "errors.h"
#define InternalEval aEnvironment.iEvaluator->Eval
#define RESULT aEnvironment.iStack.GetElement(aStackTop)
#define ARGUMENT(i) aEnvironment.iStack.GetElement(aStackTop+i)


#ifdef WIN32
#  define snprintf _snprintf
#endif


LispString the_out;
StringOutput stdOutput(the_out);

Fl_Multiline_Input *text2;
Fl_Tabs* mainTabs;
Fl_Group *inputPage;

Fl_Select_Browser *chapter;
Fl_Select_Browser *paragraph;
Fl_Multiline_Input *helptext;
Fl_Group* grapher;

Fl_Scroll *console_scroll = NULL;


HelpView* helpview_;
Fl_Button *back_;
Fl_Button *forward_;
Fl_Button *smaller_;
Fl_Button *larger_;

Fl_Group* input;

FltkConsole* console;
Fl_Group* helptab;

int index_ = 0;
int max_  = 0;
int line_[100];
char file_[100][256];

static Fl_Menu_Bar *menubar;

void HelpGo(char* f)
{
    index_ ++;
    max_ = index_;
    strcpy(file_[index_],f);
    helpview_->load(f);
}

char *outbuf;
int curoutlen=0;

int line=0;
CYacas* yacas=NULL;


class Drawer : public Fl_Widget
{
public:
    Drawer(int i1,int i2, int i3, int i4)
        : Fl_Widget(i1,i2,i3,i4) {};
        virtual void draw(); // from Fl_Widget
        virtual int handle(int);

};


Drawer *drawing;

int Drawer::handle(int event)
{

    switch (event)
    {
    case FL_KEYBOARD:
        {
            int c = Fl::event_key();
            if (c == FL_Escape)
            {
                mainTabs->value(input);
            }
        }
        return 0;
    }
    return 1;
}

void Drawer::draw()
{
    extern LispEnvironment* graphEnvironment;
    extern LispPtr graph;
    if (graph.Get())
    {
        char buf[300];
        sprintf(buf,"FlWindow:={%d,%d,%d,%d};",
                (int)x(),
                (int)y(),
                (int)w(),
                (int)h());
        yacas->Evaluate(buf);
        the_out.SetNrItems(0);
        the_out.Append('\0');
        fl_clip(x(),y(),w(),h());
        LispPtr result;
        graphEnvironment->iEvaluator->Eval(*graphEnvironment,result,graph);
        fl_pop_clip();
    }
}

void cb_helpview__i(HelpView*, void*)
{
    if (helpview_->changed())
    {
        index_ ++;

        if (index_ >= 100)
        {
            memcpy(line_, line_ + 10, sizeof(line_[0]) * 90);
            memcpy(file_, file_ + 10, sizeof(file_[0]) * 90);
            index_ -= 10;
        }

        max_ = index_;

        strcpy(file_[index_], helpview_->filename());
        line_[index_] = helpview_->topline();

        if (index_ > 0)
            back_->activate();
        else
            back_->deactivate();

        forward_->deactivate();
    }
    else if (helpview_->filename())
    {
        strcpy(file_[index_], helpview_->filename());
        line_[index_] = helpview_->topline();
    };
}
void cb_helpview_(HelpView* o, void* v)
{
    cb_helpview__i(o,v);
}


void cb_back__i(Fl_Button*, void*)
{
    if (index_ > 0)
        index_ --;

    if (index_ == 0)
        back_->deactivate();

    forward_->activate();

    if (strcmp(helpview_->filename(), file_[index_]) != 0)
        helpview_->load(file_[index_]);

    helpview_->topline(line_[index_]);
}

void cb_back_(Fl_Button* o, void* v)
{
    cb_back__i(o,v);
}

void cb_forward__i(Fl_Button*, void*)
{
    if (index_ < max_)
        index_ ++;

    if (index_ >= max_)
        forward_->deactivate();

    back_->activate();

    if (strcmp(helpview_->filename(), file_[index_]) != 0)
        helpview_->load(file_[index_]);

    helpview_->topline(line_[index_]);
}

void cb_forward_(Fl_Button* o, void* v)
{
    cb_forward__i(o,v);
}

void cb_smaller__i(Fl_Button*, void*)
{
  if (helpview_->textsize() > 8)
  helpview_->textsize(helpview_->textsize() - 2);

  if (helpview_->textsize() <= 8)
  smaller_->deactivate();
  larger_->activate();
}

void cb_smaller_(Fl_Button* o, void* v)
{
  cb_smaller__i(o,v);
}

void cb_larger__i(Fl_Button*, void*)
{
  if (helpview_->textsize() < 18)
  helpview_->textsize(helpview_->textsize() + 2);

  if (helpview_->textsize() >= 18)
  larger_->deactivate();
  smaller_->activate();
}

void cb_larger_(Fl_Button* o, void* v)
{
  cb_larger__i(o,v);
}


void cb_menu_insert(Fl_Widget* o, void* v)
{
    char* text = (char*)v;
    console->InsertText(text);
    console->redraw();
}
void cb_notepad(Fl_Widget* o, void* v)
{
    char *newfile;
    newfile = fl_file_chooser("Open file?","*","");
    if (newfile != NULL)
    {
        console->LoadNotePad(newfile);
        console->redraw();
    }
}


void cb_save_notepad(Fl_Widget* o, void* v)
{
    char *newfile;
    newfile = fl_file_chooser("Save to file?",NULL,"");
    if (newfile != NULL)
    {
        console->SaveNotePad(newfile);
    }
}


void quit_cb(Fl_Widget *,void *)
{
    exit(0);
}

void new_cb(Fl_Widget *,void *)
{
  console->Restart();
}

void help_intro_cb(Fl_Widget *,void *page)
{
  extern char defdir[128];
  char buf[120];
  sprintf(buf,"%s/%s",defdir,(char*)page);
  HelpGo(buf);
  mainTabs->value(helptab);
}


void help_detailed_function_cb(Fl_Widget *,void *)
{
  const char *input = fl_input("Find help on function:", "");
  if (input) 
  {
    extern char defdir[128];
    char buf[256];
    snprintf(buf,256,"%s/documentation/ref.html#%s",defdir,input);
    HelpGo(buf);
    mainTabs->value(helptab);
  }
}
#include "proteusmenu.h"


void LispExit(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    exit(0);
}

void LispShowInput(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    if (IsTrue(aEnvironment, ARGUMENT(1)))
    {
        console->ShowInput(1);
    }
    if (IsFalse(aEnvironment, ARGUMENT(1)))
    {
        console->ShowInput(0);
    }
    InternalTrue(aEnvironment,RESULT);
}

void LispEnableInput(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    LispPtr evaluated;
    if (IsTrue(aEnvironment, ARGUMENT(1)))
    {
        console->EnableInput(1);
    }
    if (IsFalse(aEnvironment, ARGUMENT(1)))
    {
        console->EnableInput(0);
    }
    InternalTrue(aEnvironment,RESULT);
}

void LispNotepad(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    CHK_ARG_CORE(ARGUMENT(1).Get() != NULL, 1);
    LispStringPtr orig = ARGUMENT(1).Get()->String();
    CHK_ARG_CORE(orig != NULL, 1);
    LispString oper;
    InternalUnstringify(oper, orig);
    console->LoadNotePad(&oper[0]);
    InternalTrue(aEnvironment,RESULT);
}

void LispNotepadAddCommand(LispEnvironment& aEnvironment,LispInt aStackTop)
{
  CHK_ARG_CORE(ARGUMENT(1).Get() != NULL, 1);
  LispStringPtr orig = ARGUMENT(1).Get()->String();
  CHK_ARG_CORE(orig != NULL, 1);
  LispString oper;
  InternalUnstringify(oper, orig);
  console->AddGroup(1, 1);
  console->AddText(&oper[0], FL_BLACK,"In> ",FL_HELVETICA,console->iDefaultFontSize);
  extern CYacas* yacas;
  yacas->Evaluate(&oper[0]);
//  if (!internal)
  {
    if (the_out[0])
    {
      console->AddText(the_out.String(), FL_RED,"  ",FL_COURIER,console->iDefaultFontSize);
    }
    if (yacas->Error()[0] != '\0')
    {
      console->AddText(yacas->Error(), FL_RED,"Error> ",FL_HELVETICA,console->iDefaultFontSize);
    }
    else
    {
      console->AddText(yacas->Result(), FL_BLUE,"Out> ",FL_HELVETICA,console->iDefaultFontSize);
    }
  }
  extern LispString the_out;
  the_out.SetNrItems(0);
  the_out.Append('\0');
  InternalTrue(aEnvironment,RESULT);
}

void LispNotepadAddLink(LispEnvironment& aEnvironment,LispInt aStackTop)
{
  LispStringPtr orig;
  CHK_ARG_CORE(ARGUMENT(1).Get() != NULL, 1);
  orig = ARGUMENT(1).Get()->String();
  CHK_ARG_CORE(orig != NULL, 1);
  LispString oper;
  InternalUnstringify(oper, orig);
  CHK_ARG_CORE(ARGUMENT(2).Get() != NULL, 2);
  orig = ARGUMENT(2).Get()->String();
  CHK_ARG_CORE(orig != NULL, 1);
  LispString text;
  InternalUnstringify(text, orig);

  LispInt showinput = IsTrue(aEnvironment, ARGUMENT(3));
  LispInt enableinput = IsTrue(aEnvironment, ARGUMENT(4));

  console->AddGroup(showinput, enableinput);
  console->AddText(&oper[0], FL_BLACK,"",FL_HELVETICA,console->iDefaultFontSize);

  char* buf = (char*)malloc(text.NrItems()+1); //TODO check for null pointer
  strcpy(buf,&text[0]);
  char*start=buf;
  char*end;
  int last = 0;
NEXTLINE:
  end = start;
  while (*end != '\n' && *end != '\0') end++;
  if (*end == '\0') last=1;
  *end=0;
  {
    console->AddText(start, FL_BLACK,"",FL_HELVETICA_BOLD,console->iDefaultFontSize);
    start=end+1;
    if (!last)
      goto NEXTLINE;
  }
  free(buf);
  InternalTrue(aEnvironment,RESULT);
}
//	Proteus'AddLink(CommandString,LinkText);
//

char defdir[128];
void GetProteusConfiguration()
{
#ifdef WIN32
    strcpy(defdir,".\\");
    return;
#else
    FILE*f;
    f = fopen("/etc/proteus.conf","r");
    if (!f) f=fopen("proteus.conf","r");
    if (f)
    {
        fscanf(f,"%s",defdir);
        fclose(f);
    }
    else
    {
        printf("Error: could not find file /etc/proteus.conf");
        exit(-1);
    }
#endif
}

void RestartYacas()
{
    if (yacas != NULL)
        delete yacas;
    yacas=NULL;
    line=0;

    yacas = CYacas::NewL(new StringOutput(the_out));

#define CORE_KERNEL_FUNCTION(iname,fname,nrargs,flags) (*yacas)()().SetCommand(fname,iname,nrargs,flags)
CORE_KERNEL_FUNCTION("NoteShowInput",LispShowInput,1,YacasEvaluator::Function | YacasEvaluator::Fixed);
CORE_KERNEL_FUNCTION("NoteEnableInput",LispEnableInput,1,YacasEvaluator::Function | YacasEvaluator::Fixed);
CORE_KERNEL_FUNCTION("Notepad",LispNotepad,1,YacasEvaluator::Function | YacasEvaluator::Fixed);
CORE_KERNEL_FUNCTION("Exit",LispExit,0,YacasEvaluator::Function | YacasEvaluator::Fixed);

CORE_KERNEL_FUNCTION("Proteus'AddCommand",LispNotepadAddCommand,1,YacasEvaluator::Function | YacasEvaluator::Fixed);
CORE_KERNEL_FUNCTION("Proteus'AddLink",LispNotepadAddLink,4,YacasEvaluator::Function | YacasEvaluator::Fixed);


#undef CORE_KERNEL_FUNCTION




    char cmd[128];
    sprintf(cmd,"DefaultDirectory(\"%s\");",defdir);
    yacas->Evaluate(cmd);

    AddGraphingCapabilities((*yacas)()());


    {
        extern char defdir[128];
        char buf[128];
        sprintf(buf,"%sWorksheetBanner",defdir);
        console->LoadNotePad(buf);
        console->handle_key(eEscape); // Highlight should be on bottom line
    }

    yacas->Evaluate("Load(\"yacasinit.ys\");");
}


void myexit()
{
    console->SaveHistory();
//    printf("Quitting...\n");
    delete yacas;
    yacas = NULL;
}



int main(int argc, char **argv)
{
  GetProteusConfiguration();
  curoutlen=10;
  outbuf = (char*)malloc(curoutlen);

  Fl_Window* w;
  {
    Fl_Window* o = new Fl_Window(640, 320);
    w = o;
    {
      mainTabs = new Fl_Tabs(5, 1, 630, 315);
      o->selection_color(15);
      {

        Fl_Group* o = input = new Fl_Group(10, 20, 630, 310, "Input");
        {
          menubar = new Fl_Menu_Bar(11,23,618,20);
          menubar->menu(menuitems);
          menubar->textfont(10);
          menubar->textsize(12);
        }
        console_scroll = new Fl_Scroll(11,43,618,265);
        console = new FltkConsole(11,43,2048,265,12);
//        console->resize(console->x(),console->y(),console->w()+1024,console->h()+1024);
//        console = new FltkConsole(11,43,618,265,12);
        console_scroll->end();
        o->end();
//        Fl_Group::current()->resizable(o);
        Fl_Group::current()->resizable(console_scroll);
      }
      {
        Fl_Group* o = grapher = new Fl_Group(10, 20, 630, 310, "Graph");
        drawing = new Drawer(11,23,618,285);
        o->end();
      }
      {
        Fl_Group* o = new Fl_Group(10, 20, 630, 310, "Edit");
         editor_add_items(11,23,618, 285);
        o->end();
      }
      {
        Fl_Group* helptab;
        Fl_Group* o = helptab = new Fl_Group(10, 20, 630, 310, "Help");
        {
          HelpView* o = helpview_ = new HelpView(11, 23, 620, 260);
          o->box(FL_DOWN_BOX);
          o->callback((Fl_Callback*)cb_helpview_);
          o->end();
          Fl_Group::current()->resizable(o);
        }
        {
          Fl_Button* o = back_ = new Fl_Button(430, 285, 25, 25, "@<");
          o->shortcut(0xff51);
          o->labeltype(FL_SYMBOL_LABEL);
          o->callback((Fl_Callback*)cb_back_);
        }
        {
          Fl_Button* o = forward_ = new Fl_Button(465, 285, 25, 25, "@>");
          o->shortcut(0xff53);
          o->labeltype(FL_SYMBOL_LABEL);
          o->callback((Fl_Callback*)cb_forward_);
        }
        {
          Fl_Button* o = smaller_ = new Fl_Button(360, 285, 25, 25, "F");
          o->labelfont(1);
          o->labelsize(9);
          o->callback((Fl_Callback*)cb_smaller_);
        }
        {
          Fl_Button* o = larger_ = new Fl_Button(395, 285, 25, 25, "F");
          o->labelfont(11);
          o->labelsize();
          o->callback((Fl_Callback*)cb_larger_);
        }
        {
          char helpfile[128];
#ifdef WIN32
          sprintf(helpfile,"proteusbooks.html");
#else
          sprintf(helpfile,"%sdocumentation/%s",defdir,YACAS_DOC);
#endif
          helpview_->load(helpfile);
        }
        o->end();
      }
      init_editor();
      mainTabs->end();
      Fl_Group::current()->resizable(o);

    }
    Fl_Group::current()->resizable(o);
    o->end();
  }

  RestartYacas();


  w->callback(quit_cb);
  w->show(1, argv);
  atexit(myexit);
  if (argc>1)
  {
    console->LoadNotePad(argv[1]);
    console->redraw();
  }
  return Fl::run();
}


