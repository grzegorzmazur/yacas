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
#include <Fl/Fl_Help_Dialog.H>
#include <stdlib.h>
#include "FltkConsole.h" 
// FltkConsole.h should be included before yacas.h, so that #undef Status 
// hack works
#include "yacas.h"
#include "editor.h"
#include "HelpView.h"

#include "yacdefines.h"
#include "grapher.h"
#include "stringio.h"
#include "arggetter.h"

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

char notepad_to_load[256];
int release_structure = 1;
int custom_notepad = 0;

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

Fl_Menu_Bar *menubar;

void HelpGo(char* f)
{
/*Fl_Help_Dialog does not seem ready for prime time yet :-(
    Fl_Help_Dialog *help = new Fl_Help_Dialog;
    help->load(f);
    help->show(0,NULL);
    Fl::run();
    delete help;
*/

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

void restart_cb(Fl_Widget *,void *)
{
  console->Restart();
}

void new_cb(Fl_Widget *,void *)
{
  extern char defdir[128];
  if (release_structure)
    sprintf(notepad_to_load,"%sWorksheetBanner",defdir);
  else
    sprintf(notepad_to_load,"%sproteus/WorksheetBanner",defdir);
  console->Restart();
}

void help_intro_cb(Fl_Widget *,void *page)
{
  extern char defdir[128];
  char buf[120];
  if (release_structure)
    sprintf(buf,"%s/documentation/%s",defdir,(char*)page);
  else
    sprintf(buf,"%s/manmake/%s",defdir,(char*)page);

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

    if (release_structure)
      snprintf(buf,256,"%s/documentation/ref.html#%s",defdir,input);
    else
      snprintf(buf,256,"%s/manmake/ref.html#%s",defdir,input);
    HelpGo(buf);
    mainTabs->value(helptab);
  }
}

static Fl_Menu_Item menuitems[] = 
{
 {"&File", FL_ALT+'f',  0, 0, FL_SUBMENU},
   {"&new", 	FL_ALT+'n',	(Fl_Callback *)new_cb },
   {"&restart",	FL_ALT+'r',	(Fl_Callback *)restart_cb, 0, FL_MENU_DIVIDER },
   {"l&oad", 	FL_ALT+'o',	(Fl_Callback *)cb_notepad },
//TODO   {"TODO save", 0,  cb_menu_insert, (void*)"TODO", 0, 0, 0, font_size, 0},
   {"s&ave as",	FL_ALT+'a',	(Fl_Callback *)cb_save_notepad },
   {"TODO about", 0, 0, 0, FL_MENU_DIVIDER },
   {"&quit", 	FL_ALT+'q',  	(Fl_Callback *)quit_cb },
   {0},
/*TODO
 {"&Edit", FL_ALT+'e',  0, NULL, 64, 0, 0, font_size, 0},
   {"TODO cut", 0,  cb_menu_insert, (void*)"TODO", 0, 0, 0, font_size, 0},
   {"TODO copy", 0,  cb_menu_insert, (void*)"TODO", 0, 0, 0, font_size, 0},
   {"TODO paste", 0,  cb_menu_insert, (void*)"TODO", 0, 0, 0, font_size, 0},
   {0},
 {"&Notepad", FL_ALT+'n',  0, NULL, 64, 0, 0, font_size, 0},
   {"TODO edit code", 0,  cb_menu_insert, (void*)"TODO", 0, 0, 0, font_size, 0},
   {"insert", 0,  0, NULL, 64, 0, 0, font_size, 0},
     {"TODO section title", 0,  cb_menu_insert, (void*)"TODO", 0, 0, 0, font_size, 0},
     {"TODO flat text", 0,  cb_menu_insert, (void*)"TODO", 0, 0, 0, font_size, 0},
     {"TODO 2d graph", 0,  cb_menu_insert, (void*)"TODO", 0, 0, 0, font_size, 0},
     {"TODO link to other notepad", 0,  cb_menu_insert, (void*)"TODO", 0, 0, 0, font_size, 0},
     {"TODO link command", 0,  cb_menu_insert, (void*)"TODO", 0, 0, 0, font_size, 0},
     {0},
   {"TODO edit", 0,  cb_menu_insert, (void*)"TODO", 0, 0, 0, font_size, 0},
   {"TODO delete", 0,  cb_menu_insert, (void*)"TODO", 0, 0, 0, font_size, 0},
   {0},
*/
 {"&Help", FL_ALT+'h',  0, 0, FL_SUBMENU},
   {"&contents", FL_ALT+'c',	(Fl_Callback *)help_intro_cb },
   {"&detailed help on function", FL_ALT+'d',	(Fl_Callback *)help_detailed_function_cb },
   {0},
 {0}
};


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
    if (oper[0] == '\0') RaiseError("Could not open notepad file %s for reading",&oper[0]);
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
  console->AddText(&oper[0], console->NotepadFontColor(),"In> ",console->NotepadFontType(),console->NotepadFontSize());
  extern CYacas* yacas;
  yacas->Evaluate(&oper[0]);
//  if (!internal)
  {
    if (the_out[0])
    {
      console->AddText(the_out.String(), FL_RED,"  ",FL_COURIER,console->NotepadFontSize());
    }
    if (yacas->Error()[0] != '\0')
    {
      console->AddText(yacas->Error(), FL_RED,"Error> ",console->NotepadFontType(),console->NotepadFontSize());
    }
    else
    {
      console->AddText(yacas->Result(), FL_BLUE,"Out> ",console->NotepadFontType(),console->NotepadFontSize());
    }
    extern ConsoleOutBase* cell_to_insert;
    if (cell_to_insert)
    {
      console->iOutputHeight+=cell_to_insert->height();
      console->iLast->Add(cell_to_insert);
      cell_to_insert = NULL;
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
  console->AddText(&oper[0], console->NotepadFontColor(),"",console->NotepadFontType(),console->NotepadFontSize());

  char* buf = (char*)malloc(text.NrItems()+1); //TODO check for null pointer
  strcpy(buf,&text[0]);
  char*start=buf;
  char*end;
  int last = 0;
NEXTLINE:
  end = start;
  while (*end != '\n' && *end != '\0')
  {
    if (*end == '\r') *end = ' ';
    end++;
  }
  if (*end == '\0') last=1;
  *end=0;
  {
    console->AddText(start, console->NotepadFontColor(),"",console->NotepadFontType(),console->NotepadFontSize());
    start=end+1;
    if (!last)
      goto NEXTLINE;
  }
  free(buf);
  InternalTrue(aEnvironment,RESULT);
}
//	Proteus'AddLink(CommandString,LinkText);
//


void LispNotepadFontType(LispEnvironment& aEnvironment,LispInt aStackTop)
{
  ShortIntegerArgument(arg1,  1 );
  console->NotepadFontType(arg1);
  InternalTrue(aEnvironment,RESULT);
}

void LispNotepadFontSize(LispEnvironment& aEnvironment,LispInt aStackTop)
{
  ShortIntegerArgument(arg1,  1 );
  console->NotepadFontSize(arg1);
  InternalTrue(aEnvironment,RESULT);
}
void LispNotepadFontColor(LispEnvironment& aEnvironment,LispInt aStackTop)
{
  ShortIntegerArgument(arg1,  1 );
  console->NotepadFontColor(arg1);
  InternalTrue(aEnvironment,RESULT);
}


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

CORE_KERNEL_FUNCTION("Proteus'FontType",LispNotepadFontType,1,YacasEvaluator::Function | YacasEvaluator::Fixed);
CORE_KERNEL_FUNCTION("Proteus'FontSize",LispNotepadFontSize,1,YacasEvaluator::Function | YacasEvaluator::Fixed);
CORE_KERNEL_FUNCTION("Proteus'FontColor",LispNotepadFontColor,1,YacasEvaluator::Function | YacasEvaluator::Fixed);

#undef CORE_KERNEL_FUNCTION




    char cmd[128];
    if (release_structure)
      sprintf(cmd,"DefaultDirectory(\"%s\");",defdir);
    else
      sprintf(cmd,"DefaultDirectory(\"%s/scripts/\");",defdir);
      
    yacas->Evaluate(cmd);
    AddGraphingCapabilities((*yacas)()());
    yacas->Evaluate("Load(\"yacasinit.ys\");");
    yacas->Evaluate("DefLoad(\"flplot.ys\");");
    console->LoadNotePad(notepad_to_load);
    if (!custom_notepad)
    {
      console->handle_key(eEscape); // Highlight should be on bottom line
    }
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
  sprintf(notepad_to_load,"%sWorksheetBanner",defdir);
  Fl::scheme("plastic");
  {
    int i=1;
    while (i<argc)
    {
      if (!strcmp(argv[i],"--rootdir"))
      {
        i++;
        if (i<argc)
          strncpy(defdir,argv[i],128);
      }
      else if (!strcmp(argv[i],"--srcdir-structure"))
      {
        release_structure = 0;
        sprintf(notepad_to_load,"%sproteus/WorksheetBanner",defdir);
      }
      else break;
      i++;
    }
    if (i<argc)
    {
      strncpy(notepad_to_load, argv[i],256);
      custom_notepad = 1;
    }
  }

  curoutlen=10;
  outbuf = (char*)malloc(curoutlen);

  Fl_Window* w;
  {
    Fl_Window* o = new Fl_Window(640, 480);
    w = o;
    {
      mainTabs = new Fl_Tabs(5, 3, 630, 474);
      o->selection_color(15);
      {

        Fl_Group* o = input = new Fl_Group(10, 24, 625, 452, "Input");
        {
          menubar = new Fl_Menu_Bar(11,25,621,15);
          menubar->menu(menuitems);
          //menubar->textfont(10);
          //menubar->textsize(14);
        }
        console_scroll = new Fl_Scroll(11,41,621,435);
        console = new FltkConsole(11,41,1200,435);
//        console->resize(console->x(),console->y(),console->w()+1024,console->h()+1024);
//        console = new FltkConsole(11,43,618,265,12);
        console_scroll->end();
        o->end();
//        Fl_Group::current()->resizable(o);
        Fl_Group::current()->resizable(console_scroll);
      }

/*TODO remove, graphs are now embedded
      {
        Fl_Group* o = grapher = new Fl_Group(10, 20, 630, 310, "Graph");
        drawing = new Drawer(11,23,618,285);
        o->end();
      }
*/
/*TODO editor should be separate app, with debugger*/
      {
        Fl_Group* o = new Fl_Group(10, 25, 625, 452, "Editor");
         editor_add_items(o, 11, 26, 623, 440);
        o->end();
      }
/**/
      {
        Fl_Group* helptab;
        Fl_Group* o = helptab = new Fl_Group(10, 25, 625, 452, "Help");
        {
          HelpView* o = helpview_ = new HelpView(11, 25, 621, 420);
          o->box(FL_DOWN_BOX);
          o->callback((Fl_Callback*)cb_helpview_);
          o->end();
          Fl_Group::current()->resizable(o);
        }
        {
          Fl_Button* o = back_ = new Fl_Button(465, 447, 25, 25, "@<");
          o->shortcut(0xff51);
          o->labeltype(FL_SYMBOL_LABEL);
          o->callback((Fl_Callback*)cb_back_);
        }
        {
          Fl_Button* o = forward_ = new Fl_Button(500, 447, 25, 25, "@>");
          o->shortcut(0xff53);
          o->labeltype(FL_SYMBOL_LABEL);
          o->callback((Fl_Callback*)cb_forward_);
        }
        {
          Fl_Button* o = smaller_ = new Fl_Button(395, 447, 25, 25, "F");
          o->labelfont(1);
          o->labelsize(9);
          o->callback((Fl_Callback*)cb_smaller_);
        }
        {
          Fl_Button* o = larger_ = new Fl_Button(430, 447, 25, 25, "F");
          o->labelfont(11);
          o->labelsize();
          o->callback((Fl_Callback*)cb_larger_);
        }
        {
          char helpfile[128];
//#ifdef WIN32
//          sprintf(helpfile,"proteusbooks.html");
//#else
          if (release_structure)
            sprintf(helpfile,"%sdocumentation/%s",defdir,YACAS_DOC);
          else
            sprintf(helpfile,"%smanmake/%s",defdir,YACAS_DOC);
//#endif
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
  w->resize(50,50,800,600);
//  w->fullscreen(); //TODO is this acceptable?
  atexit(myexit);

  if (argc>1)
  {
    load_file(argv[1], -1);
    console->LoadNotePad(argv[1]);
    console->redraw();
  }

  return Fl::run();
}


