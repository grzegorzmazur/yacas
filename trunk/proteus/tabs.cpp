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
#include "fl_adjustable_file_chooser.H"		// FLTK file chooser
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



LispString the_out;
StringOutput stdOutput(the_out);

Fl_Multiline_Input *text2;
Fl_Tabs* mainTabs;
Fl_Group *inputPage;

Fl_Select_Browser *chapter;
Fl_Select_Browser *paragraph;
Fl_Multiline_Input *helptext;
Fl_Group* grapher;

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
    newfile = fl_adjustable_file_chooser("Open File?", "*", "");
    if (newfile != NULL)
    {
        console->LoadNotePad(newfile);
        console->redraw();
    }
}


#include "proteusmenu.h"


void LispExit(LispEnvironment& aEnvironment, LispPtr& aResult,
              LispPtr& aArguments)
{
    exit(0);
//    InternalTrue(aEnvironment, aResult);
}

void LispShowInput(LispEnvironment& aEnvironment, LispPtr& aResult,
              LispPtr& aArguments)
{
    TESTARGS(2);
    LispPtr evaluated;
    InternalEval(aEnvironment, evaluated, Argument(aArguments,1));
    if (IsTrue(aEnvironment, evaluated))
    {
        console->ShowInput(1);
    }
    if (IsFalse(aEnvironment, evaluated))
    {
        console->ShowInput(0);
    }
    InternalTrue(aEnvironment,aResult);
}

void LispEnableInput(LispEnvironment& aEnvironment, LispPtr& aResult,
              LispPtr& aArguments)
{
    TESTARGS(2);
    LispPtr evaluated;
    InternalEval(aEnvironment, evaluated, Argument(aArguments,1));
    if (IsTrue(aEnvironment, evaluated))
    {
        console->EnableInput(1);
    }
    if (IsFalse(aEnvironment, evaluated))
    {
        console->EnableInput(0);
    }
    InternalTrue(aEnvironment,aResult);
}

void LispNotepad(LispEnvironment& aEnvironment, LispPtr& aResult,
              LispPtr& aArguments)
{
    TESTARGS(2);
    LispPtr evaluated;
    InternalEval(aEnvironment, evaluated, Argument(aArguments,1));

    CHK_ARG(evaluated.Get() != NULL, 1);
    LispStringPtr orig = evaluated.Get()->String();
    CHK_ARG(orig != NULL, 1);
    LispString oper;
    InternalUnstringify(oper, orig);
    console->LoadNotePad(&oper[0]);
    InternalTrue(aEnvironment,aResult);
}


char defdir[128];
void GetProteusConfiguration()
{
#ifdef _WINDOWS
    strcpy(defdir,".\\");
    return;
#else
    FILE*f = fopen("/etc/proteus.conf","r");
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
    (*yacas)()().Commands().SetAssociation(LispEvaluator(LispExit),
                                           (*yacas)()().HashTable().LookUp("Exit"));

    (*yacas)()().Commands().SetAssociation(LispEvaluator(LispShowInput),
                                           (*yacas)()().HashTable().LookUp("NoteShowInput"));
    (*yacas)()().Commands().SetAssociation(LispEvaluator(LispEnableInput),
                                           (*yacas)()().HashTable().LookUp("NoteEnableInput"));
    (*yacas)()().Commands().SetAssociation(LispEvaluator(LispNotepad),
                                           (*yacas)()().HashTable().LookUp("Notepad"));


    char cmd[128];
    sprintf(cmd,"DefaultDirectory(\"%s\");",defdir);
    yacas->Evaluate(cmd);

    AddGraphingCapabilities((*yacas)()());


    {
        extern char defdir[128];
        char buf[128];
#ifdef CALCULATOR
        sprintf(buf,"%sCalculatorBanner",defdir);
#endif
#ifdef WORKSHEET
        sprintf(buf,"%sWorksheetBanner",defdir);
#endif
        console->LoadNotePad(buf);
        console->handle_key(eEscape); // Highlight should be on bottom line
    }

    /*
     console->AddGroup(1,0);
#ifdef WORKSHEET
    console->AddText("Proteus Notepad\nTo exit Yacas, enter 'quit'.\nType ?? for help, or type ?function for help on a function.\nType 'restart' to restart Yacas.\nTo see example commands, keep typing Example();\n", FL_BLACK,"",FL_HELVETICA_BOLD,12);
#endif
#ifdef CALCULATOR
    console->AddText("Proteus Notepad", FL_BLACK,"",FL_HELVETICA,9);
    #endif
    */
    yacas->Evaluate("Load(\"yacasinit.ys\");");
}


void myexit()
{
    console->SaveHistory();
    printf("Quitting...\n");
    delete yacas;
    yacas = NULL;
}

void quit_cb(Fl_Widget *,void *)
{
    exit(0);
}


#ifdef WORKSHEET

int main(int argc, char **argv)
{
    GetProteusConfiguration();
    curoutlen=10;
    outbuf = (char*)malloc(curoutlen);

    Fl_Window* w;
    {
        Fl_Window* o = /* foo_window = */ new Fl_Window(640, 320);

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
				{
                console = new FltkConsole(11,43,618,265,12);
				}
                o->end();
                Fl_Group::current()->resizable(o);
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
#ifdef _WINDOWS
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
    w->show(argc, argv);
    atexit(myexit);
    return Fl::run();
}

#endif


#ifdef CALCULATOR


Fl_Button *num_zero_;
Fl_Button *num_one_;
Fl_Button *num_two_;
Fl_Button *num_three_;
Fl_Button *num_four_;
Fl_Button *num_five_;
Fl_Button *num_six_;
Fl_Button *num_seven_;
Fl_Button *num_eight_;
Fl_Button *num_nine_;
Fl_Button *num_point_;
Fl_Button *num_quote_;
Fl_Button *num_equal_;
Fl_Button *num_colon_;
Fl_Button *num_leftpar_;
Fl_Button *num_rightpar_;
Fl_Button *num_leftbrace_;
Fl_Button *num_rightbrace_;
Fl_Button *num_comma_;
Fl_Button *num_x_;
Fl_Button *num_y_;
Fl_Button *num_z_;
Fl_Button *num_a_;
Fl_Button *num_b_;
Fl_Button *num_c_;
Fl_Button *num_i_;
Fl_Button *num_j_;
Fl_Button *num_k_;
Fl_Button *num_f_;
Fl_Button *num_g_;
Fl_Button *num_t_;
Fl_Button *num_plus_;
Fl_Button *num_minus_;
Fl_Button *num_times_;
Fl_Button *num_divide_;
Fl_Button *num_power_;
Fl_Button *num_enter_;
Fl_Button *num_back_;
Fl_Button *num_space_;
Fl_Button *num_help_;

void cb_menu_backspace(Fl_Widget* o, void*)
{
    console->handle_key(eBackSpace);
    console->redraw();
}

void cb_menu_enter(Fl_Widget* o, void*)
{
    console->handle_key(eEnter);
    console->redraw();
}

int main(int argc, char **argv)
{

    GetProteusConfiguration();
    curoutlen=10;
    outbuf = (char*)malloc(curoutlen);

    Fl_Window* w;
    {
        Fl_Window* o = /* foo_window = */ new Fl_Window(160, 220);

        w = o;
        {
            mainTabs = new Fl_Tabs(0, 1, 160, 219);
            o->selection_color(15);

            {
                Fl_Group* o = input = new Fl_Group(2, 15, 158, 215, "Input");

				{
                	console = new FltkConsole(2,16,156,153,9);
				}
				{
					menubar = new Fl_Menu_Bar(2, 169, 156, 15);
					menubar->menu(menuitems);
					menubar->textfont(8);
					menubar->textsize(10);
				}
				{
					Fl_Button* o = num_seven_ = new Fl_Button(2,184,12,12,"7");
                	o->labelfont(1);
                	o->labelsize(9);
                	o->callback((Fl_Callback*)cb_menu_insert,(void*)"7");
				}
				{
					Fl_Button* o = num_four_ = new Fl_Button(2,196,12,12,"4");
                	o->labelfont(1);
                	o->labelsize(9);
                	o->callback((Fl_Callback*)cb_menu_insert,(void*)"4");
				}
				{
					Fl_Button* o = num_one_ = new Fl_Button(2,208,12,12,"1");
                	o->labelfont(1);
                	o->labelsize(9);
                	o->callback((Fl_Callback*)cb_menu_insert,(void*)"1");
				}
				{
					Fl_Button* o = num_eight_ = new Fl_Button(14,184,12,12,"8");
                	o->labelfont(1);
                	o->labelsize(9);
                	o->callback((Fl_Callback*)cb_menu_insert,(void*)"8");
				}
				{
					Fl_Button* o = num_five_ = new Fl_Button(14,196,12,12,"5");
                	o->labelfont(1);
                	o->labelsize(9);
                	o->callback((Fl_Callback*)cb_menu_insert,(void*)"5");
				}
				{
					Fl_Button* o = num_two_ = new Fl_Button(14,208,12,12,"2");
                	o->labelfont(1);
                	o->labelsize(9);
                	o->callback((Fl_Callback*)cb_menu_insert,(void*)"2");
				}
				{
					Fl_Button* o = num_nine_ = new Fl_Button(26,184,12,12,"9");
                	o->labelfont(1);
                	o->labelsize(9);
                	o->callback((Fl_Callback*)cb_menu_insert,(void*)"9");
				}
				{
					Fl_Button* o = num_six_ = new Fl_Button(26,196,12,12,"6");
                	o->labelfont(1);
                	o->labelsize(9);
                	o->callback((Fl_Callback*)cb_menu_insert,(void*)"6");
				}
				{
					Fl_Button* o = num_three_ = new Fl_Button(26,208,12,12,"3");
                	o->labelfont(1);
                	o->labelsize(9);
                	o->callback((Fl_Callback*)cb_menu_insert,(void*)"3");
				}
				{
					Fl_Button* o = num_plus_ = new Fl_Button(38,184,12,12,"+");
                	o->labelfont(1);
                	o->labelsize(10);
                	o->callback((Fl_Callback*)cb_menu_insert,(void*)"+");
				}
				{
					Fl_Button* o = num_minus_ = new Fl_Button(38,196,12,12,"-");
                	o->labelfont(1);
                	o->labelsize(10);
                	o->callback((Fl_Callback*)cb_menu_insert,(void*)"-");
				}
				{
					Fl_Button* o = num_zero_ = new Fl_Button(38,208,12,12,"0");
                	o->labelfont(1);
                	o->labelsize(9);
                	o->callback((Fl_Callback*)cb_menu_insert,(void*)"0");
				}
				{
					Fl_Button* o = num_times_ = new Fl_Button(50,184,12,12,"*");
                	o->labelfont(1);
                	o->labelsize(10);
                	o->callback((Fl_Callback*)cb_menu_insert,(void*)"*");
				}
				{
					Fl_Button* o = num_divide_ = new Fl_Button(50,196,12,12,"/");
                	o->labelfont(1);
                	o->labelsize(9);
                	o->callback((Fl_Callback*)cb_menu_insert,(void*)"/");
				}
				{
					Fl_Button* o = num_point_ = new Fl_Button(50,208,12,12,".");
                	o->labelfont(1);
                	o->labelsize(12);
                	o->callback((Fl_Callback*)cb_menu_insert,(void*)".");
				}
				{
					Fl_Button* o = num_power_ = new Fl_Button(62,184,12,12,"^");
                	o->labelfont(1);
                	o->labelsize(10);
                	o->callback((Fl_Callback*)cb_menu_insert,(void*)"^");
				}
				{
					Fl_Button* o = num_space_ = new Fl_Button(62,196,12,12," ");
                	o->labelfont(1);
                	o->labelsize(9);
                	o->callback((Fl_Callback*)cb_menu_insert,(void*)" ");
				}
				{
					Fl_Button* o = num_comma_ = new Fl_Button(62,208,12,12,",");
                	o->labelfont(1);
                	o->labelsize(12);
                	o->callback((Fl_Callback*)cb_menu_insert,(void*)",");
				}
				{
					Fl_Button* o = num_leftpar_ = new Fl_Button(74,184,12,12,"(");
                	o->labelfont(1);
                	o->labelsize(9);
                	o->callback((Fl_Callback*)cb_menu_insert,(void*)"(");
				}
				{
					Fl_Button* o = num_leftbrace_ = new Fl_Button(74,196,12,12,"{");
                	o->labelfont(1);
                	o->labelsize(9);
                	o->callback((Fl_Callback*)cb_menu_insert,(void*)"{");
				}
				{
					Fl_Button* o = num_colon_ = new Fl_Button(74,208,12,12,":");
                	o->labelfont(1);
                	o->labelsize(9);
                	o->callback((Fl_Callback*)cb_menu_insert,(void*)":");
				}
				{
					Fl_Button* o = num_rightpar_ = new Fl_Button(86,184,12,12,")");
                	o->labelfont(1);
                	o->labelsize(9);
                	o->callback((Fl_Callback*)cb_menu_insert,(void*)")");
				}
				{
					Fl_Button* o = num_rightbrace_ = new Fl_Button(86,196,12,12,"}");
                	o->labelfont(1);
                	o->labelsize(9);
                	o->callback((Fl_Callback*)cb_menu_insert,(void*)"}");
				}
				{
					Fl_Button* o = num_equal_ = new Fl_Button(86,208,12,12,"=");
                	o->labelfont(1);
                	o->labelsize(9);
                	o->callback((Fl_Callback*)cb_menu_insert,(void*)"=");
				}
				{
					Fl_Button* o = num_x_ = new Fl_Button(98,184,12,12,"x");
                	o->labelfont(1);
                	o->labelsize(9);
                	o->callback((Fl_Callback*)cb_menu_insert,(void*)"x");
				}
				{
					Fl_Button* o = num_a_ = new Fl_Button(98,196,12,12,"a");
                	o->labelfont(1);
                	o->labelsize(9);
                	o->callback((Fl_Callback*)cb_menu_insert,(void*)"a");
				}
				{
					Fl_Button* o = num_i_ = new Fl_Button(98,208,12,12,"i");
                	o->labelfont(1);
                	o->labelsize(9);
                	o->callback((Fl_Callback*)cb_menu_insert,(void*)"i");
				}
				{
					Fl_Button* o = num_y_ = new Fl_Button(110,184,12,12,"y");
                	o->labelfont(1);
                	o->labelsize(9);
                	o->callback((Fl_Callback*)cb_menu_insert,(void*)"y");
				}
				{
					Fl_Button* o = num_b_ = new Fl_Button(110,196,12,12,"b");
                	o->labelfont(1);
                	o->labelsize(9);
                	o->callback((Fl_Callback*)cb_menu_insert,(void*)"b");
				}
				{
					Fl_Button* o = num_j_ = new Fl_Button(110,208,12,12,"j");
                	o->labelfont(1);
                	o->labelsize(9);
                	o->callback((Fl_Callback*)cb_menu_insert,(void*)"j");
				}
				{
					Fl_Button* o = num_z_ = new Fl_Button(122,184,12,12,"z");
                	o->labelfont(1);
                	o->labelsize(9);
                	o->callback((Fl_Callback*)cb_menu_insert,(void*)"z");
				}
				{
					Fl_Button* o = num_c_ = new Fl_Button(122,196,12,12,"c");
                	o->labelfont(1);
                	o->labelsize(9);
                	o->callback((Fl_Callback*)cb_menu_insert,(void*)"c");
				}
				{
					Fl_Button* o = num_k_ = new Fl_Button(122,208,12,12,"k");
                	o->labelfont(1);
                	o->labelsize(9);
                	o->callback((Fl_Callback*)cb_menu_insert,(void*)"k");
				}
				{
					Fl_Button* o = num_f_ = new Fl_Button(134,184,12,12,"f");
                	o->labelfont(1);
                	o->labelsize(9);
                	o->callback((Fl_Callback*)cb_menu_insert,(void*)"f");
				}
				{
					Fl_Button* o = num_g_ = new Fl_Button(134,196,12,12,"%");
                	o->labelfont(1);
                	o->labelsize(9);
                	o->callback((Fl_Callback*)cb_menu_insert,(void*)"%");
				}
				{
					Fl_Button* o = num_t_ = new Fl_Button(134,208,12,12,"t");
                	o->labelfont(1);
                	o->labelsize(9);
                	o->callback((Fl_Callback*)cb_menu_insert,(void*)"t");
				}
				{
					Fl_Button* o = num_enter_ = new Fl_Button(146,184,12,12,"@#<-");
                	o->labeltype(FL_SYMBOL_LABEL);
                	o->labelsize(9);
                	o->callback((Fl_Callback*)cb_menu_backspace);
				}
				{
					Fl_Button* o = num_help_ = new Fl_Button(146,196,12,12,"?");
                	o->labelfont(1);
                	o->labelsize(9);
                	o->callback((Fl_Callback*)cb_menu_insert,(void*)"?");
				}
				{
					Fl_Button* o = num_back_ = new Fl_Button(146,208,12,12,"@returnarrow");
                	o->labeltype(FL_SYMBOL_LABEL);
                	o->labelsize(9);
                	o->callback((Fl_Callback*)cb_menu_enter);
				}

                o->labelsize(10);
                o->end();
                Fl_Group::current()->resizable(o);
            }

            {
                Fl_Group* o = grapher = new Fl_Group(2, 15, 156, 215, "Graph");
                drawing = new Drawer(2,16,156,203);
      		    o->labelsize(10);
                o->end();
            }

            {
                Fl_Group* o = new Fl_Group(2, 15, 156, 215, "Edit");
                editor_add_items(2,16,156,203,9);
				o->labelsize(10);
                o->end();
            }

            {
                Fl_Group* o = new Fl_Group(2, 15, 156, 215, "?");


                {
                    HelpView* o = helpview_ = new HelpView(2, 16, 156, 181);
                    helpview_->textsize(8);
                    o->box(FL_DOWN_BOX);
                    o->callback((Fl_Callback*)cb_helpview_);
                    o->end();
                    Fl_Group::current()->resizable(o);
                }
                {
                    Fl_Button* o = back_ = new Fl_Button(80, 198, 15, 15, "@<");
                    o->shortcut(0xff51);
                    o->labeltype(FL_SYMBOL_LABEL);
                    o->callback((Fl_Callback*)cb_back_);
                }
                {
                    Fl_Button* o = forward_ = new Fl_Button(100, 198, 15, 15, "@>");
                    o->shortcut(0xff53);
                    o->labeltype(FL_SYMBOL_LABEL);
                    o->callback((Fl_Callback*)cb_forward_);
                }
                {
                    Fl_Button* o = smaller_ = new Fl_Button(40, 198, 15, 15, "F");
                    o->labelfont(1);
                    o->labelsize(5);
                    o->callback((Fl_Callback*)cb_smaller_);
                }
                {
                    Fl_Button* o = larger_ = new Fl_Button(60, 198, 15, 15, "F");
                    o->labelfont(1);
                    o->labelsize(9);
                    o->callback((Fl_Callback*)cb_larger_);
                } 

                {
                    char helpfile[128];
                    sprintf(helpfile,"%sdocumentation/%s",defdir,YACAS_DOC);
                    helpview_->load(helpfile);
                }
                o->labelsize(10);
                o->end();

            }
			
            init_editor();

            mainTabs->end();
            Fl_Group::current()->resizable(o);

        }
        {
            Fl_Button* o = new Fl_Button(148,0,12,12,"X");
            o->labelfont(1);
            o->labelsize(9);
            o->callback((Fl_Callback*)exit);
        }  
        Fl_Group::current()->resizable(o);
        o->end();
    }

    RestartYacas();
	w->callback(quit_cb);
    w->show(argc, argv);
    atexit(myexit);
    return Fl::run();
}

#endif
