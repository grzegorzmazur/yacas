
#include <FL/Fl.H>
#include <FL/Fl_Select_Browser.H>
#include <FL/Fl_Double_Window.H>
#include <FL/Fl_Tabs.H>
#include <FL/Fl_Button.H>
#include <FL/Fl_Return_Button.H>
#include <FL/Fl_Tile.H>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include "editor.h"
#include "yacas.h"

struct LineInfo
{
    int toExpand;
    int expanded;
    char* file;
    int line;
};

class Fl_My_Browser : public Fl_Select_Browser 
{
public:
    Fl_My_Browser(int x,int y,int w,int h,const char *l=0)
        : Fl_Select_Browser(x,y,w,h,l)
    {
        type(FL_HOLD_BROWSER);
        strcpy(oldfile,"");
    };

void my_item_select(void *l) {item_select(l,1);};

void* my_selection() {return selection();}
void* my_item_first() {return item_first();}
void* my_item_next(void*i) {return item_next(i);}
void *my_find_item(int my) {return find_item(my);};
int my_lineno(void*l) const {return lineno(l);};
void my_remove(int line);
void my_insert_line(int line, int expandInfo, const char* text,
                   char*f,int l);
void Fl_My_Browser::collapse(int line);
int load_in(int line, int fileNo);
void switchToFile(char* newfile,int line);
char oldfile[200];
};

Fl_Tabs* mainTabs;
Fl_My_Browser *tracer;
Fl_My_Browser *fileViewer;
Fl_My_Browser *profiler;
Fl_Group* input;
int selected = 1;
void* lineptr = NULL;
void Fl_My_Browser::collapse(int line)
{
    LineInfo *lineinfo =  (LineInfo *)data(line);
    while (lineinfo->expanded)
    {
        my_remove(line+1);
        lineinfo->expanded--;
    }
}

void Fl_My_Browser::my_remove(int line)
{
    LineInfo *lineinfo =  (LineInfo *)data(line);
    if (lineinfo)
    {
        collapse(line);
        if (lineinfo->file)
            free(lineinfo->file);
        free(lineinfo);
        data(line, NULL);
    }
    remove(line);
}


void Fl_My_Browser::my_insert_line(int line, int expandInfo, const char* text,
                                  char*f, int l)
{
    if (expandInfo >= 0)
    {
        LineInfo *fexpand = (LineInfo *)malloc(sizeof(LineInfo));
        fexpand->toExpand = expandInfo;
        fexpand->expanded = 0;
        fexpand->file = f;
        fexpand->line = l;
        char buf[256];
        strcpy(buf,"@b");
        strncpy(&buf[2],text,250);
        insert(line, buf, fexpand);
    }
    else
    {
        insert(line, text, NULL);
    }
}

int Fl_My_Browser::load_in(int line, int fileNo)
{
    int added = 0;
    char cmd[8192];
    char buf[256];
    sprintf(buf,"exp.%d",fileNo);
    FILE* f=fopen(buf,"r");
    if (!f) return 0;
    for(;;)
    {
        fgets(cmd,8192,f);
        if (!strncmp(cmd,"end",3)) break;
        int info,l;
        char *file = NULL;
        sscanf(cmd,"%d%d",&info,&l);
        if (l == 0)
        {
        }
        else
        {
            char fn[200];
            fgets(cmd,8192,f);
            sscanf(cmd,"%s",fn);
            file = strdup(fn);
        }
        fgets(cmd,8192,f);
        my_insert_line(line, info, cmd,file,l);
        line++;
        added++;
    }
    
    fclose(f);
    return added;
}

void Fl_My_Browser::switchToFile(char* newfile,int line)
{
    if (strcmp(oldfile,newfile))
    {
        fileViewer->clear();
        fileViewer->load(newfile);
        strcpy(oldfile,newfile);
    }
    fileViewer->select(line);
    fileViewer->make_visible(line);
}

void CallSelect()
{
    Fl_My_Browser* oo = tracer;
    LineInfo *lineinfo =  (LineInfo *)oo->data(selected);

    if (lineinfo != NULL)
    {
        if (lineinfo->expanded)
        {
            oo->collapse(selected);
        }
        else
        {
            if (lineinfo->file)
            {
                char buf[200];
                sprintf(buf,"/usr/local/share/yacas/%s",lineinfo->file);
                FILE*f = fopen(buf,"r");
                if (f)
                {
                    fclose(f);
                    fileViewer->switchToFile(buf, lineinfo->line);
                }

            }
            lineinfo->expanded = oo->load_in(selected+1, lineinfo->toExpand);
        }
    }
}

void b_cb(Fl_Widget* o, void* )
{
    int my = Fl::event_y();
    lineptr = tracer->my_find_item(my);
    selected = tracer->my_lineno(lineptr);
    CallSelect();
}

void stepcb(Fl_Widget *, void *)
{
    if (lineptr == NULL)
    {
        lineptr = tracer->my_item_first();
        selected = 1;
    }
    LineInfo *lineinfo =  (LineInfo *)tracer->data(selected);
    if (lineinfo != NULL)
    {
        if (!lineinfo->expanded)
        {
            lineinfo->expanded = tracer->load_in(selected+1, lineinfo->toExpand);
        }
    }
    lineinfo = NULL;
    while (lineinfo == NULL && lineptr != NULL)
    {
        selected++;
        lineptr = tracer->my_item_next(lineptr);
        lineinfo =  (LineInfo *)tracer->data(selected);
    }
    if (lineptr != NULL)
    {
        tracer->select(selected,1);
        CallSelect();
    }

}

int get_string(char*buffer) {
  Fl_Window window(320,75);
  Fl_Input input(60, 10, 250, 25, "Input:");
  input.value(buffer);
  Fl_Button cancel(60, 40, 80, 25, "cancel");
  Fl_Return_Button ok(150, 40, 80, 25, "OK");
  window.hotspot(&cancel); // you must position modal windows
  window.end();
  window.set_modal();
  window.show();
  for (;;) {
    Fl::wait();
    Fl_Widget *o;
    while ((o = Fl::readqueue())) {
      if (o == &ok) {
	strcpy(buffer,input.value());
	return 1;
      } else if (o == &cancel || o == &window) {
	return 0;
      }
    }
  }
}



int main(int argc, char **argv)
{
    char expression[200];
    get_string(expression);
    {
        CYacas *yacas = CYacas::NewL();
        yacas->Evaluate("DefaultDirectory(\"/usr/local/share/yacas/\");");
        yacas->Evaluate("Load(\"yacasinit.ys\");");
        yacas->Evaluate("ForEach(item,DefPackages)Use(item);");
        yacas->Evaluate("Load(\"tracer.ys\");");
        {
            char buf[300];
            sprintf(buf,"NiceTraceExp(%s)",expression);
            yacas->Evaluate(buf);
        }
        delete yacas;
    }


    int i;
    int file=0;
//  if (!Fl::args(argc,argv,i)) Fl::fatal(Fl::help);
//  const char* fname = (argc>1 ) ? argv[1] : "exp.1";
  Fl_Window window(400,400,"Proteus Debugger");
  window.box(FL_NO_BOX); // because it is filled with browser

  selected = 1;
  lineptr = NULL;
  
  mainTabs = new Fl_Tabs(0, 0, 400, 400);
  window.resizable(mainTabs);
  {
      Fl_Group *o = input = new Fl_Group(10, 20, 390, 390, "Trace");
//      tracer = new Fl_My_Browser(0,20,200,378,0);

      Fl_Tile *t = new Fl_Tile(10,20,390,390);

      tracer = new Fl_My_Browser(0,210,400,160,0);
      tracer->callback(b_cb);
      tracer->load_in(1,0);
      tracer->position(0);
      tracer->align(FL_ALIGN_CLIP);
//      fileViewer = new Fl_My_Browser(200,20,200,378,0);
      fileViewer = new Fl_My_Browser(0,20,400,190,0);
      fileViewer->align(FL_ALIGN_CLIP);
      t->end();

      Fl_Button *b = new Fl_Button(20, 375, 80, 20, "Step");
      b->callback(stepcb,0);

      
      o->end();
      Fl_Group::current()->resizable(o);
  }
  {
      Fl_Group *o = new Fl_Group(10, 20, 390, 380, "Profile");
      profiler = new Fl_My_Browser(0,20,400,378,0);
      o->end();
      Fl_Group::current()->resizable(o);
  }
  {
      Fl_Group *o = new Fl_Group(10, 20, 400, 380, "Edit");
      editor_add_items(10,20,390, 380);
      o->end();
      Fl_Group::current()->resizable(o);
  }
  {
      Fl_Group *o = new Fl_Group(10, 20, 400, 380, "Help");
      o->end();
      Fl_Group::current()->resizable(o);
  }
  
  /*
   browser.type(FL_HOLD_BROWSER);
  //browser.color(42);
  browser.callback(b_cb);
  // browser.scrollbar_right();
  //browser.has_scrollbar(Fl_Browser::BOTH_ALWAYS);

  browser.load_in(1,0);

  browser.position(0);
  window.resizable(&browser);
  */
  window.show(argc,argv);
  return Fl::run();
}


