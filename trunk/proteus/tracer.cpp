
#include <FL/Fl.H>
#include <FL/Fl_Select_Browser.H>
#include <FL/Fl_Double_Window.H>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>

struct LineInfo
{
    int toExpand;
    int expanded;
};

class Fl_My_Browser : public Fl_Select_Browser 
{
public:
    Fl_My_Browser(int x,int y,int w,int h,const char *l=0)
        : Fl_Select_Browser(x,y,w,h,l)
    {};

void *my_find_item(int my) {return find_item(my);};
int my_lineno(void*l) const {return lineno(l);};
void my_remove(int line);
void my_insert_line(int line, int expandInfo, const char* text);
void Fl_My_Browser::collapse(int line);
int load_in(int line, int fileNo);
};
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
        free(lineinfo);
        data(line, NULL);
    }
    remove(line);
}


void Fl_My_Browser::my_insert_line(int line, int expandInfo, const char* text)
{
    if (expandInfo >= 0)
    {
        LineInfo *fexpand = (LineInfo *)malloc(sizeof(LineInfo));
        fexpand->toExpand = expandInfo;
        fexpand->expanded = 0;
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
        int info;
        sscanf(cmd,"%d",&info);
        fgets(cmd,8192,f);
        my_insert_line(line, info, cmd);
        line++;
        added++;
    }
    
    fclose(f);
    return added;
}


void b_cb(Fl_Widget* o, void* )
{
    Fl_My_Browser* oo = (Fl_My_Browser*)o;
    int my = Fl::event_y();
    void* l = oo->my_find_item(my);
    int line = oo->my_lineno(l);
    LineInfo *lineinfo =  (LineInfo *)oo->data(line);

    if (lineinfo != NULL)
    {
        if (lineinfo->expanded)
        {
            oo->collapse(line);
        }
        else
        {
            lineinfo->expanded = oo->load_in(line+1, lineinfo->toExpand);
        }
    }
}


int main(int argc, char **argv) {
    int i;
    int file=0;
//  if (!Fl::args(argc,argv,i)) Fl::fatal(Fl::help);
//  const char* fname = (argc>1 ) ? argv[1] : "exp.1";
  Fl_Window window(400,400,"Proteus Tracer");
  window.box(FL_NO_BOX); // because it is filled with browser
  Fl_My_Browser browser(0,0,400,400,0);
//  if (argc>1)
//  {
//      sscanf(argv[1],"-f%d",file);
//  }
//  printf("Going to file exp.%d\n",file);
  
  browser.type(FL_SELECT_BROWSER);
  //browser.color(42);
  browser.callback(b_cb);
  // browser.scrollbar_right();
  //browser.has_scrollbar(Fl_Browser::BOTH_ALWAYS);
  LineInfo *fexpand = (LineInfo *)malloc(sizeof(LineInfo));
  fexpand->toExpand = 10;
  fexpand->expanded = 0;

  browser.load_in(1,0);

  browser.position(0);
  window.resizable(&browser);
  window.show(argc,argv);
  return Fl::run();
}


