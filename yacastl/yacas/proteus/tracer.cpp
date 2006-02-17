
#include <FL/Fl.H>
#include <FL/Fl_Select_Browser.H>
#include <FL/Fl_Double_Window.H>
#include <FL/Fl_Tabs.H>
#include <FL/Fl_Button.H>
#include <FL/Fl_Return_Button.H>
#include <FL/Fl_Tile.H>
#include <FL/Fl_Round_Button.H>
#include <FL/Fl_Multiline_Input.H>	// Fl_Multiline_Input header file
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <signal.h>
#include <stdlib.h>

void call_yacas(void);
void InterruptHandler(int errupt);


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
        last_selected = -1;
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
int switchToFile(char* newfile,int line);
void go_viewer_line(int line) {last_selected = line;select(line);}
char oldfile[200];
char wouldbefile[200];
int last_selected;
};



Fl_Tabs* mainTabs;
Fl_My_Browser *tracer;
Fl_My_Browser *fileViewer;
Fl_My_Browser *profiler;
Fl_Group* input;
Fl_Round_Button *locked;
int selected = 1;
void* lineptr = NULL;
char expression[200],scriptdir[200],tempdir[200],pre_eval[200];



int get_string(char*expression,char*scriptdir,char*tempdir,
              char*pre_eval)
{
printf("gs 1...\n");
    Fl_Window *window = new Fl_Window(320,145);
printf("gs 2...\n");

    Fl_Input e(60, 10, 250, 25, "Input:");
printf("gs 3...\n");
    e.value(expression);
printf("gs 4...\n");

    Fl_Input s(60, 35, 250, 25, "Scripts:");
printf("gs 5...\n");
    s.value(scriptdir);
printf("gs 6...\n");

    Fl_Input t(60, 60, 250, 25, "Temp dir:");
    t.value(tempdir);
printf("gs 7...\n");

    Fl_Input p(60, 85, 250, 25, "Pre-exec:");
    p.value(pre_eval);
printf("gs 8...\n");

    Fl_Button cancel(60, 110, 80, 25, "cancel");
    Fl_Return_Button ok(150, 110, 80, 25, "OK");
    window->hotspot(&cancel); // you must position modal windows
    window->end();
    window->set_modal();
    window->show();
    for (;;)
    {
        Fl::wait();
        Fl_Widget *o;
        while ((o = Fl::readqueue()))
        {
            if (o == &ok)
            {
                strncpy(expression,e.value(),199);
                expression[199] = '\0';
                strncpy(scriptdir,s.value(),199);
                scriptdir[199] = '\0';
                strncpy(tempdir, t.value(),199);
                tempdir[199] = '\0';
                strncpy(pre_eval, p.value(),199);
                pre_eval[199] = '\0';

                return 1;
            }
            else if (o == &cancel || o == window)
            {
                return 0;
            }
        }
    }
printf("gs 9...\n");
    delete window;
printf("gs 10...\n");
}


#include "editor.h"





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
    LineInfo *fexpand = NULL;
    char buf[256];

    if ((expandInfo >= 0) || (f != NULL && l > 0))
    {
        fexpand = (LineInfo *)malloc(sizeof(LineInfo));
        fexpand->toExpand = expandInfo;
        fexpand->expanded = 0;
        fexpand->file = f;
        fexpand->line = l;
    }
    if (expandInfo >= 0)
    {
        strcpy(buf,"@b");
        strncpy(&buf[2],text,250);
    }
    else
    {
        strncpy(&buf[0],text,250);
    }
    insert(line, buf, fexpand);
}

int Fl_My_Browser::load_in(int line, int fileNo)
{
    int added = 0;
    char cmd[8192];
    char buf[256];
    sprintf(buf,"%sexp.%d",tempdir,fileNo);
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

        int nrs=0;
//HIER
        fscanf(f,"%d ",&nrs);
        if (nrs>7192) nrs=7192;
        if (nrs) memset(cmd,' ',nrs);
        cmd[nrs] = '\0';
//HIER
        fgets(&cmd[nrs],8192-nrs,f);
        my_insert_line(line, info, cmd,file,l);
        line++;
        added++;
    }
    
    fclose(f);
    return added;
}

int Fl_My_Browser::switchToFile(char* newfile,int line)
{
    int changed=0;
    strcpy(wouldbefile,newfile);
    if (locked->value())
    {
        if (!strcmp(oldfile,newfile))
        {
            if (line != fileViewer->last_selected)
                changed = 1;
            fileViewer->go_viewer_line(line);
            fileViewer->make_visible(line);
        }
        return changed;
    }
    if (strcmp(oldfile,newfile))
    {
        changed=1;
        fileViewer->clear();
        fileViewer->load(newfile);
        strcpy(oldfile,newfile);
    }
    fileViewer->go_viewer_line(line);
    fileViewer->make_visible(line);
    return changed;
}

int CallSelect()
{
    Fl_My_Browser* oo = tracer;
    LineInfo *lineinfo =  (LineInfo *)oo->data(selected);
    int linechanged = 0;
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
                FILE*f;
                sprintf(buf,"%s",lineinfo->file);
                f = fopen(buf,"r");
                if (!f)
                {
                    sprintf(buf,"%s%s",scriptdir,lineinfo->file);
                    f = fopen(buf,"r");
                }
                if (f)
                {
                    fclose(f);
                    linechanged = fileViewer->switchToFile(buf, lineinfo->line);
                }

            }
            lineinfo->expanded = oo->load_in(selected+1, lineinfo->toExpand);
        }
    }
    return linechanged;
}

void b_cb(Fl_Widget* o, void* )
{
    int my = Fl::event_y();
    lineptr = tracer->my_find_item(my);
    selected = tracer->my_lineno(lineptr);
    CallSelect();
}


void DoStep(int gonewline,int stepover)
{
    int changed = 0;
REDO:
    if (lineptr == NULL)
    {
        lineptr = tracer->my_item_first();
        selected = 1;
    }

    if (lineptr == NULL)
        return;

    LineInfo *lineinfo =  (LineInfo *)tracer->data(selected);
    if (lineinfo != NULL)
    {
        if (stepover)
        {
            if (lineinfo->expanded)
            {
                tracer->collapse(selected);
            }
        }
        else
        {
            if (!lineinfo->expanded)
            {
                lineinfo->expanded = tracer->load_in(selected+1, lineinfo->toExpand);
            }
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
        changed = CallSelect();
    }
    if (gonewline!=0 && changed == 0 && lineptr != NULL)
        goto REDO;

//printf("%s %s",fileViewer->oldfile,fileViewer->wouldbefile);
    if (changed == 0 && locked->value() && lineptr != NULL && strcmp(fileViewer->oldfile,fileViewer->wouldbefile))
        goto REDO;

    if (lineptr != NULL)
    {
        lineinfo =  (LineInfo *)tracer->data(selected);
        if (lineinfo->expanded)
        {
            tracer->collapse(selected);
        }
    }
    else
    {
        gonewline = 0;
        stepover = 0;
        goto REDO;
    }
}
void stepcb(Fl_Widget *, void *)
{
    DoStep(0,0);
}

void multistepcb(Fl_Widget *, void *)
{
    DoStep(1,0);
}
void multistepovercb(Fl_Widget *, void *)
{
    DoStep(1,1);
}

void steptostopcb(Fl_Widget *, void *)
{
    
    const char* curlinestring = "";
    int selected =tracer->my_lineno(tracer->my_selection());
    while (!strstr(curlinestring,"STOPPED"))
    {
        LineInfo *lineinfo =  (LineInfo *)tracer->data(selected);
        if (lineinfo)
        {
            if (!lineinfo->expanded)
            {
                lineinfo->expanded = tracer->load_in(selected+1, lineinfo->toExpand);
            }
        }
        selected++;
        curlinestring = tracer->text(selected);
        printf("%s\n",curlinestring);
        //         getchar();
     }
    tracer->select(selected,1);
}




void NewCalculation()
{
    if (!get_string(expression,scriptdir,tempdir, pre_eval))
        return;
    {
        FILE* f = fopen("tracerrc","w");
        if (f)
        {
            fprintf(f,"%s\n",expression);
            fprintf(f,"%s\n",scriptdir);
            fprintf(f,"%s\n",tempdir);
            fprintf(f,"%s\n",pre_eval);
            fclose(f);
        }
    }
    {
        char buf[200];
        sprintf(buf,"mkdir %s",tempdir);
        system(buf);
    }

    call_yacas();
printf("1...\n");
    fileViewer->clear();
printf("2...\n");
    tracer->clear();
printf("3...\n");
    tracer->load_in(1,0);
    {
        char fn[200];
        sprintf(fn,"%sprofile.sortcounted",tempdir);
        profiler->load(fn);
    }
printf("4...\n");
    selected = 1;
printf("5...\n");
    lineptr = NULL;
printf("6...\n");

}

void newcb(Fl_Widget *, void *)
{
    NewCalculation();
}


int main(int argc, char **argv)
{
    signal(SIGINT, InterruptHandler);
    extern void
        Malloc_SetHooks( void *(*malloc_func)(size_t),
                         void *(*calloc_func)(size_t, size_t),
                         void *(*realloc_func)(void *, size_t),
                         void (*free_func)(void *) );

    Malloc_SetHooks( malloc,
                     calloc,
                     realloc,
                     free );

    expression[0] = '\0';
    strcpy(scriptdir,"/usr/local/share/yacas/");
    strcpy(tempdir,"/tmp/proteusdebugger/");
    pre_eval[0] = '\0';

    {
        FILE* f = fopen("tracerrc","r");
        if (f)
        {
            fgets(expression,199,f);
            expression[strlen(expression)-1] = '\0';
            fgets(scriptdir,199,f);
            scriptdir[strlen(scriptdir)-1] = '\0';
            fgets(tempdir,199,f);
            tempdir[strlen(tempdir)-1] = '\0';
            fgets(pre_eval,199,f);
            pre_eval[strlen(pre_eval)-1] = '\0';
            fclose(f);
        }
    }
    
    int i;
    int file=0;
//  if (!Fl::args(argc,argv,i)) Fl::fatal(Fl::help);
//  const char* fname = (argc>1 ) ? argv[1] : "exp.1";
  Fl_Window window(400,400,"Proteus Debugger");
  window.box(FL_NO_BOX); // because it is filled with browser
  window.resizable(window);
  
  mainTabs = new Fl_Tabs(0, 0, 400, 400);
  window.resizable(mainTabs);
  {
      Fl_Group *o = input = new Fl_Group(10, 20, 390, 380, "Trace");
//      tracer = new Fl_My_Browser(0,20,200,378,0);

      Fl_Tile *t = new Fl_Tile(10,20,390,380);

//      fileViewer = new Fl_My_Browser(200,20,200,378,0);
      fileViewer = new Fl_My_Browser(0,20,400,190,0);
      fileViewer->align(FL_ALIGN_CLIP);

      
      tracer = new Fl_My_Browser(0,210,400,160,0);
      tracer->callback(b_cb);
      //      tracer->load_in(1,0);
      
//      tracer->position(0);
      tracer->align(FL_ALIGN_CLIP);
      
//      Fl_Group::current()->resizable(t);
      t->end();

      
      Fl_Group *buts = new Fl_Group(0, 372, 400, 20);
      Fl_Button *b = new Fl_Button(20, 375, 40, 20, ">");

      b->callback(stepcb,0);
      b = new Fl_Button(60, 375, 40, 20, ">>");
      b->callback(multistepcb,0);
      b = new Fl_Button(100, 375, 40, 20, "\\/");
      b->callback(multistepovercb,0);
      b = new Fl_Button(140, 375, 40, 20, "*");
      b->callback(steptostopcb,0);

      locked = new Fl_Round_Button(190,375,100,20,"Lock file");

      b = new Fl_Button(350, 375, 40, 20, "New");
      b->callback(newcb,0);

      o->end();
      o->resizable(t);
      Fl_Group::current()->resizable(t);
//      tracer->resizable(t);
//      t->resizable(b);

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


  
  window.show(argc,argv);
//  NewCalculation();

  return Fl::run();
}


#include "yacas.h"
#include "debugclass.h"

CYacas* yacas = NULL;
void InterruptHandler(int errupt)
{
    if (yacas)
    {
        printf("^C pressed\n");
        (*yacas)()().iEvalDepth = (*yacas)()().iMaxEvalDepth+100;
    }
    else
    {
        exit(0);
    }
}

    void call_yacas(void)
    {
return;
        yacas = CYacas::NewL();
        char buf[200];
        sprintf(buf,"DefaultDirectory(\"%s\");",scriptdir);
        yacas->Evaluate(buf);
        yacas->Evaluate("Load(\"yacasinit.ys\");");
        yacas->Evaluate("ForEach(item,DefPackages())Use(item);");
        yacas->Evaluate("Load(\"tracer.ys\");");
        if (pre_eval[0])
        {
            yacas->Evaluate(pre_eval);
        }
        {
            char buf[300];
            sprintf(buf,"ToFile(\"trace.tmp\")TraceExp(%s);",expression);
            YacasDebuggerBase* prev = (*yacas)()().iDebugger;
            (*yacas)()().iDebugger = NEW ProteusDebugger(tempdir);
            yacas->Evaluate(buf);
            delete (*yacas)()().iDebugger;
            (*yacas)()().iDebugger = prev;
        }
        delete yacas;
        yacas=NULL;
    }
