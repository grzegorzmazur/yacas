
/*
 - Title with file currently loaded
 - status line
 - buttons for next/previous error.
 */

#include <FL/Fl.H>
#include <FL/Fl_Select_Browser.H>
#include <FL/Fl_Double_Window.H>
#include <FL/Fl_Button.H>
#include <FL/Fl_Input.H>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <FL/Fl_Box.H>

Fl_Window *w=NULL;
Fl_Select_Browser *showBrowser;
Fl_Select_Browser *selectBrowser;
Fl_Box *fileNameLabel;
Fl_Box *statusLabel;
char curfname[512];
char* theErrStr=NULL;
char* theFileName=NULL;

char* fullDir = NULL;
void select_cb(Fl_Widget* o, void*)
{
    /*
        printf("callback, selection = %d, event_clicks = %d\n",
           ((Fl_Browser*)o)->value(), Fl::event_clicks());
           printf("Text is: %s\n",selectBrowser->text(((Fl_Browser*)o)->value()));
           */
    int selectLine = ((Fl_Browser*)o)->value();
    const char* origTxt = selectBrowser->text(selectLine);
    const char* endStr = "(-1) : ";
    char* txt = (char*)malloc(strlen(origTxt)+1+strlen(endStr));
    strcpy(txt,origTxt);
    strcat(txt,endStr);
    char* ptr = txt;
    while (*ptr != '(')ptr++;
    *ptr = '\0';
    ptr++;
    char* num = ptr;
    while (*ptr != ')')ptr++;
    *ptr = '\0';
    ptr++;
    int errorLine = atoi(num);

    if (errorLine<0)
    {
//        printf("Not an error line\n");
        goto END;
    }

    curfname[0] = '\0';
    if (fullDir)
    {
        strcat(curfname,fullDir);
    }
    strcat(curfname,txt);

    
    if (!showBrowser->load(curfname))
    {
        printf("Can't load %s, %s\n", curfname, strerror(errno));
        goto END;
        return;
    }

    {
        while (*ptr != ':')ptr++;
        ptr++;
        char* erstr = ptr;
        txt[strlen(origTxt)] = '\0';
        if (theErrStr)
        {
            free(theErrStr);
        }
        theErrStr = strdup(erstr);
        statusLabel->label(theErrStr);
    }
    if (theFileName)
    {
        free(theFileName);
    }
    theFileName = strdup(txt);
    fileNameLabel->label(theFileName);
    w->redraw();
    showBrowser->position(errorLine);
    showBrowser->select(errorLine);
END:
    free(txt);
}


int main(int argc, char **argv)
{
    if (argc<2) return 0;
    const char* fname;
    fname = argv[1];

    if (argc>=2)
    {
        fullDir = argv[2];
    }

    Fl_Window window(400,400,"Error browser");
    w = &window;
    fileNameLabel = new Fl_Box(FL_NO_BOX,10,0,400,20,"");
    fileNameLabel->align(FL_ALIGN_CENTER);
    
    showBrowser = new Fl_Select_Browser(0,20,400,200-20,0);
    showBrowser->type(FL_HOLD_BROWSER);
    
    selectBrowser = new Fl_Select_Browser(0,200,400,200-20,0);
    //selectBrowser->type(FL_HOLD_BROWSER);
    selectBrowser->callback(select_cb);
    if (!selectBrowser->load(fname))
    {
        printf("Can't load %s, %s\n", fname, strerror(errno));
        exit(1);
    }

    statusLabel = new Fl_Box(FL_NO_BOX,10,400-20,400,20,"Status line");
//    statusLabel->align(FL_ALIGN_LEFT);

    selectBrowser->position(0);
    window.resizable(showBrowser);
    window.resizable(selectBrowser);

    window.show(1,argv);
    return Fl::run();
}

