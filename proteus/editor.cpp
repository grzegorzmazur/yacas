//
// "$Id: editor.cpp,v 1.1 2001-02-22 19:19:55 ayalpinkus Exp $"
//
// A simple text editor program for the Fast Light Tool Kit (FLTK).
//
// This program is described in Chapter 4 of the FLTK Programmer's Guide.
//
// Copyright 1998-1999 by Bill Spitzak and others.
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Library General Public
// License as published by the Free Software Foundation; either
// version 2 of the License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Library General Public License for more details.
//
// You should have received a copy of the GNU Library General Public
// License along with this library; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA.
//
// Please report all bugs and problems to "fltk-bugs@easysw.com".
//

//
// Include necessary headers...
//

#include <stdio.h>			// Standard library files
#include <stdlib.h>
#include <string.h>

#include <FL/Fl.H>			// Main FLTK header file
#include <FL/Fl_Group.H>		// Fl_Group header file
#include <FL/Fl_Window.H>		// Fl_Window header file
#include <FL/fl_ask.H>			// FLTK convenience functions
#include "fl_adjustable_file_chooser.H"		// FLTK file chooser
//#include <FL/fl_file_chooser.H>		// FLTK file chooser
#include <FL/Fl_Menu_Bar.H>		// Fl_Menu_Bar header file
#include <FL/Fl_Input.H>		// Fl_Input header file
#include <FL/Fl_Multiline_Input.H>	// Fl_Multiline_Input header file
#include <FL/Fl_Button.H>		// Fl_Button header file
#include <FL/Fl_Return_Button.H>	// Fl_Return_Button header file
#include <FL/Fl_Tabs.H>
#include <FL/fl_draw.H>
#include <FL/Fl.H>


#define MAX_INPUTS 10



class ProteusInput : public Fl_Multiline_Input
{
public:
    FL_EXPORT ProteusInput(int x, int y, int w, int h)
        : Fl_Multiline_Input(x,y,w,h)
    {};
    virtual FL_EXPORT int handle(int event);
    FL_EXPORT void draw();
};
class Fl_MyLabel : public Fl_Widget
{
public:
    FL_EXPORT Fl_MyLabel(int x, int y, int w, int h)
        : Fl_Widget(x,y,w,h) {};

    FL_EXPORT void draw();
    virtual FL_EXPORT int handle(int event);
};

void editor_add_editor();
void redo_title();
ProteusInput *EditorInput();
void go_next_input();


int Fl_MyLabel::handle(int event)
{
    if (event == FL_PUSH)
    {
        go_next_input();
        return 1;
    }
    return 0;
}



static Fl_Menu_Bar        *menubar;

struct EditDocument
{
    ProteusInput       *minput;
    int                mchanged;
    char               mfilename[128];
    int position;
    int mark;
};

EditDocument doc[MAX_INPUTS];
static int current_input = 0;
static int nr_inputs     = 0;

static Fl_Window          *replace_dlg;
static Fl_Input           *replace_find;
static Fl_Input           *replace_with;
static Fl_Button          *replace_all;
static Fl_Return_Button   *replace_next;
static Fl_Button          *replace_cancel;
Fl_MyLabel* labelizer=NULL;


EditDocument* Document()
{
    return &doc[current_input];
}

void make_current_visible()
{
    int i;
    for (i=0;i<MAX_INPUTS;i++)
    {
        if (i == current_input)
            doc[i].minput->show();
        else
            doc[i].minput->hide();
    }
    EditorInput()->take_focus();
    EditorInput()->position(Document()->position);
    EditorInput()->mark(Document()->mark);

    redo_title();
}

void go_next_input()
{
    if (nr_inputs==1)
        return;

    Document()->position = EditorInput()->position();
    Document()->mark = EditorInput()->mark();

    current_input++;
    if (current_input == nr_inputs)
        current_input = 0;
    make_current_visible();
}


int ProteusInput::handle(int event)
{
//printf("input %d, event %d\n",current_input,event);
    switch (event)
    {
    case FL_KEYBOARD :
        {
            if (Fl::event_key() == FL_Tab && (Fl::event_state() & FL_CTRL))
            {
                go_next_input();
                return 1;
            }
            else if (Fl::event_key() == FL_Escape)
            {
                extern Fl_Tabs* mainTabs;
                extern Fl_Group* input;
                mainTabs->value(input);
                return 1;
            }
            else if (Fl::event_key() == FL_Page_Up)
            {
                int nr = (h()/fl_height())>>1;
                int j;
                for (j=0;j<nr;j++)
                {
                    int i;
                    if (type()!=FL_MULTILINE_INPUT) return 0;
                    for (i = position(); i > 0 && index(i-1) != '\n'; i--) ;
                    if (!i) return 0;
                    up_down_position(i-1, Fl::event_state(FL_SHIFT));
                }
                return 1;
            }
            else if (Fl::event_key() == FL_Page_Down)
            {
                int nr = (h()/fl_height())>>1;
                int j;
                for (j=0;j<nr;j++)
                {
                    if (type()!=FL_MULTILINE_INPUT) return 0;
                    int i;
                    for (i=position(); index(i)!='\n'; i++)
                        if (!index(i)) return 0;
                    up_down_position(i+1, Fl::event_state(FL_SHIFT));
                }
                return 1;
            }
            return Fl_Multiline_Input::handle(event);
        }
        break;
    default:
        return Fl_Multiline_Input::handle(event);
    }
}

ProteusInput *EditorInput();
void ProteusInput::draw()
{
    if (this == EditorInput())
        Fl_Multiline_Input::draw();
}


void Fl_MyLabel::draw()
{
    fl_color(FL_LIGHT1);
    fl_rectf(x(),y(),w(),h());
    fl_color(FL_BLACK);
    draw_label();
}


static char               search[256] = "";

    


void set_changed(int);
void save_cb(void);
void saveas_cb(void);
void find2_cb(void);
void editor_close_cb(void);

ProteusInput *EditorInput()
{
    return Document()->minput;
}

static int *Changed()
{
    return &Document()->mchanged;
}

static char *FileName()
{
    return Document()->mfilename;
}


void redo_title()
{
    static char title[1024];
    char *slash;
    if (FileName()[0] == '\0')
        strcpy(title, "Untitled");
    else
    {
        slash = strrchr(FileName(), '/');
        if (slash == NULL) slash = strrchr(FileName(), '\\');

        if (slash != NULL) strcpy(title, slash + 1);
        else strcpy(title, FileName());
    }

    if (*Changed()) strcat(title, " (modified)");
    if (labelizer)
    {
        labelizer->label(title);
        labelizer->redraw();
    }
}

int check_save(void)
{
    if (!(*Changed())) return 1;

    if (fl_ask("The current file has not been saved.\n"
               "Would you like to save it now?")) {
        // Save the file...
        save_cb();

        return 1;
    }
    else
        return 0;
}

void load_file(char *newfile)
{
  FILE *fp;
  char buffer[8192];
  int  nbytes;
  int  pos;

  EditorInput()->value("");
  
  fp = fopen(newfile, "r");
  if (fp != NULL) {
    // Was able to open file; let's read from it...
    strcpy(FileName(), newfile);
    pos = 0;

//TODO remove    int col=2;
    while ((nbytes = fread(buffer, 1, sizeof(buffer), fp)) > 0)
    {
        EditorInput()->replace(pos, pos, buffer, nbytes);
        pos += nbytes;
    }

    fclose(fp);
    EditorInput()->position(0);
    set_changed(0);
  } else {
    // Couldn't open file - say so...
    fl_alert("Unable to open \'%s\' for reading!");
  }
}

void save_file(char *newfile) {
  FILE *fp;

  fp = fopen(newfile, "w");
  if (fp != NULL) {
    // Was able to create file; let's write to it...
    strcpy(FileName(), newfile);

    if (fwrite(EditorInput()->value(), 1, EditorInput()->size(), fp) < 1) {
      fl_alert("Unable to write file!");
      fclose(fp);
      return;
    }

    fclose(fp);
    *Changed() = 1;
    set_changed(0);
  } else {
    // Couldn't open file - say so...
    fl_alert("Unable to create \'%s\' for writing!");
  }
}

void set_changed(int c)
{
    if (c != *Changed())
    {
        *Changed() = c;

        redo_title();
    }
}

void changed_cb(void) {
  set_changed(1);
}

void copy_cb(void) {
  EditorInput()->copy();
}

void cut_cb(void) {
  EditorInput()->copy();
  EditorInput()->cut();
}

void delete_cb(void) {
  EditorInput()->cut();
}

void find_cb(void) {
  const char *val;

  val = fl_input("Search String:", search);
  if (val != NULL) {
    // User entered a string - go find it!
    strcpy(search, val);
    find2_cb();
  }
}

void find2_cb(void) {
  const char *val, *found;
  int pos;

  if (search[0] == '\0') {
    // Search string is blank; get a new one...
    find_cb();
    return;
  }

  val   = EditorInput()->value() + EditorInput()->mark();
  found = strstr(val, search);

  if (found != NULL) {
    // Found a match; update the position and mark...
    pos = EditorInput()->mark() + found - val;
    EditorInput()->position(pos, pos + strlen(search));
  }
  else fl_alert("No occurrences of \'%s\' found!", search);
}

void new_cb(void)
{
    editor_add_editor();

    if (*Changed())
        if (!check_save()) return;

    FileName()[0] = '\0';
    EditorInput()->value("");
    set_changed(0);
    make_current_visible();
}

void open_cb(void)
{
    char *newfile;

    if (nr_inputs == MAX_INPUTS)
        if (*Changed())
            check_save();

    newfile = fl_adjustable_file_chooser("Open File?", "*", FileName());
    if (newfile != NULL)
    {
        if (FileName()[0] == '\0' && *Changed() == 0) {}
        else
            editor_add_editor();

        load_file(newfile);
        make_current_visible();
    }
}

void paste_cb(void) {
  Fl::paste(*EditorInput());
}

void editor_quit_cb(void)
{
  if (*Changed())
    if (!check_save())
      return;

//TODO  window->hide();
}

void replace_cb(void) {
  replace_dlg->show();
}

void replace2_cb() {
  const char *find, *val, *found;
  int pos;

  find = replace_find->value();
  if (find[0] == '\0') {
    // Search string is blank; get a new one...
    replace_dlg->show();
    return;
  }

  replace_dlg->hide();

  val   = EditorInput()->value() + EditorInput()->position();
  found = strstr(val, find);

  if (found != NULL) {
    // Found a match; update the position and replace text...
    pos = EditorInput()->position() + found - val;
    EditorInput()->replace(pos, pos + strlen(find), replace_with->value());
    EditorInput()->position(pos + strlen(replace_with->value()));
  }
  else fl_alert("No occurrences of \'%s\' found!", find);
}

void replall_cb() {
  const char *find, *val, *found;
  int pos;
  int times;

  find = replace_find->value();
  if (find[0] == '\0') {
    // Search string is blank; get a new one...
    replace_dlg->show();
    return;
  }

  replace_dlg->hide();

  EditorInput()->position(0);
  times = 0;

  // Loop through the whole string
  do {
    val   = EditorInput()->value() + EditorInput()->position();
    found = strstr(val, find);

    if (found != NULL) {
      // Found a match; update the position and replace text...
      times ++;
      pos = EditorInput()->position() + found - val;
      EditorInput()->replace(pos, pos + strlen(find), replace_with->value());
      EditorInput()->position(pos + strlen(replace_with->value()));
    }
  } while (found != NULL);

  if (times > 0) fl_message("Replaced %d occurrences.", times);
  else fl_alert("No occurrences of \'%s\' found!", find);
}

void replcan_cb()
{
    replace_dlg->hide();
}

void save_cb(void)
{
    if (FileName()[0] == '\0')
    {
        // No filename - get one!
        saveas_cb();
        return;
    }
    else
        save_file(FileName());
}
void editor_close_cb(void)
{
    if (*Changed())
        check_save();

    EditDocument old = *Document();
    int i;
    for (i=current_input;i<nr_inputs-1;i++)
    {
        doc[i] = doc[i+1];

    }
    old.minput->value("");
    strcpy(old.mfilename,"");
    old.mchanged=0;
    doc[nr_inputs-1] = old;
    if (nr_inputs>1)
    {
        nr_inputs--;
    }
    if (current_input>=nr_inputs)
        current_input=nr_inputs-1;
    make_current_visible();
}



void saveas_cb(void) {
  char *newfile;

  newfile = fl_adjustable_file_chooser("Save File As?", "*", FileName());
  if (newfile != NULL) save_file(newfile);
}

void undo_cb(void) {
  EditorInput()->undo();
}

static Fl_Menu_Item menuitems[] = {
  { "&File", 0, 0, 0, FL_SUBMENU },
    { "&New",        FL_ALT + 'n', (Fl_Callback *)new_cb },
    { "&Open...",    FL_ALT + 'o', (Fl_Callback *)open_cb, 0, FL_MENU_DIVIDER },
    { "&Save",       FL_ALT + 's', (Fl_Callback *)save_cb },
    { "Save &As...", FL_ALT + FL_SHIFT + 's', (Fl_Callback *)saveas_cb, 0, FL_MENU_DIVIDER },
    { "C&lose", FL_ALT + 'l', (Fl_Callback *)editor_close_cb },
    { "&Quit", FL_ALT + 'q', (Fl_Callback *)editor_quit_cb },
    { 0 },

  { "&Edit", 0, 0, 0, FL_SUBMENU },
    { "&Undo",       FL_ALT + 'z', (Fl_Callback *)undo_cb, 0, FL_MENU_DIVIDER },
    { "Cu&t",        FL_ALT + 'x', (Fl_Callback *)cut_cb },
    { "&Copy",       FL_ALT + 'c', (Fl_Callback *)copy_cb },
    { "&Paste",      FL_ALT + 'v', (Fl_Callback *)paste_cb },
    { "&Delete",     0, (Fl_Callback *)delete_cb },
    { 0 },

  { "&Search", 0, 0, 0, FL_SUBMENU },
    { "&Find...",       FL_ALT + 'f', (Fl_Callback *)find_cb },
    { "F&ind Again",    FL_ALT + 'g', (Fl_Callback *)find2_cb },
    { "&Replace...",    FL_ALT + 'r', (Fl_Callback *)replace_cb },
    { "Re&place Again", FL_ALT + 't', (Fl_Callback *)replace2_cb },
    { 0 },

  { 0 }
};

void init_editor()
{
  replace_dlg = new Fl_Window(300, 105, "Replace");
    replace_find = new Fl_Input(70, 10, 210, 25, "Find:");
    replace_find->align(FL_ALIGN_LEFT);

    replace_with = new Fl_Input(70, 40, 210, 25, "Replace:");
    replace_with->align(FL_ALIGN_LEFT);

    replace_all = new Fl_Button(10, 70, 90, 25, "Replace All");
    replace_all->callback((Fl_Callback *)replall_cb);

    replace_next = new Fl_Return_Button(105, 70, 120, 25, "Replace Next");
    replace_next->callback((Fl_Callback *)replace2_cb);

    replace_cancel = new Fl_Button(230, 70, 60, 25, "Cancel");
    replace_cancel->callback((Fl_Callback *)replcan_cb);
  replace_dlg->end();
  replace_dlg->set_modal();
}

int    the_minx;
int    the_miny;
int    the_width;
int    the_height;
int    the_fontSize;

void editor_add_editor()
{
    if (nr_inputs < MAX_INPUTS)
    {
        current_input = nr_inputs;
        nr_inputs++;
        *Changed() = 0;
        strcpy(FileName(),"");
    }

    EditorInput()->callback((Fl_Callback *)changed_cb);
    EditorInput()->when(FL_WHEN_CHANGED);
    EditorInput()->textfont(FL_COURIER);
    EditorInput()->textsize(the_fontSize);
    Document()->position = EditorInput()->position();
    Document()->mark = EditorInput()->mark();
}

void editor_add_items(int minx,int miny,int width, int height, int fontSize)
{
    the_minx = minx;
    the_miny = miny;
    the_width = width;
    the_height = height;
    the_fontSize = fontSize;
    labelizer = new Fl_MyLabel(minx, miny, width, fontSize);
    labelizer->labelsize(fontSize);
    
    menubar = new Fl_Menu_Bar(minx, miny+fontSize, width, 6+2*fontSize);
    menubar->menu(menuitems);
    menubar->textsize(fontSize);

    current_input = -1;
    nr_inputs = 0;

    int i;
    int delta = the_fontSize+6+2*the_fontSize;
    for (i=0;i<MAX_INPUTS;i++)
        doc[i].minput = new ProteusInput(the_minx, the_miny+delta, the_width, the_height-delta);

    editor_add_editor();
    make_current_visible();
}


//
// End of "$Id: editor.cpp,v 1.1 2001-02-22 19:19:55 ayalpinkus Exp $".
//
