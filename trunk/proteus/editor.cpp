//
// "$Id: editor.cpp,v 1.7 2003-06-09 10:24:04 ayalpinkus Exp $"
//
// A simple text editor program for the Fast Light Tool Kit (FLTK).
//
// This program is described in Chapter 4 of the FLTK Programmer's Guide.
//
// Copyright 1998-2003 by Bill Spitzak and others.
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
// Please report all bugs and problems to "fltk-bugs@fltk.org".
//

//
// Include necessary headers...
//

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>

#ifdef __MWERKS__
# define FL_DLL
#endif

#include <FL/Fl.H>
#include <FL/Fl_Group.H>
#include <FL/Fl_Double_Window.H>
#include <FL/fl_ask.H>
#include <FL/Fl_File_Chooser.H>
#include <FL/Fl_Menu_Bar.H>
#include <FL/Fl_Input.H>
#include <FL/Fl_Button.H>
#include <FL/Fl_Return_Button.H>
#include <FL/Fl_Text_Buffer.H>
#include <FL/Fl_Text_Editor.H>
#include <FL/Fl_Scroll.H>
#include <FL/Fl_Tabs.H>
#include "FltkConsole.h"
#include "FltkHintWindow.h"

/*
static int                changed = 0;
static char               filename[256] = "";
static char               title[256];
static Fl_Text_Buffer     *textbuf = 0;
// Syntax highlighting stuff...
static Fl_Text_Buffer     *stylebuf = 0;
*/

FltkHintWindow *hints = NULL;
int yhints = 0;
void go_next_input();
void update_hints(const char *text,int length);



class ProteusInput : public Fl_Text_Editor
{
public:
  FL_EXPORT ProteusInput(int x, int y, int w, int h)
      : Fl_Text_Editor(x,y,w,h)
  {};
  virtual int handle(int event);
  virtual void draw();
  inline int CursorPos() const {return mCursorPos;};
  inline void cursor_xy(int* ix, int* iy)
  {
    position_to_xy(CursorPos(), ix,iy);
  }

};

struct EditDocument
{
	int                changed;
	char               filename[256];
	char               title[256];
  Fl_Text_Buffer     *textbuf;
  // Syntax highlighting stuff...
  Fl_Text_Buffer     *stylebuf;
  ProteusInput* editor;
};
#define MAX_INPUTS 10
EditDocument doc[MAX_INPUTS];
static int current_input = 0;
static int nr_inputs     = 1;

EditDocument* Document();


void ProteusInput::draw()
{
  Fl_Text_Editor::draw();
  if (hints)
  {
      fl_font(FL_HELVETICA,14);
      fl_clip(x(),y(),w(),h());
      hints->draw(x()+10,yhints);
      fl_pop_clip();
  }
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
            int result = Fl_Text_Editor::handle(event);
            
            {
              int cstart = Document()->textbuf->line_start( Document()->editor->CursorPos());
              int cpos = Document()->editor->CursorPos();
              int lenc = cpos-cstart;
              char* text  = Document()->textbuf->text_range(cstart, cpos);
/*
{
  char* buf = malloc(lenc+1);
  buf[lenc] = '\0';
  memcpy(buf,text,lenc);
  printf("[%s]\n",buf);
  free(buf);
}
*/
              update_hints(text,lenc);
            }
            return result;
        }
        break;
    default:
        return Fl_Text_Editor::handle(event);
    }
}








class Fl_MyLabel : public Fl_Widget
{
public:
    FL_EXPORT Fl_MyLabel(int x, int y, int w, int h)
        : Fl_Widget(x,y,w,h) {};

    FL_EXPORT void draw();
    virtual FL_EXPORT int handle(int event);
};
int Fl_MyLabel::handle(int event)
{
    if (event == FL_PUSH)
    {
        go_next_input();
        return 1;
    }
    return 0;
}
void Fl_MyLabel::draw()
{
    fl_color(FL_LIGHT1);
    fl_rectf(x(),y(),w(),h());
    fl_color(FL_BLACK);
    draw_label();
}

Fl_MyLabel* labelizer=NULL;



EditDocument* Document()
{
    return &doc[current_input];
}

static int *Changed()
{
    return &Document()->changed;
}
static char *FileName()
{
    return Document()->filename;
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

void set_changed(int c)
{
    if (c != *Changed())
    {
        *Changed() = c;

        redo_title();
    }
}

void make_current_visible()
{
    int i;
    for (i=0;i<MAX_INPUTS;i++)
    {
        if (i == current_input)
            doc[i].editor->show();
        else
            doc[i].editor->hide();
    }
//    EditorInput()->take_focus();
//    EditorInput()->position(Document()->position);
//    EditorInput()->mark(Document()->mark);

    redo_title();
    delete hints;
    hints = NULL;
}

void go_next_input()
{
    if (nr_inputs==1)
        return;

//    Document()->position = EditorInput()->position();
//    Document()->mark = EditorInput()->mark();

    current_input++;
    if (current_input == nr_inputs)
        current_input = 0;
    make_current_visible();
}


Fl_Text_Display::Style_Table_Entry
                   styletable[] = {	// Style table
		     { FL_BLACK,      FL_COURIER,        14 }, // A - Plain
		     { FL_DARK_GREEN, FL_COURIER_ITALIC, 14 }, // B - Line comments
		     { FL_DARK_GREEN, FL_COURIER_ITALIC, 14 }, // C - Block comments
		     { FL_BLUE,       FL_COURIER,        14 }, // D - Strings
		     { FL_DARK_RED,   FL_COURIER,        14 }, // E - Directives
		     { FL_DARK_RED,   FL_COURIER_BOLD,   14 }, // F - Types
		     { FL_BLUE,       FL_COURIER_BOLD,   14 }, // G - Keywords
		     { FL_DARK_MAGENTA, FL_COURIER_BOLD, 14 },  // H - Operators
		     { FL_RED,   FL_COURIER,        14 } // I - Digits
		   };
const char         *code_keywords[] = {	// List of known C/C++ keywords...
		     "and",
		     "and_eq",
		     "asm",
		     "bitand",
		     "bitor",
		     "break",
		     "case",
		     "catch",
		     "compl",
		     "continue",
		     "default",
		     "delete",
		     "do",
		     "else",
		     "false",
		     "for",
		     "goto",
		     "if",
		     "new",
		     "not",
		     "not_eq",
		     "operator",
		     "or",
		     "or_eq",
		     "return",
		     "switch",
		     "template",
		     "this",
		     "throw",
		     "true",
		     "try",
		     "while",
		     "xor",
		     "xor_eq"
		   };
const char         *code_types[] = {	// List of known C/C++ types...
		     "auto",
		     "bool",
		     "char",
		     "class",
		     "const",
		     "const_cast",
		     "double",
		     "dynamic_cast",
		     "enum",
		     "explicit",
		     "extern",
		     "float",
		     "friend",
		     "inline",
		     "int",
		     "long",
		     "mutable",
		     "namespace",
		     "private",
		     "protected",
		     "public",
		     "register",
		     "short",
		     "signed",
		     "sizeof",
		     "static",
		     "static_cast",
		     "struct",
		     "template",
		     "typedef",
		     "typename",
		     "union",
		     "unsigned",
		     "virtual",
		     "void",
		     "volatile"
		   };


//
// 'compare_keywords()' - Compare two keywords...
//

int
compare_keywords(const void *a,
                 const void *b) {
  return (strcmp(*((const char **)a), *((const char **)b)));
}


void update_hints(const char *text,int length)
{
  extern FltkConsole* console;

  int iNrLines = -1;
  int iNrDescriptions=-1;
  int iMaxWidth=-1;
  int iTextSize=-1;
  int oldyhints=yhints;

  if (hints)
  {
    iNrLines = hints->iNrLines;
    iNrDescriptions=hints->iNrDescriptions;
    iMaxWidth=hints->iMaxWidth;
    iTextSize=hints->iTextSize;
  }
  delete hints;
  hints = console->CheckForNewHints((char*)text, length);
  if (hints)
  {
    {
      int ix,iy;
      Document()->editor->cursor_xy(&ix,&iy);
      yhints = iy-2;
      if (yhints-hints->height()-10 < Document()->editor->y())
      {
        yhints = iy+20+hints->height();  
      }
    }
    if (
        iNrLines > hints->iNrLines ||
        iNrDescriptions>hints->iNrDescriptions ||
        iMaxWidth>hints->iMaxWidth ||
        iTextSize>hints->iTextSize || 
        yhints != oldyhints
        ) 
    {
      Document()->editor->redisplay_range(0, Document()->textbuf->length());
    }
  }
  else if (iNrLines >=0 )
    Document()->editor->redisplay_range(0, Document()->textbuf->length());
}

//
// 'style_parse()' - Parse text and produce style data.
//

void
style_parse(const char *text,
            char       *style,
	    int        length) 
{
  char	     current;
  int	     col;
  int	     last;
  char	     buf[255],
             *bufptr;
  const char *temp;

  for (current = *style, col = 0, last = 0; length > 0; length --, text ++) 
  {
    if (current == 'B') current = 'A';
    if (*text == '/' || (isspace(*text) && current != 'C' && current != 'D')) current = 'A';

    if (current == 'A') 
    {
      // Check for directives, comments, strings, and keywords...
      if (col == 0 && *text == '#') 
      {
        // Set style to directive
        current = 'E';
      } 
      else if (strncmp(text, "//", 2) == 0) 
      {
        current = 'B';
        for (; length > 0 && *text != '\n'; length --, text ++) *style++ = 'B';

        if (length == 0) break;
      } 
      else if (strncmp(text, "/*", 2) == 0) 
      {
        current = 'C';
        *style++ = current; text++; length--; col++;
        *style++ = current; text++; length--; col++;
        while (length>1)
        {
          if (*text == '\n') col=0;
          if (!strncmp(text, "*/", 2))
          {
            *style++ = current; text++; length--; col++;
            *style++ = current; text++; length--; col++;
            current = 'A';
            break;
          }
          *style++ = current; text++; length--; col++;
        }
        if (length == 0) break;
      } 
      else if (strncmp(text, "\\\"", 2) == 0) 
      {
        // Quoted quote...
        *style++ = current;
        *style++ = current;
        text ++;
        length --;
        col += 2;
        continue;
      } 
      else if (*text == '\"') 
      {
        current = 'D';
      } 
      else if (!last && islower(*text)) 
      {
        // Might be a keyword...
        for (temp = text, bufptr = buf;
        islower(*temp) && bufptr < (buf + sizeof(buf) - 1);
        *bufptr++ = *temp++);

        if (!islower(*temp)) 
        {
          *bufptr = '\0';
          bufptr = buf;

          if (current != 'D' && bsearch(&bufptr, code_types,
	              sizeof(code_types) / sizeof(code_types[0]),
		      sizeof(code_types[0]), compare_keywords)) 
          {
            while (text < temp) 
            {
              *style++ = 'F';
              text ++;
              length --;
              col ++;
            }
            text --;
            length ++;
            last = 1;
            continue;
          } 
          else if (current != 'D' && bsearch(&bufptr, code_keywords,
	                     sizeof(code_keywords) / sizeof(code_keywords[0]),
		             sizeof(code_keywords[0]), compare_keywords)) 
          {
            while (text < temp) 
            {
              *style++ = 'G';
              text ++;
              length --;
              col ++;
            }
            text --;
            length ++;
            last = 1;
            continue;
          }
        }
      }
    } 
    else if (current == 'C' && strncmp(text, "*/", 2) == 0) 
    {
      // Close a C comment...
      *style++ = current;
      *style++ = current;
      text ++;
      length --;
      current = 'A';
      col += 2;
      continue;
    } 
    else if (current == 'D') 
    {
      // Continuing in string...
      if (strncmp(text, "\\\"", 2) == 0) 
      {
        // Quoted end quote...
        *style++ = current;
        *style++ = current;
        text ++;
        length --;
        col += 2;
        continue;
      } 
      else if (*text == '\"') 
      {
        // End quote...
        *style++ = current;
        col ++;
        current = 'A';
        continue;
      }
    }

    // Copy style info...
    if (current == 'A' && 
         (*text == '{' || *text == '}'
       || *text == '(' || *text == ')'
       || *text == '[' || *text == ']'
         )) *style++ = 'G';
    else if (current == 'A' && 
              (
                (isgraph(*text) && !isalnum(*text)) ||
                ispunct(*text)
              )
            )
      *style++ = 'H';
    else if (current == 'A' && isdigit(*text))
      *style++ = 'I';
    else 
      *style++ = current;
    col ++;

    last = isalnum(*text) || *text == '.';

    if (isspace(*text) && current != 'C')
    {
      // Reset column and possibly reset the style
      if (*text == '\n') col = 0;
      if (current == 'B' || current == 'E' || current == 'H' || current == 'I') current = 'A';
    }
  }
}


//
// 'style_init()' - Initialize the style buffer...
//

Fl_Text_Buffer     *
style_init(Fl_Text_Buffer     *textbuf) 
{
  Fl_Text_Buffer     *stylebuf = NULL;
  char *style = new char[textbuf->length() + 1];
  char *text = textbuf->text();
  

  memset(style, 'A', textbuf->length());
  style[textbuf->length()] = '\0';

  if (!stylebuf) stylebuf = new Fl_Text_Buffer(textbuf->length());

  style_parse(text, style, textbuf->length());

  stylebuf->text(style);
  delete[] style;
  free(text);
  return stylebuf;
}


//
// 'style_unfinished_cb()' - Update unfinished styles.
//

void
style_unfinished_cb(int, void*) {
}


//
// 'style_update()' - Update the style buffer...
//

void
style_update(int        pos,		// I - Position of update
             int        nInserted,	// I - Number of inserted chars
	     int        nDeleted,	// I - Number of deleted chars
             int        /*nRestyled*/,	// I - Number of restyled chars
	     const char * /*deletedText*/,// I - Text that was deleted
             void       *cbArg) {	// I - Callback data
  int	start,				// Start of text
	end;				// End of text
  char	last,				// Last style on line
	*style,				// Style data
	*text;				// Text data


  // If this is just a selection change, just unselect the style buffer...
  if (nInserted == 0 && nDeleted == 0) {
    Document()->stylebuf->unselect();
    return;
  }

  // Track changes in the text buffer...
  if (nInserted > 0) {
    // Insert characters into the style buffer...
    style = new char[nInserted + 1];
    memset(style, 'A', nInserted);
    style[nInserted] = '\0';

    Document()->stylebuf->replace(pos, pos + nDeleted, style);
    delete[] style;
  } else {
    // Just delete characters in the style buffer...
    Document()->stylebuf->remove(pos, pos + nDeleted);
  }

  // Select the area that was just updated to avoid unnecessary
  // callbacks...
  Document()->stylebuf->select(pos, pos + nInserted - nDeleted);

  // Re-parse the changed region; we do this by parsing from the
  // beginning of the line of the changed region to the end of
  // the line of the changed region...  Then we check the last
  // style character and keep updating if we have a multi-line
  // comment character...
  start = Document()->textbuf->line_start(pos);
  end   = Document()->textbuf->line_end(pos + nInserted);
  text  = Document()->textbuf->text_range(start, end);
  style = Document()->stylebuf->text_range(start, end);
  last  = style[end - start - 1];


//  printf("start = %d, end = %d, text = \"%s\", style = \"%s\"...\n",
//         start, end, text, style);

  style_parse(text, style, end - start);

//  printf("new style = \"%s\"...\n", style);

  Document()->stylebuf->replace(start, end, style);
  ((ProteusInput *)cbArg)->redisplay_range(start, end);

  if (last != style[end - start - 1]) {
    // The last character on the line changed styles, so reparse the
    // remainder of the buffer...
    free(text);
    free(style);

    end   = Document()->textbuf->length();
    text  = Document()->textbuf->text_range(start, end);
    style = Document()->stylebuf->text_range(start, end);

    style_parse(text, style, end - start);

    Document()->stylebuf->replace(start, end, style);
    ((ProteusInput *)cbArg)->redisplay_range(start, end);
  }

  free(text);
  free(style);
}


// Editor window functions and class...
void save_cb();
void saveas_cb();
void find2_cb(Fl_Widget*, void*);
void replall_cb(Fl_Widget*, void*);
void replace2_cb(Fl_Widget*, void*);
void replcan_cb(Fl_Widget*, void*);

//class EditorWindow : public Fl_Double_Window {
//  public:
//    EditorWindow(int w, int h, const char* t);
//    ~EditorWindow();

    Fl_Window          *replace_dlg;
    Fl_Input           *replace_find;
    Fl_Input           *replace_with;
    Fl_Button          *replace_all;
    Fl_Return_Button   *replace_next;
    Fl_Button          *replace_cancel;

//    ProteusInput     *editor;
    char               search[256];
//};

void init_editor(){
//EditorWindow::EditorWindow(int x, int y, int w, int h, const char* t) : Fl_Double_Window(x,y,w, h, t) {
  replace_dlg = new Fl_Window(300, 105, "Replace");
    replace_find = new Fl_Input(80, 10, 210, 25, "Find:");
    replace_find->align(FL_ALIGN_LEFT);

    replace_with = new Fl_Input(80, 40, 210, 25, "Replace:");
    replace_with->align(FL_ALIGN_LEFT);

    replace_all = new Fl_Button(10, 70, 90, 25, "Replace All");
    replace_all->callback((Fl_Callback *)replall_cb, NULL);

    replace_next = new Fl_Return_Button(105, 70, 120, 25, "Replace Next");
    replace_next->callback((Fl_Callback *)replace2_cb, NULL);

    replace_cancel = new Fl_Button(230, 70, 60, 25, "Cancel");
    replace_cancel->callback((Fl_Callback *)replcan_cb, NULL);
  replace_dlg->end();
  replace_dlg->set_non_modal();
//  editor = 0;
  *search = (char)0;
}

/*
EditorWindow::~EditorWindow() {
  delete replace_dlg;
}
*/

int check_save(void) {
  if (!Document()->changed) return 1;

  int r = fl_choice("The current file has not been saved.\n"
                    "Would you like to save it now?",
                    "Cancel", "Save", "Discard");

  if (r == 1) {
    save_cb(); // Save the file...
    return !Document()->changed;
  }

  return (r == 2) ? 1 : 0;
}

int loading = 0;
void load_file(char *newfile, int ipos) {
  loading = 1;
  int insert = (ipos != -1);
  if (insert) set_changed(1);
  if (!insert) strcpy(Document()->filename, "");
  int r;
  if (!insert) r = Document()->textbuf->loadfile(newfile);
  else r = Document()->textbuf->insertfile(newfile, ipos);
  if (r)
    fl_alert("Error reading from file \'%s\':\n%s.", newfile, strerror(errno));
  else
    if (!insert) strcpy(Document()->filename, newfile);
  loading = 0;
  Document()->textbuf->call_modify_callbacks();
  make_current_visible();
}

void save_file(char *newfile) {
  if (Document()->textbuf->savefile(newfile))
    fl_alert("Error writing to file \'%s\':\n%s.", newfile, strerror(errno));
  else
    strcpy(Document()->filename, newfile);
  Document()->changed = 0;
  Document()->textbuf->call_modify_callbacks();
  redo_title();
}

void copy_cb(Fl_Widget*, void* v) {
//  EditorWindow* e = (EditorWindow*)v;
  ProteusInput::kf_copy(0, Document()->editor);
}

void cut_cb(Fl_Widget*, void* v) {
//  EditorWindow* e = (EditorWindow*)v;
  ProteusInput::kf_cut(0, Document()->editor);
}

void delete_cb(Fl_Widget*, void*) {
  Document()->textbuf->remove_selection();
}

void find_cb(Fl_Widget* w, void* v) {
//  EditorWindow* e = (EditorWindow*)v;
  const char *val;

  val = fl_input("Search String:", search);
  if (val != NULL) {
    // User entered a string - go find it!
    strcpy(search, val);
    find2_cb(w, v);
  }
}

void find2_cb(Fl_Widget* w, void* v) {
//  EditorWindow* e = (EditorWindow*)v;
  if (search[0] == '\0') {
    // Search string is blank; get a new one...
    find_cb(w, v);
    return;
  }

  int pos = Document()->editor->insert_position();
  int found = Document()->textbuf->search_forward(pos, search, &pos);
  if (found) {
    // Found a match; select and update the position...
    Document()->textbuf->select(pos, pos+strlen(search));
    Document()->editor->insert_position(pos+strlen(search));
    Document()->editor->show_insert_position();
  }
  else fl_alert("No occurrences of \'%s\' found!", search);
}

void set_title(Fl_Window* w) {
/*
  if (Document()->filename[0] == '\0') strcpy(title, "Untitled");
  else {
    char *slash;
    slash = strrchr(filename, '/');
#ifdef WIN32
    if (slash == NULL) slash = strrchr(filename, '\\');
#endif
    if (slash != NULL) strcpy(title, slash + 1);
    else strcpy(title, filename);
  }

  if (changed) strcat(title, " (modified)");

  w->label(title);
*/
}

static void editor_changed_cb(int, int nInserted, int nDeleted,int, const char*, void* v) {
  if ((nInserted || nDeleted) && !loading) set_changed(1);
  ProteusInput *w = (ProteusInput *)v;
//  set_title(w);
  if (loading) w->show_insert_position();
}

static void new_cb(Fl_Widget*, void*) 
{
  if (nr_inputs < MAX_INPUTS)
  {
    current_input = nr_inputs;
    nr_inputs++;
  }
  else
  {
    if (!check_save()) return;
  }
  Document()->filename[0] = '\0';
  Document()->textbuf->select(0, Document()->textbuf->length());
  Document()->textbuf->remove_selection();
  Document()->changed = 0;
  Document()->textbuf->call_modify_callbacks();
  make_current_visible();
}

static void run_cb(Fl_Widget*, void*) 
{
  save_cb();
  extern char notepad_to_load[256];
  sprintf(notepad_to_load,"%s",FileName());

  extern Fl_Tabs* mainTabs;
  extern Fl_Group* input;
  mainTabs->value(input);

  extern FltkConsole* console;
  console->Restart();
}

static void open_cb(Fl_Widget*, void*) 
{
//  if (!check_save()) return;
  if (nr_inputs < MAX_INPUTS && (FileName()[0] != '\0'))
  {
    current_input = nr_inputs;
    nr_inputs++;
  }
  else
  {
    if (*Changed())
      if (!check_save()) return;
  }

  char *newfile = fl_file_chooser("Open File?", "*", Document()->filename);
  if (newfile != NULL) load_file(newfile, -1);
}

void insert_cb(Fl_Widget*, void *v) {
  char *newfile = fl_file_chooser("Insert File?", "*", Document()->filename);
//  EditorWindow *w = (EditorWindow *)v;
  if (newfile != NULL) load_file(newfile, Document()->editor->insert_position());
}

void paste_cb(Fl_Widget*, void* v) {
//  EditorWindow* e = (EditorWindow*)v;
  ProteusInput::kf_paste(0, Document()->editor);
}

int num_windows = 0;

/*
void close_cb(Fl_Widget*, void* v) {
  Fl_Window* w = (Fl_Window*)v;
  if (num_windows == 1 && !check_save()) {
    return;
  }

  w->hide();
  textbuf->remove_modify_callback(changed_cb, w);
  delete w;
  num_windows--;
  if (!num_windows) exit(0);
}
*/

static void quit_cb(Fl_Widget*, void*) {
/*
  if (changed && !check_save())
    return;

  exit(0);
*/
}

void replace_cb(Fl_Widget*, void* v) {
//  EditorWindow* e = (EditorWindow*)v;
  replace_dlg->show();
}

void replace2_cb(Fl_Widget*, void* v) {
//  EditorWindow* e = (EditorWindow*)v;
  const char *find = replace_find->value();
  const char *replace = replace_with->value();

  if (find[0] == '\0') {
    // Search string is blank; get a new one...
    replace_dlg->show();
    return;
  }

  replace_dlg->hide();

  int pos = Document()->editor->insert_position();
  int found = Document()->textbuf->search_forward(pos, find, &pos);

  if (found) {
    // Found a match; update the position and replace text...
    Document()->textbuf->select(pos, pos+strlen(find));
    Document()->textbuf->remove_selection();
    Document()->textbuf->insert(pos, replace);
    Document()->textbuf->select(pos, pos+strlen(replace));
    Document()->editor->insert_position(pos+strlen(replace));
    Document()->editor->show_insert_position();
  }
  else fl_alert("No occurrences of \'%s\' found!", find);
}

void replall_cb(Fl_Widget*, void* v) {
//  EditorWindow* e = (EditorWindow*)v;
  const char *find = replace_find->value();
  const char *replace = replace_with->value();

  find = replace_find->value();
  if (find[0] == '\0') {
    // Search string is blank; get a new one...
    replace_dlg->show();
    return;
  }

  replace_dlg->hide();

  Document()->editor->insert_position(0);
  int times = 0;

  // Loop through the whole string
  for (int found = 1; found;) {
    int pos = Document()->editor->insert_position();
    found = Document()->textbuf->search_forward(pos, find, &pos);

    if (found) {
      // Found a match; update the position and replace text...
      Document()->textbuf->select(pos, pos+strlen(find));
      Document()->textbuf->remove_selection();
      Document()->textbuf->insert(pos, replace);
      Document()->editor->insert_position(pos+strlen(replace));
      Document()->editor->show_insert_position();
      times++;
    }
  }

  if (times) fl_message("Replaced %d occurrences.", times);
  else fl_alert("No occurrences of \'%s\' found!", find);
}

void replcan_cb(Fl_Widget*, void* v) {
//  EditorWindow* e = (EditorWindow*)v;
  replace_dlg->hide();
}

void save_cb() {
  if (Document()->filename[0] == '\0') 
  {
    // No filename - get one!
    saveas_cb();
  }
  else 
    save_file(Document()->filename);
}

void saveas_cb() {
  char *newfile;

  newfile = fl_file_chooser("Save File As?", "*", Document()->filename);
  if (newfile != NULL) save_file(newfile);
}

//Fl_Window* new_view();

//void view_cb(Fl_Widget*, void*) {
//  Fl_Window* w = new_view();
//  w->show();
//}

Fl_Menu_Item menuitems[] = {
  { "&File",              FL_ALT + 'f', 0, 0, FL_SUBMENU },
    { "&Run as notepad",  FL_ALT + 'r', (Fl_Callback *)run_cb },
    { "&New File",        FL_ALT + 'n', (Fl_Callback *)new_cb },
    { "&Open File...",    FL_ALT + 'o', (Fl_Callback *)open_cb },
    { "&Insert File...",  FL_ALT + 'i', (Fl_Callback *)insert_cb, 0, FL_MENU_DIVIDER },
    { "&Save File",       FL_ALT + 's', (Fl_Callback *)save_cb },
    { "Save File &As...", FL_ALT + FL_SHIFT + 's', (Fl_Callback *)saveas_cb, 0, FL_MENU_DIVIDER },
//    { "New &View", FL_ALT + 'v', (Fl_Callback *)view_cb, 0 },
//    { "&Close View", FL_CTRL + 'w', (Fl_Callback *)close_cb, 0, FL_MENU_DIVIDER },
//    { "E&xit", FL_ALT + 'x', (Fl_Callback *)quit_cb, 0 },
    { 0 },

  { "&Edit",         FL_ALT + 'e', 0, 0, FL_SUBMENU },
    { "Cu&t",        FL_CTRL + 'x', (Fl_Callback *)cut_cb },
    { "&Copy",       FL_CTRL + 'c', (Fl_Callback *)copy_cb },
    { "&Paste",      FL_CTRL + 'v', (Fl_Callback *)paste_cb },
    { "&Delete",     0, (Fl_Callback *)delete_cb },
    { 0 },

  { "&Search",          FL_ALT + 's', 0, 0, FL_SUBMENU },
    { "&Find...",       FL_ALT + 'f', (Fl_Callback *)find_cb },
    { "F&ind Again",    FL_ALT + 'g', find2_cb },
    { "&Replace...",    FL_ALT + 'r', replace_cb },
    { "Re&place Again", FL_ALT + 't', replace2_cb },
    { 0 },

  { 0 }
};

void editor_add_items(Fl_Group* o,int minx,int miny,int width, int height, int fontSize)
{
//  textbuf = new Fl_Text_Buffer;
//  stylebuf = style_init(textbuf);

  labelizer = new Fl_MyLabel(minx, miny, width, fontSize);
  labelizer->labelsize(fontSize);

  Fl_Menu_Bar* m = new Fl_Menu_Bar(minx, miny+fontSize, width, 6+2*fontSize);
  m->copy(menuitems, o);

  int i;
  for (i=0;i<MAX_INPUTS;i++)
  {
    doc[i].textbuf = new Fl_Text_Buffer;
    doc[i].changed = 0;
    doc[i].filename[0] = '\0';
    doc[i].stylebuf = style_init(doc[i].textbuf);

    doc[i].editor = new ProteusInput(minx, miny+6+3*fontSize, width, height-(6+3*fontSize));
    doc[i].editor->buffer(doc[i].textbuf);
    doc[i].editor->highlight_data(doc[i].stylebuf, styletable,
                              sizeof(styletable) / sizeof(styletable[0]),
			      'A', style_unfinished_cb, 0);
    doc[i].editor->textfont(FL_COURIER);
    doc[i].editor->take_focus();
    doc[i].editor->callback(quit_cb,0);

    doc[i].textbuf->add_modify_callback(style_update, doc[i].editor);
    doc[i].textbuf->add_modify_callback(editor_changed_cb, doc[i].editor);
    doc[i].textbuf->call_modify_callbacks();
  }
  make_current_visible();

/*
//Fl_Window* new_view() {
//  EditorWindow* w = new EditorWindow(660, 400, title);
//    w->begin();

//    Fl_Scroll *scroll = new Fl_Scroll(minx, miny+6+3*fontSize, width, height-(6+3*fontSize));

    editor = new ProteusInput(minx, miny+6+3*fontSize, width, height-(6+3*fontSize));
    editor->buffer(textbuf);
    editor->highlight_data(stylebuf, styletable,
                              sizeof(styletable) / sizeof(styletable[0]),
			      'A', style_unfinished_cb, 0);
    editor->textfont(FL_COURIER);
    editor->take_focus();
    editor->callback(quit_cb,0);
//    editor->end();
//    scroll->end();
//    o->resizable(editor);
//    o->resizable(scroll);

//  w->end();
//  o->resizable(editor);
//  o->callback((Fl_Callback *)quit_cb, o);

  textbuf->add_modify_callback(style_update, editor);
  textbuf->add_modify_callback(editor_changed_cb, o);
  textbuf->call_modify_callbacks();
//  num_windows++;
//  return w;
*/
}

/*
int main(int argc, char **argv) {
  textbuf = new Fl_Text_Buffer;
  style_init();

  Fl_Window* window = new_view();

  window->show(1, argv);

  if (argc > 1) load_file(argv[1], -1);

  return Fl::run();
}
*/

//
// End of "$Id: editor.cpp,v 1.7 2003-06-09 10:24:04 ayalpinkus Exp $".
//






































#if 0 //TODO remove, older editor

//
// "$Id: editor.cpp,v 1.7 2003-06-09 10:24:04 ayalpinkus Exp $"
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
#include <FL/fl_file_chooser.H>		// FLTK file chooser
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
  EditorInput()->copy(0);
}

void cut_cb(void) {
  EditorInput()->copy(0);
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

    newfile = fl_file_chooser("Open File?", "*", FileName());

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

  newfile = fl_file_chooser("Save File As?", "*", FileName());

  if (newfile != NULL) save_file(newfile);
}

void undo_cb(void) {
  EditorInput()->undo();
}

static Fl_Menu_Item menuitems[] = {
  { "&File", FL_ALT+'f', 0, 0, FL_SUBMENU },
    { "&New",        FL_ALT + 'n', (Fl_Callback *)new_cb },
    { "&Open...",    FL_ALT + 'o', (Fl_Callback *)open_cb, 0, FL_MENU_DIVIDER },
    { "&Save",       FL_ALT + 's', (Fl_Callback *)save_cb },
    { "Save &As...", FL_ALT + FL_SHIFT + 's', (Fl_Callback *)saveas_cb, 0, FL_MENU_DIVIDER },
    { "C&lose", FL_ALT + 'l', (Fl_Callback *)editor_close_cb },
    { "&Quit", FL_ALT + 'q', (Fl_Callback *)editor_quit_cb },
    { 0 },

  { "&Edit", FL_ALT+'e', 0, 0, FL_SUBMENU },
    { "&Undo",       FL_ALT + 'z', (Fl_Callback *)undo_cb, 0, FL_MENU_DIVIDER },
    { "Cu&t",        FL_ALT + 'x', (Fl_Callback *)cut_cb },
    { "&Copy",       FL_ALT + 'c', (Fl_Callback *)copy_cb },
    { "&Paste",      FL_ALT + 'v', (Fl_Callback *)paste_cb },
    { "&Delete",     0, (Fl_Callback *)delete_cb },
    { 0 },

  { "&Search", FL_ALT+'s', 0, 0, FL_SUBMENU },
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

void editor_add_items(Fl_Group* o,int minx,int miny,int width, int height, int fontSize)
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
// End of "$Id: editor.cpp,v 1.7 2003-06-09 10:24:04 ayalpinkus Exp $".
//

#endif

