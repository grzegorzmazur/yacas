
#ifndef __editor_h__
#define __editor_h__

#include <FL/Fl_Multiline_Input.H>	// Fl_Multiline_Input header file

extern Fl_Widget* labelizer;

Fl_Multiline_Input *EditorInput();
void load_file(char *newfile);
void init_editor();
void editor_add_items(int minx,int miny,int width, int height, int fontSize=12);
void editor_quit_cb(void);

#endif
