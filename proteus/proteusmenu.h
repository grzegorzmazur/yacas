
#ifndef __proteusmenu_h__
#define __proteusmenu_h__

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

#endif
