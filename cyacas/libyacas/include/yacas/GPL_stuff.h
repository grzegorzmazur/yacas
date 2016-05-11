#ifndef YACAS_GPL_STUFF_H
#define YACAS_GPL_STUFF_H

#define GPL_base_text \
"Yacas is Free Software--Free as in Freedom--so you can redistribute Yacas or\n" \
"modify it under certain conditions. Yacas comes with ABSOLUTELY NO WARRANTY.\n" \
"See the GNU Lesser General Public License (LGPL) version 2.1 or (at your\n" \
"discretion) any later version for the full conditions.\n"

#define Yacas_Web_info \
"See http://www.yacas.org/ for more information on yacas. and documentation.\n"\
"Type ?? for help. Or type ?function for help on a function.\n"

#define Yacas_help_info \
"Type ?license or ?licence to see the LGPL version 2.1;\n"

// This is the full text for systems where the online help (?blah) is available
#define GPL_blurb GPL_base_text Yacas_help_info Yacas_Web_info "\n"

// This is for systems where online help (?blah) is normally not available
#define GPL_blurb_nohelp GPL_base_text Yacas_Web_info "\n"



#endif
