unit yacas;

{

    Yacas for Delphi
    Copyright (C) 2003  Franz Hack

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 }

interface
 const YACASAPI='yacasdll.dll';
{
/** yacas_init : initialize Yacas. This function has to be called before calling the
 *  other functions. This function establishes a main evaluation environment
 *  for Yacas expressions to be simplified in.
 */
 YACASDLL_API void yacas_init(); }
 procedure yacas_init; cdecl; external  YACASAPI;


{
/** yacas_eval : evaluate an expression. The result (or possible error)
 *  can be obtained through the yacas_error and yacas_result functions,
 *  if so desired.
 */
 YACASDLL_API void yacas_eval(char* expression); }
 procedure yacas_eval(expression:Pchar);cdecl; external  YACASAPI;


 {
/** yacas_error : return a pointer to a string explaining the error
 *  if an error occurred, or NULL otherwise.
 */
  YACASDLL_API char* yacas_error();
 } function yacas_error:PChar; cdecl; external  YACASAPI;

 {
/** yacas_result : return a string representation of the result of
 *  evaluating an expression. This function is only meaningful if
 *  there was no error. In the case of an error, the return value
 *  of yacas_result should be considered undefined.
 */
YACASDLL_API char* yacas_result();}
 function yacas_result:PChar; cdecl; external  YACASAPI;

{
/** yacas_output : return pointer to output printed while evaluating
 * an expression.
 */
YACASDLL_API char* yacas_output(); }
 function yacas_output:PChar; cdecl; external  YACASAPI;

 {
 /** yacas_exit : clean up all things related to the main Yacas
 *  evaluation environment
 */
YACASDLL_API void yacas_exit();  }
 procedure yacas_exit; cdecl; external  YACASAPI;

 {
/** yacas_interrupt : interrupt a calculation.
 */
YACASDLL_API void yacas_interrupt();
  }
procedure yacas_interrupt(); cdecl; external  YACASAPI;


implementation


end.

