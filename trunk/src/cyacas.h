#ifndef __cyacas_h__
#define __cyacas_h__

#ifdef __cplusplus
extern "C" {
#endif

#ifdef WIN32
  // The following ifdef block is the standard way of creating macros which make exporting 
  // from a DLL simpler. All files within this DLL are compiled with the YACASDLL_EXPORTS
  // symbol defined on the command line. this symbol should not be defined on any project
  // that uses this DLL. This way any other project whose source files include this file see 
  // YACASDLL_API functions as being imported from a DLL, wheras this DLL sees symbols
  // defined with this macro as being exported.
  #ifdef YACASDLL_EXPORTS
  #define YACASDLL_API __declspec(dllexport)
  #else
  #define YACASDLL_API __declspec(dllimport)
  #endif
#endif

#ifndef YACASDLL_API
#define YACASDLL_API
#endif

/** yacas_init : initialize Yacas. This function has to be called before calling the
 *  other functions. This function establishes a main evaluation environment
 *  for Yacas expressions to be simplified in.
 */
YACASDLL_API void yacas_init();
/** yacas_eval : evaluate an expression. The result (or possible error)
 *  can be obtained through the yacas_error and yacas_result functions,
 *  if so desired.
 */
YACASDLL_API void yacas_eval(char* expression);

/** yacas_error : return a pointer to a string explaining the error
 *  if an error occurred, or NULL otherwise.
 */
YACASDLL_API char* yacas_error();
/** yacas_result : return a string representation of the result of
 *  evaluating an expression. This function is only meaningful if
 *  there was no error. In the case of an error, the return value
 *  of yacas_result should be considered undefined.
 */
YACASDLL_API char* yacas_result();

/** yacas_output : return pointer to output printed while evaluating
 * an expression.
 */
YACASDLL_API char* yacas_output();


/** yacas_exit : clean up all things related to the main Yacas
 *  evaluation environment
 */
YACASDLL_API void yacas_exit();

/** yacas_interrupt : interrupt a calculation.
 */
YACASDLL_API void yacas_interrupt();

    

#ifdef __cplusplus
}
#endif


#endif
