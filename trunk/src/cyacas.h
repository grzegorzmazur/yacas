#ifndef __cyacas_h__
#define __cyacas_h__

#ifdef __cplusplus
extern "C" {
#endif

/** yacas_init : initialize Yacas. This function has to be called before calling the
 *  other functions. This function establishes a main evaluation environment
 *  for Yacas expressions to be simplified in.
 */
void yacas_init();
/** yacas_eval : evaluate an expression. The result (or possible error)
 *  can be obtained through the yacas_error and yacas_result functions,
 *  if so desired.
 */
void yacas_eval(char* expression);

/** yacas_error : return a pointer to a string explaining the error
 *  if an error occurred, or NULL otherwise.
 */
char* yacas_error();
/** yacas_result : return a string representation of the result of
 *  evaluating an expression. This function is only meaningful if
 *  there was no error. In the case of an error, the return value
 *  of yacas_result should be considered undefined.
 */
char* yacas_result();
/** yacas_exit : clean up all things related to the main Yacas
 *  evaluation environment
 */
void yacas_exit();

/** yacas_interrupt : interrupt a calculation.
 */
void yacas_interrupt();

    

#ifdef __cplusplus
};
#endif


#endif
