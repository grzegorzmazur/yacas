/** \file choices.h switches that control debugging in source code.
 *  Uncomment the defines below to allow a specific type of compilation
 *
 */

#ifndef __choices_h__
#define __choices_h__

/** Turn on YACAS_DEBUG if you want to see run-time statistics
 * after typing Exit()
 */
//#define YACAS_DEBUG

/** Turn on USE_ASSERT to find programming errors through the asserts
 *  placed in various places of the application.
 */
#define USE_ASSERT

/** Turn on NO_EXCEPTIONS if you want to disable run-time checking
 *  while executing commands.
 */
//#define NO_EXCEPTIONS


/** Use Karatsuba multiplication instead of O(n^2) multiplication.
 *  WARNING! This is currently not working and not suggested unless
 *      you want to help debug it.
 */
//#define USE_KARATSUBA


#endif

