
#include <stdio.h>
#include "yacas/lispevalhash.h"

/// Print some documentation based on the list of kernel functions.
/// Usage: gencorefunctions [option]
/// Options: the function does not really parse the options, it only counts how many arguments are given.
/// one argument: print the information for the ref manual.
/// Default (no arguments): print the CompileFn() definitions for the compiler

/// Print some information for a given function. If the do_refman argument is equal to 0, print the compiler info; if it's 1, print the refman info; otherwise print nothing.

const char* REFMAN_PREFIX = "*\t";
 
void Emit(const char* iname,const char* fname, int flags, int nrargs, int do_refman)
{
//
 if (do_refman == 1)  // print the information for the ref manual
 {
 // e.g. CORE_KERNEL_FUNCTION("Write", LispWrite, 1, YacasEvaluator::Function | YacasEvaluator::Variable);
 // prints
 // {Write} -- 1 or more argument(s)
 //
 // OPERATOR(prefix,0,`);
 // prints
 // {`} -- prefix operator, prec. {0}

    printf("%s{%s} -- %s%d%s argument%s\n"
      , REFMAN_PREFIX
      , iname
      , (flags & YacasEvaluator::Macro) ? "macro, " : "function, "
      , nrargs
      , (flags & YacasEvaluator::Variable) ? " or more" : ""
      , (nrargs != 1 || flags & YacasEvaluator::Variable ) ? "s" : ""
  );

 }
 else if (do_refman == 0)  // print the information for the compiler
 {
  printf("coreFunctions[\"%s\"] := {\"%s\",%s,%s,%d};\n",
         iname,
         fname,
         (flags & YacasEvaluator::Macro) ? "Macro":"Function",
         (flags & YacasEvaluator::Variable)    ? "Variable":"Fixed",
         nrargs
         );
/*
  if (flags == (YacasEvaluator::Function | YacasEvaluator::Fixed))
    printf("10  # CompileFn(\"%s\") <-- \"%s\";\n",iname,fname);
*/
  }
}

void print_operator_info(const char* kind, const char* prec, const char* yacas_name, int do_refman)
{
  if (do_refman == 1)
  {
    printf("%s{%s} -- %s operator, prec. {%s}\n", REFMAN_PREFIX, yacas_name, kind, prec);
  }
 
}

int main(int number_of_main_args, char**)
{
  if (number_of_main_args == 1)
  {
    printf("coreFunctions := {};\n\n");
  }

#define CORE_KERNEL_FUNCTION(iname,fname,nrargs,flags) Emit(iname,#fname,flags, nrargs, (number_of_main_args>1) ? 1 : 0);
#define CORE_KERNEL_FUNCTION_ALIAS(iname,fname,nrargs,flags) Emit(iname,#fname,flags, nrargs, (number_of_main_args>1) ? 1 : -1);
#define OPERATOR(kind,precedence,yacas_name) print_operator_info(#kind,#precedence,#yacas_name, (number_of_main_args>1) ? 1 : -1);

#include "yacas/corefunctions.h"

  // this file is generated from core_yacasmain.h
#include "yacas/core_yacasmain.h"

#undef CORE_KERNEL_FUNCTION
#undef CORE_KERNEL_FUNCTION_ALIAS
  return 0;
}
