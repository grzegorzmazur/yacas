
#include <stdio.h>
#include "cyacas.h"

int verbose_debug = 0;	// avoid linkage error


void eval_lisp(char* expr)
{
  char buf[1024];
  sprintf(buf,"Eval(FromString(\"%s\")LispRead())",expr);
  yacas_eval(buf);
  printf(yacas_output());
}

int main(int argc, char** argv)
{
  yacas_init();
  eval_lisp("(PrettyForm (Taylor x 0 5 (Sin x)))");
  eval_lisp("(PrettyForm (D x (Sin x)))");
  yacas_exit();
  return 0;
}
