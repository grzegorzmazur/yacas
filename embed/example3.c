
#include <stdio.h>
#include "cyacas.h"


void eval_lisp(char* expr)
{
  char buf[1024];
  sprintf(buf,"Eval(FromString(\"%s\")LispRead())",expr);
  yacas_eval(buf);
}

int main(int argc, char** argv)
{
  yacas_init();
  eval_lisp("(PrettyForm (Taylor x 0 15 (Sin x)))");
  yacas_exit();
  return 0;
}
