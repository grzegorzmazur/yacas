
#include <stdio.h>
#include "cyacas.h"

void runexpr(char* expr)
{
  printf("Input> %s\n",expr);
  yacas_eval(expr);
  if (yacas_error())
  {
    printf("Error> %s\n",yacas_error());
  }  
  else if (yacas_result())
  {
    printf("Output> %s\n",yacas_result());
  }
}

int main(int argc, char** argv)
{
  int i;
  yacas_init();
  
  for (i=1;i<argc;i++)
  {
    runexpr(argv[i]);
  }
  yacas_exit();
  
  return 0;
}
