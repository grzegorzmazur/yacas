
#include <stdio.h>
#include "cyacas.h"

int main(int argc, char** argv)
{
  yacas_init();
  yacas_eval("D(x)Sin(x)");
  if (!yacas_error()) printf("%s\n",yacas_result());  
  yacas_exit();
  return 0;
}
