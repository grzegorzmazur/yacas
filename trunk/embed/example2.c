
#include <stdio.h>
#include "cyacas.h"

int main(int argc, char** argv)
{
  
  yacas_init();
  yacas_eval("D(x)Sin(x)");
  if (!yacas_error()) printf("%s\n",yacas_result());  
  else printf("%s\n", yacas_error());
  yacas_exit();
  return 0;
}
