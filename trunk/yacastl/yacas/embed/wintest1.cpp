

/*
 * This example allows you to pass expressions on the command line, which will then be 
 * evaluated (each argument being a separate expression). 
 * Calling sequence: 
 *   wintest1.exe <expr1> <expr2> ...
 * 
 * Example:
 * 
 * C:\yacas\DLL>wintest1.exe "Echo(\"Hello World!\")" "D(x) Sin(x)"
 * Input> Echo("Hello World!")
 * Output>
 * Hello World!
 * 
 * Result> True;
 * Input> D(x) Sin(x)
 * Result> Cos(x);
 * 
 * C:\yacas\DLL>
 * 
 * 
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cyacas.h"



int main(int argc, char** argv)
{
  yacas_init();

  int i;
  for (i=1;i<argc;i++)
  {
    printf("Input> %s\n",argv[i]);
  
    yacas_eval(argv[i]);
    if (yacas_error())
    {
      printf("Error> %s\n",yacas_error());
    }
    else
    {
      if (yacas_output())
      if (yacas_output()[0])
      {
        printf("Output>\n%s\n",yacas_output());
      }
      printf("Result> %s\n",yacas_result());
    }
  }
  yacas_exit();
  return 0;
}