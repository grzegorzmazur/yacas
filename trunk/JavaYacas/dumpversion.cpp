#include <stdio.h>
#include <stdlib.h>
#include "version.h"

int main(int argc, char** argv)
{
  printf("package net.sf.yacas;\n");
  printf("class CVersion { static String VERSION = \"%s\"; }\n", VERSION);
  return 0;
}

