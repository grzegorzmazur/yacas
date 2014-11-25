#include <stdio.h>
#include <stdlib.h>
#include "yacas/yacas_version.h"

int main(int argc, char** argv)
{
  printf("package net.sf.yacas;\n");
  printf("class CVersion { static String VERSION = \"%s\"; }\n", YACAS_VERSION);
  return 0;
}

