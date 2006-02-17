
#include <stdio.h>
#include <stdlib.h>
#include "../config.h"

int main(int argc, char** argv)
{
  FILE*f=fopen("net/sf/yacas/CVersion.java","wb");
  if (!f)
  {
    printf("Warning, could not create file CVersion.java\n");
    exit(-1);
  }
  fprintf(f,"package net.sf.yacas;\n");
  fprintf(f,"class CVersion { static String VERSION = \"%s\"; }\n",VERSION);
  fclose(f);
  return 0;
}

