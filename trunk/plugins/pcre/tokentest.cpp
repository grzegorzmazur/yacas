

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <locale.h>

#include "pcre.h"


struct TPattern
{
  pcre *re;
  pcre_extra *pe;
  int type;
};

#define KMaxPatterns 256
TPattern patterns[KMaxPatterns];
int nrPatterns = 0;

void AddPattern(const char* aPattern, int aType)
{
  const char *error;
  int erroffset;
  patterns[nrPatterns].re = pcre_compile(
    aPattern,          /* the pattern */
    PCRE_DOTALL,                /* default options */
    &error,           /* for error message */
    &erroffset,       /* for error offset */
    NULL);            /* use default character tables */

  patterns[nrPatterns].pe = pcre_study(
    patterns[nrPatterns].re,             /* result of pcre_compile() */
    0,              /* no options exist */
    &error);        /* set to NULL or points to a message */
  patterns[nrPatterns].type = aType;
  nrPatterns++;
}

void FreePatterns(void)
{
  int i;
  for (i=0;i<nrPatterns;i++)
  {
    free(patterns[i].pe);
    free(patterns[i].re);
  }
  nrPatterns=0;
}

int main(int argc, char** argv)
{
  if (argc<3)
  {
    fprintf(stderr,"%s regexes infile\n",argv[0]);
    fprintf(stderr,"  where a regex looks like num=pattern");
    exit(-1);
  }

  FILE*fin;
  fin = fopen(argv[1],"rb");
  if (!fin)
  {
    fprintf(stderr,"Error opening file %s for reading\n",argv[2]);
    exit(-1);
  }
  while (!feof(fin))
  {
    int type;
    char buf[256];
    int returned = fscanf(fin,"%d=%s",&type,buf);
    if (returned == 2)
    {
      AddPattern(buf,type);
    }
    else
    {
      break;
    }
  }
  fclose(fin);

  fin = fopen(argv[2],"rb");
  if (!fin)
  {
    fprintf(stderr,"Error opening file %s for reading\n",argv[2]);
    exit(-1);
  }

  fseek(fin,0,SEEK_END);
  long size = ftell(fin);
  fseek(fin,0,SEEK_SET);
  char *text = (char *)malloc(size+1);
  char *printbuf = (char *)malloc(size+1);
  fread(text,1,size,fin);
  text[size] = '\0';
  fclose(fin);

  printf("{\n");
  char* trav = text;
  int first = 1;
  int currentline = 1;
  while (*trav)
  {
    int count;
    int ovector[10];
    int i;
    for (i=0;i<nrPatterns;i++)
    {
      count = pcre_exec(
        patterns[i].re, patterns[i].pe, trav, strlen(trav), 0, 0, ovector, 10); 

      if (count == 1)
      {
        break;
      }
    }
    if (count == 1)
    {
      char* trg = printbuf;
      char* src = &trav[ovector[0]];
      char* endp= src+ovector[1]-ovector[0];
      while (src != endp)
      {
        switch (*src)
        {
        case '\"':
          *trg++ = '\\';
        default:
          if (*src == '\n') currentline++;
          *trg++ = *src++;
          break;
        }
      }
      *trg++ = '\0';
      if (!first) printf(",\n");
      first=0;
      printf("  {\"%s\",%d}",printbuf,patterns[i].type);
      trav += ovector[1]-ovector[0];
    }
    else
    {
      char errorbuf[256];
      strncpy(errorbuf,trav,200);
      errorbuf[200] = '\0';
      char* ptr = errorbuf;
      int lines=1;
      while (*ptr)
      {
        if (*ptr == '\n') lines++;
        if (lines>4) 
        {
          ptr--;
          break;
        }
        ptr++;
      }
      *ptr = '\0';
      strcat(errorbuf,"...");
      fprintf(stderr,"\n%s(%d):unrecognized token at\n%s\n",argv[2],currentline,errorbuf);
      break;
    }
  }
  printf("\n};\n");
  free(text);
  free(printbuf);
  FreePatterns();
  return 0;
}


