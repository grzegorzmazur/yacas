
/*
 * Codereviewer, part of Yacas. This file implements a lint-like facility that can check if the
 * Yacas coding standards are upheld, and possibly assist in fixing mistakes if they are local.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "GPL_stuff.h"

int java_mode=0;
int c_mode   =0;
int fix_mode =0;

int nr_warnings_reported = 0;
int nr_warnings_fixed = 0;

#define MAX_LINE_LENGTH 16384
char line_read_buffer[MAX_LINE_LENGTH];

void ShowCharacter(int startPos, int nr)
{
  fprintf(stderr,"%s",line_read_buffer);
  int i;
  for (i=0;i<startPos;i++)
    fprintf(stderr," ");
  for (i=0;i<nr;i++)
    fprintf(stderr,"^");
  fprintf(stderr,"\n");
}

#define START_WARN {fprintf(stderr,"%s:%d: ",fileName,line);}

void CheckFileForTabs(char* fileName)
{
  FILE*f = fopen(fileName,"rb");
  if (!f)
  {
    fprintf(stderr,"Warning: could not open file %s\n",fileName);
    return;
  }
  int warned = 0;

  fseek(f,0,SEEK_END);
  long size = ftell(f);
  fseek(f,0,SEEK_SET);
  char* result = NULL;
  int result_pos = 0;
  result = (char*)malloc(2*size+10); // This should definitely be enough
 
  int line = 0;
  while (!feof(f))
  {
    if (!fgets(line_read_buffer,MAX_LINE_LENGTH-1,f))
      break;
    line++;
    int i,length=strlen(line_read_buffer);
    for (i=0;i<length;i++)
    {
      if (line_read_buffer[i] == '\t')
      {
        warned=1;
        nr_warnings_reported++;
        START_WARN;
        fprintf(stderr,"found a tab:\n");
        ShowCharacter(i, 1);
        nr_warnings_fixed++;
        result[result_pos++] = ' ';
        result[result_pos++] = ' ';
      }
      else
      {
        result[result_pos++] = line_read_buffer[i];
      }
    }
  }
  fclose(f);
  // Now fix, if so required
  if (warned && fix_mode)
  {
    FILE*f=fopen(fileName,"wb");
    fwrite(result,1,result_pos,f);
    fclose(f);
  }
  free(result);
}

void CheckFileForWhiteSpacesAtEol(char* fileName)
{
  FILE*f = fopen(fileName,"rb");
  if (!f)
  {
    fprintf(stderr,"Warning: could not open file %s\n",fileName);
    return;
  }
  int warned = 0;

  fseek(f,0,SEEK_END);
  long size = ftell(f);
  fseek(f,0,SEEK_SET);
  char* result = NULL;
  int result_pos = 0;
  result = (char*)malloc(2*size+10); // This should definitely be enough
 
  int line = 0;
  while (!feof(f))
  {
    if (!fgets(line_read_buffer,MAX_LINE_LENGTH-1,f))
      break;
    line++;
    int i,length=strlen(line_read_buffer);
    for (i=0;i<length;i++)
    {
      result[result_pos++] = line_read_buffer[i];
    }
    int j = length-1;
    while (j > 0 && (line_read_buffer[j] == '\r' || line_read_buffer[j] == '\n')) j--;
 
    if (j > 0 && isspace(line_read_buffer[j]))
    {
      warned=1;
      nr_warnings_reported++;
      START_WARN;
      fprintf(stderr,"line ends with space:\n");
      ShowCharacter(j, 1);

      // Now fix it
      i=j;
      while (i>0 && isspace(line_read_buffer[i])) i--;

      int nr_spaces_to_remove = j-i;
      int nr_eol_chars = (length-1)-j;
 
      result_pos = result_pos - (nr_spaces_to_remove+nr_eol_chars);
      for (i=0;i<nr_eol_chars;i++)
      {
        result[result_pos] = result[result_pos+nr_spaces_to_remove];
        result_pos++;
      }
      nr_warnings_fixed++;
    }
  }
  fclose(f);
  // Now fix, if so required
  if (warned && fix_mode)
  {
    FILE*f=fopen(fileName,"wb");
    fwrite(result,1,result_pos,f);
    fclose(f);
  }
  free(result);
}


void CheckFile(char* fileName)
{
  CheckFileForTabs(fileName);
  CheckFileForWhiteSpacesAtEol(fileName);
}

void PrintBanner()
{
  printf("Codereviewer, part of Yacas. This tool does a static analysis of\n");
  printf("code to check that coding standards are adhered to.\n\n");
  printf(GPL_blurb_nohelp);
}

void PrintUsage(char* exeName)
{
  printf("Usage: %s [jcf] [list of files]\n",exeName);
  printf("The tool will run over the files provided and do its work, one at a time.\n");
  printf("The first argument is a list of command flags:\n");
  printf("j - assume this is a Java source file\n");
  printf("c - assume this is a C or C++ source file\n");
  printf("f - try to fix if possible (otherwise this tool just checks)\n");
}

int main(int argc, char** argv)
{
  int i;
  PrintBanner();
  if (argc<2)
  {
    PrintUsage(argv[0]);
    return 0;
  }
  {
    int nr=strlen(argv[1]);
    for (i=0;i<nr;i++)
    {
      switch (argv[1][i])
      {
      case 'j': java_mode=1;break;
      case 'c': c_mode   =1;break;
      case 'f': fix_mode =1;break;
      }
    }
  }
  for (i=2;i<argc;i++)
  {
    CheckFile(argv[i]);
  }
 
  printf("%d warnings generated.\n",nr_warnings_reported);
  if (fix_mode) printf("%d warnings fixed.\n",nr_warnings_fixed);
  return 0;
}

