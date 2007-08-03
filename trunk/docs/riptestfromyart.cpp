
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char* indexOf(char* haystack, char* needle)
{
  int pos = 0;
  int needleLength = strlen(needle);
  while (haystack[pos])
  {
    if (haystack[pos] == needle[0])
    {
      if (!strncmp(&haystack[pos],needle,needleLength))
      {
        return &haystack[pos];
      }
    }
    pos++;
  }
  return NULL;
}

int main(int argc, char** argv)
{
  if (argc<3)
    exit(-1);
  char* inName  = argv[1];
  char* outName = argv[2];

  char* inbuffer = NULL;

  FILE* fin = fopen(inName,"rb");
  if (!fin)
  {
    fprintf(stderr,"Could not open file %s for reading\n",inName);
    exit(-1);
  }
  fseek(fin,0,SEEK_END);
  int size = ftell(fin);
  fseek(fin,0,SEEK_SET);
  inbuffer = (char*)malloc(size+1);
  fread(inbuffer,1,size,fin);
  inbuffer[size] = '\0';
  fclose(fin);
  
  FILE* fout=fopen(outName,"wb");
  if (!fout)
  {
    fprintf(stderr,"Could not open file %s for writing\n",outName);
    exit(-1);
  }
  
  
  char* pos;
  
  pos = indexOf(inbuffer,"{{code:");
  while (pos != NULL)
  {
    char* start = pos+7;
    char* end = indexOf(start,":code}}");
    char c = *end;
    *end = '\0';
    //printf("%s",start);
    fprintf(fout,"%s",start);
    *end = c;
    pos = indexOf(end+7,"{{code:");
  }

  pos = indexOf(inbuffer,"{{test:");
  while (pos != NULL)
  {
    char* start = pos+7;
    char* end = indexOf(start,":test}}");
    char c = *end;
    *end = '\0';
//    printf("%s",start);
    fprintf(fout,"%s",start);
    *end = c;
    pos = indexOf(end+7,"{{test:");
  }



  free(inbuffer);
  fclose(fout);
  
  return 0;
}
