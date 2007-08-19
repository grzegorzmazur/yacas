
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

char lastKey[128];
char lastVal[16384];
char lastDes[16384];
char escaped[16384];
char* escape(char* text)
{
  char* trg = escaped;
  while (*text)
  {
    if (*text == '\'')
    {
      *trg++ = '\\';
    }
    *trg++ = *text++;
  }
  *trg = '\0';

  return escaped;
}
FILE*fout;
int first = 1;
void WriteLine()
{
  if (lastKey[0])
  {
    if (!first)
    {
      fprintf(fout,",\n");
    }
    first = 0;
    fprintf(fout,"\'%s\',\n",lastKey);
    fprintf(fout,"\'%s",escape(lastVal));
    fprintf(fout,"%s\'\n",escape(lastDes));
    lastKey[0] = '\0';
    lastVal[0] = '\0';
    lastDes[0] = '\0';
  }
}

int main(int argc, char** argv)
{
  lastKey[0] = '\0';
  lastVal[0] = '\0';
  lastDes[0] = '\0';
  char* inName = "hints.txt";
  char* outName = "autocompleter.js";
  if (argc>2)
  {
    inName = argv[1];
    outName = argv[2];
  }
  FILE*fin=fopen(inName,"rb");
  if (!fin)exit(-1);
  fout=fopen(outName,"wb");
  if (!fout)exit(-1);

  {
    FILE* fin2 = fopen("autocompleterheader.txt","rb");
    if (!fin2)
      exit(-1);
    fseek(fin2,0,SEEK_END);
    int size = ftell(fin2);
    fseek(fin2,0,SEEK_SET);
    char* inbuffer = (char*)malloc(size);
    fread(inbuffer,1,size,fin2);
    fwrite(inbuffer,1,size,fout);
    free(inbuffer);
    fclose(fin2);
  }

  fprintf(fout,"var hints = new Array(\n");

  char buffer[16384];
  fgets(buffer,16384,fin);
  while (!feof(fin))
  {
    char *items[10];
    int nrItems=0;
    char* ptr = buffer;
    items[nrItems++] = ptr;
    while (*ptr)
    {
      while (*ptr && *ptr != ':') ptr++;
      if (*ptr)
      {
        *ptr++ = '\0';
        items[nrItems++] = ptr;    
      }
    }
    if (nrItems>3)
    {
      if (strcmp(lastKey,items[1]))
      {
        WriteLine();
        strcpy(lastKey,items[1]);
        strcpy(lastVal,"");
        if (!strncmp(lastKey,items[2],strlen(lastKey)))
        {
          strcat(lastVal,"<b>");
          strcat(lastVal,lastKey);
          strcat(lastVal,"</b>");
          strcat(lastVal,&items[2][strlen(lastKey)]);
        }
        else
        {
          strcat(lastVal,items[2]);
        }
        strcat(lastVal,"<br />");
        strcpy(lastDes,items[3]);
      }
      else
      {
        if (!strncmp(lastKey,items[2],strlen(lastKey)))
        {
          strcat(lastVal,"<b>");
          strcat(lastVal,lastKey);
          strcat(lastVal,"</b>");
          strcat(lastVal,&items[2][strlen(lastKey)]);
        }
        else
        {
          strcat(lastVal,items[2]);
        }
        strcat(lastVal,"</br>");
        strcat(lastDes,items[3]);
      }
    }
    fgets(buffer,16384,fin);
  } 
  WriteLine();

  fclose(fin);
  
  fprintf(fout,");\n");

  fclose(fout);
	return 0;
}


