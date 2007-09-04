
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

struct Example
{
  char name[32];
  char example[512];
};

Example examples[1024];
int nrExamples = 0;

int exampleCompare(const void *v1, const void *v2)
{
  Example* e1 = (Example*)v1;
  Example* e2 = (Example*)v2;
  return strcmp(e1->name,e2->name);
}

char *items[10];
int nrItems=0;
void getfields(char* buffer)
{
  nrItems=0;
  char* ptr = buffer;
  items[nrItems++] = ptr;
  while (*ptr)
  {
    while (*ptr && *ptr != ':') 
    {
      // Use backslash for escaping for example ':' characters
      if (*ptr == '\\')
        ptr++;
      ptr++;
    }
    if (*ptr)
    {
      *ptr++ = '\0';
      items[nrItems++] = ptr;    
    }
  }
}

char* findexample(char* name)
{
  Example key;
  strcpy(key.name,name);
  void *found = bsearch(&key, examples, nrExamples, sizeof(Example),exampleCompare);
  if (found)
    return ((Example*) found)->example;
  return "";
}

void WriteLine()
{
  if (lastKey[0])
  {
    if (!first)
    {
      fprintf(fout,",\n");
    }
    first = 0;
    fprintf(fout,"\'%s\',\n",escape(lastKey));
    fprintf(fout,"\'<ul>%s</ul>\',\n",escape(lastVal));
    fprintf(fout,"\'%s\',\n",escape(lastDes));
    fprintf(fout,"\'%s\'\n",findexample(lastKey)); // placeholder for examples
    lastKey[0] = '\0';
    lastVal[0] = '\0';
    lastDes[0] = '\0';
  }
}


int main(int argc, char** argv)
{
  char buffer[16384];
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
  {
    FILE* fex = fopen("examples-static.txt","rb");
    if (!fex)
      exit(-1);
    fgets(buffer,16384,fex);
    while (!feof(fex))
    {
      getfields(buffer);
      if (nrItems>1)
      {
        strcpy(examples[nrExamples].name,items[0]);
        strcpy(examples[nrExamples].example,items[1]);
        nrExamples++;
      }
      fgets(buffer,16384,fex);
    } 
    fclose(fex);
    qsort(examples, nrExamples, sizeof(Example),exampleCompare);
    {
      int i;
      for (i=0;i<nrExamples-1;i++)
      {
        if (!strcmp(examples[i].name,examples[i+1].name))
        {
          printf("ERROR generating autocompleter array: duplicate entries found for %s\n",examples[i].name);
          exit(-1);
        }
      }
    }
  }

  FILE*fin=fopen(inName,"rb");
  if (!fin)exit(-1);
  fout=fopen(outName,"wb");
  if (!fout)exit(-1);

  fprintf(fout,"var hints = new Array(\n");

  fgets(buffer,16384,fin);
  while (!feof(fin))
  {
    getfields(buffer);
    if (nrItems>3)
    {
      if (strcmp(lastKey,items[1]))
      {
        WriteLine();
        strcpy(lastKey,items[1]);
        strcpy(lastVal,"");
        if (!strncmp(lastKey,items[2],strlen(lastKey)))
        {
          strcat(lastVal,"<li><b>");
          strcat(lastVal,lastKey);
          strcat(lastVal,"</b>");
          strcat(lastVal,&items[2][strlen(lastKey)]);
        }
        else
        {
          strcat(lastVal,items[2]);
        }
        strcpy(lastDes,items[3]);
      }
      else
      {
//        strcat(lastVal,", \\n");
        if (!strncmp(lastKey,items[2],strlen(lastKey)))
        {
          strcat(lastVal,"<li><b>");
          strcat(lastVal,lastKey);
          strcat(lastVal,"</b>");
          strcat(lastVal,&items[2][strlen(lastKey)]);
        }
        else
        {
          strcat(lastVal,items[2]);
        }
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


