
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <string>

std::string lastKey;
std::string lastVal;
std::string lastDes;

std::string escape(const std::string& text)
{
    std::string r;
    for (std::string::const_iterator i = text.begin(); i != text.end(); ++i) {
        if (*i == '\'')
            r += '\\';
        r += *i;
    }

    return r;
}

FILE*fout;
int first = 1;

struct Example
{
    std::string name;
    std::string example;
};

Example examples[1024];
int nrExamples = 0;

int exampleCompare(const void *v1, const void *v2)
{
  Example* e1 = (Example*)v1;
  Example* e2 = (Example*)v2;
  return strcmp(e1->name.c_str(), e2->name.c_str());
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

const char* findexample(const char* name)
{
  Example key;
  key.name = name;
  void *found = bsearch(&key, examples, nrExamples, sizeof(Example),exampleCompare);
  if (found)
      return ((Example*) found)->example.c_str();
  return "";
}

void WriteLine()
{
  if (!lastKey.empty())
  {
    if (!first)
    {
      fprintf(fout,",\n");
    }
    first = 0;
    fprintf(fout,"\'%s\',\n",escape(lastKey).c_str());
    fprintf(fout,"\'<ul>%s</ul>\',\n",escape(lastVal).c_str());
    fprintf(fout,"\'%s\',\n",escape(lastDes).c_str());
    fprintf(fout,"\'%s\'\n",findexample(lastKey.c_str())); // placeholder for examples
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
  const char* inName = "hints.txt";
  const char* outName = "autocompleter.js";
  if (argc>2)
  {
    inName = argv[1];
    outName = argv[2];
  }
  {
    FILE* fex = fopen("examples-static.txt","rb");
    if (!fex)
      exit(-1);

    while (fgets(buffer,16384,fex) && !feof(fex))
    {
      getfields(buffer);
      if (nrItems>1)
      {
        examples[nrExamples].name = items[0];
        examples[nrExamples].example = items[1];
        nrExamples++;
      }
    } 
    fclose(fex);
    qsort(examples, nrExamples, sizeof(Example),exampleCompare);
    {
      int i;
      for (i=0;i<nrExamples-1;i++)
      {
          if (!strcmp(examples[i].name.c_str(), examples[i+1].name.c_str()))
          {
              printf("ERROR generating autocompleter array: duplicate entries found for %s\n",examples[i].name.c_str());
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

  while (fgets(buffer,16384,fin) && !feof(fin))
  {
    getfields(buffer);
    if (nrItems>3)
    {
      if (strcmp(lastKey.c_str(), items[1]))
      {
        WriteLine();
        lastKey = items[1];
        lastVal = "";
        if (!strncmp(lastKey.c_str(),items[2],lastKey.length()))
        {
          lastVal += "<li><b>";
          lastVal += lastKey;
          lastVal += "</b>";
          lastVal += &items[2][lastKey.length()];
        }
        else
        {
          lastVal += items[2];
        }
        lastDes = items[3];
      }
      else
      {
//        strcat(lastVal,", \\n");
        if (!strncmp(lastKey.c_str(),items[2],lastKey.length()))
        {
          lastVal += "<li><b>";
          lastVal += lastKey;
          lastVal += "</b>";
          lastVal += &items[2][lastKey.length()];
        }
        else
        {
          lastVal += items[2];
        }
        lastDes += items[3];
      }
    }
  } 
  WriteLine();

  fclose(fin);
  
  fprintf(fout,");\n");

  fclose(fout);
	return 0;
}


