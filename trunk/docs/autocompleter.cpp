
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

  fprintf(fout,"var prevOnLoadAutocompl = window.onload;\n");
  fprintf(fout,"window.onload = initPage;\n");
  fprintf(fout,"function initPage()\n");
  fprintf(fout,"{\n");
  fprintf(fout,"  if (prevOnLoadAutocompl) prevOnLoadAutocompl();\n");
  fprintf(fout,"  if (document.getElementById(\"funcLookup\")) document.getElementById(\"funcLookup\").onkeyup = searchSuggest;\n");
  fprintf(fout,"}\n");
  fprintf(fout,"function searchSuggest()\n");
  fprintf(fout,"{\n");
  fprintf(fout,"  var searchString = this.value;\n");
  fprintf(fout,"  updateHints(searchString)\n");
  fprintf(fout,"}\n");
  fprintf(fout,"function updateHints(searchString)\n");
  fprintf(fout,"{\n");
  fprintf(fout,"  var popups = document.getElementById(\"popups\");\n");
  fprintf(fout,"  popups.innerHTML = \"\";\n");
  fprintf(fout,"  if (searchString != \"\")\n");
  fprintf(fout,"  {\n");
  fprintf(fout,"    var match1 = \"\";\n");
  fprintf(fout,"    var match2 = \"\";\n");
  fprintf(fout,"    var match3 = \"\";\n");
  fprintf(fout,"    var lwr = searchString.toLowerCase();\n");
  fprintf(fout,"    for (var i=0;i<hints.length;i=i+2)\n");
  fprintf(fout,"    {\n");
  fprintf(fout,"      var line = '<div class=\"suggestions\">'+hints[i+1]+'<\\/div>';\n");
  fprintf(fout,"      if (hints[i].indexOf(searchString) == 0)\n");
  fprintf(fout,"      {\n");
  fprintf(fout,"        match1 = match1 + line;\n");
  fprintf(fout,"      }\n");
  fprintf(fout,"      else if (hints[i].indexOf(searchString) > 0)\n");
  fprintf(fout,"      {\n");
  fprintf(fout,"        match2 = match2 + line;\n");
  fprintf(fout,"      }\n");
  fprintf(fout,"      else if (hints[i].toLowerCase().indexOf(lwr) > -1)\n");
  fprintf(fout,"      {\n");
  fprintf(fout,"        match3 = match3 + line;\n");
  fprintf(fout,"      }\n");
  fprintf(fout,"    }\n");
  fprintf(fout,"    popups.innerHTML = match1+match2+match3;\n");
  fprintf(fout,"  }\n");
  fprintf(fout,"}\n");
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


