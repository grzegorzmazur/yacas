
#include <stdio.h>
#include <ctype.h>
#include <string>
#include <map>

using namespace std;
struct CallEntry
{
  string command;
  string call;
  string description;
};

typedef map<string,string> CmdEntries;


CmdEntries commands;

void GetBf(char* buf, int size, FILE*f)
{
  fgets(buf,2048,f);
  if (buf[strlen(buf)-1] == '\n') buf[strlen(buf)-1] = '\0';
  char* ptr=buf;
  while (*ptr != '\0')
  {
    if (*ptr == ':') *ptr = ' ';
    ptr++;
  }
}

void ProcessFile(char* fname)
{
fprintf(stderr,"FILE %s\n",fname);

  FILE*f = fopen(fname,"r");
  if (!f) return;
  char buf[2048];
  while (!feof(f))
  {
    GetBf(buf,2048,f);
    if (buf[strlen(buf)-1] == '\n') buf[strlen(buf)-1] = '\0';

    //*CMD Sin, Cos, Tan --- trigonometric functions
    if (!strncmp(buf,"*CMD",4))
    {
      char* description=buf;
      while (*description != '\0' && *description != '-') description++;
      if (*description == '\0') continue;
      while (*description == '-') description++;
      while (*description == ' ') description++;
      
      char* command = &buf[5];
      for(;;)
      {
        char* end = command;
        while (*end != ',' && *end != ' ') end++;
        char cmd[256];
        memset(cmd,0,255);
        memcpy(cmd,command,end-command);
//        cmd[end-command] = '\0';

        commands[cmd] = description;

//printf("COMMAND [%s], description [%s]\n",cmd,description);
        if (*end == ' ') break;
        end++;
        if (*end == ' ') end++;
        command = end;
      }
    }
    else if (!strncmp(buf,"*CALL",5))
    {
      while (buf[0] != '\t')
        GetBf(buf,2048,f);
      while (buf[0] == '\t')
      {
        if (buf[strlen(buf)-1] == '\n') buf[strlen(buf)-1] = '\0';
        char* ptr = &buf[1];
        while (isalnum(*ptr)) ptr++;
        char cmd[256];
        memset(cmd,0,255);
        memcpy(cmd,&buf[1],ptr-(&buf[1]));
        
        if (cmd[0] && strncmp(&buf[1],"In>",3) && strncmp(&buf[1],"Out>",4))
        {
          CmdEntries::const_iterator iter = commands.find(cmd);
          const char* desc = "";
          if (iter != commands.end())
          {
            desc = iter->second.c_str();
          }
  
          printf(":%s:%s:%s:\n",cmd,&buf[1],desc);
          commands[cmd] = "";
        }
        GetBf(buf,2048,f);
      }
    }
    else if (!strncmp(buf,"*INCLUDE",8))
    {
      char*fn = &buf[8];
      while (*fn == ' ') fn++;
      char fullname[512];
      snprintf(fullname,512,"%s.txt",fn);
//DEBUG printf("FILE %s\n",fullname);
      ProcessFile(fullname);

    }
  }
  fclose(f);
}

int main(int argc, char** argv)
{
  int i;
  for (i=1;i<argc;i++)
  {
    fprintf(stderr,"******* BOOK ******* %s\n",argv[i]);
    ProcessFile(argv[i]);
  }
}
