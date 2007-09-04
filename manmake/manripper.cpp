
#include <stdio.h>
#include <ctype.h>
#include <string>
#include <map>

using namespace std;

typedef map<string,string> CmdEntries;


CmdEntries commands;
CmdEntries files;

int fileline=0;
void GetBf(char* buf, int size, FILE*f)
{
  fgets(buf,2048,f);
  fileline++;
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
  fprintf(stderr,"\tFILE %s\n",fname);
  FILE*f = fopen(fname,"r");
  if (!f) return;
  int prevline = fileline;
  fileline = 0;
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
        files[cmd] = fname;

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
      char cmd[256];
      while (buf[0] == '\t')
      {
        if (buf[strlen(buf)-1] == '\n') buf[strlen(buf)-1] = '\0';
        char* ptr = &buf[1];
        while (isalnum(*ptr) || *ptr == '\'') ptr++;
        memset(cmd,0,255);
        memcpy(cmd,&buf[1],ptr-(&buf[1]));
        
        if (cmd[0] && strncmp(&buf[1],"In>",3) && strncmp(&buf[1],"Out>",4) && isupper(buf[1]))
        {
          CmdEntries::const_iterator iter = commands.find(cmd);
          const char* desc = "";
          if (iter != commands.end())
          {
            desc = iter->second.c_str();
          }
          else
          {
            fprintf(stderr,"WARNING: function %s does not have a short description (%s:%d)\n",cmd,fname,fileline);
          }
          printf(":%s:%s:%s:\n",cmd,&buf[1],desc);
          commands[cmd] = "";
        }
        GetBf(buf,2048,f);
        while (buf[0] == '\0') GetBf(buf,2048,f);
      }
      if (buf[0] == ' ')
      {
        fprintf(stderr,"WARNING: spaces might have been used in stead of tabs (%s, %s:%d)\n",cmd,fname,fileline);
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
  fileline = prevline;
}

int main(int argc, char** argv)
{
  int i;
  for (i=1;i<argc;i++)
  {
    fprintf(stderr,"BOOK %s\n",argv[i]);
    ProcessFile(argv[i]);
  }
  CmdEntries::const_iterator iter = commands.begin();
  while (iter != commands.end())
  {
    if (iter->second.c_str()[0])
    {
      CmdEntries::const_iterator fiter = files.find(iter->first.c_str());
      if (fiter != files.end())
        fprintf(stderr,"WARNING: no call sequences for function %s (%s)\n",iter->first.c_str(),fiter->second.c_str());
      else
        fprintf(stderr,"WARNING: no call sequences for function %s\n",iter->first.c_str());
    }
    iter++;
  }


}
