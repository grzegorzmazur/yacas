
/*
 * Example terminal client for the yacas Computer Algebra library.
 * It is heavily tailored to Unix (Linux), but you should be able
 * to easily make a version that links with libyacas.a and provides
 * an interface for a different platform.
 * The platform-dependent parts are readline.cpp (which maintains
 * a history for keyed-in expressions on the command line), and
 * the directories it looks in for input files.
 */

// Usage :
//   1) yacas
//      just runs yacas in interactive command line mode.
//   2) yacas <file>
//      executes file <file> and exits
//   3) yacas <options>
//      where options is of the form -<opt>. <opt> can be any
//      of the combinations of:
//      - d for returning the directory of the default scripts.
//      - v prints the version number
//      - f : treats stdin as one file, and executes the first
//            read statement only.
//      - p : plain mode. No fancy readline functionality.
//      - c : inhibits printing the prompt to the console
//      - t : follow history when entering on the command line
//      - w : hides the console window in Windows
//
// Example: 'yacas -pc' will use minimal command line interaction,
//          showing no prompts, and with no readline functionality.
//

#include "yacasprivate.h"

#include <stdio.h>
#ifndef WIN32
#include <dirent.h>
#include <sys/stat.h>
#endif

#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif

#ifdef WIN32
  #define PATH_SEPARATOR '\\'
#else
  #define PATH_SEPARATOR '/'
#endif

#include "yacas.h"

#ifndef WIN32
  #include "unixcommandline.h"
  #define FANCY_COMMAND_LINE CUnixCommandLine
  /* PLATFORM_OS is defined in config.h */
#else  
  #define _WINSOCKAPI_            // Prevent inclusion of winsock.h in windows.h 
  #define _WIN32_WINDOWS 0x0410      // Make sure that Waitable Timer functions are declared in winbase.h 
  #include "win32commandline.h"      
  #define FANCY_COMMAND_LINE CWin32CommandLine
  #define SCRIPT_DIR ""
  #define PLATFORM_OS "Win32"
#endif

#include "stdcommandline.h"
#include "standard.h"
#include "numbers.h"
#include "ramdisk.h" //TODO keep this?
#include "arggetter.h"

//TODO time.h and errors.h are only needed for LispTime
#include <time.h>
#include "errors.h"


#ifndef VERSION
//#define VERSION "Windows latest"
#include "version.h"
#endif  //VERSION

//#define PROMPT_SHOW_FREE_MEMORY

#ifdef SUPPORT_SERVER
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

#ifndef WIN32
  #include <sys/wait.h>
  #include <sys/socket.h>
  #include <netinet/in.h>
  #include <arpa/inet.h>
  #include <unistd.h>
  #include <sys/ioctl.h>
#else
  #include <winsock2.h>
#endif

#define SOCKLEN_T unsigned int //socklen_t
#endif

#include "GPL_stuff.h"

CYacas* yacas=NULL;
CCommandLine *commandline = NULL;
int use_stdin = 0;
int use_plain = 0;
int show_prompt = 1;
int trace_history = 0;
int use_texmacs_out = 0;
#ifdef YACAS_DEBUG
  int verbose_debug=0;
#endif
int patchload=0;
int winsockinitialised=0;
int hideconsolewindow=0;
char* root_dir    = SCRIPT_DIR;
#ifndef WIN32
  char* archive     = NULL;
#else
  char* archive     = "scripts.dat";
  HANDLE htimer = 0;
#endif
char* init_script = "yacasinit.ys";

char* read_eval_print = "REP()";


static int readmode = 0;

int compressed_archive = 1;

int server_mode = 0;
int server_port = 9734;

//TODO global!!!
static LispBoolean busy=LispTrue;
static LispBoolean restart=LispFalse;

static LispBoolean Busy()
{
  return busy;
}


  
void ReportNrCurrent()
{
#ifdef YACAS_DEBUG
  if (verbose_debug)
  {
    extern long theNrCurrent;
    extern long theNrConstructed;
    extern long theNrDestructed;
    extern long theNrTokens;
    extern long theNrDefinedBuiltIn;
    extern long theNrDefinedUser;
    printf("left-over: %ld objects\n",theNrCurrent);
    printf("%ld constructed, %ld destructed\n",theNrConstructed,theNrDestructed);
    printf("nr tokens: %ld \n",theNrTokens);
    printf("-------------------------------\n");
    printf("Total %ld functions defined (%ld built-in, %ld user)\n",
           theNrDefinedBuiltIn+theNrDefinedUser,
           theNrDefinedBuiltIn,theNrDefinedUser);
  }
#endif
}




#define RESULT aEnvironment.iStack.GetElement(aStackTop)
#define ARGUMENT(i) aEnvironment.iStack.GetElement(aStackTop+i)
void LispPlatformOS(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  RESULT.Set(LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUp("\"" PLATFORM_OS "\"")));
}

void LispExit(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    busy=LispFalse;
    InternalTrue(aEnvironment, RESULT);
}

void LispExitRequested(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  if (!busy)
    InternalTrue(aEnvironment, RESULT);
  else
    InternalFalse(aEnvironment, RESULT);
}


unsigned char *the_first_stack_var;

void LispStackSize(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispChar buf[30];
    InternalIntToAscii(buf, (int)(the_first_stack_var-(unsigned char*)&buf[0]));
    RESULT.Set(LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUp(buf)));
}


char* ReadInputString(char* prompt)
{
  char *inpline;
REDO:
  readmode = 1;
  commandline->ReadLine(prompt);
  readmode = 0;
  inpline =  commandline->iLine.String();

  if (inpline)
  {
    if(*inpline)
    {
      if (inpline[0] == '?')
      {
        if (inpline[1] == '?')
        {
          yacas->Evaluate("Help()");
          goto REDO;
        }
        else
        {
          if (strlen(&inpline[1]) < 100)
          {
            char buf[120];
            sprintf(buf,"Help(\"%s\")",&inpline[1]);
            yacas->Evaluate(buf);
            goto REDO;
          }
        }
      }
      else
      {
        if (!strncmp(inpline,"restart",7))
        {
          restart=LispTrue;
          busy=LispFalse;
        }
        else if (!strncmp(inpline,"quit",4))
        {
          busy=LispFalse;
        }
      }
    }
  }
  if (inpline)
    if(*inpline)
      return inpline;
  return "True";
}

static void LispReadCmdLineString(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispPtr promptObject;
    promptObject.Set(ARGUMENT(1).Get());
    CHK_ISSTRING_CORE(promptObject,1);
    LispString prompt;
    InternalUnstringify(prompt, promptObject.Get()->String());
    ReadInputString(prompt.String());
/*TODO remove
    readmode = 1;
    commandline->ReadLine(prompt.String());
    readmode = 0;
*/
    char *output =  commandline->iLine.String();
    RESULT.Set(LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUpStringify(output)));
}

static void LispHistorySize(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    /* Obtain arguments passed in. */
    ShortIntegerArgument(depth, 1);
    commandline->MaxHistoryLinesSaved(depth);

    /* Return result. */
    InternalTrue(aEnvironment,RESULT);
}

void LispTime(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    clock_t starttime = clock();
    LispPtr res;
    aEnvironment.iEvaluator->Eval(aEnvironment, res, ARGUMENT(1));
    clock_t endtime = clock();
    double timeDiff;

//    printf("%ld to %ld\n",starttime,endtime);
    timeDiff = endtime-starttime;
    timeDiff /= CLOCKS_PER_SEC;
    char buf[100];
    sprintf(buf,"%g",timeDiff);
    RESULT.Set(LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUp(buf)));
}


void LispIsPromptShown(LispEnvironment& aEnvironment,LispInt aStackTop)
{ // this function must access show_prompt which is a *global* in yacasmain.cpp, so it's not possible to put this function in mathcommands.cpp
    InternalBoolean(aEnvironment,RESULT, show_prompt==1);
}


void my_exit(void)
{
    if (show_prompt)
        printf("Quitting...\n");
    delete yacas;
    delete commandline;
    ReportNrCurrent();
#ifdef YACAS_DEBUG
    YacasCheckMemory();
#endif

#ifdef WIN32
  #ifdef SUPPORT_SERVER
    if (winsockinitialised)
    {
        WSACleanup();
    }
  #endif
#endif
}

#ifdef PROMPT_SHOW_FREE_MEMORY
void build_full_prompt(char* full_prompt, const char* prompt, const int maxlen)
{
    if (!full_prompt) return;
    if (!prompt) prompt = "";
    FILE* meminfo;
    meminfo = fopen("/proc/meminfo", "r");
    if (meminfo)
    {
        char entry[20];
        unsigned int current_pos;
        int current_char;
        bool entry_read;
        int entry_countdown = -1;
        while (!feof(meminfo) && entry_countdown != 0)
        {
            entry_read = false;
            current_pos = 0;
            while (!entry_read && !feof(meminfo))
            {
                current_char = fgetc(meminfo);
                switch (current_char)
                {
                  case ' ':
                  case '\n':
                  case '\r':
                  case '\t':
                  case EOF:
                    if (current_pos > 0)
                      entry_read = true;
                    break;
                  default:
                    entry[current_pos] = (char) current_char;
                    if (current_pos < 20 - 4)
                      ++current_pos;
                    break;
                }
            }
            entry[current_pos] = (char) 0;
            if (entry_read)
            {
                if (0 == strncmp("MemFree:", entry, 20))
                  entry_countdown = 1;
                else if (entry_countdown > 0) --entry_countdown;
            }
        }
        fclose(meminfo);
        meminfo = (FILE*) NULL;
        if (strlen(entry) + strlen(prompt) + 5 < maxlen)
          sprintf(full_prompt, "%sk %s", entry, prompt);
        else
        {
            strncpy(full_prompt, prompt, maxlen);
            full_prompt[maxlen-1] = (char) 0;
        }
    }
    else
    {
        strncpy(full_prompt, prompt, maxlen);
        full_prompt[maxlen-1] = (char) 0;
    }
}
#endif


#define TEXMACS_DATA_BEGIN   ((char)2)
#define TEXMACS_DATA_END     ((char)5)
#define TEXMACS_DATA_ESCAPE  ((char)27)

void ShowResult(char *prompt)
{
    if (use_texmacs_out)
    {
//        printf("%cverbatim:",TEXMACS_DATA_BEGIN);
        printf("%clatex:",TEXMACS_DATA_BEGIN);
    }

    if (yacas->IsError())
    {
        printf("%s\n",yacas->Error());
    }
    else
    {
        if ((*yacas)()().PrettyPrinter()==NULL)
            printf("%s%s\n",prompt,yacas->Result());
    }
    if (use_texmacs_out)
    {
        printf("%c",TEXMACS_DATA_END);
    }
    fflush(stdout);
}

void DeclarePath(char *ptr2)
{
  char buf[1000];
  if (ptr2[strlen(ptr2)-1] != PATH_SEPARATOR)
    sprintf(buf,"DefaultDirectory(\"%s%c\");",ptr2,PATH_SEPARATOR);
  else
    sprintf(buf,"DefaultDirectory(\"%s\");",ptr2);
  yacas->Evaluate(buf);
}

void LoadYacas(LispOutput* aOutput=NULL)
{
  busy=LispTrue;
  restart=LispFalse;

  if (aOutput)
    yacas = CYacas::NewL(aOutput);
  else
    yacas = CYacas::NewL();


#define CORE_KERNEL_FUNCTION(iname,fname,nrargs,flags) (*yacas)()().SetCommand(fname,iname,nrargs,flags);

CORE_KERNEL_FUNCTION("OSVersion",LispPlatformOS,0,YacasEvaluator::Function | YacasEvaluator::Fixed)
CORE_KERNEL_FUNCTION("Exit",LispExit,0,YacasEvaluator::Function | YacasEvaluator::Fixed)
CORE_KERNEL_FUNCTION("IsExitRequested",LispExitRequested,0,YacasEvaluator::Function | YacasEvaluator::Fixed)
CORE_KERNEL_FUNCTION("HistorySize",LispHistorySize,1,YacasEvaluator::Function | YacasEvaluator::Fixed)
CORE_KERNEL_FUNCTION("StaSiz",LispStackSize,0,YacasEvaluator::Function | YacasEvaluator::Fixed)
CORE_KERNEL_FUNCTION("IsPromptShown",LispIsPromptShown,0,YacasEvaluator::Function | YacasEvaluator::Fixed)
CORE_KERNEL_FUNCTION("ReadCmdLineString",LispReadCmdLineString,1,YacasEvaluator::Function | YacasEvaluator::Fixed)
CORE_KERNEL_FUNCTION("GetTime",LispTime,1,YacasEvaluator::Macro | YacasEvaluator::Fixed)

#undef CORE_KERNEL_FUNCTION
/*TODO remove
    (*yacas)()().Commands().SetAssociation(LispEvaluator(LispPlatformOS),(*yacas)()().HashTable().LookUp("OSVersion"));
    (*yacas)()().Commands().SetAssociation(LispEvaluator(LispExit),(*yacas)()().HashTable().LookUp("Exit"));
   // this function is declared in yacasapi.cpp rather than here
    (*yacas)()().Commands().SetAssociation(LispEvaluator(LispHistorySize),(*yacas)()().HashTable().LookUp("HistorySize"));
    (*yacas)()().Commands().SetAssociation(LispEvaluator(LispStackSize),(*yacas)()().HashTable().LookUp("StaSiz"));
    (*yacas)()().Commands().SetAssociation(LispEvaluator(LispIsPromptShown),(*yacas)()().HashTable().LookUp("IsPromptShown"));
    (*yacas)()().Commands().SetAssociation(LispEvaluator(LispReadCmdLineString),(*yacas)()().HashTable().LookUp("ReadCmdLineString"));
    (*yacas)()().Commands().SetAssociation(LispEvaluator(LispTime),(*yacas)()().HashTable().LookUp("GetTime"));
*/    
    

    if (archive)
    {
        FILE*fin = fopen(archive,"rb");
        if (!fin)
        {
            printf("Error, could not open archive file %s\n",archive);
        }
        else
        {
            fseek(fin,0,SEEK_END);
            int fullsize = ftell(fin);
            fseek(fin,0,SEEK_SET);
            unsigned char* fullbuf = (unsigned char*)PlatAlloc(fullsize);
            if (fullbuf)
            {
                fread(fullbuf,1,fullsize,fin);
                CCompressedArchive *a =
                    NEW CCompressedArchive(fullbuf, fullsize, compressed_archive);
                if (a->iFiles.IsValid())
                {
                    (*yacas)()().iArchive = a;
                }
                else
                {
                    printf("Error, %s is not a valid archive file.\n",archive);
                    delete a;
                }
            }
            else
            {
                printf("Archive file %s too large, perhaps it is time we\nimplement disk-accessed compressed files.\n",archive);
            }
            fclose(fin);
        }
    }
    
    //TODO #include "../ramscripts/some.inc"

    /*TODO test code!!!
     (*yacas)()().iRamDisk.SetAssociation(
     LispRamFile("WriteString(\"Hello world!\");"),
     (*yacas)()().HashTable().LookUp("testfile",LispTrue)
                                        );
                                        */
    {
      /* Split up root_dir in pieces separated by colons, and run 
    DefaultDirectory on each of them. */
        char *ptr1, *ptr2;
   ptr1 = ptr2 = root_dir;
   while (*ptr1 != '\0') {
     while (*ptr1 != '\0' && *ptr1 != ':') ptr1++;
     if (*ptr1 == ':') {
       *ptr1 = '\0';
            DeclarePath(ptr2);
       ptr1++;
       ptr2 = ptr1;
     }
   }
        DeclarePath(ptr2);
        char buf[1000];
        sprintf(buf,"Load(\"%s\");",init_script);
        yacas->Evaluate(buf);
    }
    if (yacas->IsError())
        ShowResult("");

    /* renaming .cc to .cpp files (I know, doesn't belong here ;-) ) */
    /*
    {
        DIR *dp;
        struct dirent* entry;
        struct stat statbuf;
        char dir[256];
        strcpy(dir,"/root/myprojects/yacas-latest/src/");
        if ((dp = opendir(dir)) != NULL)
        {
            while ((entry = readdir(dp)) != NULL)
            {
                memset(&statbuf,0,sizeof(struct stat));
                stat(entry->d_name,&statbuf);
                if (S_ISDIR(statbuf.st_mode))
                    continue;
                char *ext;
                ext = strstr(entry->d_name,".cc");
                if (ext==NULL)
                    continue;
                if (strcmp(ext,".cc"))
                    continue;
                char dummy[256];
                strcpy(dummy,entry->d_name);
                strstr(dummy,".cc")[0] = '\0';
                char cmd[256];
                sprintf(cmd,"mv %s.cc %s.cpp",dummy,dummy);
                system(cmd);
            }
        }
    }
    return;
*/
    //Loading user addons
    if (use_texmacs_out)
    {
        printf("%cverbatim:",TEXMACS_DATA_BEGIN);
    }
#ifndef WIN32 //TODO fix, with the file scanner in ramscripts
    {
        DIR *dp;
        struct dirent* entry;
        struct stat statbuf;
        char dir[256];
        char cwd[256];
        sprintf(dir,"%saddons/",root_dir);

        if ((dp = opendir(dir)) != NULL)
        {
            {
                char ld[256];
                sprintf(ld,"DefaultDirectory(\"%saddons/\");",root_dir);
                yacas->Evaluate(ld);
            }
            getcwd(cwd,256);
            chdir(dir);
            while ((entry = readdir(dp)) != NULL)
            {
                memset(&statbuf,0,sizeof(struct stat));
                stat(entry->d_name,&statbuf);
                if (S_ISDIR(statbuf.st_mode))
                    continue;
                char *ext;
                ext = strstr(entry->d_name,".def");
                if (ext==NULL)
                    continue;
                if (strcmp(ext,".def"))
                    continue;
                char buf[512];
                char dummy[256];
                strcpy(dummy,entry->d_name);
                strstr(dummy,".def")[0] = '\0';
                if (show_prompt && !use_texmacs_out)
                    printf("[%s] ",dummy);
                sprintf(buf,"DefLoad(\"%s\");",dummy);
                yacas->Evaluate(buf);
            }
            closedir(dp);
            chdir(cwd);
            if (show_prompt)
                printf("\n");
        }
    }
#endif
    {
        char fname[256];
        sprintf(fname,"%s/.yacasrc",getenv("HOME"));
        FILE* test=fopen(fname,"r");
        if (test)
        {
            fclose(test);

            sprintf(fname,"Load(\"%s/.yacasrc\");",getenv("HOME"));
            yacas->Evaluate(fname);
        }
    }
    if (use_texmacs_out)
    {
        printf("%c",TEXMACS_DATA_END);
    }
    fflush(stdout);
}

#ifdef SIGHANDLER_NO_ARGS
void InterruptHandler(void)
#else
void InterruptHandler(int errupt)
#endif
{
    printf("^C pressed\n");
    (*yacas)()().iEvalDepth = (*yacas)()().iMaxEvalDepth+100;
    if (readmode)
        exit(0);
}

/*
void BusErrorHandler(int errupt)
{
#ifdef SIGBUS
  signal(SIGBUS,SIG_IGN);
#endif
#ifdef SIGBUS
  signal(SIGSEGV,SIG_IGN);
#endif

    printf("Bus error/segfault\n");
    (*yacas)()().iEvalDepth = (*yacas)()().iMaxEvalDepth+100;
}
*/

#ifdef SUPPORT_SERVER

CYacas* clientToStop = NULL;

#ifndef WIN32
#ifdef SIGHANDLER_NO_ARGS
void stopClient(void)
#else
void stopClient(int sig)
#endif
#else
VOID CALLBACK stopClient(LPVOID lpArgToCompletionRoutine, DWORD dwTimerLowValue, DWORD dwTimerHighValue)
#endif
{
  if (clientToStop)
  {
    (*clientToStop)()().iEvalDepth = (*clientToStop)()().iMaxEvalDepth+100;
  }
}

#define MAX_CONNECTIONS  10
#define BUFFER_CHUNKSIZE 256

int runserver(int argc,char** argv)
{
#ifdef WIN32
   if (hideconsolewindow)
   {
      // format a "unique" newWindowTitle
      char newWindowTitle[256];   
      wsprintf(newWindowTitle,"%d/%d", GetTickCount(), GetCurrentProcessId());
      // change current window title
      SetConsoleTitle(newWindowTitle);
      // ensure window title has been updated
      Sleep(40);
      // look for newWindowTitle
      HWND hwndFound = FindWindow(NULL, newWindowTitle);
      // If found, hide it
      if ( hwndFound != NULL)
      {
         ShowWindow( hwndFound, SW_HIDE);
      }
   }
#endif
   
   int server_sockfd, client_sockfd;
    SOCKLEN_T server_len, client_len;
    struct sockaddr_in server_address;
    struct sockaddr_in client_address;
    int result;
    int maxConnections;
    int nrSessions = 0;
    fd_set readfds, testfds;
    // LispString outStrings[MAX_CONNECTIONS];
   LispString outStrings;

    CYacas* used_clients[MAX_CONNECTIONS];
    int serverbusy;


    int seconds = 30; // give each calculation only so many calculations

    serverbusy=1;
    maxConnections=MAX_CONNECTIONS;
    nrSessions=0;

#ifndef WIN32
    signal(SIGPIPE,SIG_IGN);
#else
    WSADATA wsadata;

    if (WSAStartup(0x101, &wsadata))
    {  
       perror("YacasServer Could not initiate Winsock DLL\n");
       exit(1);
    }
    else
    {
       winsockinitialised = 1;
    }
#endif

    server_sockfd = socket(AF_INET, SOCK_STREAM, 0);
    {
        int rsp;
#ifndef WIN32
        if (setsockopt(server_sockfd,SOL_SOCKET,SO_REUSEADDR,(void*)&rsp,sizeof(int)))
#else
        if (setsockopt(server_sockfd,SOL_SOCKET,SO_REUSEADDR,(const char *)&rsp,sizeof(int)))
#endif
        {
            perror("YacasServer Could not set socket options\n");            
            exit(1);
        }
    }

    server_address.sin_family = AF_INET;
    server_address.sin_addr.s_addr = htonl(INADDR_ANY);
    server_address.sin_port = htons(server_port);
    server_len = sizeof(server_address);

    printf("Accepting requests from port %d\n",server_port);

    if (bind(server_sockfd, (struct sockaddr *)&server_address, server_len))
    {
        perror("YacasServer Could not bind to the socket\n");        
        exit(1);
    }
    if (listen(server_sockfd, maxConnections))
    {
        perror("YacasServer Could not listen to the socket\n");        
        exit(1);
    }
    
    {
      int i;
      for (i=0;i<MAX_CONNECTIONS;i++)
      {
        used_clients[i] = NULL;
      }
    }

    FD_ZERO(&readfds);
    FD_SET(server_sockfd, &readfds);
    while(serverbusy)
    {
        int fd;
        int nread;
        testfds = readfds;

        result = select(FD_SETSIZE, &testfds, (fd_set *)0, 
                        (fd_set *)0, (struct timeval *) 0);

        if(result < 1)
        {
            perror("server5");            
            exit(1);
        }
        
#ifndef WIN32
        int socketcount = FD_SETSIZE;
#else
        int socketcount = readfds.fd_count;
#endif        
        for(int sockindex = 0; sockindex < socketcount; sockindex++)
        {
#ifndef WIN32
            while(waitpid(-1,NULL,WNOHANG) > 0); /* clean up child processes */
            fd = sockindex;
#else
            fd = readfds.fd_array[sockindex];
#endif
            if(FD_ISSET(fd,&testfds))
            {
                if(fd == server_sockfd)
                {
                    client_len = sizeof(client_address);
#ifndef WIN32
                    client_sockfd = accept(server_sockfd, (struct sockaddr *)&client_address, (ACCEPT_TYPE_ARG3)&client_len);
#else
                    client_sockfd = accept(server_sockfd, (struct sockaddr *)&client_address, (int *)&client_len);
#endif

#ifdef __CYGWIN__
                    if (client_sockfd != 0xffffffff)
#endif
                    {
                        FD_SET(client_sockfd, &readfds);
#ifdef YACAS_DEBUG
                        printf("adding client on fd %d\n", client_sockfd);
#endif
                    }

                }
                else
                {
                    int clsockindex = sockindex - 1;
#ifndef WIN32
                    ioctl(fd, FIONREAD, &nread);
#else
                    ioctlsocket(fd, FIONREAD, (unsigned long *)&nread);
#endif
                    if(nread == 0)
                    {
#ifndef WIN32
                        close(fd);
#else   
                        closesocket(fd);
#endif
                        FD_CLR(fd, &readfds);
                        delete used_clients[clsockindex];
                        used_clients[clsockindex] = NULL;
                        nrSessions--;
#ifdef YACAS_DEBUG
                        printf("Removing client on %d\n",fd);
#endif
                    }
                    else
                    {
//                        LispString cmd;
//                        cmd.SetNrItems(0);
                        char* buffer = (char*)malloc(nread+1);
                        if (!buffer) 
                        {
                          printf("Error: seem to have run out of memory\n");                          
                          exit(-1);
                        }
                        
                        int finalbuffersize = BUFFER_CHUNKSIZE;
                        char* finalbuffer = (char*)malloc(finalbuffersize);
                        if (!finalbuffer) 
                        {
                          printf("Error: seem to have run out of memory\n");                          
                          exit(-1);
                        }                
                        
                        finalbuffer[0] = '\0';

                        int bytesread;
                        int totalbytesread = 0;
                        
#ifndef WIN32
                        while((bytesread = read(fd, buffer, nread)) != 0)
#else                        
                        while(bytesread = recv(fd, buffer, nread, 0))
#endif                   
	                     {
                           buffer[bytesread] = '\0';
                           totalbytesread += bytesread;
                           
                           if (totalbytesread >= finalbuffersize)
                           {
                              // If input string has exeeded the buffer size reallocate the buffer
                              finalbuffersize += BUFFER_CHUNKSIZE;
                              if (finalbuffersize < totalbytesread)
                                finalbuffersize = totalbytesread;
//                              finalbuffersize = max(totalbytesread, finalbuffersize + BUFFER_CHUNKSIZE);
                              char* newfinalbuffer = (char*)malloc(finalbuffersize);
                              if (!newfinalbuffer) 
                              {
                                printf("Error: seem to have run out of memory\n");                          
                                exit(-1);
                              }

                              strcpy(newfinalbuffer, finalbuffer); 

                              free(finalbuffer);
                              finalbuffer = newfinalbuffer;
                           }
                           
                           // Append current buffer to finalbuffer string
                           strncat(finalbuffer, buffer, bytesread);

                           // Check for semicolon terminator
                           char * ptr;
                           if ((ptr = strchr(finalbuffer, ';')) != NULL)
                           {                              
                              *ptr = '\0';
                              break;
                           }
                        }                        
                        free(buffer);
/*
                        int i;
                        for (i=0;i<nread;i++) 
                        {
                          cmd.Append(buffer[i]);
                        }
                        free(buffer);
                        cmd.Append('\0');                        

*/
#ifdef YACAS_DEBUG
printf("Servicing on %ld (%ld)\n",(long)fd,(long)used_clients[clsockindex]);
#endif
                            
                            if (clsockindex < maxConnections)
                            {
                               if (used_clients[clsockindex] == NULL)
                               {                                                                                      

   #ifdef YACAS_DEBUG
     printf("Loading new Yacas environment\n");
   #endif
                                 StringOutput *out = NEW StringOutput(outStrings);
                                 LoadYacas(out);
                                 used_clients[clsockindex] = yacas;
                                 yacas = NULL;
                                 nrSessions++;
                               }

   // enable if fork needed
   //                        if (fork() == 0)
                           {
                               char* response = finalbuffer;
                               (*used_clients[clsockindex])()().iSecure = 1;
                               if (seconds>0)
                               {
                                   clientToStop = used_clients[clsockindex];
   #ifndef WIN32
                                   signal(SIGALRM,stopClient);
                                   alarm(seconds);
   #else
                                   LARGE_INTEGER timedue;
                                   timedue.QuadPart = seconds  * -10000000;
                                
                                   htimer = CreateWaitableTimer(NULL, true, "WaitableTimer");
                                   SetWaitableTimer(htimer, &timedue, 0, stopClient, NULL, 0);
   #endif                                
                               }
   #ifdef YACAS_DEBUG
                               printf("In> %s\n",finalbuffer);
   #endif
                               outStrings.SetNrItems(1);
                               outStrings[0] = '\0';
                               used_clients[clsockindex]->Evaluate(finalbuffer);
                               free(finalbuffer);
                               if (seconds>0)
                               {
   #ifndef WIN32
                                   signal(SIGALRM,SIG_IGN);
   #else

                                   if (htimer)
                                   {
                                      CancelWaitableTimer(htimer);
                                   }
   #endif
                               }
                               if (used_clients[clsockindex]->IsError())
                               {
                                   response = used_clients[clsockindex]->Error();
                               }
                               else
                               {
                                   response = used_clients[clsockindex]->Result();
                               }

                               int buflen=strlen(response);
   #ifdef YACAS_DEBUG
                               printf("%s",outStrings.String());
                               printf("Out> %s\n",response);
   #endif
                               if (response)
                               {
   #ifndef WIN32
                                 write(fd, outStrings.String(), strlen(outStrings.String()));
                                 write(fd,"]\r\n",3);
                                 if (buflen>0)
                                 {
                                   write(fd, response, buflen);
                                   write(fd,"\r\n",2);
                                 }
                                 write(fd,"]\r\n",3);
   #else
                                 send(fd, outStrings.String(), strlen(outStrings.String()), 0);
                                 send(fd,"]\r\n",3, 0);
                                 if (buflen>0)
                                 {
                                   send(fd, response, buflen, 0);
                                   send(fd,"\r\n",2, 0);
                                 }
                                 send(fd,"]\r\n",3, 0);
   #endif
                               }
                           }                        

   // enable if fork needed
   //                            exit(0);
                        }
                        else
                        {
                           char* limtxt = "Maximum number of connections reached, sorry\r\n";
#ifndef WIN32
                           write(fd,"]\r\n",3);
                           write(fd, limtxt, strlen(limtxt));
                           write(fd,"]\r\n",3);
#else
                           send(fd,"]\r\n",3, 0);
                           send(fd, limtxt, strlen(limtxt), 0);
                           send(fd,"]\r\n",3, 0);
#endif                          
                        }
                    }
                }
            }
        }
    }
#ifdef WIN32
    WSACleanup();
#endif
    return 0;
}
#endif





#define SHOWSIZE(a)    printf("   sizeof(" #a ") = %ld\n",sizeof(a));

int main(int argc, char** argv)
{
    unsigned char first_stack_var=0;
    the_first_stack_var = &first_stack_var;


//printf("sizeof(LispPtr) = %d\n",sizeof(LispPtr));
#ifdef YACAS_DEBUG
  if (verbose_debug)
  {
    SHOWSIZE(LispUserFunctions);
    SHOWSIZE(LispPtr);
    SHOWSIZE(LispString);
    SHOWSIZE(LispAtom);
    SHOWSIZE(LispHashTable);
    SHOWSIZE(LispEnvironment);
    SHOWSIZE(CYacas);
    SHOWSIZE(LispStringSmartPtr);
  }
#endif

    
#ifdef YACAS_DEBUG
    //    PlatAlloc(100); // test the alloc memory leak checker
    CHECKPTR(NULL);
#endif
    
/*
    printf("sizeof(LispAtom) = %d\n",sizeof(LispAtom));
    printf("sizeof(LispSubList) = %d\n",sizeof(LispSubList));
    printf("sizeof(LispString) = %d\n",sizeof(LispString));
*/
    
    char* file_to_load=NULL;
    
    int fileind=1;
    if (argc > 1)
    {
        while (fileind<argc && argv[fileind][0] == '-')
        {
            if (!strcmp(argv[fileind],"--texmacs"))
            {
                use_texmacs_out = 1;
//                show_prompt=0;
                trace_history=1;
                use_plain = 1;
                read_eval_print = NULL;
            }
            else if (!strcmp(argv[fileind],"--patchload"))
            {
                patchload=1;
            }
#ifdef YACAS_DEBUG
            else if (!strcmp(argv[fileind],"--verbose-debug"))
            {
                verbose_debug=1;
            }
#endif
            else if (!strcmp(argv[fileind],"--init"))
            {
                fileind++;
                init_script = argv[fileind];
            }
            else if (!strcmp(argv[fileind],"--read-eval-print"))
            {
                fileind++;

//printf("rep %d\n",argv[fileind][0]);
                if (argv[fileind][0])
                  read_eval_print = argv[fileind];
                else
                  read_eval_print = NULL;
            }
            else if (!strcmp(argv[fileind],"--rootdir"))
            {
                fileind++;
                root_dir = argv[fileind];
            }
            else if (!strcmp(argv[fileind],"--archive"))
            {
                fileind++;
                archive = argv[fileind];
            }
            else if (!strcmp(argv[fileind],"--server"))
            {    
                fileind++;

                if (fileind<argc)
                {
                  server_mode=1;
                  server_port = atoi(argv[fileind]);
                }                
            }

            else if (!strcmp(argv[fileind],"--uncompressed-archive"))
            {
                // This is just test code, to see if the engine
                // can handle uncompressed files. Uncompressed
                // files will not be used in general, except for
                // platforms where minilzo.c doesn't compile.
                fileind++;
                archive = argv[fileind];
                compressed_archive = 0;
            }
            else
            {
                if (strchr(argv[fileind],'f'))
                {
                    use_stdin = 1;
                }
                if (strchr(argv[fileind],'p'))
                {
                    use_plain = 1;
                }
                if (strchr(argv[fileind],'c'))
                {
                    show_prompt=0;
                }
                if (strchr(argv[fileind],'t'))
                {
                    trace_history=1;
                }
                if (strchr(argv[fileind],'d'))
                {
                    printf("%s\n",SCRIPT_DIR);
                    return 0;
                }
                if (strchr(argv[fileind],'w'))
                {
                    hideconsolewindow=1;
                }
              
#ifdef HAVE_CONFIG_H
                if (strchr(argv[1],'v'))
                {
                    printf("%s\n",VERSION);
                    return 0;
                }
#endif

#ifndef NO_GLOBALS
                if (strchr(argv[1],'m'))
                {
                    extern void
                        Malloc_SetHooks( void *(*malloc_func)(size_t),
                                         void *(*calloc_func)(size_t, size_t),
                                         void *(*realloc_func)(void *, size_t),
                                         void (*free_func)(void *) );

                    Malloc_SetHooks( malloc,
                                     calloc,
                                     realloc,
                                     free );



                }
#endif
            }
            fileind++;
        }
        if (fileind<argc)
            file_to_load=argv[fileind];
    }

    /*
     {
        printf("[%d %d]\n",LispHash("\"a\""),
               LispHashStringify( "a" ));

    }
    */

    atexit(my_exit);

#ifdef SUPPORT_SERVER
    if (server_mode)
    {
      runserver(argc,argv);
      return 0;
    }
#endif

/*
#ifdef SIGBUS
  signal(SIGBUS,BusErrorHandler);
#endif
#ifdef SIGBUS
  signal(SIGSEGV,BusErrorHandler);
#endif
*/

    signal(SIGINT, InterruptHandler);

// define STD_COMMANDLINE if you want the standard command line always
#ifndef STD_COMMANDLINE
    if (use_plain)
#endif
        commandline = NEW CStdCommandLine;
#ifndef STD_COMMANDLINE
    else
        commandline = NEW FANCY_COMMAND_LINE;
#endif


    commandline->iTraceHistory = trace_history;

    LoadYacas();


    char* inprompt="",*outprompt="";
    if (show_prompt && !use_texmacs_out)
    {
        inprompt = "In> ";
        outprompt = "Out> ";
    }

    if (use_texmacs_out)
    {
        (*yacas)()().SetPrettyPrinter((*yacas)()().HashTable().LookUp("\"TexForm\""));
//        yacas->Evaluate("ToString()PrettyPrinter(\"TexForm\");");
    }

    if (fileind<argc)
    {
        while (fileind<argc)
        {
            char s[200];
            if (patchload)
            {
                sprintf(s,"PatchLoad(\"%s\");",argv[fileind]);
            }
            else
            {
                sprintf(s,"Load(\"%s\");",argv[fileind]);
            }
            yacas->Evaluate(s);

            if (yacas->IsError())
            {
                printf("Error in file %s:\n",argv[fileind]);
                printf("%s\n",yacas->Error());
            }
            fileind++;
        }
        exit(0);
    }
    if (show_prompt && (!use_texmacs_out))
        ShowResult("");

    if (use_stdin)
    {
        char buffer[4001];
        int offs=0;
    MORE:
        fgets(&buffer[offs],4000-offs,stdin);
        offs=strlen(buffer);
        if (!feof(stdin) && offs>0)
            goto MORE;

        yacas->Evaluate(buffer);
        ShowResult(outprompt);
        exit(0);
    }

RESTART:

    if (show_prompt)
    {
        if (use_texmacs_out)
        {
            printf("%cverbatim:",TEXMACS_DATA_BEGIN);
            printf("This is Yacas version `" VERSION "' under TeXmacs\n");
            printf(GPL_blurb_nohelp);
            printf("%c",TEXMACS_DATA_END);
        }
        else 
        {
            printf("This is Yacas version '" VERSION "'.\n");
#ifdef WIN32
            printf(GPL_blurb_nohelp);
#else
            printf(GPL_blurb);
#endif
            printf("Numeric mode: \"%s\"\n",NumericLibraryName());
            printf("To exit Yacas, enter  Exit(); or quit or Ctrl-c. Type ?? for help.\n");
            printf("Or type ?function for help on a function.\n");
            printf("Type 'restart' to restart Yacas.\n");
            printf("To see example commands, keep typing Example();\n");
        }
        fflush(stdout);
    }

    if (read_eval_print)
    {
      while (Busy())
      {
#ifdef YACAS_DEBUG
        LispLocalEvaluator local((*yacas)()(),NEW TracedStackEvaluator);
#endif
        yacas->Evaluate(read_eval_print);
      }
    }
    else
    {
      while (Busy())
      {
#ifdef YACAS_DEBUG
        LispLocalEvaluator local((*yacas)()(),NEW TracedStackEvaluator);
#endif
        
#ifdef PROMPT_SHOW_FREE_MEMORY
        char full_prompt[30];
        if (show_prompt)
          build_full_prompt(full_prompt, inprompt, 30);
        else
          full_prompt[0] = (char) 0;
        ReadInputString(full_prompt);
#else
        ReadInputString(inprompt);
#endif
        char *inpline =  commandline->iLine.String();
        if (use_texmacs_out)
        {
            printf("%cverbatim:",TEXMACS_DATA_BEGIN);
        }

        if (busy)
        {
          if(*inpline)
          {
            if (use_texmacs_out)
            {
              printf("%clatex:",TEXMACS_DATA_BEGIN);
            }
            yacas->Evaluate(inpline);
            if (use_texmacs_out)
            {
              printf("%c",TEXMACS_DATA_END);
            }
#ifdef PROMPT_SHOW_FREE_MEMORY
            char full_prompt[30];
            if (show_prompt)
              build_full_prompt(full_prompt, outprompt, 30);
            else
              full_prompt[0] = (char) 0;
            ShowResult(full_prompt);
#else
            ShowResult(outprompt);
#endif
          }
        }
        if (use_texmacs_out)
        {
            printf("%c",TEXMACS_DATA_END);
        }
        fflush(stdout);
    }
  }
  if (restart)
  {
    delete yacas;
    LoadYacas();
    goto RESTART;
  }
  return 0;
}

