
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
//   4)
//  -i <command> : execute <command>
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

// For all platforms, assume forward slash as path separator (handle at the lowest level).
#define PATH_SEPARATOR   '/'
#define PATH_SEPARATOR_2 "/"

#include "yacas.h"

#ifndef WIN32
  #include "unixcommandline.h"
  #define FANCY_COMMAND_LINE CUnixCommandLine
#else
  #define _WINSOCKAPI_            // Prevent inclusion of winsock.h in windows.h
  #define _WIN32_WINDOWS 0x0410      // Make sure that Waitable Timer functions are declared in winbase.h
  #include "win32commandline.h"
  #define FANCY_COMMAND_LINE CWin32CommandLine
  #define SCRIPT_DIR ""
#endif

#include "stdcommandline.h"
#include "standard.h"
#include "numbers.h"
#include "arggetter.h"
#include "archiver.h"

//time.h and errors.h are only needed for LispTime
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
int use_texmacs_out = 0;
int compiled_plugins = 1;

int stack_size = 50000;

#ifdef YACAS_DEBUG
int verbose_debug=0;
#endif
int patchload=0;
int winsockinitialised=0;
int hideconsolewindow=0;

#ifndef SCRIPT_DIR
  #define SCRIPT_DIR "none"
#endif

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
int server_single_user = 0;
int server_port = 9734;

char* execute_commnd = NULL;

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
    RESULT = (LispAtom::New(aEnvironment,buf));
}


char* ReadInputString(char* prompt)
{
  if (!commandline) return "False";
  char *inpline;
  readmode = 1;
  commandline->ReadLine(prompt);
  readmode = 0;
  inpline =  commandline->iLine.c_str();

  if (inpline)
  {
    if(*inpline)
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
  if (inpline)
    if(*inpline)
      return inpline;
  return "True";
}

static void LispReadCmdLineString(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispPtr promptObject = (ARGUMENT(1));
    CHK_ISSTRING_CORE(promptObject,1);
    LispString prompt;
    InternalUnstringify(prompt, promptObject->String());
    char* output = ReadInputString(prompt.c_str());
    RESULT = (LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUpStringify(output)->c_str()));
}

static void LispHistorySize(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    /* Obtain arguments passed in. */
    ShortIntegerArgument(depth, 1);
    if (commandline)
    {
      commandline->MaxHistoryLinesSaved(depth);
    }
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
#ifdef HAVE_VSNPRINTF
    snprintf(buf,100,"%g",timeDiff);
#else
    sprintf(buf,"%g",timeDiff);
#endif
    RESULT = (LispAtom::New(aEnvironment,buf));
}




void LispFileSize(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispPtr fnameObject = (ARGUMENT(1));
    CHK_ISSTRING_CORE(fnameObject,1);
    LispString fname;
    InternalUnstringify(fname, fnameObject->String());

    long fileSize = 0;
    FILE* f = fopen(fname.c_str(),"rb");
    if(f != NULL)
    {
      fseek(f,0,SEEK_END);
      fileSize = ftell(f);
      fclose(f);
    }
    else
    {
      printf("[[%s]]\n",fname.c_str());
    }
    char buf[100];
 
#ifdef HAVE_VSNPRINTF
    snprintf(buf,100,"%ld",fileSize);
#else
    sprintf(buf,"%ld",fileSize);
#endif
    RESULT = (LispAtom::New(aEnvironment,buf));
}




void LispIsPromptShown(LispEnvironment& aEnvironment,LispInt aStackTop)
{ // this function must access show_prompt which is a *global* in yacasmain.cpp, so it's not possible to put this function in mathcommands.cpp
    InternalBoolean(aEnvironment,RESULT, show_prompt==1);
}


void my_exit(void)
{
  if (yacas)
  {
    if (show_prompt)
        printf("Quitting...\n");
    // Delete the command line first, so that if in debug mode and some assert fires while
    // deleting the Yacas environment object, at least we have a saved history
    if (commandline) delete commandline; commandline = NULL;
    if (yacas) delete yacas; yacas = NULL;
 
    ReportNrCurrent();
#ifdef YACAS_DEBUG
    CheckAllPtrs(1);
#endif
  }
#ifdef WIN32
  #ifdef SUPPORT_SERVER
    if (winsockinitialised)
    {
        WSACleanup();
        winsockinitialised = 0;
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
        {
          sprintf(full_prompt, "%sk %s", entry, prompt); //TODO use snprintf instead!
        }
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
        if (yacas->getDefEnv().getEnv().PrettyPrinter()==NULL)
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
  {
#ifdef HAVE_VSNPRINTF
    snprintf(buf,1000,"DefaultDirectory(\"%s%s\");",ptr2,PATH_SEPARATOR_2);
#else
    sprintf(buf,"DefaultDirectory(\"%s%s\");",ptr2,PATH_SEPARATOR_2);
#endif
  }
  else
  {
#ifdef HAVE_VSNPRINTF
    snprintf(buf,1000,"DefaultDirectory(\"%s\");",ptr2);
#else
    sprintf(buf,"DefaultDirectory(\"%s\");",ptr2);
#endif
  }
  yacas->Evaluate(buf);
}

void LoadYacas(LispOutput* aOutput=NULL)
{
  if (yacas) return;
  busy=LispTrue;
  restart=LispFalse;

  yacas = CYacas::NewL(aOutput,stack_size);


#define CORE_KERNEL_FUNCTION(iname,fname,nrargs,flags) yacas->getDefEnv().getEnv().SetCommand(fname,iname,nrargs,flags);

CORE_KERNEL_FUNCTION("Exit",LispExit,0,YacasEvaluator::Function | YacasEvaluator::Fixed)
CORE_KERNEL_FUNCTION("IsExitRequested",LispExitRequested,0,YacasEvaluator::Function | YacasEvaluator::Fixed)
CORE_KERNEL_FUNCTION("HistorySize",LispHistorySize,1,YacasEvaluator::Function | YacasEvaluator::Fixed)
CORE_KERNEL_FUNCTION("StaSiz",LispStackSize,0,YacasEvaluator::Function | YacasEvaluator::Fixed)
CORE_KERNEL_FUNCTION("IsPromptShown",LispIsPromptShown,0,YacasEvaluator::Function | YacasEvaluator::Fixed)
CORE_KERNEL_FUNCTION("ReadCmdLineString",LispReadCmdLineString,1,YacasEvaluator::Function | YacasEvaluator::Fixed)
CORE_KERNEL_FUNCTION("GetTime",LispTime,1,YacasEvaluator::Macro | YacasEvaluator::Fixed)
CORE_KERNEL_FUNCTION("FileSize",LispFileSize,1,YacasEvaluator::Function | YacasEvaluator::Fixed)


#undef CORE_KERNEL_FUNCTION
 

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
                    yacas->getDefEnv().getEnv().iArchive = a;
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
        if (!compiled_plugins)
        {
          yacas->Evaluate("Set(LoadPlugIns,False);");
        }
 
        char buf[1000];
#ifdef HAVE_VSNPRINTF
        snprintf(buf,1000,"Load(\"%s\");",init_script);
#else
        sprintf(buf,"Load(\"%s\");",init_script);
#endif
        yacas->Evaluate(buf);
        if (yacas->IsError())
        {
          ShowResult("");
          read_eval_print = NULL;
        }
    }

    if (yacas->IsError())
        ShowResult("");

    if (use_texmacs_out)
    {
        printf("%cverbatim:",TEXMACS_DATA_BEGIN);
    }
    {
        char fname[256];
#ifdef HAVE_VSNPRINTF
        snprintf(fname,256,"%s/.yacasrc",getenv("HOME"));
#else
        sprintf(fname,"%s/.yacasrc",getenv("HOME"));
#endif
        FILE* test=fopen(fname,"r");
        if (test)
        {
            fclose(test);
#ifdef HAVE_VSNPRINTF
            snprintf(fname,256,"Load(\"%s/.yacasrc\");",getenv("HOME"));
#else
            sprintf(fname,"Load(\"%s/.yacasrc\");",getenv("HOME"));
#endif
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
    yacas->getDefEnv().getEnv().iEvalDepth = yacas->getDefEnv().getEnv().iMaxEvalDepth+100;
    if (readmode)
    {
      my_exit();
      exit(0);
    }
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
    yacas->getDefEnv().getEnv().iEvalDepth = yacas->getDefEnv().getEnv().iMaxEvalDepth+100;
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
    clientToStop->getDefEnv().getEnv().iEvalDepth = clientToStop->getDefEnv().getEnv().iMaxEvalDepth+100;
  }
}

#define MAX_CONNECTIONS  1000
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
    if (server_single_user)
      seconds = 0;

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
 
                        if (server_single_user && nrSessions == 0)
                        {
                          exit(0);
                        }

 
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
                               if (!server_single_user)
                               {
                                 used_clients[clsockindex]->getDefEnv().getEnv().iSecure = 1;
                               }
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
                               outStrings.ResizeTo(1);
                               outStrings[0] = '\0';
                               used_clients[clsockindex]->Evaluate(finalbuffer);
                               free(finalbuffer);

                                if (server_single_user && !Busy())
                                {
                                  exit(0);
                                }

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
                               printf("%s",outStrings.c_str());
                               printf("Out> %s\n",response);
   #endif
                               if (response)
                               {
   #ifndef WIN32
                                 write(fd, outStrings.c_str(), strlen(outStrings.c_str()));
                                 write(fd,"]\r\n",3);
                                 if (buflen>0)
                                 {
                                   write(fd, response, buflen);
                                   write(fd,"\r\n",2);
                                 }
                                 write(fd,"]\r\n",3);
   #else
                                 send(fd, outStrings.c_str(), strlen(outStrings.c_str()), 0);
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





#define SHOWSIZE(a)    printf("   sizeof(" #a ") = %ld\n",(long)sizeof(a));

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
    SHOWSIZE(LispNumber);
    SHOWSIZE(LispHashTable);
    SHOWSIZE(LispEnvironment);
    SHOWSIZE(CYacas);
    SHOWSIZE(LispStringSmartPtr);
    SHOWSIZE(BigNumber);
//    SHOWSIZE(ANumber);
  }
#endif

 
#ifdef YACAS_DEBUG
//        PlatAlloc(100); // test the alloc memory leak checker
    CHECKPTR(NULL);
#endif
 
/*
    printf("sizeof(LispAtom) = %d\n",sizeof(LispAtom));
    printf("sizeof(LispSubList) = %d\n",sizeof(LispSubList));
    printf("sizeof(LispString) = %d\n",sizeof(LispString));
*/

#define USE_TEXMACS_OUT yacas->getDefEnv().getEnv().SetPrettyPrinter(yacas->getDefEnv().getEnv().HashTable().LookUp("\"TexForm\""));

 
    char* file_to_load=NULL;
    bool exit_after_files = false;

    int fileind=1;
    if (argc > 1)
    {
        for ( ; fileind<argc && argv[fileind][0] == '-'; fileind++)
        {
            if (!strcmp(argv[fileind],"--texmacs"))
            {
                use_texmacs_out = 1;
                use_plain = 1;
                read_eval_print = NULL;
            }
            else if (!strcmp(argv[fileind],"--patchload"))
            {
                patchload=1;
            }
            else if (!strcmp(argv[fileind],"--verbose-debug"))
            {
#ifdef YACAS_DEBUG
                verbose_debug=1;
#else
                printf("Warning: --verbose-debug is only supported in debug the version of this program.\n");
#endif
            }
            else if (!strcmp(argv[fileind],"--init"))
            {
                fileind++;
                if (fileind<argc)
                  init_script = argv[fileind];
            }
            else if (!strcmp(argv[fileind],"--read-eval-print"))
            {
                fileind++;
//printf("rep %d\n",argv[fileind][0]);
                if (fileind<argc)
                {
                  if (argv[fileind][0])
                    read_eval_print = argv[fileind];
                  else
                    read_eval_print = NULL;
                }
            }
            else if (!strcmp(argv[fileind],"--rootdir"))
            {
                archive = NULL;
                fileind++;
                if (fileind<argc)
                  root_dir = argv[fileind];
            }
            else if (!strcmp(argv[fileind],"--archive"))
            {
                fileind++;
                if (fileind<argc)
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
            else if (!strcmp(argv[fileind],"--single-user-server"))
            {
                server_single_user = 1;
            }
            else if (!strcmp(argv[fileind],"--stacksize"))
            {
                fileind++;
                if (fileind<argc)
                {
                  stack_size = atoi(argv[fileind]);
                }
            }
            else if (!strcmp(argv[fileind],"--execute"))
            {
                fileind++;
                if (fileind<argc)
                {
                  execute_commnd = argv[fileind];
                }
            }
            else if (!strcmp(argv[fileind],"--uncompressed-archive"))
            {
                // This is just test code, to see if the engine
                // can handle uncompressed files. Uncompressed
                // files will not be used in general, except for
                // platforms where minilzo.c doesn't compile.
                fileind++;
                if (fileind<argc)
                {
                  archive = argv[fileind];
                  compressed_archive = 0;
                }
            }
            else if (!strcmp(argv[fileind],"--disable-compiled-plugins"))
            {
              compiled_plugins = 0;
            }
      else if (!strcmp(argv[fileind],"-i"))
      {
        fileind++;
        if (fileind<argc)
        {
          char* immediate = argv[fileind];
          if (immediate)
          {
            LoadYacas();
            if (use_texmacs_out) USE_TEXMACS_OUT;
            yacas->Evaluate(immediate);
            if (yacas->IsError())
            {
              printf("Error in immediate command %s:\n",immediate);
              printf("%s\n",yacas->Error());
            }
            exit_after_files = true;
          }
        }
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
                  // Not sure what we should do with -t ... it used to be trace_history
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
                if (strchr(argv[fileind],'v'))
                {
                    printf("%s\n",VERSION);
                    return 0;
                }
#endif

#ifndef NO_GLOBALS
                if (strchr(argv[fileind],'m'))
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

//    atexit(my_exit);

#ifdef SUPPORT_SERVER
    if (server_mode)
    {
      runserver(argc,argv);
      my_exit();
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

    LoadYacas();


    char* inprompt="",*outprompt="";
    if (show_prompt && !use_texmacs_out)
    {
        inprompt = "In> ";
        outprompt = "Out> ";
    }

    if (use_texmacs_out)
    {
        USE_TEXMACS_OUT;
    }

 
    {
        for ( ; fileind<argc; fileind++)
        {
            char s[200];
            if (patchload)
            {
#ifdef HAVE_VSNPRINTF
                snprintf(s,200,"PatchLoad(\"%s\");",argv[fileind]);
#else
                sprintf(s,"PatchLoad(\"%s\");",argv[fileind]);
#endif
            }
            else
            {
#ifdef HAVE_VSNPRINTF
                snprintf(s,200,"Load(\"%s\");",argv[fileind]);
#else
                sprintf(s,"Load(\"%s\");",argv[fileind]);
#endif
            }
            yacas->Evaluate(s);

            if (yacas->IsError())
            {
                printf("Error in file %s:\n",argv[fileind]);
                printf("%s\n",yacas->Error());
            }
      exit_after_files = true;
        }
    }

    if (exit_after_files)
  {
    my_exit();
        exit(0);
  }

    if (show_prompt && (!use_texmacs_out))
        ShowResult("");
    if (execute_commnd != NULL)
    {
        yacas->Evaluate(execute_commnd);
        if (yacas->IsError())
        {
            printf("Error in file %s:\n",argv[fileind]);
            printf("%s\n",yacas->Error());
        }
        if (show_prompt && (!use_texmacs_out))
          ShowResult("");
    }

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
        my_exit();
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
            printf("To exit Yacas, enter  Exit(); or quit or Ctrl-c.\n");
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
        LispLocalEvaluator local(yacas->getDefEnv().getEnv(),NEW TracedStackEvaluator);
#endif
        yacas->Evaluate(read_eval_print);
        if (yacas->IsError())
        {
          printf("%s\n",yacas->Error());
        }
      }
    }
    else
    {
      while (Busy())
      {
#ifdef YACAS_DEBUG
        LispLocalEvaluator local(yacas->getDefEnv().getEnv(),NEW TracedStackEvaluator);
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
        char *inpline =  commandline->iLine.c_str();
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
    delete yacas; yacas = NULL;
    LoadYacas();
    goto RESTART;
  }
  my_exit();
  return 0;
}

