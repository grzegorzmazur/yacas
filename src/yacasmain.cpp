
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
//
// Example: 'yacas -pc' will use minimal command line interaction,
//          showing no prompts, and with no readline functionality.
//


#include <stdio.h>
#ifndef WIN32
#include <dirent.h>
#include <sys/stat.h>
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
  #define PLATFORM_OS "\"Unix\""
#else
  #include "win32commandline.h"
  #define FANCY_COMMAND_LINE CWin32CommandLine
  #define SCRIPT_DIR ""
  #define PLATFORM_OS "\"Win32\""
#endif


#include "stdcommandline.h"
#include "standard.h"
#include "numbers.h"
#include "ramdisk.h" //TODO keep this?
#include "arggetter.h"

//TODO time.h and errors.h are only needed for LispTime
#include <time.h>
#include "errors.h"


#ifdef HAVE_CONFIG_H
#include "../config.h"
#endif
#ifndef VERSION
//#define VERSION "Windows latest"
#include "version.h"
#endif  //VERSION

//#define PROMPT_SHOW_FREE_MEMORY

#ifdef SUPPORT_SERVER
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <sys/ioctl.h>
#define SOCKLEN_T unsigned int //socklen_t
  #define MY_SL_TYPE /*unsigned*/ int
#endif



CYacas* yacas=NULL;
CCommandLine *commandline = NULL;
int use_stdin = 0;
int use_plain = 0;
int show_prompt = 1;
int trace_history = 0;
int use_texmacs_out = 0;
int patchload=0;
char* root_dir    = SCRIPT_DIR;
#ifndef WIN32
  char* archive     = NULL;
#else
  char* archive     = "scripts.dat";
#endif
char* init_script = "yacasinit.ys";

static int readmode = 0;

int compressed_archive = 1;

int server_mode = 0;
int server_port = 9734;

  
void ReportNrCurrent()
{
#ifdef YACAS_DEBUG
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
    printf("Total %d functions defined (%d built-in, %d user)\n",
           theNrDefinedBuiltIn+theNrDefinedUser,
           theNrDefinedBuiltIn,theNrDefinedUser);
#endif
}


//TODO global!!!
static LispBoolean busy=LispTrue;

LispBoolean Busy()
{
    return busy;
}


void LispPlatformOS(LispEnvironment& aEnvironment, LispPtr& aResult,
              LispPtr& aArguments)
{
  aResult.Set(LispAtom::New(aEnvironment.HashTable().LookUp(PLATFORM_OS)));
}

void LispExit(LispEnvironment& aEnvironment, LispPtr& aResult,
              LispPtr& aArguments)
{
    busy=LispFalse;
//    Check(0,KQuitting);
    InternalTrue(aEnvironment, aResult);
}

unsigned char *the_first_stack_var;

void LispStackSize(LispEnvironment& aEnvironment, LispPtr& aResult,
              LispPtr& aArguments)
{
    LispChar buf[30];
    InternalIntToAscii(buf, (int)(the_first_stack_var-(unsigned char*)&buf[0]));
    aResult.Set(LispAtom::New(aEnvironment.HashTable().LookUp(buf)));
}

static void LispReadCmdLineString(LispEnvironment& aEnvironment, LispPtr& aResult,
              LispPtr& aArguments)
{
    TESTARGS(2);
    LispPtr promptObject;
    aEnvironment.iEvaluator->Eval(aEnvironment, promptObject, Argument(aArguments,1));
    CHK_ISSTRING(promptObject,1);
    LispString prompt;
    InternalUnstringify(prompt, promptObject.Get()->String());
    readmode = 1;
    commandline->ReadLine(prompt.String());
    readmode = 0;
    char *output =  commandline->iLine.String();
    aResult.Set(LispAtom::New(aEnvironment.HashTable().LookUpStringify(output)));
}

static void LispHistorySize(LispEnvironment& aEnvironment, LispPtr& aResult,
                            LispPtr& aArguments)
{
    /* Obtain arguments passed in. */
    LispArgGetter g(aEnvironment, aArguments);
    ShortIntegerArgument(g, depth, LispTrue);
    g.Finalize(2);

    commandline->MaxHistoryLinesSaved(depth);

    /* Return result. */
    InternalTrue(aEnvironment,aResult);
}

void LispTime(LispEnvironment& aEnvironment, LispPtr& aResult,
              LispPtr& aArguments)
{
    TESTARGS(2);
    clock_t starttime = clock();
    aEnvironment.iEvaluator->Eval(aEnvironment, aResult, Argument(aArguments,1));
    clock_t endtime = clock();
    double timeDiff;

//    printf("%ld to %ld\n",starttime,endtime);
    timeDiff = endtime-starttime;
    timeDiff /= CLOCKS_PER_SEC;
//    LispChar buf[30];
    printf("%g seconds taken\n",timeDiff);
}

void ShStack(LispEnvironment& aEnvironment, LispPtr& aResult,
             LispPtr& aArguments)
{
#if 0
    /* gcc 3.0 doesn't allow us to have these classes, since they
      are declared global */
    LispEnvironment::LocalVariableFrame* fr = aEnvironment.iLocalsList;
    
    LispEnvironment::LispLocalVariable* ptr = fr->iFirst;
    int nr=0;
    while (ptr != NULL)
    {
        printf("%s ",ptr->iVariable->String());
        nr++;
        ptr = ptr->iNext;
    }
    printf("nr = %d\n",nr);
#endif
    InternalTrue(aEnvironment, aResult);
}



void LispIsPromptShown(LispEnvironment& aEnvironment,LispPtr& aResult,
              LispPtr& aArguments)
{ // this function must access show_prompt which is a *global* in yacasmain.cpp, so it's not possible to put this function in mathcommands.cpp
    TESTARGS(1);
    InternalBoolean(aEnvironment,aResult, show_prompt==1
	);
}


//#define _TESTCODE_
#ifdef  _TESTCODE_
#include "errors.h"
#define InternalEval aEnvironment.iEvaluator->Eval
void DummyTestFunction(LispEnvironment& aEnvironment, LispPtr& aResult,
              LispPtr& aArguments)
{
    // Check that we have one argument.
    TESTARGS(2);

    // Evaluate the first argument
    LispPtr list;
    InternalEval(aEnvironment, list , Argument(aArguments,1));

    //Check that it is a compound object
    CHK_ARG(list.Get()->SubList() != NULL, 1);
    LispObject *walker = list.Get()->SubList()->Get();
    walker = walker->Next().Get();
    
    LispObject* result = ATOML("List");
    while (walker != NULL)
    {
        char buf[200];
        CHK_ARG(walker->SubList() != NULL, 1);
        LispObject* var = walker->SubList()->Get();
        CHK_ARG(var != NULL, 1);
        CHK_ARG(var->String() != NULL, 1);
        CHK_ARG(!strcmp(var->String()->String(),"_"), 1);
        var = var->Next().Get();
        CHK_ARG(var != NULL, 1);
        CHK_ARG(var->String() != NULL, 1);
        strcpy(buf,var->String()->String());
        var = var->Next().Get();
        CHK_ARG(var != NULL, 1);
        CHK_ARG(var->String() != NULL, 1);
        strcat(buf,var->String()->String());
        result = LA(result) + LA(ATOML(buf));
        walker = walker->Next().Get();
    }

    LispPtr extra;
    PARSE(extra,"p_100 And Not p_101");
    result = LA(result) + LA(extra.Get());
    aResult.Set(LIST(result));
}
#endif


void my_exit(void)
{
    if (show_prompt)
        printf("Quitting...\n");
    delete yacas;
    delete commandline;
    ReportNrCurrent();
#ifdef DEBUG_MODE
    YacasCheckMemory();
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

void LoadYacas()
{
    yacas = CYacas::NewL();

    (*yacas)()().Commands().SetAssociation(LispEvaluator(LispPlatformOS),
                                           (*yacas)()().HashTable().LookUp("PlatformOS"));

    (*yacas)()().Commands().SetAssociation(LispEvaluator(LispExit),
                                           (*yacas)()().HashTable().LookUp("Exit"));
	// this function is declared in yacasapi.cpp rather than here
    (*yacas)()().Commands().SetAssociation(LispEvaluator(LispHistorySize),
                                           (*yacas)()().HashTable().LookUp("HistorySize"));
    (*yacas)()().Commands().SetAssociation(LispEvaluator(LispStackSize),
                                           (*yacas)()().HashTable().LookUp("StaSiz"));
    (*yacas)()().Commands().SetAssociation(LispEvaluator(ShStack),
                                           (*yacas)()().HashTable().LookUp("ShStack"));

    (*yacas)()().Commands().SetAssociation(LispEvaluator(LispIsPromptShown),
                                           (*yacas)()().HashTable().LookUp("IsPromptShown"));

    (*yacas)()().Commands().SetAssociation(LispEvaluator(LispReadCmdLineString),
                                           (*yacas)()().HashTable().LookUp("ReadCmdLineString"));

#ifdef _TESTCODE_
    (*yacas)()().Commands().SetAssociation(LispEvaluator(DummyTestFunction),
                                           (*yacas)()().HashTable().LookUp("DummyTestFunction"));
#endif

    (*yacas)()().Commands().SetAssociation(LispEvaluator(LispTime),
                                           (*yacas)()().HashTable().LookUp("Time"));
    
    

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

void InterruptHandler(int errupt)
{
    printf("^C pressed\n");
    (*yacas)()().iEvalDepth = (*yacas)()().iMaxEvalDepth+100;
    if (readmode)
        exit(0);
}



#ifdef SUPPORT_SERVER
int runserver(int argc,char** argv)
{
    int server_sockfd, client_sockfd;
    SOCKLEN_T server_len, client_len;
    struct sockaddr_in server_address;
    struct sockaddr_in client_address;
    int result;
    int maxConnections;
    int process;
    fd_set readfds, testfds;
    int serverbusy;

    int seconds = 30; // give each calculation only so many calculations

    serverbusy=1;
    maxConnections=10;
    process=0;


    signal(SIGPIPE,SIG_IGN);

    server_sockfd = socket(AF_INET, SOCK_STREAM, 0);
    {
        int rsp;
        if (setsockopt(server_sockfd,SOL_SOCKET,SO_REUSEADDR,(void*)&rsp,sizeof(int)))
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
        for(fd = 0; fd < FD_SETSIZE; fd++)
        {
            while(waitpid(-1,NULL,WNOHANG) > 0); /* clean up child processes */
            if(FD_ISSET(fd,&testfds))
            {
                if(fd == server_sockfd)
                {
                    client_len = sizeof(client_address);
                    client_sockfd = accept(server_sockfd, 
                        (struct sockaddr *)&client_address, (MY_SL_TYPE*)&client_len);
#ifdef __CYGWIN__
                    if (client_sockfd != 0xffffffff)
#endif
                    {
                        FD_SET(client_sockfd, &readfds);
                        //LOGTODO                    printf("adding client on fd %d\n", client_sockfd);
                    }

                }
                else
                {
                    ioctl(fd, FIONREAD, &nread);

                    if(nread == 0)
                    {
                        close(fd);
                        FD_CLR(fd, &readfds);
                        //LOGTODO                        printf("removing client on fd %d\n", fd);
                    }
                    else
                    {
                        process++;

                        char* buffer = (char*)malloc(nread+1);
      
                        read(fd, buffer, nread);
                        buffer[nread]='\0';
                        
                        if (fork() == 0)
                        {
                        
    char* response = buffer;
    LoadYacas();
    (*yacas)()().iSecure = 1;
    if (seconds>0)
    {
        signal(SIGALRM,exit);
        alarm(seconds);
    }
    yacas->Evaluate(buffer);
    if (seconds>0)
    {
        signal(SIGALRM,SIG_IGN);
    }
    if (yacas->IsError())
    {
        response = yacas->Error();
    }
    else
    {
      response = yacas->Result();
    }

                            int buflen=strlen(response);
printf("In> %s",buffer);
printf("Out> %s",response);

                            if (response)
                            {
                                if (buflen>0)
                                    write(fd, response, buflen);
                            }
                            free(buffer);
                            delete yacas;
                            yacas = NULL;
                            close(fd);

                            FD_CLR(fd, &readfds);
                            exit(0);
                        }
                        else
                        {
                            close(fd);
                            FD_CLR(fd, &readfds);
                        }
                        
                    }
                }
            }
        }
    }
}
#endif

#include "platmath.h"
void TestNum()
{//numeric test code
  BigNumber result;
  BigNumber x("23",10);
  BigNumber y("45.23e3",10);
  result.Multiply(x,y,10);
  LispString str;
  result.ToString(str,10);
  printf("23*45.23e3 = %s\n",str.String());

  BigNumber minusOne("-1.0",10);
  minusOne.ToString(str,10);
  printf("-1 = %s\n",str.String());
}



#define SHOWSIZE(a)    printf("   sizeof(" #a ") = %d\n",sizeof(a));
int main(int argc, char** argv)
{
//TestNum();

    unsigned char first_stack_var=0;
    the_first_stack_var = &first_stack_var;

#ifdef DEBUG_MODE
    SHOWSIZE(LispString);
    SHOWSIZE(LispAtom);
    SHOWSIZE(LispHashTable);
    SHOWSIZE(LispEnvironment);
    SHOWSIZE(CYacas);
    SHOWSIZE(LispStringSmartPtr);
#endif

    
#ifdef DEBUG_MODE
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
            }
            else if (!strcmp(argv[fileind],"--patchload"))
            {
                patchload=1;
            }
            else if (!strcmp(argv[fileind],"--init"))
            {
                fileind++;
                init_script = argv[fileind];
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


#ifdef SUPPORT_SERVER
    if (server_mode)
    {
      runserver(argc,argv);
      return 0;
    }
#endif

    atexit(my_exit);

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
            printf("Yacas " VERSION " under TeXmacs\n");
            printf("%c",TEXMACS_DATA_END);
        }
        else 
        {
            printf("Numeric mode: \"%s\"\n",NumericLibraryName());
            printf("To exit Yacas, enter  Exit(); or quit or Ctrl-c. Type ?? for help.\n");
            printf("Or type ?function for help on a function.\n");
            printf("Type 'restart' to restart Yacas.\n");
            printf("To see example commands, keep typing Example();\n");
        }
        fflush(stdout);
    }

    while (Busy())
    {
#ifdef DEBUG_MODE
        LispLocalEvaluator local((*yacas)()(),NEW TracedStackEvaluator);
#endif
        
#ifdef PROMPT_SHOW_FREE_MEMORY
        char full_prompt[30];
        if (show_prompt)
          build_full_prompt(full_prompt, inprompt, 30);
        else
          full_prompt[0] = (char) 0;
        readmode = 1;
        commandline->ReadLine(full_prompt);
        readmode = 0;
#else
        readmode = 1;
        commandline->ReadLine(inprompt);
        readmode = 0;
#endif
        char *inpline =  commandline->iLine.String();
        if (use_texmacs_out)
        {
            printf("%cverbatim:",TEXMACS_DATA_BEGIN);
        }

        if (inpline)
        {
            if(*inpline)
            {
                if (inpline[0] == '?')
                {
                    if (inpline[1] == '?')
                    {
                        yacas->Evaluate("Help()");
                    }
                    else
                    {
                        if (strlen(&inpline[1]) < 100)
                        {
                            char buf[120];
                            sprintf(buf,"Help(\"%s\")",&inpline[1]);
                            yacas->Evaluate(buf);
                        }
                    }
                }
                else
                {
                    if (!strncmp(inpline,"restart",7))
                    {
                        delete yacas;
                        LoadYacas();
                        goto RESTART;
                    }
                    else if (!strncmp(inpline,"quit",4))
                    {
                        busy=LispFalse;
                    }
                    else
                    {
                        if (use_texmacs_out)
                        {
                            //        printf("%cverbatim:",TEXMACS_DATA_BEGIN);
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
            }
        }
        if (use_texmacs_out)
        {
            printf("%c",TEXMACS_DATA_END);
        }
        fflush(stdout);
    }
    return 0;
}

