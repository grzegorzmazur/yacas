
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
#include <dirent.h>
#include <sys/stat.h>
#include "yacas.h"

#include "unixcommandline.h"
#include "stdcommandline.h"
#include "standard.h"
#include "numbers.h"
#include "ramdisk.h" //TODO keep this?
#include "arggetter.h"


#ifdef HAVE_CONFIG_H
#include "../config.h"
#endif


CYacas* yacas=NULL;
CCommandLine *commandline = NULL;
int use_stdin = 0;
int use_plain = 0;
int show_prompt = 1;
int trace_history = 0;

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



void LispExit(LispEnvironment& aEnvironment, LispPtr& aResult,
              LispPtr& aArguments)
{
    busy=LispFalse;
//    Check(0,KQuitting);
    InternalTrue(aEnvironment, aResult);
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

void LispVersion(LispEnvironment& aEnvironment, LispPtr& aResult,
                 LispPtr& aArguments)
{
    aResult.Set(LispAtom::New(aEnvironment.HashTable().LookUp("\"" VERSION "\"")));
}


void my_exit(void)
{
    if (show_prompt)
        printf("Quitting...\n");

    delete yacas;
    delete commandline;
    ReportNrCurrent();
}

void ShowResult(char *prompt)
{
    if (yacas->Error()[0] != '\0')
    {
        printf("%s\n",yacas->Error());
    }
    else
    {
        printf("%s%s\n",prompt,yacas->Result());
    }
    fflush(stdout);
}


void LoadYacas()
{
    yacas = CYacas::NewL();

    (*yacas)()().Commands().SetAssociation(LispEvaluator(LispExit),
                                           (*yacas)()().HashTable().LookUp("Exit"));
    (*yacas)()().Commands().SetAssociation(LispEvaluator(LispVersion),
                                           (*yacas)()().HashTable().LookUp("Version"));

    (*yacas)()().Commands().SetAssociation(LispEvaluator(LispHistorySize),
                                           (*yacas)()().HashTable().LookUp("HistorySize"));


    //TODO #include "../ramscripts/some.inc"

    /*TODO test code!!!
     (*yacas)()().iRamDisk.SetAssociation(
     LispRamFile("WriteString(\"Hello world!\");"),
     (*yacas)()().HashTable().LookUp("testfile",LispTrue)
                                        );
                                        */

    yacas->Evaluate("DefaultDirectory(\"" SCRIPT_DIR "\");");

    yacas->Evaluate("Load(\"yacasinit\");");
    if (yacas->Error()[0] != '\0')
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
    {
        DIR *dp;
        struct dirent* entry;
        struct stat statbuf;
        char dir[256];
        strcpy(dir,SCRIPT_DIR "addons/");
    
        if ((dp = opendir(dir)) != NULL)
        {
            yacas->Evaluate("DefaultDirectory(\"" SCRIPT_DIR "addons/\");");
            //            chdir(dir);
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
                if (show_prompt)
                    printf("[%s] ",dummy);
                sprintf(buf,"DefLoad(\"%s\");",dummy);
                yacas->Evaluate(buf);
            }
            printf("\n");
        }
    }

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
}

static int readmode = 0;
void InterruptHandler(int errupt)
{
    printf("^C pressed\n");
    (*yacas)()().iEvalDepth = (*yacas)()().iMaxEvalDepth+100;
    if (readmode)
        exit(0);
}

int main(int argc, char** argv)
{
    char* file_to_load=NULL;
    int fileind=1;
    if (argc > 1)
    {
        while (fileind<argc && argv[fileind][0] == '-')
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

    signal(SIGINT, InterruptHandler);

// define STD_COMMANDLINE if you want the standard command line always
#ifndef STD_COMMANDLINE
    if (use_plain)
#endif
        commandline = new CStdCommandLine;
#ifndef STD_COMMANDLINE
    else
        commandline = new CUnixCommandLine;
#endif

    commandline->iTraceHistory = trace_history;
    
    LoadYacas();


    char* inprompt="",*outprompt="";
    if (show_prompt)
    {
        inprompt = "In> ";
        outprompt = "Out> ";
    }


    if (fileind<argc)
    {
        while (fileind<argc)
        {
            char s[200];
            sprintf(s,"Load(\"%s\");",argv[fileind]);
            yacas->Evaluate(s);

            if (yacas->Error()[0] != '\0')
            {
                printf("Error in file %s:\n",argv[fileind]);
                printf("%s\n",yacas->Error());
            }
            fileind++;
        }
        exit(0);
    }
    if (show_prompt)
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
        printf("Numeric mode: \"%s\"\n",NumericLibraryName());
        printf("To exit Yacas, enter  Exit(); or quit or Ctrl-c. Type ?? for help.\n");
        printf("Or type ?function for help on a function.\n");
        printf("Type 'restart' to restart Yacas.\n");
        printf("To see example commands, keep typing Example();\n");
    }

    while (Busy())
    {
        readmode = 1;
        commandline->ReadLine(inprompt);
        readmode = 0;
        char *inpline =  commandline->iLine.String();

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
                        yacas->Evaluate(inpline);
                        ShowResult(outprompt);
                    }
                }
            }
        }
    }
    return 0;
}


