/*
 * Example terminal client for the yacas Computer Algebra library.
 * It is heavily tailored to Unix (Linux), but you should be able
 * to easily make a version that links with libyacas.a and provides
 * an interface for a different platform.
 * The platform-dependent parts are readline.cpp (which maintains
 * a history for keyed-in expressions on the command line), and
 * the directories it looks in for input files.
 */

#include <stdio.h>
#include "yacas.h"

#include "win32commandline.h"
#include "standard.h"
// what's up with this VERSION define?
//#define VERSION "Windows-latest"

#include "GPL_stuff.h"


CYacas* yacas=NULL;
char scriptdir[512];
static LispBoolean busy=true;
static LispBoolean scripts=false;
static char yacas_dir[_MAX_PATH];
static char cfg_file_name[_MAX_PATH];
static char *init_script="yacasinit.ys";
static char *root_dir=NULL;

void ReportNrCurrent()
{
#ifdef LISP_DEBUGHEAP
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



LispBoolean Busy()
{
    return busy;
}

void LispExit(LispEnvironment& aEnvironment, LispPtr& aResult,
              LispPtr& aArguments)
{
    busy = false;
    InternalTrue(aEnvironment, aResult);
}

void my_exit(void)
{
    delete yacas;
    ReportNrCurrent();
}

void ShowResult(char *prompt)
{
    if (yacas->Error()[0] != '\0')
    {
        printf("%s\n", yacas->Error());
    }
    else
    {
        printf("%s%s\n",prompt, yacas->Result());
    }
    fflush(stdout);
}

void loadYacasScriptDir(void);
void parseCommandLine(int argc, char *argv[]);
void runYacasCalculations(char *arg);
void runYacasTestScript(void);

void processMemoryOptions(int argc, char *argv[]){
#ifndef NO_GLOBALS
	for (int i = 1; i < argc; i++) {
        if (!strcmp(argv[i], "-m")) {
            extern void Malloc_SetHooks( void *(*malloc_func)(size_t),
                                 void *(*calloc_func)(size_t, size_t),
                                 void *(*realloc_func)(void *, size_t),
                                 void (*free_func)(void *) );

            Malloc_SetHooks( malloc, calloc, realloc, free );
            return;
        }
    }
#endif
}

int main(int argc, char *argv[]){
    {
       const char* yacas_exe = argv[0];
       char yacas_drive[_MAX_DRIVE]; char fname[_MAX_FNAME]; char ext[_MAX_EXT];
       _splitpath( yacas_exe, yacas_drive, yacas_dir, fname, ext );
       if(yacas_dir[strlen(yacas_dir)-1] == '\\'){
        yacas_dir[strlen(yacas_dir)-1] = 0;
       }
       sprintf(cfg_file_name, "%s%s\\%s", yacas_drive, yacas_dir, "yacas.cfg");
    }

    int line = 0;

    processMemoryOptions(argc, argv);

    yacas = CYacas::NewL();
    atexit(my_exit);

	if(argc > 1)
		parseCommandLine(argc, argv);

    (*yacas)()().Commands().SetAssociation(LispEvaluator(LispExit),
         (*yacas)()().HashTable().LookUp("Exit"));

	loadYacasScriptDir();
	CWin32CommandLine commandline;

    printf("Yacas " VERSION "\n");
    printf(GPL_blurb_nohelp);
    printf("To exit Yacas, enter  Exit(); or Ctrl-c. Type ?? for help.\n");
    printf("Or type ?function for help on a function.\n");
    printf("To see example commands, keep typing Example();\n");
    while (Busy()) {
        commandline.iLine = "";
        commandline.ReadLine("In> ");
        char *inpline = commandline.iLine.String();
        if (inpline && *inpline) {
            yacas->Evaluate(inpline);
            ShowResult("Out> ");
            line++;
        }
    }

    printf("Quitting...\n");
    return 0;
}

void loadYacasScriptDir(){
	// Are the scripts already loaded?
	if (scripts) return;

	FILE *config;
	char fullpath[512];
    if (root_dir)
    {
      strcpy(scriptdir,root_dir);
        {
          char*ptr=scriptdir;
          while (*ptr)
          {
            if (*ptr=='\\')
              *ptr='/';
            ptr++;
          }
        }
        sprintf(fullpath, "DefaultDirectory(\"%s\");", scriptdir);
        printf("Default directory: %s \n", scriptdir);
		  yacas->Evaluate(fullpath);
		  if(yacas->Error()[0] != '\0'){
        {
          if(config = fopen(cfg_file_name, "r")) goto LOADDIR;
          goto getdir;
        }
		  }
    }else
    if(config = fopen(cfg_file_name, "r")) {
LOADDIR:
		fgets(scriptdir, 512, config);	// Use the location specified

        sprintf(fullpath, "DefaultDirectory(\"%s\")", scriptdir);
        printf("Default directory: %s \n", scriptdir);
		yacas->Evaluate(fullpath);

		fclose(config);
		if(yacas->Error()[0] != '\0'){
			goto getdir;
		}
	} else {
getdir:
		config = fopen(cfg_file_name, "w");
		printf("Directory where the scripts are (use a full path name)\n");
		printf("Path: ");
		gets(scriptdir);
		unsigned i = 0;
		for(i = 0; i <= strlen(scriptdir); i++){
			if(scriptdir[i] == '\\')
				scriptdir[i] = '/';
		}

		// Make sure the end has a ending backslash
		if(scriptdir[i-2] != '/'){
			scriptdir[--i] = '/';
			scriptdir[++i] = '\0';
		}
        printf("Saving script path %s in %s \n", scriptdir, cfg_file_name);

		fputs(scriptdir, config);			// Store the location of scripts for
											// reference later
  	fclose(config);
	}

	strcpy(fullpath, "DefaultDirectory(\"");
	strcat(fullpath, scriptdir);
	strcat(fullpath, "\");");

    yacas->Evaluate(fullpath);

    {
      char buf[500];
      sprintf(buf,"Load(\"%s\");",init_script);
      yacas->Evaluate(buf);
    }
    //yacas->Evaluate("FullForm(a_3+a_4)");
    if (yacas->Error()[0] != '\0'){
        ShowResult("");
    }

	scripts = true;
}

void runYacasCalculations(char *arg)
{
	loadYacasScriptDir();

	char s[200];
	sprintf(s,"Load(\"%s\");", arg);
	yacas->Evaluate(s);
	exit(0);
}

void runYacasTestScript(void)
{
	char fullpath[512];

	loadYacasScriptDir();

    sprintf(fullpath, "Load(\"%sexamples/tests.ys\");", scriptdir);
	yacas->Evaluate(fullpath);
}

void parseCommandLine(int argc, char *argv[]) {
	for (int i = 1; i < argc; i++) {
		if (!strcmp(argv[i],"-d") || !strcmp(argv[i],"--scriptdir")) {
			loadYacasScriptDir();
			printf("%s\n",scriptdir);
		}else
		if (!strcmp(argv[i],"-h") || !strcmp(argv[i],"--help") || !strcmp(argv[i],"/?")) {
			printf("Yacas Windows client -- version %s\n", VERSION);
			printf("The following command line options are available:\n\n");
			printf("\t-d  --scriptdir\t\tPrints the path of the script directory.\n");
			printf("\t-f  --runfile\t\tLoad and evaluate the file provided\n");
			printf("\t-h  --help\t\tPrints this message.\n");
			printf("\t-t  --test\t\tRuns the test script.\n");
            printf("\t-e  --eval\t\tEvaluate the expression passed in the command line.\n");
			printf("\t-v  --version\t\tPrints version of yacas this client uses.\n");
		}else
		if (!strcmp(argv[i],"-t") || !strcmp(argv[i],"--test")) {
			runYacasTestScript();
		}else
		if (!strcmp(argv[i],"-v") || !strcmp(argv[i],"--version")) {
			puts(VERSION);
		}else
		if (!strcmp(argv[i],"-f") || !strcmp(argv[i],"--runfile")) {
			if (i + 1 <= argc) {
				runYacasCalculations(argv[++i]);
			} else {
				printf("%s: you need to supply a filename for \'%s\'\n",
					   argv[0], argv[i]);
				exit(1);
			}
		}else
    if (!strcmp(argv[i],"--init"))
    {
        i++;
        init_script = argv[i];
            continue;
    }
    else if (!strcmp(argv[i],"--rootdir"))
    {
        i++;
        root_dir = argv[i];
            continue;
    } else 

      
      
      if(!strcmp(argv[i],"-e") || !strcmp(argv[i],"--eval")) {
            i++;
            loadYacasScriptDir();
            const char* inpline = argv[i];
            printf("In> %s \n", inpline);
            yacas->Evaluate(inpline);
            ShowResult("Out> ");
        }else
        if(!strcmp(argv[i],"-m")) {
            // do nothing here
            continue;
        }else{
            printf("Invalid argument %s \n", argv[i]);
        }
    
        exit(0);
    }
}
