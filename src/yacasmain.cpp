
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

#ifndef WIN32
#include <dirent.h>
#include <sys/stat.h>
#endif

#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif

#include <ctime>
#include <fstream>
#include <iostream>
#include <string>
#include <sstream>
#include <vector>

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
#endif

#include "stdcommandline.h"
#include "standard.h"
#include "numbers.h"
#include "arggetter.h"

#include <ctime>
#include "errors.h"


#ifndef VERSION
#include "version.h"
#endif

//#define PROMPT_SHOW_FREE_MEMORY

#ifdef SUPPORT_SERVER
#include <cstdlib>
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

CYacas* yacas = 0;
CCommandLine *commandline = 0;

bool use_stdin = false;
bool use_plain = false;
bool show_prompt = true;
bool use_texmacs_out = false;
bool compiled_plugins = true;

int stack_size = 50000;

#ifdef YACAS_DEBUG
bool verbose_debug = true;
#endif
bool patchload = false;
bool winsockinitialised = false;
bool hideconsolewindow = false;
bool exit_after_files = false;

#ifndef SCRIPT_DIR
#define SCRIPT_DIR "none"
#endif

const char* root_dir = SCRIPT_DIR;
#ifdef WIN32
HANDLE htimer = 0;
#endif
const char* init_script = "yacasinit.ys";

const char* read_eval_print = "REP()";


static int readmode = 0;

bool server_mode = false;
bool server_single_user = false;
int server_port = 9734;

char* execute_commnd = 0;

static bool busy = true;
static bool restart = false;


void ReportNrCurrent()
{
#ifdef YACAS_DEBUG
    if (verbose_debug) {
        extern long theNrCurrent;
        extern long theNrConstructed;
        extern long theNrDestructed;
        extern long theNrTokens;
        extern long theNrDefinedBuiltIn;
        extern long theNrDefinedUser;

        std::cout << "left-over: " << theNrCurrent << " objects\n"
                  << theNrConstructed << " constructed, "
                  << theNrDestructed << " destructed\n"
                  << "nr tokens: " << theNrTokens << "\n"
                  << "-------------------------------\n"
                  << "Total " << theNrDefinedBuiltIn+theNrDefinedUser
                  << " functions defined ("
                  << theNrDefinedBuiltIn << " built-in, "
                  << theNrDefinedUser << " user)\n";
    }
#endif
}




#define RESULT aEnvironment.iStack.GetElement(aStackTop)
#define ARGUMENT(i) aEnvironment.iStack.GetElement(aStackTop+i)

void LispExit(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    busy = false;
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


const char* ReadInputString(const char* prompt)
{
    if (!commandline)
        return "False";

    const char* inpline;
    readmode = 1;
    commandline->ReadLine(prompt);
    readmode = 0;
    inpline =  commandline->iLine.c_str();

    if (inpline) {
        while(isspace(*inpline))
            ++inpline;

        if(*inpline) {
            if (!std::strncmp(inpline, "restart", 7)) {
                restart = true;
                busy = false;
            } else if (!strncmp(inpline, "quit", 4)) {
                busy=false;
            } else if (*inpline == '?') {
                const std::string key(inpline + 1);

                const std::string prefix = "http://yacas.sourceforge.net/";
                std::string url = prefix + "ref.html?" + key;
                if (key == "licence" || key == "license")
                    url = prefix + "refprogchapter9.html";
                else if (key == "warranty")
                    url = prefix + "refprogchapter9.html#c9s2";
                else if (key == "?")
                    url = prefix + "refmanual.html";

                const std::string viewer = "xdg-open";

                const std::string cmd = viewer + " " + url;

                if (system(cmd.c_str()) == 0)
                    inpline = "True";
                else
                    inpline = "False";
            }
        }
    }

    if (inpline && *inpline)
        return inpline;

    return "True";
}

static void LispReadCmdLineString(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispPtr promptObject = (ARGUMENT(1));
    CHK_ISSTRING_CORE(promptObject,1);
    LispString prompt;
    InternalUnstringify(prompt, promptObject->String());
    const char* output = ReadInputString(prompt.c_str());
    RESULT = (LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUpStringify(output)->c_str()));
}

static void LispHistorySize(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    ShortIntegerArgument(depth, 1);

    if (commandline)
        commandline->MaxHistoryLinesSaved(depth);

    InternalTrue(aEnvironment, RESULT);
}

void LispTime(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    const std::clock_t starttime = std::clock();
    LispPtr res;
    aEnvironment.iEvaluator->Eval(aEnvironment, res, ARGUMENT(1));
    const std::clock_t endtime = std::clock();

    std::ostringstream os;
    os << static_cast<double>(endtime - starttime) / CLOCKS_PER_SEC;

    RESULT = (LispAtom::New(aEnvironment, os.str().c_str()));
}

void LispFileSize(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispPtr fnameObject = (ARGUMENT(1));
    CHK_ISSTRING_CORE(fnameObject,1);
    LispString fname;
    InternalUnstringify(fname, fnameObject->String());

    std::ifstream in(fname.c_str(), std::ifstream::in | std::ifstream::binary);
    in.seekg(0, std::ifstream::end);
    std::ostringstream os;
    os << in.tellg();
    RESULT = LispAtom::New(aEnvironment, os.str().c_str());
}




void LispIsPromptShown(LispEnvironment& aEnvironment,LispInt aStackTop)
{ // this function must access show_prompt which is a *global* in yacasmain.cpp, so it's not possible to put this function in mathcommands.cpp
    InternalBoolean(aEnvironment, RESULT, show_prompt);
}


void my_exit()
{
    if (yacas) {
        if (show_prompt)
            std::cout << "Quitting...\n";

        // Delete the command line first, so that if in debug mode and
        // some assert fires while deleting the Yacas environment object,
        // at least we have a saved history
        if (commandline) {
            delete commandline;
            commandline = 0;
        }

        if (yacas) {
            delete yacas;
            yacas = 0;
        }

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
        winsockinitialised = false;
    }
#endif
#endif
}

#ifdef PROMPT_SHOW_FREE_MEMORY

#ifdef HAVE_SYSINFO_H
std::string build_full_prompt(const char* prompt, const std::size_t maxlen)
{
    if (!prompt)
        prompt = "";

    std::string full_prompt(prompt);

    if (full_prompt.length() > maxlen)
        full_prompt.erase(maxlen);

    struct sysinfo si;
    if (!sysinfo(&si)) {

        const std::size_t total_mem =
            (si.totalram * si.mem_unit) / 1024;

        std::ostringstream os;
        os << total_mem << "k " << prompt;

        if (os.str().length() < maxlen)
            full_prompt = os.str();
    }

    return full_prompt;
}
#else
std::string build_full_prompt(const char* prompt, const std::size_t maxlen)
{
    if (!prompt)
        prompt = "";

    std::string full_prompt(prompt);

    if (full_prompt.length() > maxlen)
        full_prompt.erase(maxlen);

    return full_prompt;
}
#endif

#endif


#define TEXMACS_DATA_BEGIN   ((char)2)
#define TEXMACS_DATA_END     ((char)5)
#define TEXMACS_DATA_ESCAPE  ((char)27)

void ShowResult(const char *prompt)
{
    if (use_texmacs_out)
        std::cout << TEXMACS_DATA_BEGIN << "latex:";

    if (yacas->IsError())
        std::cout << yacas->Error() << "\n";
    else
        if (yacas->getDefEnv().getEnv().PrettyPrinter() == 0)
            std::cout << prompt << yacas->Result() << "\n";

    if (use_texmacs_out)
        std::cout << TEXMACS_DATA_END;

    std::cout << std::flush;
}

void DeclarePath(const char *ptr2)
{
    std::ostringstream os;

    if (ptr2[strlen(ptr2)-1] != PATH_SEPARATOR)
        os << "DefaultDirectory(\"" << ptr2 << PATH_SEPARATOR_2 << "\");";
    else
        os << "DefaultDirectory(\"" << ptr2 << "\");";

    yacas->Evaluate(os.str().c_str());
}

void LoadYacas(LispOutput* aOutput = 0)
{
    if (yacas)
        return;

    busy = true;
    restart = false;

    yacas = CYacas::NewL(aOutput,stack_size);


#define CORE_KERNEL_FUNCTION(iname,fname,nrargs,flags) yacas->getDefEnv().getEnv().SetCommand(fname,iname,nrargs,flags);

#include "core_yacasmain.h"

#undef CORE_KERNEL_FUNCTION

    {
        /* Split up root_dir in pieces separated by colons, and run
           DefaultDirectory on each of them. */
        const char *ptr1, *ptr2;
        ptr1 = ptr2 = root_dir;
        while (*ptr1 != '\0') {
            while (*ptr1 != '\0' && *ptr1 != ':') ptr1++;
            if (*ptr1 == ':') {
                const std::string path(ptr2, ptr1);
                DeclarePath(path.c_str());
                ptr1++;
                ptr2 = ptr1;
            }
        }
        DeclarePath(ptr2);
        if (!compiled_plugins)
            yacas->Evaluate("Set(LoadPlugIns,False);");

        std::ostringstream os;
        os << "Load(\"" << init_script << "\");";
        yacas->Evaluate(os.str().c_str());
        if (yacas->IsError())
        {
            ShowResult("");
            read_eval_print = 0;
        }
    }

    if (yacas->IsError())
        ShowResult("");

    if (use_texmacs_out)
        std::cout << TEXMACS_DATA_BEGIN << "verbatim:";

    std::ostringstream os;
    os << getenv("HOME") << "/.yacasrc";

    std::ifstream test(os.str().c_str(), std::ios::binary);
    if (test) {
        std::ostringstream os;
        os << "Load(\"" << getenv("HOME") << "/.yacasrc" << "\");";
        yacas->Evaluate(os.str().c_str());
    }

    if (use_texmacs_out)
        std::cout << TEXMACS_DATA_END;

    std::cout << std::flush;
}

#ifdef SIGHANDLER_NO_ARGS
void InterruptHandler(void)
#else
    void InterruptHandler(int errupt)
#endif
{
    std::cout << "^C pressed\n";
    yacas->getDefEnv().getEnv().iEvalDepth = yacas->getDefEnv().getEnv().iMaxEvalDepth+100;

    if (readmode)
        std::exit(EXIT_SUCCESS);
}


#ifdef SUPPORT_SERVER

CYacas* clientToStop = 0;

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
        clientToStop->getDefEnv().getEnv().iEvalDepth = clientToStop->getDefEnv().getEnv().iMaxEvalDepth+100;
}

#define MAX_CONNECTIONS  1000
#define BUFFER_CHUNKSIZE 256

int runserver(int argc,char** argv)
{
#ifdef WIN32
    if (hideconsolewindow) {
        // format a "unique" newWindowTitle
        char newWindowTitle[256];
        wsprintf(newWindowTitle, "%d/%d", GetTickCount(), GetCurrentProcessId());
        // change current window title
        SetConsoleTitle(newWindowTitle);
        // ensure window title has been updated
        Sleep(40);
        // look for newWindowTitle
        HWND hwndFound = FindWindow(0, newWindowTitle);
        // If found, hide it
        if (hwndFound)
            ShowWindow(hwndFound, SW_HIDE);
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
    bool serverbusy;

    int seconds = 30; // give each calculation only so many seconds
    if (server_single_user)
        seconds = 0;

    serverbusy=true;
    maxConnections=MAX_CONNECTIONS;
    nrSessions=0;

#ifndef WIN32
    signal(SIGPIPE,SIG_IGN);
#else
    WSADATA wsadata;

    if (WSAStartup(0x101, &wsadata)) {
        std::cerr << "YacasServer Could not initiate Winsock DLL\n";
        std::exit(EXIT_FAILURE);
    } else {
        winsockinitialised = true;
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
                std::cerr << "YacasServer Could not set socket options\n";
                std::exit(EXIT_FAILURE);
            }
    }

    server_address.sin_family = AF_INET;
    server_address.sin_addr.s_addr = htonl(INADDR_ANY);
    server_address.sin_port = htons(server_port);
    server_len = sizeof(server_address);

    std::cout << "Accepting requests from port " << server_port << "\n";

    if (bind(server_sockfd, (struct sockaddr *)&server_address, server_len)) {
        std::cerr << "YacasServer: Could not bind to the socket\n";
        std::exit(EXIT_FAILURE);
    }

    if (listen(server_sockfd, maxConnections)) {
        std::cerr << "YacasServer: Could not bind to the socket\n";
        std::exit(EXIT_FAILURE);
    }

    for (int i=0; i < MAX_CONNECTIONS; ++i)
        used_clients[i] = 0;

    FD_ZERO(&readfds);
    FD_SET(server_sockfd, &readfds);
    while(serverbusy)
    {
        int fd;
        int nread;
        testfds = readfds;

        result = select(FD_SETSIZE, &testfds, (fd_set *)0,
                        (fd_set *)0, (struct timeval *) 0);

        if(result < 1) {
            std::cerr << "YacasServer: select failed\n";
            std::exit(EXIT_FAILURE);
        }

#ifndef WIN32
        int socketcount = FD_SETSIZE;
#else
        int socketcount = readfds.fd_count;
#endif
        for(int sockindex = 0; sockindex < socketcount; ++sockindex) {
#ifndef WIN32
            while(waitpid(-1,0,WNOHANG) > 0); /* clean up child processes */
            fd = sockindex;
#else
            fd = readfds.fd_array[sockindex];
#endif
            if(FD_ISSET(fd,&testfds)) {
                if(fd == server_sockfd) {
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
                        std::cout << "adding client on fd " << client_sockfd << "\n";
#endif
                    }

                } else {
                    int clsockindex = sockindex - 1;
#ifndef WIN32
                    ioctl(fd, FIONREAD, &nread);
#else
                    ioctlsocket(fd, FIONREAD, (unsigned long *)&nread);
#endif
                    if(nread == 0) {
#ifndef WIN32
                        close(fd);
#else
                        closesocket(fd);
#endif
                        FD_CLR(fd, &readfds);
                        delete used_clients[clsockindex];
                        used_clients[clsockindex] = 0;
                        nrSessions--;

                        if (server_single_user && nrSessions == 0)
                            std::exit(EXIT_SUCCESS);

#ifdef YACAS_DEBUG
                        std::cout << "YacasServer: Removing client on " << fd << "\n";
#endif
                    } else {
                        std::vector<char> buffer(nread + 1);
                        std::string finalbuffer;

                        int bytesread;

#ifndef WIN32
                        while ((bytesread = read(fd, &buffer.front(), nread)) != 0)
#else
                        while(bytesread = recv(fd, &buffer.front(), nread, 0))
#endif
                        {
                            buffer[bytesread] = '\0';
                            finalbuffer.append(&buffer.front());

                            const std::size_t semi_pos =
                                finalbuffer.find_first_of(";");
                            if (semi_pos != std::string::npos) {
                                finalbuffer.erase(semi_pos);
                                break;
                            }
                        }

#ifdef YACAS_DEBUG
                        std::cout << "YacasServer: Servicing on " fd << " (" << used_clients[clsockindex] << ")\n";
#endif

                        if (clsockindex < maxConnections) {
                            if (used_clients[clsockindex] == 0) {

#ifdef YACAS_DEBUG
                                std::cout << "Loading new Yacas environment\n";
#endif
                                StringOutput *out = NEW StringOutput(outStrings);
                                LoadYacas(out);
                                used_clients[clsockindex] = yacas;
                                yacas = 0;
                                nrSessions++;
                            }

                            // enable if fork needed
                            //                        if (fork() == 0)
                            {
                                const char* response = finalbuffer.c_str();
                                if (!server_single_user)
                                    used_clients[clsockindex]->getDefEnv().getEnv().iSecure = 1;


                                if (seconds > 0) {
                                    clientToStop = used_clients[clsockindex];
#ifndef WIN32
                                    signal(SIGALRM,stopClient);
                                    alarm(seconds);
#else
                                    LARGE_INTEGER timedue;
                                    timedue.QuadPart = seconds  * -10000000;

                                    htimer = CreateWaitableTimer(0, true, "WaitableTimer");
                                    SetWaitableTimer(htimer, &timedue, 0, stopClient, 0, 0);
#endif
                                }
#ifdef YACAS_DEBUG
                                std::cout << "In> " << finalbuffer << "\n";
#endif
                                outStrings = "";
                                used_clients[clsockindex]->Evaluate(finalbuffer.c_str());

                                if (server_single_user && !busy)
                                    std::exit(EXIT_SUCCESS);

                                if (seconds>0) {
#ifndef WIN32
                                    signal(SIGALRM,SIG_IGN);
#else

                                    if (htimer)
                                        CancelWaitableTimer(htimer);
#endif
                                }

                                if (used_clients[clsockindex]->IsError())
                                    response = used_clients[clsockindex]->Error();
                                else
                                    response = used_clients[clsockindex]->Result();


                                const std::size_t buflen = std::strlen(response);
#ifdef YACAS_DEBUG
                                std::cout << outStrings.c_str()
                                          << "Out> " << response << "\n";
#endif
                                if (response) {
#ifndef WIN32
                                    ssize_t c =
                                        write(fd, outStrings.c_str(), strlen(outStrings.c_str()));
                                    if (c < 0)
                                        perror("yacasserver");

                                    c = write(fd,"]\r\n",3);
                                    if (c < 0)
                                        perror("yacasserver");

                                    if (buflen > 0) {
                                        c = write(fd, response, buflen);
                                        if (c < 0)
                                            perror("yacasserver");
                                        c = write(fd,"\r\n",2);
                                        if (c < 0)
                                            perror("yacasserver");
                                    }

                                    c = write(fd,"]\r\n",3);
                                    if (c < 0)
                                        perror("yacasserver");
#else
                                    send(fd, outStrings.c_str(), strlen(outStrings.c_str()), 0);
                                    send(fd,"]\r\n",3, 0);
                                    if (buflen > 0) {
                                        send(fd, response, buflen, 0);
                                        send(fd,"\r\n",2, 0);
                                    }
                                    send(fd,"]\r\n",3, 0);
#endif
                                }
                            }

                            // enable if fork needed
                            //                            std::exit(EXIT_SUCCESS);
                        } else {
                            const char* limtxt = "Maximum number of connections reached, sorry\r\n";
#ifndef WIN32
                            ssize_t c;
                            c = write(fd,"]\r\n",3);
                            if (c < 0)
                                perror("yacasserver");
                            c = write(fd, limtxt, strlen(limtxt));
                            if (c < 0)
                                perror("yacasserver");
                            c = write(fd,"]\r\n",3);
                            if (c < 0)
                                perror("yacasserver");
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

void runconsole(const char* inprompt, const char* outprompt)
{
    if (show_prompt) {
        if (use_texmacs_out) {
            std::cout << TEXMACS_DATA_BEGIN << "verbatim:"
                      << "This is Yacas version `"
                      << VERSION
                      << "' under TeXmacs\n"
                      << GPL_blurb_nohelp << TEXMACS_DATA_END;
        } else {
            std::cout << "This is Yacas version '" << VERSION << "'.\n";
#ifdef WIN32
            std::cout << GPL_blurb_nohelp;
#else
            std::cout << GPL_blurb;
#endif
            std::cout << "To exit Yacas, enter  Exit(); or quit or Ctrl-c.\n"
                      << "Type 'restart' to restart Yacas.\n"
                      << "To see example commands, keep typing Example();\n";
        }

        std::cout << std::flush;
    }

    if (read_eval_print) {
        while (busy) {
#ifdef YACAS_DEBUG
            LispLocalEvaluator local(yacas->getDefEnv().getEnv(), NEW TracedStackEvaluator);
#endif
            yacas->Evaluate(read_eval_print);

            if (yacas->IsError())
                std::cout << yacas->Error() << "\n";
        }
    } else {
        while (busy) {
#ifdef YACAS_DEBUG
            LispLocalEvaluator local(yacas->getDefEnv().getEnv(), NEW TracedStackEvaluator);
#endif

#ifdef PROMPT_SHOW_FREE_MEMORY
            std::string full_prompt = "";
            if (show_prompt)
                full_prompt = build_full_prompt(full_prompt, inprompt, 30);

            ReadInputString(full_prompt.c_str());
#else
            ReadInputString(inprompt);
#endif
            const char *inpline =  commandline->iLine.c_str();
            if (use_texmacs_out)
                std::cout << TEXMACS_DATA_BEGIN << "verbatim:";

            if (busy) {
                if (*inpline) {
                    if (use_texmacs_out)
                        std::cout << TEXMACS_DATA_BEGIN << "latex:";

                    yacas->Evaluate(inpline);

                    if (use_texmacs_out)
                        std::cout << TEXMACS_DATA_END;

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
                std::cout << TEXMACS_DATA_END;

            std::cout << std::flush;
        }
    }
}

int parse_options(int argc, char** argv)
{
    int fileind = 1;
    if (argc > 1) {

        for (; fileind < argc && argv[fileind][0] == '-'; ++fileind) {
            if (!std::strcmp(argv[fileind],"--texmacs")) {
                use_texmacs_out = true;
                use_plain = true;
                read_eval_print = 0;
            } else if (!std::strcmp(argv[fileind],"--patchload")) {
                patchload = true;
            } else if (!std::strcmp(argv[fileind],"--verbose-debug")) {
#ifdef YACAS_DEBUG
                verbose_debug = true;
#else
                std::cout << "Warning: --verbose-debug is only supported in debug the version of this program.\n";
#endif
            } else if (!std::strcmp(argv[fileind],"--init")) {
                fileind++;
                if (fileind<argc)
                    init_script = argv[fileind];
            } else if (!std::strcmp(argv[fileind],"--read-eval-print")) {
                fileind++;

                if (fileind<argc) {
                    if (argv[fileind][0])
                        read_eval_print = argv[fileind];
                    else
                        read_eval_print = 0;
                }
            } else if (!std::strcmp(argv[fileind],"--rootdir")) {
                fileind++;
                if (fileind < argc)
                    root_dir = argv[fileind];
            } else if (!std::strcmp(argv[fileind],"--server")) {
                fileind++;
                if (fileind < argc) {
                    server_mode = true;
                    server_port = atoi(argv[fileind]);
                }
            } else if (!std::strcmp(argv[fileind],"--single-user-server")) {
                server_single_user = true;
            } else if (!std::strcmp(argv[fileind],"--stacksize")) {
                fileind++;
                if (fileind < argc)
                    stack_size = atoi(argv[fileind]);
            } else if (!std::strcmp(argv[fileind],"--execute")) {
                fileind++;
                if (fileind < argc)
                    execute_commnd = argv[fileind];
            } else if (!std::strcmp(argv[fileind],"--disable-compiled-plugins")) {
                compiled_plugins = false;
            } else if (!std::strcmp(argv[fileind],"-i")) {
                fileind++;
                if (fileind < argc) {
                    const char* immediate = argv[fileind];
                    if (immediate) {
                        LoadYacas();

                        if (use_texmacs_out)
                            yacas->getDefEnv().getEnv().SetPrettyPrinter(yacas->getDefEnv().getEnv().HashTable().LookUp("\"TexForm\""));

                        yacas->Evaluate(immediate);

                        if (yacas->IsError())
                            std::cout << "Error in immediate command " << immediate << ":\n"
                                      << yacas->Error() << "\n";

                        exit_after_files = true;
                    }
                }
            } else {
                if (strchr(argv[fileind],'f')) {
                    use_stdin = true;
                }
                if (strchr(argv[fileind],'p')) {
                    use_plain = true;
                }
                if (strchr(argv[fileind],'c')) {
                    show_prompt = false;
                }
                if (strchr(argv[fileind],'t')) {
                    // Not sure what we should do with -t ... it used to be trace_history
                }
                if (strchr(argv[fileind],'d')) {
                    std::cout << SCRIPT_DIR << "\n";
                    return -1;
                }
                if (strchr(argv[fileind],'w')) {
                    hideconsolewindow = true;
                }

#ifdef HAVE_CONFIG_H
                if (strchr(argv[fileind],'v')) {
                    std::cout << VERSION << "\n";
                    return -1;
                }
#endif

#ifndef NO_GLOBALS
                if (strchr(argv[fileind],'m')) {
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
    }

    return fileind;
}

int main(int argc, char** argv)
{
    unsigned char first_stack_var = 0;
    the_first_stack_var = &first_stack_var;


// #ifdef YACAS_DEBUG
// //        PlatAlloc(100); // test the alloc memory leak checker
//     CHECKPTR(0);
// #endif

    int fileind = parse_options(argc, argv);

    if (fileind < 0)
        return 0;

    std::atexit(my_exit);

#ifdef SUPPORT_SERVER
    if (server_mode) {
        runserver(argc,argv);
        std::exit(EXIT_SUCCESS);
    }
#endif

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

    const char* inprompt = "";
    const char* outprompt = "";

    if (show_prompt && !use_texmacs_out) {
        inprompt = "In> ";
        outprompt = "Out> ";
    }

    LoadYacas();

    if (use_texmacs_out)
        yacas->getDefEnv().getEnv().SetPrettyPrinter(yacas->getDefEnv().getEnv().HashTable().LookUp("\"TexForm\""));

    for ( ; fileind<argc; fileind++) {
        std::ostringstream os;
        if (patchload)
            os << "PatchLoad(\"" << argv[fileind] << "\");";
        else
            os << "Load(\"" << argv[fileind] << "\");";

        yacas->Evaluate(os.str().c_str());

        if (yacas->IsError())
            std::cout << "Error in file " << argv[fileind] << "\n"
                      << yacas->Error() << "\n";

        exit_after_files = true;
    }

    if (exit_after_files)
        std::exit(EXIT_SUCCESS);

    if (show_prompt && (!use_texmacs_out))
        ShowResult("");

    if (execute_commnd) {

        yacas->Evaluate(execute_commnd);

        if (yacas->IsError())
            std::cout << "Error in file " << argv[fileind] << "\n"
                      << yacas->Error() << "\n";

        if (show_prompt && (!use_texmacs_out))
            ShowResult("");
    }

    if (use_stdin) {
        std::string buffer;

        do {
            std::string line;
            std::getline(std::cin, line);
            buffer.append(line);
        } while(std::cin.good());

        yacas->Evaluate(buffer.c_str());
        ShowResult(outprompt);

        std::exit(EXIT_SUCCESS);
    }

    do {
        runconsole(inprompt, outprompt);

        if (restart) {
            delete yacas;
            yacas = 0;
            LoadYacas();
        }

    } while (restart);

    std::exit(EXIT_SUCCESS);
}

