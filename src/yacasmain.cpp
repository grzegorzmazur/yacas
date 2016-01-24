
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
//      - w : hides the console window in Windows
//   4)
//  -i <command> : execute <command>
//
// Example: 'yacas -pc' will use minimal command line interaction,
//          showing no prompts, and with no readline functionality.
//

#include "yacas/yacasprivate.h"

#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif

#include <ctime>
#include <cstring>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <string>
#include <sstream>
#include <vector>

#define PATH_SEPARATOR   '/'
#define PATH_SEPARATOR_2 "/"

#include "yacas/yacas.h"

#ifndef _WIN32
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <libgen.h>

#include "yacas/unixcommandline.h"
#define FANCY_COMMAND_LINE CUnixCommandLine
#else
#define _WINSOCKAPI_            // Prevent inclusion of winsock.h in windows.h
#define _WIN32_WINDOWS 0x0410      // Make sure that Waitable Timer functions are declared in winbase.h
#include "yacas/win32commandline.h"
#define FANCY_COMMAND_LINE CWin32CommandLine
#include <windows.h>
#include <shlobj.h>
#include <shlwapi.h>
#endif

#if defined (__FreeBSD__) || defined (__DragonFly__)
#include <stddef.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/sysctl.h>
#endif

#include "yacas/stdcommandline.h"
#include "yacas/standard.h"
#include "yacas/numbers.h"
#include "yacas/arggetter.h"

#include "yacas/errors.h"
#include "yacas/string_utils.h"

#ifndef YACAS_VERSION
#include "yacas/yacas_version.h"
#endif

#ifdef SUPPORT_SERVER
#include <cstdlib>
#include <sys/types.h>

#ifndef _WIN32
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#else
#include <winsock2.h>
#endif

#define SOCKLEN_T unsigned int //socklen_t
#endif

#if defined(__APPLE__)
#include <mach-o/dyld.h>
#endif

#include "yacas/GPL_stuff.h"

CYacas* yacas = nullptr;
CCommandLine *commandline = nullptr;

bool use_stdin = false;
bool use_plain = false;
bool show_prompt = true;
bool use_texmacs_out = false;

int stack_size = 50000;

#ifdef YACAS_DEBUG
bool verbose_debug = true;
#endif
bool patchload = false;
bool winsockinitialised = false;
bool hideconsolewindow = false;
bool exit_after_files = false;

std::string root_dir;
std::string doc_dir;
#ifdef _WIN32
HANDLE htimer = 0;
#endif
std::string init_script = "yacasinit.ys";

const char* read_eval_print = "REP()";


static bool readmode = false;

bool server_mode = false;
bool server_single_user = false;
int server_port = 9734;

const char* execute_commnd = nullptr;

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

#ifdef _WIN32
std::string get_default_browser()
{
	HKEY key;
	RegOpenKeyEx(HKEY_CLASSES_ROOT, "http\\shell\\open\\command", 0, KEY_QUERY_VALUE, &key);
	TCHAR buf[256];
	DWORD size = 256;
	RegQueryValueEx(key, nullptr, nullptr, nullptr, (LPBYTE)buf, &size);
	RegCloseKey(key);

	return buf;
}
#endif

std::string ReadInputString(const std::string& prompt)
{
    if (!commandline)
        return "False";

    readmode = true;
    commandline->ReadLine(prompt);
    readmode = false;
    std::string inpline =  commandline->iLine;

    trim(inpline);

    if (inpline.empty())
        return "True";

    if (inpline == "restart") {
        restart = true;
        busy = false;
    } else if (inpline == "quit") {
        busy=false;
    } else if (inpline.front() == '?') {
        const std::string key(inpline.begin() + 1, inpline.end());

        const std::string prefix = "file://" + doc_dir + "/index.html";
        std::string url = prefix + "#" + key;
        if (key == "licence" || key == "license" || key == "warranty")
            url = prefix + "#document-license";
        else if (key == "?")
            url = prefix + "#document-reference_manual/index";

#ifndef _WIN32
        
#if defined (__APPLE__)
        const std::string cmd = "osascript -e 'open location \"" + url + "\"'";
#else
        const std::string viewer = "xdg-open";
        const std::string cmd = viewer + " " + url;
#endif
        if (system(cmd.c_str()) == 0)
            inpline = "True";
        else
            inpline = "False";
#else
		const std::string viewer = get_default_browser();

		std::string cmd = viewer;
		cmd.replace(cmd.find("%1"), 2, url);

		STARTUPINFO si;
		PROCESS_INFORMATION pi;

		ZeroMemory(&si, sizeof si);
		ZeroMemory(&pi, sizeof pi);

		si.cb = sizeof si;

		if (CreateProcess(nullptr, (LPSTR)cmd.c_str(), nullptr, nullptr, 0, 0,  nullptr, nullptr, &si, &pi))
            inpline = "True";
        else
            inpline = "False";
#endif
    }

    return inpline;
}

static void LispReadCmdLineString(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    CheckArgIsString(1, aEnvironment, aStackTop);
    LispPtr promptObject = (ARGUMENT(1));
    const std::string prompt = InternalUnstringify(*promptObject->String());
    const std::string output = ReadInputString(prompt);
    RESULT = LispAtom::New(aEnvironment, stringify(output));
}

static void LispHistorySize(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispInt depth = GetShortIntegerArgument(aEnvironment, aStackTop, 1);

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

    RESULT = LispAtom::New(aEnvironment, os.str());
}

void LispFileSize(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    CheckArgIsString(1, aEnvironment, aStackTop);
    LispPtr fnameObject = (ARGUMENT(1));
    const std::string fname = InternalUnstringify(*fnameObject->String());

    std::ifstream in(fname.c_str(), std::ifstream::in | std::ifstream::binary);
    in.seekg(0, std::ifstream::end);
    std::ostringstream os;
    os << in.tellg();
    RESULT = LispAtom::New(aEnvironment, os.str());
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
            commandline = nullptr;
        }

        if (yacas) {
            delete yacas;
            yacas = nullptr;
        }

        ReportNrCurrent();
#ifdef YACAS_DEBUG
        CheckAllPtrs(1);
#endif
    }
#ifdef _WIN32
#ifdef SUPPORT_SERVER
    if (winsockinitialised)
    {
        WSACleanup();
        winsockinitialised = false;
    }
#endif
#endif
}

#define TEXMACS_DATA_BEGIN   ((char)2)
#define TEXMACS_DATA_END     ((char)5)
#define TEXMACS_DATA_ESCAPE  ((char)27)

void ShowResult(const std::string& prompt)
{
    if (use_texmacs_out)
        std::cout << TEXMACS_DATA_BEGIN << "latex:";

    if (yacas->IsError())
        std::cout << yacas->Error() << "\n";
    else
        if (yacas->getDefEnv().getEnv().PrettyPrinter() == nullptr)
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

    yacas->Evaluate(os.str());

    if (yacas->IsError())
        std::cout << "Failed to set default directory: " << yacas->Error() << "\n";
}

void LoadYacas(std::ostream& os)
{
    if (yacas)
        return;

    busy = true;

    yacas = NEW CYacas(os, stack_size);


#define CORE_KERNEL_FUNCTION(iname,fname,nrargs,flags) yacas->getDefEnv().getEnv().SetCommand(fname,iname,nrargs,flags);

#include "yacas/core_yacasmain.h"

#undef CORE_KERNEL_FUNCTION

    {
        /* Split up root_dir in pieces separated by colons, and run
           DefaultDirectory on each of them. */
        const char *ptr1, *ptr2;
        ptr1 = ptr2 = root_dir.c_str();
        while (*ptr1 != '\0') {
#ifndef _WIN32
            while (*ptr1 != '\0' && *ptr1 != ':')
                ptr1++;
            if (*ptr1 == ':') {
#else
            while (*ptr1 != '\0' && *ptr1 != ';')
                ptr1++;
            if (*ptr1 == ';') {
#endif
                const std::string path(ptr2, ptr1);
                DeclarePath(path.c_str());
                ptr1++;
                ptr2 = ptr1;
            }
        }
        DeclarePath(ptr2);

        std::ostringstream os;
        os << "Load(\"" << init_script << "\");";
        yacas->Evaluate(os.str());
        if (yacas->IsError())
        {
            ShowResult("");
            read_eval_print = nullptr;
        }
    }

    if (yacas->IsError())
        ShowResult("");

    if (use_texmacs_out)
        std::cout << TEXMACS_DATA_BEGIN << "verbatim:";

#ifdef _WIN32
    char appdata_dir_buf[MAX_PATH];
    SHGetFolderPathA(nullptr, CSIDL_APPDATA, nullptr, SHGFP_TYPE_CURRENT, appdata_dir_buf);

    const std::string yacas_data_dir = std::string(appdata_dir_buf) + "\\yacas";
    std::string yacasrc_path = yacas_data_dir + "\\yacasrc";

    std::ifstream test(yacasrc_path.c_str());
    if (test) {
        for (char& c: yacasrc_path)
            if (c == '\\')
                c = '/';

        std::ostringstream os;
        os << "Load(\"" << yacasrc_path << "\");";
        yacas->Evaluate(os.str());
    }
#else
    if (const char* home = getenv("HOME")) {
        std::ostringstream os;
        os << home << "/.yacasrc";

        std::ifstream test(os.str().c_str(), std::ios::binary);
        if (test) {
            std::ostringstream os;
            os << "Load(\"" << home << "/.yacasrc" << "\");";
            yacas->Evaluate(os.str());
        }
    }
#endif

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
    yacas->getDefEnv().getEnv().stop_evaluation = true;

    if (readmode)
        std::exit(EXIT_SUCCESS);
}


#ifdef SUPPORT_SERVER

CYacas* clientToStop = 0;

#ifndef _WIN32
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
#ifdef _WIN32
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

#ifndef _WIN32
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
#ifndef _WIN32
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

#ifndef _WIN32
        int socketcount = FD_SETSIZE;
#else
        int socketcount = readfds.fd_count;
#endif
        for(int sockindex = 0; sockindex < socketcount; ++sockindex) {
#ifndef _WIN32
            while(waitpid(-1,0,WNOHANG) > 0); /* clean up child processes */
            fd = sockindex;
#else
            fd = readfds.fd_array[sockindex];
#endif
            if(FD_ISSET(fd,&testfds)) {
                if(fd == server_sockfd) {
                    client_len = sizeof(client_address);
#ifndef _WIN32
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
#ifndef _WIN32
                    ioctl(fd, FIONREAD, &nread);
#else
                    ioctlsocket(fd, FIONREAD, (unsigned long *)&nread);
#endif
                    if(nread == 0) {
#ifndef _WIN32
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

#ifndef _WIN32
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
                                std::stringstream* out = new std::stringstream;
                                LoadYacas(*out);
                                used_clients[clsockindex] = yacas;
                                yacas = 0;
                                nrSessions++;
                            }

                            // enable if fork needed
                            //                        if (fork() == 0)
                            {
                                const char* response = finalbuffer.c_str();
                                if (!server_single_user)
                                    used_clients[clsockindex]->getDefEnv().getEnv().secure = true;


                                if (seconds > 0) {
                                    clientToStop = used_clients[clsockindex];
#ifndef _WIN32
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
                                outStrings.clear();
                                used_clients[clsockindex]->Evaluate(finalbuffer.c_str());

                                if (server_single_user && !busy)
                                    std::exit(EXIT_SUCCESS);

                                if (seconds>0) {
#ifndef _WIN32
                                    signal(SIGALRM,SIG_IGN);
#else

                                    if (htimer)
                                        CancelWaitableTimer(htimer);
#endif
                                }

                                if (used_clients[clsockindex]->IsError())
                                    response = used_clients[clsockindex]->Error().c_str();
                                else
                                    response = used_clients[clsockindex]->Result().c_str();


                                const std::size_t buflen = std::strlen(response);
#ifdef YACAS_DEBUG
                                std::cout << outStrings.c_str()
                                          << "Out> " << response << "\n";
#endif
                                if (response) {
#ifndef _WIN32
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
#ifndef _WIN32
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
#ifdef _WIN32
    WSACleanup();
#endif
    return 0;
}
#endif

void runconsole(const std::string& inprompt, const std::string& outprompt)
{
    if (show_prompt) {
        if (use_texmacs_out) {
            std::cout << TEXMACS_DATA_BEGIN << "verbatim:"
                      << "This is Yacas version `"
                      << YACAS_VERSION
                      << "' under TeXmacs\n"
                      << GPL_blurb_nohelp << TEXMACS_DATA_END;
        } else {
            std::cout << "This is Yacas version '" << YACAS_VERSION << "'.\n";
            std::cout << GPL_blurb;
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

            ReadInputString(inprompt);

            const std::string inpline =  commandline->iLine;
            if (use_texmacs_out)
                std::cout << TEXMACS_DATA_BEGIN << "verbatim:";

            if (busy) {
                if (!inpline.empty()) {
                    if (use_texmacs_out)
                        std::cout << TEXMACS_DATA_BEGIN << "latex:";

                    yacas->Evaluate(inpline);

                    if (use_texmacs_out)
                        std::cout << TEXMACS_DATA_END;

                    ShowResult(outprompt);
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
                read_eval_print = nullptr;
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
                        read_eval_print = nullptr;
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
            } else if (!std::strcmp(argv[fileind],"-i")) {
                fileind++;
                if (fileind < argc) {
                    const char* immediate = argv[fileind];
                    if (immediate) {
                        LoadYacas(std::cout);

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
                if (std::strchr(argv[fileind],'f')) {
                    use_stdin = true;
                }
                if (std::strchr(argv[fileind],'p')) {
                    use_plain = true;
                }
                if (std::strchr(argv[fileind],'c')) {
                    show_prompt = false;
                }
                if (std::strchr(argv[fileind],'d')) {
                    std::cout << root_dir << "\n";
                    std::exit(EXIT_SUCCESS);
                }
                if (std::strchr(argv[fileind],'w')) {
                    hideconsolewindow = true;
                }

#ifdef HAVE_CONFIG_H
                if (std::strchr(argv[fileind],'v')) {
                    std::cout << YACAS_VERSION << "\n";
                    return -1;
                }
#endif

#ifndef NO_GLOBALS
                if (std::strchr(argv[fileind],'m')) {
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
#if defined (__APPLE__)
    char buf[PATH_MAX];
    uint32_t size = sizeof (buf);
    _NSGetExecutablePath(buf, &size);

    char path[PATH_MAX];
    realpath(buf, path);
    root_dir = dirname(dirname(path));
#elif defined (__FreeBSD__) || defined (__DragonFly__)
    int mib[4] = { CTL_KERN, KERN_PROC, KERN_PROC_PATHNAME, -1 };
    char buf[PATH_MAX] = {};
    size_t cb = sizeof(buf);
    if (sysctl(mib, 4, buf, &cb, NULL, 0) != 0) {
        std::cerr << "yacas: failed to locate the executable, bailing out\n";
        exit(EXIT_FAILURE);
    }
    root_dir = dirname(dirname(buf));
#elif defined(__linux__)
    {
        struct stat sb;
        if (stat("/proc/self/exe", &sb) == -1) {
            std::cerr << "yacas: failed to stat /proc/self/exe, bailing out\n";
            exit(EXIT_FAILURE);
        }

        std::vector<char> buf(sb.st_size + 1);

        const ssize_t r = readlink("/proc/self/exe", buf.data(), sb.st_size + 1);

        if (r == -1) {
            std::cerr << "yacas: failed to read /proc/self/exe, bailing out\n";
            std::exit(EXIT_FAILURE);
        }

        if (r > sb.st_size) {
            std::cerr << "yacas: /proc/self/exe changed between stat and readlink\n";
            std::exit(EXIT_FAILURE);
        }

        buf[r] = '\0';

        root_dir = dirname(dirname(buf.data()));
    }
#elif defined(_WIN32)
    char buf[MAX_PATH];
    if (!GetModuleFileName(nullptr, buf, MAX_PATH)) {
            std::cerr << "yacas: failed to locate the executable, bailing out\n";
            exit(EXIT_FAILURE);
    }

    PathRemoveFileSpec(buf);
    PathRemoveFileSpec(buf);

    root_dir = buf;

    for (char& c: root_dir)
        if (c == '\\')
            c = '/';
#elif defined(EMSCRIPTEN)
    root_dir = "";
    use_plain = true;
#else
#error "This platform is not yet supported. Please contact developers at yacas@googlegroups.com"
#endif

    doc_dir = root_dir + "/share/yacas/documentation/singlehtml";
    root_dir += "/share/yacas/scripts";

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

    if (!use_plain) {
        try {
            commandline = new FANCY_COMMAND_LINE;
        } catch (const std::runtime_error&) {
            use_plain = true;
        }
    }

    if (use_plain)
        commandline = new CStdCommandLine;

    std::string inprompt;
    std::string outprompt;

    if (show_prompt && !use_texmacs_out) {
        inprompt = "In> ";
        outprompt = "Out> ";
    }

    LoadYacas(std::cout);

    if (use_texmacs_out)
        yacas->getDefEnv().getEnv().SetPrettyPrinter(yacas->getDefEnv().getEnv().HashTable().LookUp("\"TexForm\""));

    for ( ; fileind<argc; fileind++) {
        std::ostringstream os;
        if (patchload)
            os << "PatchLoad(\"" << argv[fileind] << "\");";
        else
            os << "Load(\"" << argv[fileind] << "\");";

        yacas->Evaluate(os.str());

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

        yacas->Evaluate(buffer);
        ShowResult(outprompt);

        std::exit(EXIT_SUCCESS);
    }

    do {
        restart = false;

        runconsole(inprompt, outprompt);

        if (restart) {
            delete yacas;
            yacas = nullptr;
            LoadYacas(std::cout);
        }

    } while (restart);

    std::exit(EXIT_SUCCESS);
}

