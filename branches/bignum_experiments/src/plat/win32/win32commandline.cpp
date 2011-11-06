#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <windows.h>

#include "win32commandline.h"

#define BufSz 1024

bool is_NT_or_later = false;

/*
    This displays a message box.
*/
static void win_assert(BOOL condition){
    if(condition) return;
    LPVOID lpMsgBuf;
    FormatMessage( 
        FORMAT_MESSAGE_ALLOCATE_BUFFER | 
        FORMAT_MESSAGE_FROM_SYSTEM | 
        FORMAT_MESSAGE_IGNORE_INSERTS,
        NULL,
        GetLastError(),
        MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
        (LPTSTR) &lpMsgBuf,
        0,
        NULL);

    MessageBox( NULL, (LPCTSTR)lpMsgBuf, "Error", MB_OK | MB_ICONINFORMATION );
    // Free the buffer.
    LocalFree(lpMsgBuf);
    exit(1);
}

void CWin32CommandLine::color_print(const LispChar * str, WORD text_attrib){
    BOOL status;
    unsigned len = strlen(str);
    CONSOLE_SCREEN_BUFFER_INFO old_info;

    out_console = GetStdHandle(STD_OUTPUT_HANDLE);
    win_assert(INVALID_HANDLE_VALUE != out_console);
    status = GetConsoleScreenBufferInfo(out_console, &old_info);
    win_assert(status);

    WORD old_attrib = old_info.wAttributes;

    status = SetConsoleTextAttribute(out_console, text_attrib);
    win_assert(status);

    DWORD written;
    status = WriteConsole(out_console, str, len, &written, NULL);
    win_assert(status);
    // restore the attributes
    status = SetConsoleTextAttribute(out_console, old_attrib);
    win_assert(status);
}

void CWin32CommandLine::color_read(LispChar * str, WORD text_attrib){
    BOOL status;
    CONSOLE_SCREEN_BUFFER_INFO old_info;

    out_console = GetStdHandle(STD_OUTPUT_HANDLE);
    win_assert(INVALID_HANDLE_VALUE != out_console);
    status = GetConsoleScreenBufferInfo(out_console, &old_info);
    win_assert(status);

    WORD old_attrib = old_info.wAttributes;

    status = SetConsoleTextAttribute(out_console, text_attrib);

    DWORD read;
    status = ReadConsole(in_console, str, 80, &read, NULL);
    str[read - 2] = '\0';
    win_assert(status);
    // restore the attributes
    status = SetConsoleTextAttribute(out_console, old_attrib);
    win_assert(status);
}

void CWin32CommandLine::NewLine()
{
  ShowLine();
  color_print("\n", 0);
}

void CWin32CommandLine::Pause()
{
    int i = clock()+CLOCKS_PER_SEC/4;
    while (clock()<i);
}

void CWin32CommandLine::ReadLineSub(LispChar * prompt){
//    if(_is_NT_or_later){
//        char buff[BufSz];
//        color_print(prompt, FOREGROUND_RED | FOREGROUND_INTENSITY );
//        color_read(buff, FOREGROUND_GREEN | FOREGROUND_BLUE | FOREGROUND_INTENSITY);
//        iSubLine = buff;
//    }else{
        CCommandLine::ReadLineSub(prompt);
//    }
}

void CWin32CommandLine::ShowLine(){
  ShowLine(iLastPrompt, strlen(iLastPrompt), strlen(iLastPrompt)+iSubLine.Size());
}

void CWin32CommandLine::ShowLine(LispChar * prompt, LispInt promptlen, LispInt cursor){
  iLastPrompt = prompt;
  putchar('\r');							// clear line
  int i;
	for (i=0;i<79;i++) putchar(' ');

    assert(iSubLine.Size() != 0);
    char str[BufSz];
    sprintf(str, "\r%s%s", prompt, &iSubLine[0]);
    color_print(str, FOREGROUND_GREEN | FOREGROUND_BLUE | FOREGROUND_INTENSITY );


	i = strlen(&iSubLine[0]) + promptlen;		// position cursor
	for (;i > cursor+promptlen; i--)
		putchar('\b');
    fflush(stdout);
}

CWin32CommandLine::CWin32CommandLine() :
    out_console(GetStdHandle(STD_OUTPUT_HANDLE)),
    in_console(GetStdHandle(STD_INPUT_HANDLE)),
    _is_NT_or_later(false)
{
    win_assert(INVALID_HANDLE_VALUE != out_console);
    win_assert(INVALID_HANDLE_VALUE != in_console);
    
    // figure out the version of windows
    OSVERSIONINFO osvi; 
    osvi.dwOSVersionInfoSize  = sizeof(OSVERSIONINFO);
    GetVersionEx(&osvi);

    _is_NT_or_later = (osvi.dwPlatformId == VER_PLATFORM_WIN32_NT);
        /* &&
        ( (osvi.dwMajorVersion > 4) ||
        ( (osvi.dwMajorVersion == 4) && (osvi.dwMinorVersion > 0) ) );*/
 
//    if(!_is_NT_or_later)
    {
        FILE*f=fopen("history.log", "r");
        if(f){
            if(f){
                char buff[BufSz];
                while(fgets(buff,BufSz-2,f))
                {
                    int i;
                    for(i=0;buff[i] && buff[i] != '\n';++i)
                        ;
                    buff[i++] = '\0';
                    LispString * ptr = NEW LispString(buff);
                    iHistoryList.Append(ptr);
                
                }
                fclose(f);
            }
        }
    }
}

CWin32CommandLine::~CWin32CommandLine()
{
//    if(!_is_NT_or_later)
    {
        FILE*f=fopen("history.log","w");
        if (f){
            int i;
            for (i=0;i<iHistoryList.NrLines();i++)
            {
                fprintf(f,"%s\n",iHistoryList.GetLine(i)->c_str());
            }
            fclose(f);
        }
    }
}

LispInt CWin32CommandLine::GetKey(){
    LispInt c;
    c = _getch();

//    if(!_is_NT_or_later){
//        return c;
//    }

	switch (c) {
		case 8:
			c = eBackSpace;		// Backspace
            break;
		case 9:					//  Tab
            c = eTab;
			break;
        case 13:				// Enter
            c = eEnter;
            break;
        case 0xE0:
            c = _getch();		// Get extended scan code
			switch (c) {
                case 75:		// left arrow key
                    c = eLeft;
                    break;
                case 77:		// right arrow key
                    c = eRight;
                    break;
                case 72:		// up arrow key
                    c = eUp;
                    break;
                case 80:		// down arrow key
                    c = eDown;
                    break;
                case 71:		// home
                    c = eHome;
                    break;
                case 79:		// end
                    c = eEnd;
                    break;
				case 83:		// delete
					c = eDelete;
					break;
			}
        break;
	}

    return c;
}
