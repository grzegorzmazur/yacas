#include "yacas/win32commandline.h"

#include <fstream>
#include <string>

#include <conio.h>

#include <windows.h>
#include <shlobj.h>
#include <shlwapi.h>

/*
  This displays a message box.
*/
static void win_assert(BOOL condition)
{
    if (condition)
        return;

    LPVOID lpMsgBuf;
    FormatMessage(
        FORMAT_MESSAGE_ALLOCATE_BUFFER |
        FORMAT_MESSAGE_FROM_SYSTEM |
        FORMAT_MESSAGE_IGNORE_INSERTS,
        nullptr,
        GetLastError(),
        MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
        (LPTSTR) &lpMsgBuf,
        0,
        nullptr);

    MessageBox( nullptr, (LPCTSTR)lpMsgBuf, "Error", MB_OK | MB_ICONINFORMATION );

    LocalFree(lpMsgBuf);

    exit(1);
}

void CWin32CommandLine::color_print(const std::string& str, WORD text_attrib)
{
    BOOL status;
    CONSOLE_SCREEN_BUFFER_INFO old_info;

    status = GetConsoleScreenBufferInfo(out_console, &old_info);
    win_assert(status);

    WORD old_attrib = old_info.wAttributes;

    status = SetConsoleTextAttribute(out_console, text_attrib);
    win_assert(status);

    DWORD written;
    status = WriteConsole(out_console, str.c_str(), str.length(), &written, nullptr);
    win_assert(status);
    // restore the attributes
    status = SetConsoleTextAttribute(out_console, old_attrib);
    win_assert(status);
}

void CWin32CommandLine::NewLine()
{
    _cursor_line = 0;
    _last_line = 0;

    color_print("\n", 0);
}

void CWin32CommandLine::Pause()
{
    Sleep(250);
}

void CWin32CommandLine::ShowLine(const std::string& prompt, unsigned cursor)
{
    CONSOLE_SCREEN_BUFFER_INFO csbi;
    GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &csbi);

    const SHORT no_cols = csbi.dwSize.X;
    const SHORT no_rows = csbi.dwSize.Y;

    const std::size_t prompt_len = prompt.length();

    const unsigned l = (cursor + prompt_len) / no_cols;
    const unsigned c = (cursor + prompt_len) % no_cols;

    COORD coords;

    coords.X = 0;
    coords.Y = csbi.dwCursorPosition.Y - _cursor_line;

    SetConsoleCursorPosition(out_console, coords);

    GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &csbi);

    if (full_line_dirty) {
        for (int i = 0; i <= _last_line; ++i) {
            coords.X = 0;
            coords.Y = csbi.dwCursorPosition.Y + i;
            DWORD no_written;
            FillConsoleOutputCharacter(out_console, ' ', no_cols, coords, &no_written);
        }

        const std::string line = prompt + iSubLine.c_str();
        color_print(line.c_str(), FOREGROUND_GREEN | FOREGROUND_BLUE | FOREGROUND_INTENSITY );

        _last_line = line.length() / no_cols;

        if (csbi.dwCursorPosition.Y + _last_line >= no_rows)
            csbi.dwCursorPosition.Y -= csbi.dwCursorPosition.Y + _last_line + 1 - no_rows;
    }

    coords.X = c;
    coords.Y = csbi.dwCursorPosition.Y + l;
    SetConsoleCursorPosition(out_console, coords);

    _cursor_line = l;

    full_line_dirty = false;
}

CWin32CommandLine::CWin32CommandLine() :
    out_console(GetStdHandle(STD_OUTPUT_HANDLE)),
    _cursor_line(0),
    _last_line(0),
    _max_lines(1024)
{
    win_assert(INVALID_HANDLE_VALUE != out_console);

    char appdata_dir_buf[MAX_PATH];
    SHGetFolderPathA(nullptr, CSIDL_APPDATA, nullptr, SHGFP_TYPE_CURRENT, appdata_dir_buf);

    const std::string yacas_data_dir = std::string(appdata_dir_buf) + "\\yacas";

    std::ifstream is((yacas_data_dir + "\\history.log").c_str());

    while (is) {
        std::string line;
        std::getline(is, line);
        iHistoryList.Append(line.c_str());
    }
}

CWin32CommandLine::~CWin32CommandLine()
{
    char appdata_dir_buf[MAX_PATH];
    SHGetFolderPathA(nullptr, CSIDL_APPDATA, nullptr, SHGFP_TYPE_CURRENT, appdata_dir_buf);

    const std::string yacas_data_dir = std::string(appdata_dir_buf) + "\\yacas";

    if (!PathFileExistsA(yacas_data_dir.c_str()))
        CreateDirectoryA(yacas_data_dir.c_str(), nullptr);

    std::ofstream os((yacas_data_dir + "\\history.log").c_str());

    if (os) {
        std::size_t from = 0;

        if (_max_lines > 0 && iHistoryList.NrLines() > _max_lines)
            from = iHistoryList.NrLines() - _max_lines;

        for (std::size_t i = from; i < iHistoryList.NrLines(); ++i)
            os << iHistoryList.GetLine(i).c_str() << "\n";
    }
}

void CWin32CommandLine::MaxHistoryLinesSaved(std::size_t n)
{
    _max_lines = n;
}

char32_t CWin32CommandLine::GetKey()
{
    // FIXME: support surrogates?
    char32_t c = _getwch();

    switch (c) {
    case 8:
        c = eBackSpace;         // Backspace
        break;
    case 9:                                     //  Tab
        c = eTab;
        break;
    case 13:                            // Enter
        c = eEnter;
        break;
    case 0xE0:
        c = _getwch();           // Get extended scan code
        switch (c) {
        case 75:                // left arrow key
            c = eLeft;
            break;
        case 77:                // right arrow key
            c = eRight;
            break;
        case 72:                // up arrow key
            c = eUp;
            break;
        case 80:                // down arrow key
            c = eDown;
            break;
        case 71:                // home
            c = eHome;
            break;
        case 79:                // end
            c = eEnd;
            break;
        case 83:                // delete
            c = eDelete;
            break;
        }
        break;
    }

    return c;
}
