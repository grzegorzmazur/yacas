#include "yacas/yacasprivate.h"
#include "yacas/unixcommandline.h"

#include <sys/ioctl.h>

#include <iostream>
#include <fstream>
#include <stdexcept>



void CUnixCommandLine::NewLine()
{
    _cursor_line = 0;
    _last_line = 0;

    std::cout << std::endl;
}

void CUnixCommandLine::Pause()
{
    clock_t i=clock()+CLOCKS_PER_SEC/4;
    while (clock()<i);
}

void CUnixCommandLine::ShowLine(const std::string& prompt, unsigned cursor)
{
    struct winsize w;
    ioctl(0, TIOCGWINSZ, &w);
    
    const std::size_t prompt_len = prompt.length();

    const LispInt l = (cursor + prompt_len) / w.ws_col;
    const LispInt c = (cursor + prompt_len) % w.ws_col;

    if (_cursor_line)
        std::cout << "\x1b[" << _cursor_line << "F";

    if (full_line_dirty) {
        if (_last_line)
            std::cout << "\x1b[" << _last_line << "B";

        for (LispInt i = 0; i < _last_line; ++i)
            std::cout << "\r\x1b[K\x1b[F";

        std::cout << "\r\x1b[K\x1b[K" << prompt << iSubLine;

        if ((prompt_len + iSubLine.size()) % w.ws_col == 0)
            std::cout << "\n";

        _last_line = (prompt_len + iSubLine.size()) / w.ws_col;
        if (_last_line)
            std::cout << "\x1b[" << _last_line << "F";
    }

    if (l)
        std::cout << "\r\x1b[" << l << "B";

    std::cout << "\r\x1b[" << c + 1 << "G";

    std::cout << std::flush;

    _cursor_line = l;

    full_line_dirty = false;
}


CUnixCommandLine::CUnixCommandLine():
    _cursor_line(0),
    _last_line(0),
    _max_lines(1024)
{
    struct winsize w;
    int rc = ioctl(0, TIOCGWINSZ, &w);

    if (rc < 0 || w.ws_col == 0)
        throw std::runtime_error("dumb terminal");

    /* set termio so we can do our own input processing */
    tcgetattr(0, &orig_termio);
    rl_termio = orig_termio;
    rl_termio.c_iflag &= ~(BRKINT|PARMRK|INPCK/*|IUCLC*/|IXON|IXOFF);
    rl_termio.c_iflag |=  (IGNBRK|IGNPAR);
    /* rl_termio.c_oflag &= ~(ONOCR); Costas Sphocleous Irvine,CA */
    rl_termio.c_lflag &= ~(ICANON|ECHO|ECHOE|ECHOK|ECHONL|NOFLSH);
    rl_termio.c_lflag |=  (ISIG);
    rl_termio.c_cc[VMIN] = 1;
    rl_termio.c_cc[VTIME] = 0;
    term_chars[VERASE]   = orig_termio.c_cc[VERASE];
    term_chars[VEOF]     = orig_termio.c_cc[VEOF];
    term_chars[VKILL]    = orig_termio.c_cc[VKILL];
    term_chars[VWERASE]  = orig_termio.c_cc[VWERASE];
    term_chars[VREPRINT] = orig_termio.c_cc[VREPRINT];
    term_chars[VSUSP]    = orig_termio.c_cc[VSUSP];
    /* disable suspending process on ^Z */
    rl_termio.c_cc[VSUSP] = 0;
    tcsetattr(0, TCSADRAIN, &rl_termio);

    const char* home_dir = getenv("HOME");

    if (!home_dir)
        return;

    const std::string fname = std::string(home_dir) + "/.yacas_history";

    std::ifstream is(fname.c_str());

    std::string line;
    while (std::getline(is, line))
        iHistoryList.Append(line);
}

CUnixCommandLine::~CUnixCommandLine()
{
    tcsetattr(0, TCSADRAIN, &orig_termio);

    const char* home_dir = getenv("HOME");

    if (!home_dir)
        return;

    const std::string fname = std::string(home_dir) + "/.yacas_history";

    std::ofstream os(fname.c_str());

    if (os) {
        std::size_t from = 0;

        if (_max_lines > 0 && iHistoryList.NrLines() > _max_lines)
            from = iHistoryList.NrLines() - _max_lines;

        for (std::size_t i = from; i < iHistoryList.NrLines(); ++i)
            os << iHistoryList.GetLine(i) << "\n";
    }
}

void CUnixCommandLine::MaxHistoryLinesSaved(std::size_t n)
{
    _max_lines = n;
}


char32_t CUnixCommandLine::GetKey()
{
    int c = getc(stdin);

    if (feof(stdin))
        exit(0);

    char32_t ch = 0;

    if (c == term_chars[VERASE]) /* Backspace */
        ch = eBackSpace;
    else if (c == term_chars[VEOF]) /* delete */
        ch = eDelete;
    else {
        switch (c) {
            case 9: //  9   tab
                ch = eTab;
                break;
            case 10: /* Enter */
                ch = eEnter;
                break;
            case 001: /* ^A  (unix home) */
                ch = eHome;
                break;
            case 005: /* ^E  (unix end) */
                ch = eEnd;
                break;
            case 004: /* ^D  (unix delete) */
            case 127:
                ch = eDelete;
                break;
            case 8: /* ^H  (unix backspace) */
                ch = eBackSpace;
                break;
            case 11: /* ^K (unix kill to the end of line */
                ch = eKill;
                break;
            case 033:
            {
                c = getc(stdin); /* check for CSI */

                switch (c) {
                    case 27:
                        ch = eEscape;
                        break;
                    case '[':
                    case 79:
                    {
                        c = getc(stdin); /* get command character */
                        switch (c) {
                            case 'D': /* left arrow key */
                                ch = eLeft;
                                break;
                            case 'C': /* right arrow key */
                                ch = eRight;
                                break;
                            case 'A': /* up arrow key */
                                ch = eUp;
                                break;
                            case 'B': /* down arrow key */
                                ch = eDown;
                                break;
                            case 'H': /* home */
                                ch = eHome;
                                break;
                            case 'F': /* end */
                                ch = eEnd;
                                break;
                        }
                        break;
                    }
                }
                break;
            }
            default:
            {
                char p[4] = {static_cast<char>(c), 0, 0, 0};
                char* q = p + 1;
                while (!utf8::is_valid(p, q))
                    *q++ = getc(stdin);

                utf8::utf8to32(p, q, &ch);
                
                break;
            }
        }
    }
    return ch;
}
