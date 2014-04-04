
#include <time.h>
#include <stdio.h>
#include <sys/ioctl.h>

#include <sstream>

#include "yacas/yacasprivate.h"
#include "yacas/unixcommandline.h"

void CUnixCommandLine::NewLine()
{
    _cursor_line = 0;
    _last_line = 0;

    printf("\n");

    fflush(stdout);
}

void CUnixCommandLine::Pause()
{
    clock_t i=clock()+CLOCKS_PER_SEC/4;
    while (clock()<i);
}

void CUnixCommandLine::ShowLine(const std::string& prompt, LispInt cursor)
{
    struct winsize w;
    ioctl(0, TIOCGWINSZ, &w);

    const std::size_t prompt_len = prompt.length();

    const LispInt l = (cursor + prompt_len) / w.ws_col;
    const LispInt c = (cursor + prompt_len) % w.ws_col;

    if (_cursor_line)
        printf("\x1b[%dF", _cursor_line);

    if (full_line_dirty) {
        if (_last_line)
            printf("\x1b[%dB", _last_line);

        for (LispInt i = 0; i < _last_line; ++i)
            printf("\r\x1b[K\x1b[F");

        printf("\r\x1b[K\x1b[K%s%s", prompt.c_str(), &iSubLine[0]);

        if ((prompt_len + std::strlen(&iSubLine[0])) % w.ws_col == 0)
            printf("\n");

        _last_line = (prompt_len + strlen(&iSubLine[0])) / w.ws_col;
        if (_last_line)
            printf("\x1b[%dF", _last_line);
    }

    if (l)
        printf("\r\x1b[%dB", l);

    printf("\r\x1b[%dG", c + 1);

    fflush(stdout);

    _cursor_line = l;

    full_line_dirty = false;
}


CUnixCommandLine::CUnixCommandLine():
    _cursor_line(0),
    _last_line(0),
    iMaxLines(1024)
{
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

    FILE* f = fopen(fname.c_str(), "r");
    if (f)
    {
        if(f)
        {
#define BufSz 256
            char buff[BufSz];
            while(fgets(buff,BufSz-2,f))
            {
                int i;
                for(i=0;buff[i] && buff[i] != '\n';++i)
                    ;
                buff[i++] = '\0';
                iHistoryList.Append(buff);

            }
            fclose(f);
        }
    }
}

CUnixCommandLine::~CUnixCommandLine()
{
    tcsetattr(0, TCSADRAIN, &orig_termio);

    std::ostringstream fname;
    fname << getenv("HOME") << "/.yacas_history";

    FILE* f = fopen(fname.str().c_str(), "w");
    if (f)
    {
        int i;
        int from=0;
        if (iMaxLines>=0)
        {
            if (iHistoryList.NrLines()>iMaxLines)
            {
                from = iHistoryList.NrLines()-iMaxLines;
            }
        }
        for (i=from;i<iHistoryList.NrLines();i++)
        {
            fprintf(f,"%s\n",iHistoryList.GetLine(i).c_str());
        }
        fclose(f);
    }
}

void CUnixCommandLine::MaxHistoryLinesSaved(LispInt aNrLines)
{
    iMaxLines = aNrLines;
}


LispInt CUnixCommandLine::GetKey()
{
    LispInt c = getc(stdin);

    if (feof(stdin))
        exit(0);

    if (c == term_chars[VERASE]) /* Backspace */
    {
        c = eBackSpace;
    }
    else if (c == term_chars[VEOF]) /* delete */
    {
        c = eDelete;
    }
    else
    {
        switch (c)
        {
        case 9: //  9   tab
            c = eTab;
            break;
        case 10:        /* Enter */
            c = eEnter;
            break;
        case 001:    /* ^A  (unix home) */
            c = eHome;
            break;
        case 005:    /* ^E  (unix end) */
            c = eEnd;
            break;
        case 004:    /* ^D  (unix delete) */
        case 127:
            c = eDelete;
            break;
        case  8:              /* ^H  (unix backspace) */
            c = eBackSpace;
            break;
        case 11:              /* ^K (unix kill to the end of line */
            c = eKill;
            break;
        case 033:
            {
                c = getc(stdin); /* check for CSI */

                switch (c)
                {
                case 27:
                    c=eEscape;
                    break;
                case '[':
                case 79:
                    {
                        c = getc(stdin); /* get command character */
                        switch (c)
                        {
                        case 'D': /* left arrow key */
                            c = eLeft;
                            break;
                        case 'C': /* right arrow key */
                            c = eRight;
                            break;
                        case 'A': /* up arrow key */
                            c = eUp;
                            break;
                        case 'B': /* down arrow key */
                            c = eDown;
                            break;
                        case 'H': /* home */
                            c = eHome;
                            break;
                        case 'F': /* end */
                            c = eEnd;
                            break;
                        }
                    }
                    break;
                }
            }
            break;
        }
    }
    return c;
}



