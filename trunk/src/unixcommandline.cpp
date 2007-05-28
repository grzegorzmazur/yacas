
#include <time.h>
#include <stdio.h>

#include "yacasprivate.h"
#include "unixcommandline.h"

void CUnixCommandLine::NewLine()
{
    printf("\n");
}

void CUnixCommandLine::Pause()
{
    clock_t i=clock()+CLOCKS_PER_SEC/4;
    while (clock()<i);
}

void CUnixCommandLine::ShowLine(LispChar * prompt,LispInt promptlen,LispInt cursor)
{
    if (iFullLineDirty)
    {
        printf("\r%c[K",27); //Clear line
        printf("\r%s%s",prompt,&iSubLine[0]); // display line
    }
    printf("\r");                     // go back
    printf("%c[%dC",27,cursor+promptlen); // position cursor
    fflush(stdout);
    iFullLineDirty = 0;
}


CUnixCommandLine::CUnixCommandLine() : orig_termio(), rl_termio(), iMaxLines(1024)
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

    char fname[256];
#ifdef HAVE_VSNPRINTF
    snprintf(fname,256,"%s/.yacas_history",getenv("HOME"));
#else
    sprintf(fname,"%s/.yacas_history",getenv("HOME"));
#endif
    FILE*f=fopen(fname,"r");
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
                LispString * ptr = NEW LispString(buff);
                iHistoryList.Append(ptr);
 
            }
            fclose(f);
        }
    }
    iMaxLines = 1024;

}
CUnixCommandLine::~CUnixCommandLine()
{
    tcsetattr(0, TCSADRAIN, &orig_termio);
    char fname[256];
#ifdef HAVE_VSNPRINTF
    snprintf(fname,256,"%s/.yacas_history",getenv("HOME"));
#else
    sprintf(fname,"%s/.yacas_history",getenv("HOME"));
#endif
 
    FILE*f=fopen(fname,"w");
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
          LispString * ptr = iHistoryList.GetLine(i);

            fprintf(f,"%s\n",ptr->c_str());
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
    LispInt c;
    c=getc(stdin);
if (feof(stdin))
{
  exit(0);
}

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
        case 033:
            {
                c = getc(stdin); /* check for CSI */

                switch (c)
                {
                case 27:
                    c=eEscape;
                    break;
                case '[':
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



