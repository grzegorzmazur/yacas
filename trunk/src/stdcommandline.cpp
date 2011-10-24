
#include <stdio.h>

#include "yacasprivate.h"
#include "stdcommandline.h"

#define KMaxStdLen 4001

void CStdCommandLine::NewLine()
{
}

void CStdCommandLine::Pause()
{
}

void CStdCommandLine::ShowLine(const LispChar * prompt,LispInt promptlen,LispInt cursor)
{
}


CStdCommandLine::CStdCommandLine()
{
}
CStdCommandLine::~CStdCommandLine()
{
}

LispInt CStdCommandLine::GetKey()
{
    return '\n';
}

void CStdCommandLine::ReadLine(const LispChar * prompt)
{
    puts(prompt); fflush(stdout);
    char buffer[4001];
    int offs=0;
    bool ok;
MORE:
    ok = fgets(&buffer[offs],4000-offs,stdin);

    if (!ok || feof(stdin)) 
        strcpy(buffer,"quit");

    offs=strlen(buffer);

    if (buffer[offs-1] == '\n')
    {
        offs--;
        buffer[offs] = '\0';
    }

    if (offs<1)
        goto MORE;
    if (buffer[offs-1] == '\\')
        goto MORE;
    iLine.ResizeTo(offs+1);
    //iLine.Resize(offs+1);
    strcpy(&iLine[0],buffer);
}


