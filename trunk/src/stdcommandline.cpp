
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

void CStdCommandLine::ShowLine(LispCharPtr prompt,LispInt promptlen,LispInt cursor)
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

void CStdCommandLine::ReadLine(LispCharPtr prompt)
{
    printf(prompt);
    char buffer[4001];
    int offs=0;
MORE:
    fgets(&buffer[offs],4000-offs,stdin);
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
    iLine.GrowTo(offs+1);
    iLine.SetNrItems(offs+1);
    strcpy(&iLine[0],buffer);
}


