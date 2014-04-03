
#include <stdio.h>

#include "yacas/yacasprivate.h"
#include "yacas/stdcommandline.h"

#define KMaxStdLen 4001

void CStdCommandLine::NewLine()
{
}

void CStdCommandLine::Pause()
{
}

void CStdCommandLine::ShowLine(const std::string& prompt, LispInt cursor)
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

void CStdCommandLine::ReadLine(const std::string& prompt)
{
    fputs(prompt.c_str(), stdout);
    fflush(stdout);

    char buffer[4001];
    int offs=0;

    do {

        const char* ok = fgets(&buffer[offs],4000-offs,stdin);

        if (!ok || feof(stdin))
            strcpy(buffer, "quit");

        offs=strlen(buffer);

        if (buffer[offs-1] == '\n') {
            offs -= 1;
            buffer[offs] = '\0';
        }

    } while (offs < 1 || buffer[offs-1] == '\\');

    iLine = buffer;
}


