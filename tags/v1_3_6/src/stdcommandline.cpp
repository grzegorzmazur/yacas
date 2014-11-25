#include "yacas/yacasprivate.h"
#include "yacas/stdcommandline.h"

#include <iostream>

void CStdCommandLine::NewLine()
{
}

void CStdCommandLine::Pause()
{
}

void CStdCommandLine::ShowLine(const std::string& prompt, unsigned cursor)
{
}

LispInt CStdCommandLine::GetKey()
{
    return '\n';
}

void CStdCommandLine::ReadLine(const std::string& prompt)
{
    std::cout << prompt << std::flush;

    iLine.clear();

    do {

        std::getline(std::cin, iLine);
        if (!std::cin.good())
            iLine = "quit";

    } while (iLine.empty() || iLine.back() == '\\');
}


