#ifndef YACAS_STDCOMMANDLINE_H
#define YACAS_STDCOMMANDLINE_H

#include "commandline.h"
/** Simple no-frills implementation of CCommandLine, using stdlibc-functions
 *  only, and no ansi characters. No history is supported either.
 */
class CStdCommandLine : public CCommandLine
{
public:
    void ReadLine(const std::string& prompt) override;
public:
    char32_t GetKey() override;
    void NewLine() override;
    void ShowLine(const std::string& prompt, unsigned cursor) override;
    void Pause() override;
};


#endif

