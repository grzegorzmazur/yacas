
#include "yacas/yacasprivate.h"
#include "yacas/lisperror.h"
#include "yacas/xmltokenizer.h"

#include <cctype>

const LispString* XmlTokenizer::NextToken(LispInput& aInput, LispHashTable& aHashTable)
{
    LispChar c;
    LispInt firstpos=0;

    if (aInput.EndOfStream())
        goto FINISH;

    //skipping spaces
    while (std::isspace(aInput.Peek()))
        aInput.Next();

    firstpos = aInput.Position();

    c = aInput.Next();
    if (c == '<')
    {
        while (c != '>')
        {
            c = aInput.Next();

            if (aInput.EndOfStream())
                throw LispErrCommentToEndOfFile();
        }
    }
    else
    {
        while (aInput.Peek() != '<' && !aInput.EndOfStream())
        {
            c = aInput.Next();
        }
    }
FINISH:
    return aHashTable.LookUp(std::string(&aInput.StartPtr()[firstpos],aInput.Position()-firstpos));
}

