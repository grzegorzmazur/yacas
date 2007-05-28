
#include "yacasprivate.h"
#include "lisperror.h"
#include "xmltokenizer.h"

static int IsSpace(int c)
{
    switch (c)
    {
    case 0x20:
    case 0x0D:
    case 0x0A:
    case 0x09:
        return 1;
    default:
        return 0;
    }
}


LispString * XmlTokenizer::NextToken(LispInput& aInput,
                                LispHashTable& aHashTable)
{
    LispChar c;
    LispInt firstpos=0;

    if (aInput.EndOfStream())
        goto FINISH;

    //skipping spaces
    while (IsSpace(aInput.Peek())) aInput.Next();
    firstpos = aInput.Position();
 
    c = aInput.Next();
    if (c == '<')
    {
        while (c != '>')
        {
            c = aInput.Next();
            Check(!aInput.EndOfStream(),KLispErrCommentToEndOfFile);
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
    return aHashTable.LookUpCounted(&aInput.StartPtr()[firstpos],aInput.Position()-firstpos);
}


XmlTokenizer::~XmlTokenizer()
{
}

