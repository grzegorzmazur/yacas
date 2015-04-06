#include "yacas/xmltokenizer.h"
#include "yacas/lisperror.h"

#include <cctype>

const LispString* XmlTokenizer::NextToken(LispInput& aInput, LispHashTable& aHashTable)
{
    char c;

    if (aInput.EndOfStream())
        return aHashTable.LookUp("");

    while (std::isspace(aInput.Peek()))
        aInput.Next();

    if (aInput.EndOfStream())
        return aHashTable.LookUp("");

    std::string s;

    c = aInput.Next();
    s.push_back(c);

    if (c == '<') {
        while (c != '>') {
            c = aInput.Next();

            if (aInput.EndOfStream())
                throw LispErrCommentToEndOfFile();

            s.push_back(c);
        }
    } else {
        while (aInput.Peek() != '<' && !aInput.EndOfStream()) {
            c = aInput.Next();
            s.push_back(c);
        }
    }

    return aHashTable.LookUp(s);
}
