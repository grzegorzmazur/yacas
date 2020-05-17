#include "yacas/xmltokenizer.h"
#include "yacas/lisperror.h"

#include <cctype>
#include <iostream>

std::string XmlTokenizer::NextToken(LispInput& aInput)
{
    if (aInput.EndOfStream())
        return "";

    std::string leading_spaces;
    while (std::isspace(aInput.Peek()))
        leading_spaces.push_back(aInput.Next());

    if (aInput.EndOfStream())
        return "";

    std::string s;

    char c = aInput.Next();
    s.push_back(c);

    if (c == '<') {
        while (c != '>') {
            if (aInput.EndOfStream())
                throw LispErrCommentToEndOfFile();

            c = aInput.Next();

            s.push_back(c);
        }
    } else {
        while (aInput.Peek() != '<' && !aInput.EndOfStream())
            s.push_back(aInput.Next());
        s = leading_spaces + s;
    }

    return s;
}
