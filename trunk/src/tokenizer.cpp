#include "yacas/tokenizer.h"

#include "yacas/lisperror.h"

namespace {
    static const char symbolics[] = "~`!@#$^&*-=+:<>?/\\|";
}

bool IsSymbolic(LispChar c)
{
    for (const char *ptr = symbolics; *ptr; ++ptr)
        if (*ptr == c)
            return true;

    return false;
}

const LispString* LispTokenizer::NextToken(LispInput& aInput,
                                           LispHashTable& aHashTable)
{
    char c;

    // skip whitespaces and comments
    for (;;) {
        // End of stream: return empty string
        if (aInput.EndOfStream())
            return aHashTable.LookUp("");

        c = aInput.Next();

        if (std::isspace(c))
            continue;

        // parse comments
        if (c == '/' && aInput.Peek() == '*') {
            aInput.Next();
            for (;;) {
                while (aInput.Next() != '*' && !aInput.EndOfStream())
                    ;

                if (aInput.EndOfStream())
                    throw LispErrCommentToEndOfFile();

                if (aInput.Peek() == '/') {
                    aInput.Next();
                    break;
                }
            }

            continue;
        }

        if (c == '/' && aInput.Peek() == '/') {
            aInput.Next();
            while (aInput.Next() != '\n' && !aInput.EndOfStream())
                ;
            continue;
        }

        break;
    }

    // parse brackets
    if (c == '(' || c == ')' || c == '{' || c == '}' || c == '[' || c == ']')
        return aHashTable.LookUp(std::string(1, c));

    // percent
    if (c == '%')
        return aHashTable.LookUp(std::string(1, c));

    // comma and semicolon
    if (c == ',' || c == ';')
        return aHashTable.LookUp(std::string(1, c));

    // parse . or ..
    if (c == '.' && !std::isdigit(aInput.Peek())) {
        std::string token;
        token.push_back(c);
        while (aInput.Peek() == '.')
            token.push_back(aInput.Next());
        return aHashTable.LookUp(token);
    }

    // parse literal strings
    if (c == '\"') {
        std::string str;
        str.push_back(c);
        while (aInput.Peek() != '\"') {
            if (aInput.Peek() == '\\') {
                aInput.Next();

                if (aInput.EndOfStream())
                    throw LispErrParsingInput();
            }
            str.push_back(aInput.Next());

            if (aInput.EndOfStream())
                throw LispErrParsingInput();
        }
        str.push_back(aInput.Next());
        return aHashTable.LookUp(str);
    }

    // parse atoms
    if (IsAlpha(c)) {
        std::string atom;
        atom.push_back(c);
        while (IsAlNum(aInput.Peek()))
            atom.push_back(aInput.Next());
        return aHashTable.LookUp(atom);
    }

    // parse operators
    if (IsSymbolic(c)) {
        std::string op;
        op.push_back(c);
        while (IsSymbolic(aInput.Peek()))
            op.push_back(aInput.Next());
        return aHashTable.LookUp(op);
    }

    // parse subscripts
    if (c == '_') {
        std::string token;
        token.push_back(c);
        while (aInput.Peek() == '_')
            token.push_back(aInput.Next());
        return aHashTable.LookUp(token);
    }

    // parse numbers
    if (std::isdigit(c) || c == '.') {
        std::string number;
        number.push_back(c);

        while (std::isdigit(aInput.Peek()))
            number.push_back(aInput.Next());

        if (aInput.Peek() == '.') {
            number.push_back(aInput.Next());
            while (std::isdigit(aInput.Peek()))
                number.push_back(aInput.Next());
        }

        if (aInput.Peek() == 'e' || aInput.Peek() == 'E') {
            number.push_back(aInput.Next());
            if (aInput.Peek() == '-' || aInput.Peek() == '+')
                number.push_back(aInput.Next());
            while (std::isdigit(aInput.Peek()))
                number.push_back(aInput.Next());
        }

        return aHashTable.LookUp(number);
    }

    throw InvalidToken();
}
