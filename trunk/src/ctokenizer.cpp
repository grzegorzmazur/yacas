

#include "yacas.h"

#include "tokenizer.h"

#include "lispassert.h"
#include "lisperror.h"
#include "numbers.h"
#include "lispstring.h"

#include "ctokenizer.h"


LispStringPtr CTokenizer::NextToken(LispInput& aInput,
                                             LispHashTable& aHashTable)
{
    LispChar c;
    LispInt firstpos;

REDO:
    firstpos = aInput.Position();

    // End of stream: return empty string
    if (aInput.EndOfStream())
        goto FINISH;

    c = aInput.Next();
    //printf("%c",c);

    //Parse brackets
    if (c == '(')      {}
    else if (c == ')') {}
    else if (c == '{') {}
    else if (c == '}') {}
    else if (c == '[') {}
    else if (c == ']') {}
    else if (c == ',') {}
    else if (c == ';') {}
    else if (c == '%') {}
//    else if (c == '\'') {}
    else if (c == '.' && !IsDigit(aInput.Peek()) )
    {
        while (aInput.Peek() == '.')
        {
            aInput.Next();
        }
    }
    // parse comments
    else if (c == '/' && aInput.Peek() == '*')
    {
        aInput.Next(); //consume *
    FALSEALARM:
        while (aInput.Next() != '*' && !aInput.EndOfStream());
        Check(!aInput.EndOfStream(),KLispErrCommentToEndOfFile);
        if (aInput.Peek() == '/')
        {
            aInput.Next();  // consume /
            goto REDO;
        }
        goto FALSEALARM;
    }
    else if (c == '/' && aInput.Peek() == '/')
    {
        aInput.Next(); //consume /
        while (aInput.Next() != '\n' && !aInput.EndOfStream());
        goto REDO;
    }
    // parse literal strings
    else if (c == '\"')
    {
        LispString aResult;
        aResult.SetNrItems(0);
        aResult.Append(c);
        while (aInput.Peek() != '\"')
        {
            if (aInput.Peek() == '\\')
            {
                aInput.Next();
                Check(!aInput.EndOfStream(),KLispErrParsingInput);
            }
            aResult.Append(aInput.Next());
            Check(!aInput.EndOfStream(),KLispErrParsingInput);
        }
        aResult.Append(aInput.Next()); // consume the close quote
        aResult.Append('\0');
        return aHashTable.LookUp(aResult.String());
    }
    //parse atoms
    else if (IsAlpha(c) || c == '_')
    {
        while (IsAlNum( aInput.Peek())  || aInput.Peek() == '_')
        {
            aInput.Next();
        }
    }

    else if (IsSymbolic(c))
    {
        while (IsSymbolic( aInput.Peek()))
        {
            aInput.Next();
        }
    }
    /*REMOVED
     else if (c == '_')
    {
        while (aInput.Peek() == '_')
        {
            aInput.Next();
        }
        }
        */
    else if (IsDigit(c) || c == '.')
    {
        while (IsDigit( aInput.Peek())) aInput.Next();
        if (aInput.Peek() == '.')
        {
            aInput.Next();
            while (IsDigit( aInput.Peek())) aInput.Next();
        }
        if (NumericSupportForMantissa())
        {
            if (aInput.Peek() == 'e' || aInput.Peek() == 'E')
            {
                aInput.Next();
                if (aInput.Peek() == '-' || aInput.Peek() == '+')
                    aInput.Next();
                while (IsDigit( aInput.Peek())) aInput.Next();
            }
        }
    }
    // Treat the char as a space.
    else
    {
        goto REDO;
    }

FINISH:
    return aHashTable.LookUpCounted(&aInput.StartPtr()[firstpos],aInput.Position()-firstpos);
}



