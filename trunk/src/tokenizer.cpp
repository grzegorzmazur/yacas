
#include "tokenizer.h"

/* Default parse algorithm:
 *
 * Numbers: start with + or -, and continues to have digits or .
 * ( and ) as brackets
 * Alpha atom: First char isalpha, and after that isalnum.
 * Oper atom:  all chars have to be symbolic
 *
 */

#include "lispassert.h"
#include "lisperror.h"
#include "numbers.h"

static LispBoolean IsDigit(LispChar c)
{
    return ((c>='0' && c<='9'));
}

LispBoolean IsAlpha(LispChar c)
{
    return ( (c>='a' && c<='z') || (c>='A' && c<='Z') || (c == '\'') );
}

LispBoolean IsAlNum(LispChar c)
{
    return (IsAlpha(c) || IsDigit(c));
}

static const char symbolics[] = "~`!@#$^&*-=+:<>?/\\|";


LispBoolean IsSymbolic(LispChar c)
{
    const char *ptr=symbolics;
    while (*ptr)
    {
        if ((*ptr) == c)
        {
            return 1;
        }
        ptr++;
    }
    return 0;
}




LispStringPtr LispTokenizer::NextToken(LispInput& aInput,
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
    else if (IsAlpha(c))
    {
        while (IsAlNum( aInput.Peek()))
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
    else if (c == '_')
    {
        while (aInput.Peek() == '_')
        {
            aInput.Next();
        }
    }
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



