

#include "yacasprivate.h"
#include "yacas.h"

#include "tokenizer.h"

#include "lispassert.h"
#include "lisperror.h"
#include "numbers.h"
#include "lispstring.h"

#include "ctokenizer.h"
#include "standard.h"

void CTokenizer::EmitRemark(LispString * remark)
{
    if (iEnvironment)
    {
        //TODO ugly! Slow on resources!
        remark = iEnvironment->HashTable().LookUpStringify(remark->c_str());
        //TODO ugly!
        LispEnvironment& aEnvironment = *iEnvironment;
        LispString * oper = iEnvironment->HashTable().LookUp("\"TokenizerEmitRemark\"");
        LispPtr result;
        LispPtr args(ATOML(remark->c_str()));
        InternalApplyString(*iEnvironment, result,oper,args);
    }
}


LispString * CTokenizer::NextToken(LispInput& aInput,
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
    else if (c == '~') {}
    else if (c == '?') {}
    else if (c == '$') {} // '$' is reserved for pattern matching!
    else if (c == '\\')
    {
        while (aInput.Next() != '\n' && !aInput.EndOfStream());
//        goto REDO;
//        if (iPreProcessLine)
//        {
//        }
 
    }
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

            if (iEnvironment)
            {
                LispString * remark = iEnvironment->HashTable().LookUpCounted(&aInput.StartPtr()[firstpos],aInput.Position()-firstpos);
                EmitRemark(remark);
            }
            goto REDO;
        }
        goto FALSEALARM;
    }
    else if (c == '/' && aInput.Peek() == '/')
    {
        aInput.Next(); //consume /
        while (aInput.Next() != '\n' && !aInput.EndOfStream());

        if (iEnvironment)
        {
            LispString * remark = iEnvironment->HashTable().LookUpCounted(&aInput.StartPtr()[firstpos],aInput.Position()-firstpos);
            EmitRemark(remark);
        }

        goto REDO;
    }
    // parse literal strings
    else if (c == '\"')
    {
        LispString aResult;
        aResult.ResizeTo(0);
        aResult.Append(c);
        while (aInput.Peek() != '\"')
        {
            if (aInput.Peek() == '\\')
            {
                aResult.Append('\\');
                aInput.Next();
                Check(!aInput.EndOfStream(),KLispErrParsingInput);
            }
            aResult.Append(aInput.Next());
            Check(!aInput.EndOfStream(),KLispErrParsingInput);
        }
        aResult.Append(aInput.Next()); // consume the close quote
        aResult.Append('\0');
        return aHashTable.LookUp(aResult.c_str());
    }
    else if (c == '\'')
    {
        LispString aResult;
        aResult.ResizeTo(0);
        aResult.Append(c);
        while (aInput.Peek() != '\'')
        {
            if (aInput.Peek() == '\\')
            {
                aResult.Append('\\');
                aInput.Next();
                Check(!aInput.EndOfStream(),KLispErrParsingInput);
            }
            aResult.Append(aInput.Next());
            Check(!aInput.EndOfStream(),KLispErrParsingInput);
        }
        aResult.Append(aInput.Next()); // consume the close quote
        aResult.Append('\0');
        return aHashTable.LookUp(aResult.c_str());
    }
    //parse atoms
    else if (IsAlpha(c) || c == '_')
    {
        while (IsAlNum( aInput.Peek())  || aInput.Peek() == '_')
        {
            aInput.Next();
        }
    }
    else if (IsDigit(c) || c == '.')
    {
        if (c == '0' && aInput.Peek() == 'x')
        {
            aInput.Next();
            while (IsDigit(aInput.Peek()) ||
                   (aInput.Peek() >= 'a' && aInput.Peek() <= 'f') ||
                   (aInput.Peek() >= 'A' && aInput.Peek() <= 'F')
                  )
            {
                aInput.Next();
            }
        }
        else
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
    }
    else
    {
        switch (c)
        {
        case '<':
        case '>':
        case '#':
        case '!':
        case '=':
        case '+':
        case '-':
        case '*':
        case '/':
        case '^':
        case '%':
        case '&':
        case ':':
        case '|':
            {
                int c2 = '\0';
                if (!aInput.EndOfStream())
                    c2 = aInput.Peek();
                switch (c)
                {
                case '+':
                case '-':
                case '*':
                case '/':
                case '^':
                case '%':
                case '&':
                case '=':
                case '|':
                case '!':
                case '<':
                case '>':
                    {
                        int take=0;
                        if (c2 == '=') take = 1;
                        if (c == '-' && c2 == '>') take = 1;
                        if (c == '+' && c2 == '+') take = 1;
                        if (c == '|' && c2 == '|') take = 1;
                        if (c == '&' && c2 == '&') take = 1;
                        if (c == '-' && c2 == '-') take = 1;
                        if (c == '<' && c2 == '<')
                        {
                            aInput.Next();
                            c2 = aInput.Peek();
                            if (c2 == '=')
                                take = 1;
                        }
                        if (c == '>' && c2 == '>')
                        {
                            aInput.Next();
                            c2 = aInput.Peek();
                            if (c2 == '=')
                                take = 1;
                        }
                        if (take)
                        {
                            aInput.Next();
                            c2 = aInput.Peek();
                        }
                    }
                    goto FINISH;
                case '#':
                    {
                        int take=0;
                        if (c2 == '#') take = 1;
                        if (take)
                        {
                            aInput.Next();
                            c2 = aInput.Peek();
                        }
                        else
                        {
                            iPreProcessLine = 1;
                        }
                    }
                    goto FINISH;
                case ':':
                    {
                        int take=0;
                        if (c2 == ':') take = 1;
                        if (take)
                        {
                            aInput.Next();
                            c2 = aInput.Peek();
                        }
                    }
                    goto FINISH;
                default:
                    goto REDO;
                }
                c = c2;
            }

        case '\n':
            if (iPreProcessLine)
            {
                iPreProcessLine = 0;
                return aHashTable.LookUpCounted("@EndLine@",9);
            }
            goto REDO;
        default:
            if (iPreProcessLine)
            {
                goto FINISH;
            }
            goto REDO;
        }
    }

FINISH:
    return aHashTable.LookUpCounted(&aInput.StartPtr()[firstpos],aInput.Position()-firstpos);
}


