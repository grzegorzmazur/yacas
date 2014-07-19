
#include "yacas/yacasprivate.h"
#include "yacas/tokenizer.h"

/* Default parse algorithm:
 *
 * Numbers: start with + or -, and continues to have digits or .
 * ( and ) as brackets
 * Alpha atom: First char isalpha, and after that isalnum.
 * Oper atom:  all chars have to be symbolic
 *
 */

#include "yacas/yacasprivate.h"
#include "yacas/lisperror.h"
#include "yacas/numbers.h"

#include <cassert>

static const char symbolics[] = "~`!@#$^&*-=+:<>?/\\|";

bool IsSymbolic(LispChar c)
{
    for (const char *ptr = symbolics; *ptr; ++ptr)
        if (*ptr == c)
            return true;

    return false;
}

LispString * LispTokenizer::NextToken(LispInput& aInput,
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
  else if (c == '.' && !std::isdigit(aInput.Peek()) )
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

    if (aInput.EndOfStream())
        throw LispErrCommentToEndOfFile();

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
    aResult.ResizeTo(0);
    aResult.Append(c);
    while (aInput.Peek() != '\"')
    {
      if (aInput.Peek() == '\\')
      {
        aInput.Next();

        if (aInput.EndOfStream())
            throw LispErrParsingInput();
      }
      aResult.Append(aInput.Next());

      if (aInput.EndOfStream())
          throw LispErrParsingInput();
    }
    aResult.Append(aInput.Next()); // consume the close quote
    aResult.Append('\0');
    return aHashTable.LookUp(aResult.c_str());
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
  else if (std::isdigit(c) || c == '.')
  {
    while (std::isdigit( aInput.Peek())) aInput.Next();
    if (aInput.Peek() == '.')
    {
      aInput.Next();
      while (std::isdigit( aInput.Peek())) aInput.Next();
    }
    if (NumericSupportForMantissa())
    {
      if (aInput.Peek() == 'e' || aInput.Peek() == 'E')
      {
        aInput.Next();
        if (aInput.Peek() == '-' || aInput.Peek() == '+')
            aInput.Next();
        while (std::isdigit( aInput.Peek())) aInput.Next();
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



/*TODO the ugly thing here is this routine is a copy-and-tweak
 of the default tokenizer.
 */
LispString * CommonLispTokenizer::NextToken(LispInput& aInput, LispHashTable& aHashTable)
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
  else if (c == '\'') {}
  else if (c == '%') {}
  else if (c == '.' && !std::isdigit(aInput.Peek()) )
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

    if (aInput.EndOfStream())
        throw LispErrCommentToEndOfFile();

    if (aInput.Peek() == '/')
    {
      aInput.Next();  // consume /
      goto REDO;
    }
    goto FALSEALARM;
  }
  else if (c == ';')
  {
    while (aInput.Next() != '\n' && !aInput.EndOfStream());
    goto REDO;
  }
  // parse literal strings
  else if (c == '\"')
  {
    LispString aResult;
    aResult.Append(c);
    while (aInput.Peek() != '\"')
    {
      if (aInput.Peek() == '\\')
      {
        aInput.Next();

        if (aInput.EndOfStream())
            throw LispErrParsingInput();
      }
      aResult.Append(aInput.Next());

      if (aInput.EndOfStream())
          throw LispErrParsingInput();
    }
    aResult.Append(aInput.Next()); // consume the close quote
    aResult.Append('\0');
    return aHashTable.LookUp(aResult.c_str());
  }
  //parse atoms
  else if (IsAlpha(c) || IsSymbolic(c))
  {
    while (IsAlNum( aInput.Peek()) || IsSymbolic( aInput.Peek()))
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
  else if (std::isdigit(c) || c == '.')
  {
    while (std::isdigit( aInput.Peek())) aInput.Next();
    if (aInput.Peek() == '.')
    {
      aInput.Next();
      while (std::isdigit( aInput.Peek())) aInput.Next();
    }
    if (NumericSupportForMantissa())
    {
      if (aInput.Peek() == 'e' || aInput.Peek() == 'E')
      {
        aInput.Next();
        if (aInput.Peek() == '-' || aInput.Peek() == '+')
          aInput.Next();
        while (std::isdigit( aInput.Peek())) aInput.Next();
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


