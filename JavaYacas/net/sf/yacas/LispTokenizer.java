package net.sf.yacas;


class LispTokenizer
{

    /// NextToken returns a string representing the next token,
    /// or an empty list.
    String NextToken(LispInput aInput, LispHashTable aHashTable) throws Exception
  {
    char c;
    int firstpos = aInput.Position();

    boolean redo = true;
    while (redo)
    {
      redo = false;
//REDO: //TODO FIXME
      firstpos = aInput.Position();

      // End of stream: return empty string
      if (aInput.EndOfStream())
        break;

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
        while (true)
        {
          while (aInput.Next() != '*' && !aInput.EndOfStream());
          LispError.Check(!aInput.EndOfStream(),LispError.KLispErrCommentToEndOfFile);
          if (aInput.Peek() == '/')
          {
            aInput.Next();  // consume /
            redo = true;
            break;
          }
        }
        if (redo)
        {
          continue;
        }
      }
      else if (c == '/' && aInput.Peek() == '/')
      {
        aInput.Next(); //consume /
        while (aInput.Next() != '\n' && !aInput.EndOfStream());
        redo = true;
        continue;
      }
      // parse literal strings
      else if (c == '\"')
      {
        String aResult;
        aResult = "";
        //TODO FIXME is following append char correct?
        aResult = aResult + ((char)c);
        while (aInput.Peek() != '\"')
        {
          if (aInput.Peek() == '\\')
          {
            aInput.Next();
            LispError.Check(!aInput.EndOfStream(),LispError.KLispErrParsingInput);
          }
          //TODO FIXME is following append char correct?
          aResult = aResult + ((char)aInput.Next());
          LispError.Check(!aInput.EndOfStream(),LispError.KLispErrParsingInput);
        }
        //TODO FIXME is following append char correct?
        aResult = aResult + ((char)aInput.Next()); // consume the close quote
        return aHashTable.LookUp(aResult);
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
        if (BigNumber.NumericSupportForMantissa())
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
        redo = true;
        continue;
      }
    }
    return aHashTable.LookUp(aInput.StartPtr().substring(firstpos,aInput.Position()));
  }

  static boolean IsDigit(char c)
  {
    return ((c>='0' && c<='9'));
  }

  static boolean IsAlpha(char c)
  {
    return ( (c>='a' && c<='z') || (c>='A' && c<='Z') || (c == '\'') );
  }

  static boolean IsAlNum(char c)
  {
    return (IsAlpha(c) || IsDigit(c));
  }
  static String symbolics = new String("~`!@#$^&*-=+:<>?/\\|");
  static boolean IsSymbolic(char c)
  {
    return (symbolics.indexOf(c) >= 0);
  }
    String iToken; //Can be used as a token container.
};

