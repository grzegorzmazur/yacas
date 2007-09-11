
package net.sf.yacas;

public class TeXParser
{
  static String singleOps = "^_+=,";
  String iCurrentExpression;
  int currentPos;
  String nextToken;

  private void showToken()
  {
    System.out.println("["+nextToken+"]");
  }

  void NextToken()
  {
    nextToken = "";
    if (currentPos == iCurrentExpression.length())
    {
//showToken();
      return;
    }
    while (currentPos < iCurrentExpression.length() && IsSpace(iCurrentExpression.charAt(currentPos)))
      currentPos++;
    if (currentPos == iCurrentExpression.length())
    {
//showToken();
      return;
    }
    else if (IsAlNum(iCurrentExpression.charAt(currentPos)))
    {
      int startPos = currentPos;
      while (currentPos < iCurrentExpression.length() && IsAlNum(iCurrentExpression.charAt(currentPos)))
      {
        currentPos++;
      }
      nextToken = iCurrentExpression.substring(startPos,currentPos);
//showToken();
      return;
    }
    int c = iCurrentExpression.charAt(currentPos);
    if (c == '{')
    {
      nextToken = "{";
      currentPos++;
//showToken();
      return;
    }
    else if (c == '}')
    {
      nextToken = "}";
      currentPos++;
//showToken();
      return;
    }
    else if (singleOps.indexOf(c)>=0)
    {
      nextToken = ""+((char)c);
      currentPos++;
//showToken();
      return;
    }
    else if (c == '\\')
    {
      int startPos = currentPos;
      while (currentPos < iCurrentExpression.length() && (IsAlNum(iCurrentExpression.charAt(currentPos)) || iCurrentExpression.charAt(currentPos) == '\\'))
      {
        currentPos++;
      }
      nextToken = iCurrentExpression.substring(startPos,currentPos);
//showToken();
      return;
    }
//showToken();
  }

  boolean matchToken(String token)
  {
    if (nextToken.equals(token))
      return true;
    System.out.println("Found "+nextToken+", expected "+token);
    return false;
  }
  public SBox parse(String aExpression)
  {
    iCurrentExpression = aExpression;
    currentPos = 0;
    NextToken();
    return parseTopExpression();
  }

  SBox parseTopExpression()
  {
    SBoxBuilder builder = new SBoxBuilder();
    parseOneExpression10(builder);
    SBox expression = builder.pop();
    return expression;
  }

  void parseOneExpression10(SBoxBuilder builder)
  {
    parseOneExpression20(builder);
    // = ,
    while (nextToken.equals("=") || nextToken.equals("\\neq") || nextToken.equals(","))
    {
      String token = nextToken;
      NextToken();
      parseOneExpression20(builder);
      builder.process(token);
    }
  }

  void parseOneExpression20(SBoxBuilder builder)
  {
    parseOneExpression25(builder);
    // +, -
    while (nextToken.equals("+") || nextToken.equals("-") ||
           nextToken.equals("\\wedge") || nextToken.equals("\\vee") ||
           nextToken.equals("<") || nextToken.equals(">") ||
           nextToken.equals("\\leq") || nextToken.equals("\\geq")
           )
    {
      String token = nextToken;
      if (token.equals("-"))
        token = "-/2";
      else if (token.equals("\\leq"))
        token = "<=";
      else if (token.equals("\\geq"))
        token = ">=";
      NextToken();
      parseOneExpression25(builder);
      builder.process(token);
    }
  }

  void parseOneExpression25(SBoxBuilder builder)
  {
    parseOneExpression30(builder);
    // implicit *
    while (nextToken.length() > 0 &&
          !nextToken.equals("+") &&
          !nextToken.equals("-") &&
          !nextToken.equals("=") &&
          !nextToken.equals("\\neq") &&
          !nextToken.equals("}") &&
          !nextToken.equals("&") &&
          !nextToken.equals("\\wedge") &&
          !nextToken.equals("\\vee") &&

          !nextToken.equals("<") &&
          !nextToken.equals(">") &&
          !nextToken.equals("\\leq") &&
          !nextToken.equals("\\geq") &&

          !nextToken.equals("\\end") &&
          !nextToken.equals("\\\\") &&
          !nextToken.equals("\\right)") &&
          !nextToken.equals("\\right]") &&
          !nextToken.equals(",")
          )
    {

//System.out.println("nextToken = "+nextToken);
      String token = "*";
      parseOneExpression30(builder);
//System.out.println("After: nextToken = "+nextToken);
      builder.process(token);
    }
  }

  void parseOneExpression30(SBoxBuilder builder)
  {
    parseOneExpression40(builder);
    // _, ^
    while (nextToken.equals("_") || nextToken.equals("^") || nextToken.equals("!"))
    {
      if (nextToken.equals("!"))
      {
        builder.process(nextToken);
        NextToken();
      }
      else
      {
        String token = nextToken;
        NextToken();
        parseOneExpression40(builder);
        builder.process(token);
      }
    }
  }
  void parseOneExpression40(SBoxBuilder builder)
  {
    // atom
    if (nextToken.equals("{"))
    {
      NextToken();
      parseOneExpression10(builder);
      if (!nextToken.equals("}"))
      {
        System.out.println("Got "+nextToken+", expected }");
        return;
      }
    }
    else if (nextToken.equals("\\left("))
    {
      NextToken();
      parseOneExpression10(builder);
      if (!nextToken.equals("\\right)"))
      {
        System.out.println("Got "+nextToken+", expected \\right)");
        return;
      }
      builder.process("[roundBracket]");
    }
    else if (nextToken.equals("\\left["))
    {
      NextToken();
      parseOneExpression10(builder);
      if (!nextToken.equals("\\right]"))
      {
        System.out.println("Got "+nextToken+", expected \\right]");
        return;
      }
      builder.process("[squareBracket]");
    }
    else if (nextToken.equals("\\sqrt"))
    {
      NextToken();
      parseOneExpression25(builder);
      builder.process("[sqrt]");
      return;
    }
    else if (nextToken.equals("\\exp"))
    {
      NextToken();
      builder.process("e");
      parseOneExpression40(builder);
      builder.process("^");
      return;
    }
    else if (nextToken.equals("\\imath"))
    {
      builder.process("i");
    }
    else if (nextToken.equals("\\mathrm"))
    {
      NextToken();
      if (!matchToken("{")) return;

      int startPos = currentPos;
      while (currentPos < iCurrentExpression.length() && iCurrentExpression.charAt(currentPos) != '}' )
        currentPos++;
      String literal = iCurrentExpression.substring(startPos,currentPos);
      currentPos++;
      builder.processLiteral(literal);
      NextToken();
      return;
    }
    else if (nextToken.equals("-"))
    {
      NextToken();
      parseOneExpression30(builder);
      builder.process("-/1");
      return;
    }
    else if (nextToken.equals("\\neg"))
    {
      NextToken();
      parseOneExpression30(builder);
      builder.process("~");
      return;
    }
    else if (nextToken.equals("\\sum"))
    {
      builder.process("[sum]");
    }
    else if (nextToken.equals("\\int"))
    {
      builder.process("[int]");
    }
    else if (nextToken.equals("\\frac"))
    {
      NextToken();
      parseOneExpression40(builder);
      parseOneExpression40(builder);
      builder.process("/");
      return;
    }
    else if (nextToken.equals("\\begin"))
    {
      NextToken(); if (!matchToken("{")) return;
      NextToken(); String name = nextToken;
      NextToken(); if (!matchToken("}")) return;
      if (name.equals("array"))
      {
        int nrColumns = 0;
        int nrRows    = 0;
        NextToken(); if (!matchToken("{")) return;
        NextToken(); String coldef = nextToken;
        NextToken(); if (!matchToken("}")) return;
        nrColumns = coldef.length();
        nrRows = 1;
        NextToken();
        while (!nextToken.equals("\\end"))
        {
          parseOneExpression10(builder);

          if (nextToken.equals("\\\\"))
          {
            nrRows++;
            NextToken();
          }
          else if (nextToken.equals("&"))
          {
            NextToken();
          }
          else
          {
  //  System.out.println("END? "+nextToken);
          }
        }
        NextToken(); if (!matchToken("{")) return;
        NextToken(); String name2 = nextToken;
        NextToken(); if (!matchToken("}")) return;
        if (name2.equals("array"))
        {
          builder.process(""+nrRows);
          builder.process(""+nrColumns);
          builder.process("[grid]");
        }
      }
    }
    else
    {
      builder.process(nextToken);
    }
    NextToken();
  }

  boolean IsSpace(int c)
  {
    if (c == ' ' || c == '\t' || c == '\r' || c == '\n')
      return true;
    return false;
  }

  boolean IsAlNum(int c)
  {
    if (IsSpace(c))
      return false;
    if (c == '{')
      return false;
    if (c == '}')
      return false;
    if (c == '\\')
      return false;
    if (singleOps.indexOf(c) >= 0)
      return false;
    return true;
  }
}

