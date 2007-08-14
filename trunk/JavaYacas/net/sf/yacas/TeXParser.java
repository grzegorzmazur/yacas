
package net.sf.yacas;

public class TeXParser
{
  static String singleOps = "^_+=,";
  String iCurrentExpression;
  int currentPos;
  String nextToken;
  void NextToken()
  {
    nextToken = "";
    if (currentPos == iCurrentExpression.length())
    {
      return;
    }
    while (currentPos < iCurrentExpression.length() && IsSpace(iCurrentExpression.charAt(currentPos))) 
      currentPos++;
    if (currentPos == iCurrentExpression.length())
    {
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
      return;
    }
    int c = iCurrentExpression.charAt(currentPos);
    if (c == '{')
    {
      nextToken = "{";
      currentPos++;
      return;
    }
    else if (c == '}')
    {
      nextToken = "}";
      currentPos++;
      return;
    }
    else if (singleOps.indexOf(c)>=0)
    {
      nextToken = ""+((char)c);
      currentPos++;
      return;
    }
    else if (c == '\\')
    {
      currentPos++;
      NextToken();
      nextToken = "\\"+nextToken;
      return;
    }
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
    while (nextToken.length() > 0)
    {
      parseOneExpression10(builder);
      NextToken();
    }
    while (builder.StackDepth() > 1)
    {
      builder.process("*");
    }
    SBox expression = builder.pop();
    return expression;
  }

  void parseOneExpression10(SBoxBuilder builder)
  {
    parseOneExpression20(builder);
    // = ,
    while (nextToken.equals("=") || nextToken.equals(","))
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
    while (nextToken.equals("+") || nextToken.equals("-"))
    {
      String token = nextToken;
      if (token.equals("-"))
        token = "-/2";
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
          !nextToken.equals("}") && 
          !nextToken.equals("\\right)") && 
          !nextToken.equals("\\right]") && 
          !nextToken.equals(",") 
          )
    {
      String token = "*";
      parseOneExpression30(builder);
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
      parseOneExpression40(builder);
      builder.process("[sqrt]");
    }
    else if (nextToken.equals("-"))
    {
      NextToken();
      parseOneExpression30(builder);
      builder.process("-/1");
      return;
    }
    else if (nextToken.equals("\\sum"))
    {
      builder.process("[sum]");
    }
    else if (nextToken.equals("\\frac"))
    {
      NextToken();
      parseOneExpression40(builder);
      parseOneExpression40(builder);
      builder.process("/");
      return;
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
    if (singleOps.indexOf(c) >= 0)
      return false;
    return true;
  }
}

