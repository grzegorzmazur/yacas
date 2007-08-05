
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
      return;
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
      parseOneExpression(builder);
      NextToken();
    }
    while (builder.StackDepth() > 1)
    {
      builder.process("*");
    }
    SBox expression = builder.pop();
    return expression;
  }
  void parseOneExpression(SBoxBuilder builder)
  {
    if (nextToken.equals("{"))
    {
      SBoxBuilder builder2 = new SBoxBuilder();
      NextToken();
      while (!nextToken.equals("}"))
      {
        parseOneExpression(builder2);
        NextToken();
      }
      while (builder2.StackDepth() > 1)
      {
        builder2.process("*");
      }
      SBox expression = builder2.pop();
      builder.push(expression);
    }
    else if (nextToken.equals("\\sqrt"))
    {
      NextToken();
      parseOneExpression(builder);
      builder.process("[sqrt]");
    }
    else if (nextToken.equals("\\sum"))
    {
      builder.process("[sum]");
    }
    else if (nextToken.equals("\\frac"))
    {
      NextToken();
      parseOneExpression(builder);
      NextToken();
      parseOneExpression(builder);
      builder.process("/");
    }
    else if (nextToken.equals("\\left("))
    {
      SBoxBuilder builder2 = new SBoxBuilder();
      NextToken();
      while (!nextToken.equals("\\right)"))
      {
        parseOneExpression(builder2);
        NextToken();
      }
      while (builder2.StackDepth() > 1)
      {
        builder2.process("*");
      }
      SBox expression = builder2.pop();
      builder.push(expression);
      builder.process("[roundBracket]");
    }
    else if (nextToken.equals("\\left["))
    {
      SBoxBuilder builder2 = new SBoxBuilder();
      NextToken();
      while (!nextToken.equals("\\right]"))
      {
        parseOneExpression(builder2);
        NextToken();
      }
      while (builder2.StackDepth() > 1)
      {
        builder2.process("*");
      }
      SBox expression = builder2.pop();
      builder.push(expression);
      builder.process("[squareBracket]");
    }

    
    else if (
   nextToken.equals("+")
|| nextToken.equals("^")
|| nextToken.equals("_")
|| nextToken.equals("=")
|| nextToken.equals(",")
     )
    {
      String token = nextToken;
      NextToken();
      parseOneExpression(builder);
      builder.process(token);
    }
    else
    {
      builder.process(nextToken);
    }
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

