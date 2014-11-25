package net.sf.yacas;



class XmlTokenizer extends LispTokenizer
{
  /// NextToken returns a string representing the next token,
  /// or an empty list.
  public String NextToken(LispInput aInput, LispHashTable aHashTable) throws Exception
  {
    char c;
    int firstpos=0;
    if (aInput.EndOfStream())
      return aHashTable.LookUp(aInput.StartPtr().substring(firstpos,aInput.Position()));
    //skipping spaces
    while (IsSpace(aInput.Peek())) aInput.Next();
    firstpos = aInput.Position();
        c = aInput.Next();
    if (c == '<')
    {
      while (c != '>')
      {
        c = aInput.Next();
        LispError.Check(!aInput.EndOfStream(),LispError.KLispErrCommentToEndOfFile);
      }
    }
    else
    {
      while (aInput.Peek() != '<' && !aInput.EndOfStream())
      {
        c = aInput.Next();
      }
    }
    return aHashTable.LookUp(aInput.StartPtr().substring(firstpos,aInput.Position()));
  }

  private static boolean IsSpace(int c)
  {
    switch (c)
    {
    case 0x20:
    case 0x0D:
    case 0x0A:
    case 0x09:
        return true;
    default:
        return false;
    }
  }

}

