package net.sf.yacas;


class ParsedObject
{
  public ParsedObject(InfixParser aParser)
  {
    iParser = aParser;
    iError = false;
    iEndOfFile = false;
    iLookAhead = null;
  }
  public void Parse() throws Exception
  {
    ReadToken();
    if (iEndOfFile)
    {
        iResult.Set(iParser.iEnvironment.iEndOfFile.Copy(true));
        return;
    }

    ReadExpression(InfixPrinter.KMaxPrecedence);  // least precedence

    if (iLookAhead != iParser.iEnvironment.iEndStatement.String())
    {
      Fail();
    }
    if (iError)
    {
      while (iLookAhead.length() > 0 && iLookAhead != iParser.iEnvironment.iEndStatement.String())
      {
        ReadToken();
      }
    }

    if (iError)
    {
      iResult.Set(null);
    }
    LispError.Check(!iError,LispError.KLispErrInvalidExpression);
  }
  void ReadToken() throws Exception
  {
    // Get token.
    iLookAhead = iParser.iTokenizer.NextToken(iParser.iInput,
                                              iParser.iEnvironment.HashTable());
    if (iLookAhead.length() == 0)
        iEndOfFile=true;
  }
  void MatchToken(String aToken) throws Exception
  {
    if (aToken != iLookAhead)
      Fail();
    ReadToken();
  }
  void ReadExpression(int depth) throws Exception
  {
    ReadAtom();

    for(;;)
    {
        //Handle special case: a[b]. a is matched with lowest precedence!!
        if (iLookAhead == iParser.iEnvironment.iProgOpen.String())
        {
            // Match opening bracket
            MatchToken(iLookAhead);
            // Read "index" argument
            ReadExpression(InfixPrinter.KMaxPrecedence);
            // Match closing bracket
            if (iLookAhead != iParser.iEnvironment.iProgClose.String())
            {
                LispError.RaiseError("Expecting a ] close bracket for program block, but got "+iLookAhead+" instead");
                return;
            }
            MatchToken(iLookAhead);
            // Build into Ntn(...)
            String theOperator = iParser.iEnvironment.iNth.String();
            InsertAtom(theOperator);
            Combine(2);
        }
        else
        {
            LispInFixOperator op = (LispInFixOperator)iParser.iInfixOperators.LookUp(iLookAhead);
            if (op == null)
            {
//printf("op [%s]\n",iLookAhead.String());
              if (LispTokenizer.IsSymbolic(iLookAhead.charAt(0)))
              {
                int origlen = iLookAhead.length();
                int len = origlen;
//printf("IsSymbolic, len=%d\n",len);

                while (len>1)
                {
                  len--;
                  String lookUp =
                    iParser.iEnvironment.HashTable().LookUp(iLookAhead.substring(0,len));

//printf("trunc %s\n",lookUp.String());
                  op = (LispInFixOperator)iParser.iInfixOperators.LookUp(lookUp);
//if (op) printf("FOUND\n");
                  if (op != null)
                  {
String toLookUp = iLookAhead.substring(len,origlen);
                    String lookUpRight =
                      iParser.iEnvironment.HashTable().LookUp(toLookUp);

//printf("right: %s (%d)\n",lookUpRight.String(),origlen-len);

                    if (iParser.iPrefixOperators.LookUp(lookUpRight) != null)
                    {
//printf("ACCEPT %s\n",lookUp.String());
                      iLookAhead = lookUp;
                      LispInput input = iParser.iInput;
                      int newPos = input.Position()-(origlen-len);
                      input.SetPosition(newPos);
//printf("Pushhback %s\n",&input.StartPtr()[input.Position()]);
                      break;
                    }
                    else op=null;
                  }
                }
                if (op == null) return;
              }
              else
              {
                return;
              }




//              return;
            }
            if (depth < op.iPrecedence)
                return;
            int upper=op.iPrecedence;
            if (op.iRightAssociative == 0)
                upper--;
            GetOtherSide(2,upper);
        }
    }
  }
  void ReadAtom() throws Exception
  {
    LispInFixOperator op;
    // Parse prefix operators
    op = (LispInFixOperator)iParser.iPrefixOperators.LookUp(iLookAhead);
    if (op != null)
    {
        String theOperator = iLookAhead;
        MatchToken(iLookAhead);
        {
            ReadExpression(op.iPrecedence);
            InsertAtom(theOperator);
            Combine(1);
        }
    }
    // Else parse brackets
    else if (iLookAhead == iParser.iEnvironment.iBracketOpen.String())
    {
        MatchToken(iLookAhead);
        ReadExpression(InfixPrinter.KMaxPrecedence);  // least precedence
        MatchToken(iParser.iEnvironment.iBracketClose.String());
    }
    //Parse lists
    else if (iLookAhead == iParser.iEnvironment.iListOpen.String())
    {
        int nrargs=0;
        MatchToken(iLookAhead);
        while (iLookAhead != iParser.iEnvironment.iListClose.String())
        {
            ReadExpression(InfixPrinter.KMaxPrecedence);  // least precedence
            nrargs++;

            if (iLookAhead == iParser.iEnvironment.iComma.String())
            {
                MatchToken(iLookAhead);
            }
            else if (iLookAhead != iParser.iEnvironment.iListClose.String())
            {
                LispError.RaiseError("Expecting a } close bracket for a list, but got "+iLookAhead+" instead");
                return;
            }
        }
        MatchToken(iLookAhead);
        String theOperator = iParser.iEnvironment.iList.String();
        InsertAtom(theOperator);
        Combine(nrargs);

    }
    // Parse prog bodies
    else if (iLookAhead == iParser.iEnvironment.iProgOpen.String())
    {
        int nrargs=0;

        MatchToken(iLookAhead);
        while (iLookAhead != iParser.iEnvironment.iProgClose.String())
        {
            ReadExpression(InfixPrinter.KMaxPrecedence);  // least precedence
            nrargs++;

            if (iLookAhead == iParser.iEnvironment.iEndStatement.String())
            {
                MatchToken(iLookAhead);
            }
            else
            {
                LispError.RaiseError("Expecting ; end of statement in program block, but got "+iLookAhead+" instead");
                return;
            }
        }
        MatchToken(iLookAhead);
        String theOperator = iParser.iEnvironment.iProg.String();
        InsertAtom(theOperator);

        Combine(nrargs);
    }
    // Else we have an atom.
    else
    {
        String theOperator = iLookAhead;
        MatchToken(iLookAhead);

        int nrargs=-1;
        if (iLookAhead == iParser.iEnvironment.iBracketOpen.String())
        {
            nrargs=0;
            MatchToken(iLookAhead);
            while (iLookAhead != iParser.iEnvironment.iBracketClose.String())
            {
                ReadExpression(InfixPrinter.KMaxPrecedence);  // least precedence
                nrargs++;

                if (iLookAhead == iParser.iEnvironment.iComma.String())
                {
                    MatchToken(iLookAhead);
                }
                else if (iLookAhead != iParser.iEnvironment.iBracketClose.String())
                {
                    LispError.RaiseError("Expecting ) closing bracket for sub-expression, but got "+iLookAhead+" instead");
                    return;
                }
            }
            MatchToken(iLookAhead);

            op = (LispInFixOperator)iParser.iBodiedOperators.LookUp(theOperator);
            if (op != null)
            {
                ReadExpression(op.iPrecedence); // InfixPrinter.KMaxPrecedence
                nrargs++;
            }
        }
        InsertAtom(theOperator);
        if (nrargs>=0)
            Combine(nrargs);

    }

    // Parse postfix operators

    while ((op = (LispInFixOperator)iParser.iPostfixOperators.LookUp(iLookAhead)) != null)
    {
        InsertAtom(iLookAhead);
        MatchToken(iLookAhead);
        Combine(1);
    }
  }
  void GetOtherSide(int aNrArgsToCombine, int depth) throws Exception
  {
    String theOperator = iLookAhead;
    MatchToken(iLookAhead);
    ReadExpression(depth);
    InsertAtom(theOperator);
    Combine(aNrArgsToCombine);
  }
  void Combine(int aNrArgsToCombine) throws Exception
  {
    LispPtr subList = new LispPtr();
    subList.Set(LispSubList.New(iResult.Get()));
    LispIterator iter = new LispIterator(iResult);
    int i;
    for (i=0;i<aNrArgsToCombine;i++)
    {
      if (iter.GetObject() == null)
      {
        Fail();
        return;
      }
      iter.GoNext();
    }
    if (iter.GetObject() == null)
    {
      Fail();
      return;
    }
    subList.Get().Next().Set(iter.GetObject().Next().Get());
    iter.GetObject().Next().Set(null);

    LispStandard.InternalReverseList(subList.Get().SubList().Get().Next(),
                     subList.Get().SubList().Get().Next());
    iResult.Set(subList.Get());
  }
  void InsertAtom(String aString) throws Exception
  {
    LispPtr ptr = new LispPtr();
    ptr.Set(LispAtom.New(iParser.iEnvironment,aString));
    ptr.Get().Next().Set(iResult.Get());
    iResult.Set(ptr.Get());
  }
  void Fail()  throws Exception // called when parsing fails, raising an exception
  {
    iError = true;
    if (iLookAhead != null)
    {
      LispError.RaiseError("Error parsing expression, near token "+iLookAhead);
    }
    LispError.RaiseError("Error parsing expression");
  }

  InfixParser iParser;
  boolean iError;
  boolean iEndOfFile;
  String iLookAhead;

  public LispPtr iResult =  new LispPtr();
};
