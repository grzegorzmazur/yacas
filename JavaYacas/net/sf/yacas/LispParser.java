package net.sf.yacas;


class LispParser
{
   public LispParser(LispTokenizer aTokenizer, LispInput aInput,
              LispEnvironment aEnvironment)
   {
     iTokenizer = aTokenizer;
   iInput = aInput;
   iEnvironment = aEnvironment;
     iListed = false;
   }
   public void Parse(LispPtr aResult ) throws Exception
   {
  aResult.Set(null);

  String token;
  // Get token.
  token = iTokenizer.NextToken(iInput,iEnvironment.HashTable());
  if (token.length() == 0) //TODO FIXME either token == null or token.length() == 0?
  {
    aResult.Set(LispAtom.New(iEnvironment,"EndOfFile"));
    return;
  }
  ParseAtom(aResult,token);
   }

   void ParseList(LispPtr aResult) throws Exception
   {
    String token;

    LispPtr iter = aResult;
    if (iListed)
    {
        aResult.Set(LispAtom.New(iEnvironment,"List"));
        iter  = (aResult.Get().Next()); //TODO FIXME
    }
    for (;;)
    {
        //Get token.
        token = iTokenizer.NextToken(iInput,iEnvironment.HashTable());
        // if token is empty string, error!
        LispError.Check(token.length() > 0,LispError.KInvalidToken); //TODO FIXME
        // if token is ")" return result.
        if (token == iEnvironment.HashTable().LookUp(")"))
        {
            return;
        }
        // else parse simple atom with Parse, and append it to the
        // results list.

        ParseAtom(iter,token);
        iter = (iter.Get().Next()); //TODO FIXME
    }
   }

   void ParseAtom(LispPtr aResult,String aToken) throws Exception
   {
    // if token is empty string, return null pointer (no expression)
    if (aToken.length() == 0) //TODO FIXME either token == null or token.length() == 0?
        return;
    // else if token is "(" read in a whole array of objects until ")",
    //   and make a sublist
    if (aToken == iEnvironment.HashTable().LookUp("("))
    {
        LispPtr subList = new LispPtr();
        ParseList(subList);
        aResult.Set(LispSubList.New(subList.Get()));
        return;
    }
    // else make a simple atom, and return it.
    aResult.Set(LispAtom.New(iEnvironment,aToken));
   }
   public LispTokenizer iTokenizer;
   public LispInput iInput;
   public LispEnvironment iEnvironment;
   public boolean iListed;
};

