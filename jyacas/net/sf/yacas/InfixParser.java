package net.sf.yacas;


final class InfixParser extends LispParser
{
  public InfixParser(LispTokenizer aTokenizer, LispInput aInput,
                LispEnvironment aEnvironment,
                LispOperators aPrefixOperators,
                LispOperators aInfixOperators,
                LispOperators aPostfixOperators,
                LispOperators aBodiedOperators)
  {
    super( aTokenizer,  aInput, aEnvironment);
    iPrefixOperators = aPrefixOperators;
    iInfixOperators = aInfixOperators;
    iPostfixOperators = aPostfixOperators;
    iBodiedOperators = aBodiedOperators;
  }
  @Override
  public void Parse(LispPtr aResult) throws Exception
  {
    ParseCont(aResult);
  }
  public void ParseCont(LispPtr aResult) throws Exception
  {
    ParsedObject object = new ParsedObject(this);
    object.Parse();
    aResult.Set(object.iResult.Get());
  }
  public LispOperators iPrefixOperators;
  public LispOperators iInfixOperators;
  public LispOperators iPostfixOperators;
  public LispOperators iBodiedOperators;
}


