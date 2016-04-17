package net.sf.yacas;

import java.io.Writer;


final class InfixPrinter extends LispPrinter
{

  static int KMaxPrecedence = 60000;
  public InfixPrinter(LispOperators aPrefixOperators,
                 LispOperators aInfixOperators,
                 LispOperators aPostfixOperators,
                 LispOperators aBodiedOperators)
  {
    iPrefixOperators = aPrefixOperators;
    iInfixOperators = aInfixOperators;
    iPostfixOperators = aPostfixOperators;
    iBodiedOperators = aBodiedOperators;
    iPrevLastChar = 0;
  }
  @Override
  public void Print(LispPtr aExpression, Writer aOutput, LispEnvironment aEnvironment) throws Exception
  {
    iCurrentEnvironment = aEnvironment;
    Print(aExpression, aOutput, KMaxPrecedence);
  }
  @Override
  public void RememberLastChar(char aChar)
  {
    iPrevLastChar = aChar;
  }
  void Print(LispPtr aExpression, Writer aOutput, int iPrecedence) throws Exception
  {
    LispError.LISPASSERT(aExpression.Get() != null);

    String string = aExpression.Get().String();
    if (string != null)
    {
        boolean bracket=false;
        if (iPrecedence<KMaxPrecedence &&
            string.charAt(0) == '-' &&
            (Character.isDigit(string.charAt(1)) || string.charAt(1) == '.')
           )
        {
            bracket=true;
        }
        if (bracket) WriteToken(aOutput,"(");
        WriteToken(aOutput,string);
        if (bracket) WriteToken(aOutput,")");
        return;
    }

    if (aExpression.Get().Generic() != null) {
        if (aExpression.Get().Generic() instanceof ArrayClass) {
            WriteToken(aOutput, "Array");
            WriteToken(aOutput, "(");
            ArrayClass a = (ArrayClass)aExpression.Get().Generic();
            int n = a.Size();
            for (int i = 1; i <=n; ++i) {
                Print(new LispPtr(a.GetElement(i)), aOutput, KMaxPrecedence);
                if (i != n)
                    WriteToken(aOutput, ",");
            }
            WriteToken(aOutput, ")");
        } else if (aExpression.Get().Generic() instanceof AssociationClass) {
            WriteToken(aOutput, "Association");
            WriteToken(aOutput, "(");
            Print(((AssociationClass)aExpression.Get().Generic()).ToList(), aOutput, KMaxPrecedence);
            WriteToken(aOutput, ")");
        } else {
            WriteToken(aOutput,aExpression.Get().Generic().TypeName());
        }
        
        return;
    }

    LispPtr subList = aExpression.Get().SubList();
    LispError.Check(subList!=null, LispError.KLispErrUnprintableToken);
    if (subList.Get() == null)
    {
        WriteToken(aOutput,"( )");
    }
    else
    {
        int length = LispStandard.InternalListLength(subList);
        string = subList.Get().String();
        LispInFixOperator prefix  = iPrefixOperators.get(string);
        LispInFixOperator infix   = iInfixOperators.get(string);
        LispInFixOperator postfix = iPostfixOperators.get(string);
        LispInFixOperator bodied  = iBodiedOperators.get(string);
        LispInFixOperator op = null;

        if (length!=2)
        {
            prefix=null;
            postfix=null;
        }
        if (length!=3)
        {
            infix=null;
        }
        if (prefix != null)   op=prefix;
        if (postfix != null)  op=postfix;
        if (infix != null)    op=infix;

        if (op != null)
        {
            LispPtr left  = null;
            LispPtr right = null;

            if (prefix != null)
            {
                right = subList.Get().Next();
            }
            else if (infix != null)
            {
                left  = subList.Get().Next();
                right = subList.Get().Next().Get().Next();
            }
            else if (postfix != null)
            {
                left = subList.Get().Next();
            }

            if (iPrecedence < op.iPrecedence)
            {
                WriteToken(aOutput,"(");
            }
            else
            {
            //Vladimir?    aOutput.Write(" ");
            }
            if (left != null)
                Print(left, aOutput,op.iLeftPrecedence);
            WriteToken(aOutput,string);
            if (right != null)
                Print(right, aOutput,op.iRightPrecedence);
            if (iPrecedence < op.iPrecedence)
                WriteToken(aOutput,")");
        }
        else
        {
            LispIterator iter = new LispIterator(subList.Get().Next());
            if (string == iCurrentEnvironment.iList.String())
            {
                WriteToken(aOutput,"{");
                while (iter.GetObject() != null)
                {
                    Print(iter.Ptr(), aOutput, KMaxPrecedence);
                    iter.GoNext();
                    if (iter.GetObject() != null)
                        WriteToken(aOutput,",");
                }
                WriteToken(aOutput,"}");
            }
            else if (string == iCurrentEnvironment.iProg.String())
            {
                WriteToken(aOutput,"[");
                while (iter.GetObject() != null)
                {
                    Print(iter.Ptr(), aOutput, KMaxPrecedence);
                    iter.GoNext();
                    WriteToken(aOutput,";");
                }
                WriteToken(aOutput,"]");
            }
            else if (string == iCurrentEnvironment.iNth.String())
            {
                Print(iter.Ptr(), aOutput, 0);
                iter.GoNext();
                WriteToken(aOutput,"[");
                Print(iter.Ptr(), aOutput, KMaxPrecedence);
                WriteToken(aOutput,"]");
            }
            else
            {
                boolean bracket = false;
                if (bodied != null)
                {
//printf("%d > %d\n",iPrecedence, bodied.iPrecedence);
                  if (iPrecedence < bodied.iPrecedence)
                    bracket = true;
                }
                if (bracket) WriteToken(aOutput,"(");
                if (string != null)
                {
                  WriteToken(aOutput,string);
                }
                else
                {
                  Print(subList,aOutput,0);
                }
                WriteToken(aOutput,"(");

                LispIterator counter = new LispIterator(iter.Ptr());
                int nr=0;

                while (counter.GetObject() != null)
                {
                    counter.GoNext();
                    nr++;
                }

                if (bodied != null)
                    nr--;
                while (nr-- != 0)
                {
                    Print(iter.Ptr(), aOutput, KMaxPrecedence);

                    iter.GoNext();
                    if (nr != 0)
                        WriteToken(aOutput,",");
                }
                WriteToken(aOutput,")");
                if (iter.GetObject() != null)
                    Print(iter.Ptr(), aOutput, bodied.iPrecedence);

                if (bracket) WriteToken(aOutput,")");
            }
        }
    }
  }
  void WriteToken(Writer aOutput,String aString) throws Exception
  {
    if (LispTokenizer.IsAlNum(iPrevLastChar) && (LispTokenizer.IsAlNum(aString.charAt(0)) || aString.charAt(0)=='_'))
    {
        aOutput.write(" ");
    }
    else if (LispTokenizer.IsSymbolic(iPrevLastChar) && LispTokenizer.IsSymbolic(aString.charAt(0)))
    {
        aOutput.write(" ");
    }
    aOutput.write(aString);
    RememberLastChar(aString.charAt(aString.length()-1));
  }
  LispOperators iPrefixOperators;
  LispOperators iInfixOperators;
  LispOperators iPostfixOperators;
  LispOperators iBodiedOperators;
  char iPrevLastChar;
  LispEnvironment iCurrentEnvironment;
};
