package net.sf.yacas;


class LispPrinter
{
  public void Print(LispPtr aExpression, LispOutput aOutput, LispEnvironment aEnvironment) throws Exception
  {
    PrintExpression(aExpression, aOutput, aEnvironment,0);
  }
  public void RememberLastChar(char aChar)
  {
  }

   void PrintExpression(LispPtr aExpression, LispOutput aOutput,
                      LispEnvironment aEnvironment,int aDepth /* =0 */) throws Exception
   {
    LispPtr iter = new LispPtr();
    iter.Set(aExpression.Get());
    int item = 0;
    while (iter.Get() != null)
    {
        // if String not null pointer: print string
        String string = iter.Get().String();

        if (string != null)
        {
            aOutput.Write(string);
            aOutput.PutChar(' ');
        }
        // else print "(", print sublist, and print ")"
        else if (iter.Get().SubList() != null)
        {
      if (item != 0)
      {
        Indent(aOutput,aDepth+1);
      }
            aOutput.Write("(");
            PrintExpression((iter.Get().SubList()),aOutput, aEnvironment,aDepth+1);
            aOutput.Write(")");
      item=0;
        }
        else
        {
            aOutput.Write("[GenericObject]");
        }
        iter = (iter.Get().Next());
  item++;
    } // print next element
   }

   void Indent(LispOutput aOutput, int aDepth) throws Exception
   {
    aOutput.Write("\n");
    int i;
    for (i=aDepth;i>0;i--)
    {
    aOutput.Write("  ");
    }
   }
};


