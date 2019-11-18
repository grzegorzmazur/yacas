package net.sf.yacas;

import java.io.Writer;


class LispPrinter
{
  public void Print(LispPtr aExpression, Writer aOutput, LispEnvironment aEnvironment) throws Exception
  {
    PrintExpression(aExpression, aOutput, aEnvironment,0);
  }
  public void RememberLastChar(char aChar)
  {
  }

   void PrintExpression(LispPtr aExpression, Writer aOutput,
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
            aOutput.write(string);
            aOutput.write(" ");
        }
        // else print "(", print sublist, and print ")"
        else if (iter.Get().SubList() != null)
        {
      if (item != 0)
      {
        Indent(aOutput,aDepth+1);
      }
            aOutput.write("(");
            PrintExpression((iter.Get().SubList()),aOutput, aEnvironment,aDepth+1);
            aOutput.write(")");
      item=0;
        }
        else
        {
            aOutput.write("[GenericObject]");
        }
        iter = (iter.Get().Next());
  item++;
    } // print next element
   }

    void Indent(Writer aOutput, int aDepth) throws Exception {
        aOutput.write("\n");
        int i;
        for (i = aDepth; i > 0; i--) {
            aOutput.write("  ");
        }
    }
}
