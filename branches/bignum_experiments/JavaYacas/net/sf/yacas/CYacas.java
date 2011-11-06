package net.sf.yacas;


public class CYacas
{
  public CYacas(LispOutput stdoutput)
  {
    try
    {
      env = new LispEnvironment(stdoutput);
      tokenizer = new LispTokenizer();
      printer = new InfixPrinter(env.iPrefixOperators, env.iInfixOperators, env.iPostfixOperators, env.iBodiedOperators);
    }
    catch (Exception e)
    {
       e.printStackTrace();
      System.out.println(e.toString());
    }
  }
  public String Evaluate(String input)
  {
    if (input.length() == 0)
      return "";
    String rs = "";
    try
    {
       env.iEvalDepth=0;
       env.iEvaluator.ResetStack();


      iError = null;

      LispPtr in_expr = new LispPtr();
      if (env.iPrettyReader != null)
      {
        InputStatus someStatus = new InputStatus();
        StringBuffer inp = new StringBuffer();
        inp.append(input);
        InputStatus oldstatus = env.iInputStatus;
        env.iInputStatus.SetTo("String");
        StringInput newInput = new StringInput(new StringBuffer(input),env.iInputStatus);

        LispInput previous = env.iCurrentInput;
        env.iCurrentInput = newInput;
        try
        {
         LispPtr args = new LispPtr();
         LispStandard.InternalApplyString(env, in_expr,
                             env.iPrettyReader,
                             args);
        }
        catch (Exception e)
        {
          throw e;
        }
        finally
        {
          env.iCurrentInput = previous;
          env.iInputStatus.RestoreFrom(oldstatus);
        }
      }
      else
      {
        InputStatus someStatus = new InputStatus();
        StringBuffer inp = new StringBuffer();
        inp.append(input);
        inp.append(";");
        StringInput input_str = new StringInput(inp,someStatus);
        LispParser parser = new InfixParser(tokenizer, input_str, env, env.iPrefixOperators, env.iInfixOperators, env.iPostfixOperators, env.iBodiedOperators);
        parser.Parse( in_expr );
      }

      LispPtr result = new LispPtr();
      env.iEvaluator.Eval(env, result, in_expr);

      String percent = env.HashTable().LookUp("%");
      env.SetVariable(percent,result,true);

      StringBuffer string_out = new StringBuffer();
      LispOutput output = new StringOutput(string_out);

      if (env.iPrettyPrinter != null)
      {
         LispPtr nonresult = new LispPtr();
         LispStandard.InternalApplyString(env, nonresult,
                             env.iPrettyPrinter,
                             result);
        rs = string_out.toString();
      }
      else
      {
        printer.RememberLastChar(' ');
        printer.Print(result, output, env);
        rs = string_out.toString();
      }
    }
    catch (Exception e)
    {
//      e.printStackTrace();
      System.out.println(e.toString());
      iError = e.toString();
    }
    return rs;
  }
  public LispEnvironment env = null;
  LispTokenizer tokenizer = null;
  LispPrinter printer = null;
  String iError = null;
}
