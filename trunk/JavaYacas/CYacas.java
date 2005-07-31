
class CYacas
{
  CYacas(LispOutput stdoutput)
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
  String Evaluate(String input)
  {
    if (input.length() == 0)
      return "";
    String rs = "";
    try
    {
      iError = null;
      InputStatus someStatus = new InputStatus();
      StringBuffer inp = new StringBuffer();
      inp.append(input);
      inp.append(";");
      StringInput input_str = new StringInput(inp,someStatus);

//example creating an infix operator      env.iInfixOperators.SetOperator(10,"+");
//      LispParser parser = new LispParser(tokenizer, input_str,env);
      LispParser parser = new InfixParser(tokenizer, input_str, env, env.iPrefixOperators, env.iInfixOperators, env.iPostfixOperators, env.iBodiedOperators);
      LispPtr in_expr = new LispPtr();
      parser.Parse( in_expr );
      LispPtr result = new LispPtr();
      env.iEvaluator.Eval(env, result, in_expr);

      String percent = env.HashTable().LookUp("%");
      env.SetVariable(percent,result);
      env.SetGlobalEvaluates(percent);

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
      e.printStackTrace();
      System.out.println(e.toString());
      iError = e.toString();
    }
    return rs;
  }
  LispEnvironment env = null;
  LispTokenizer tokenizer = null;
  LispPrinter printer = null;
  String iError = null;
}
