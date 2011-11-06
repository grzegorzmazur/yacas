package net.sf.yacas;


class LispStandard
{
  static boolean IsNumber(String ptr, boolean aAllowFloat)
  {
    int pos = 0;
    if (ptr.charAt(pos) == '-' || ptr.charAt(pos) == '+')
        pos++;

    int nrDigits=0;
    int index=0;
    if (pos+index == ptr.length())
      return false;
    while(ptr.charAt(pos+index) >= '0' && ptr.charAt(pos+index) <= '9')
    {
        nrDigits++;
        index++;
        if (pos+index == ptr.length())
          return true;
    }
    if (ptr.charAt(pos+index) == '.')
    {
        if (!aAllowFloat)
            return false;
        index++;
        if (pos+index == ptr.length())
          return true;
        while(ptr.charAt(pos+index) >= '0' && ptr.charAt(pos+index) <= '9')
        {
            nrDigits++;
            index++;
            if (pos+index == ptr.length())
              return true;
        }
    }
    if (nrDigits == 0)
        return false;
    if (ptr.charAt(pos+index) == 'e' || ptr.charAt(pos+index) == 'E')
    {
        if (!aAllowFloat)
            return false;
        if (!BigNumber.NumericSupportForMantissa())
            return false;
        index++;
        if (pos+index == ptr.length())
          return true;
        if (ptr.charAt(pos+index) == '-' || ptr.charAt(pos+index) == '+') index++;
        while(ptr.charAt(pos+index) >= '0' && ptr.charAt(pos+index) <= '9')
        {
          index++;
          if (pos+index == ptr.length())
            return true;
        }
    }
    if (ptr.length() != (pos+index)) return false;
    return true;
  }

  static int InternalListLength(LispPtr aOriginal) throws Exception
  {
    LispIterator iter = new LispIterator(aOriginal);
    int length = 0;
    while (iter.GetObject() != null)
    {
      iter.GoNext();
      length++;
    }
    return length;
  }
  static void InternalReverseList(LispPtr aResult, LispPtr aOriginal)
  {
    LispPtr iter = new LispPtr(aOriginal);
    LispPtr previous = new LispPtr();
    LispPtr tail = new LispPtr();
    tail.Set(aOriginal.Get());
 
    while (iter.Get() != null)
    {
      tail.Set(iter.Get().Next().Get());
      iter.Get().Next().Set(previous.Get());
      previous.Set(iter.Get());
      iter.Set(tail.Get());
    }
    aResult.Set(previous.Get());
  }

  public static void ReturnUnEvaluated(LispPtr aResult,LispPtr aArguments, LispEnvironment aEnvironment) throws Exception
  {
    LispPtr full = new LispPtr();
    full.Set(aArguments.Get().Copy(false));
    aResult.Set(LispSubList.New(full.Get()));

    LispIterator iter = new LispIterator(aArguments);
    iter.GoNext();

    while (iter.GetObject() != null)
    {
      LispPtr next = new LispPtr();
      aEnvironment.iEvaluator.Eval(aEnvironment, next, iter.Ptr());
      full.Get().Next().Set(next.Get());
      full.Set(next.Get());
      iter.GoNext();
    }
    full.Get().Next().Set(null);
  }




  public static void InternalApplyString(LispEnvironment aEnvironment, LispPtr aResult,
                 String aOperator,LispPtr aArgs) throws Exception
  {
    LispError.Check(InternalIsString(aOperator),LispError.KLispErrNotString);

    LispObject head =
        LispAtom.New(aEnvironment,SymbolName(aEnvironment, aOperator));
    head.Next().Set(aArgs.Get());
    LispPtr body = new LispPtr();
    body.Set(LispSubList.New(head));
    aEnvironment.iEvaluator.Eval(aEnvironment, aResult, body);
  }

  public static void InternalApplyPure(LispPtr oper,LispPtr args2,LispPtr aResult, LispEnvironment aEnvironment) throws Exception
  {
      LispError.Check(oper.Get().SubList() != null,LispError.KLispErrInvalidArg);
      LispError.Check(oper.Get().SubList().Get() != null,LispError.KLispErrInvalidArg);
      LispPtr oper2 = new LispPtr();
      oper2.Set(oper.Get().SubList().Get().Next().Get());
      LispError.Check(oper2.Get() != null,LispError.KLispErrInvalidArg);

      LispPtr body = new LispPtr();
      body.Set(oper2.Get().Next().Get());
      LispError.Check(body.Get() != null,LispError.KLispErrInvalidArg);

      LispError.Check(oper2.Get().SubList() != null,LispError.KLispErrInvalidArg);
      LispError.Check(oper2.Get().SubList().Get() != null,LispError.KLispErrInvalidArg);
      oper2.Set(oper2.Get().SubList().Get().Next().Get());

      aEnvironment.PushLocalFrame(false);
      try
      {
        while (oper2.Get() != null)
        {
            LispError.Check(args2.Get() != null,LispError.KLispErrInvalidArg);

            String var = oper2.Get().String();
            LispError.Check(var != null,LispError.KLispErrInvalidArg);
            LispPtr newly = new LispPtr();
            newly.Set(args2.Get().Copy(false));
            aEnvironment.NewLocal(var,newly.Get());
            oper2.Set(oper2.Get().Next().Get());
            args2.Set(args2.Get().Next().Get());
        }
        LispError.Check(args2.Get() == null,LispError.KLispErrInvalidArg);
        aEnvironment.iEvaluator.Eval(aEnvironment, aResult, body);
      }
      catch (Yacasexception e) { throw e; }
      finally { aEnvironment.PopLocalFrame(); }
 
  }

  public static void InternalTrue(LispEnvironment aEnvironment, LispPtr aResult) throws Exception
  {
    aResult.Set(aEnvironment.iTrue.Copy(false));
  }

  public static void InternalFalse(LispEnvironment aEnvironment, LispPtr aResult) throws Exception
  {
    aResult.Set(aEnvironment.iFalse.Copy(false));
  }
  public static void InternalBoolean(LispEnvironment aEnvironment, LispPtr aResult, boolean aValue) throws Exception
  {
    if (aValue)
    {
      InternalTrue(aEnvironment, aResult);
    }
    else
    {
      InternalFalse(aEnvironment, aResult);
    }
  }

  public static void InternalNth(LispPtr aResult, LispPtr aArg, int n) throws Exception
  {
    LispError.Check(aArg.Get() != null,LispError.KLispErrInvalidArg);
    LispError.Check(aArg.Get().SubList() != null,LispError.KLispErrInvalidArg);
    LispError.Check(n>=0,LispError.KLispErrInvalidArg);
    LispIterator iter = new LispIterator(aArg.Get().SubList());

    while (n>0)
    {
      LispError.Check(iter.GetObject() != null,LispError.KLispErrInvalidArg);
      iter.GoNext();
      n--;
    }
    LispError.Check(iter.GetObject() != null,LispError.KLispErrInvalidArg);
    aResult.Set(iter.GetObject().Copy(false));
  }


  public static void InternalTail(LispPtr aResult, LispPtr aArg) throws Exception
  {
    LispError.Check(aArg.Get() != null,LispError.KLispErrInvalidArg);
    LispError.Check(aArg.Get().SubList() != null,LispError.KLispErrInvalidArg);

    LispPtr iter = aArg.Get().SubList();

    LispError.Check(iter.Get() != null,LispError.KLispErrInvalidArg);
    aResult.Set(LispSubList.New(iter.Get().Next().Get()));
  }

  public static boolean IsTrue(LispEnvironment aEnvironment, LispPtr aExpression) throws Exception
  {
    LispError.LISPASSERT(aExpression.Get() != null);
    return aExpression.Get().String() == aEnvironment.iTrue.String();
  }
  public static boolean IsFalse(LispEnvironment aEnvironment, LispPtr aExpression) throws Exception
  {
    LispError.LISPASSERT(aExpression.Get() != null);
    return aExpression.Get().String() == aEnvironment.iFalse.String();
  }

  public static String SymbolName(LispEnvironment aEnvironment, String aSymbol)
  {
    if (aSymbol.charAt(0) == '\"')
    {
      return aEnvironment.HashTable().LookUpUnStringify(aSymbol);
    }
    else
    {
      return aEnvironment.HashTable().LookUp(aSymbol);
    }
  }

  public static boolean InternalIsList(LispPtr aPtr) throws Exception
  {
    if (aPtr.Get() == null)
        return false;
    if (aPtr.Get().SubList() == null)
        return false;
    if (aPtr.Get().SubList().Get() == null)
        return false;
    //TODO this StrEqual is far from perfect. We could pass in a LispEnvironment object...
    if (!aPtr.Get().SubList().Get().String().equals("List"))
        return false;
    return true;
  }

  public static boolean InternalIsString(String aOriginal)
  {
    if (aOriginal != null)
      if (aOriginal.charAt(0) == '\"')
        if (aOriginal.charAt(aOriginal.length()-1) == '\"')
          return true;
    return false;
  }

  public static void InternalNot(LispPtr aResult, LispEnvironment aEnvironment, LispPtr aExpression) throws Exception
  {
    if (IsTrue(aEnvironment, aExpression))
    {
        InternalFalse(aEnvironment,aResult);
    }
    else
    {
        LispError.Check(IsFalse(aEnvironment, aExpression),LispError.KLispErrInvalidArg);
        InternalTrue(aEnvironment,aResult);
    }
  }

  public static void InternalFlatCopy(LispPtr aResult, LispPtr aOriginal) throws Exception
  {
    LispIterator orig = new LispIterator(aOriginal);
    LispIterator res = new LispIterator(aResult);

    while (orig.GetObject() != null)
    {
      res.Ptr().Set(orig.GetObject().Copy(false));
      orig.GoNext();
      res.GoNext();
    }
  }


  public static boolean InternalEquals(LispEnvironment aEnvironment, LispPtr aExpression1, LispPtr aExpression2) throws Exception
  {
    // Handle pointers to same, or null
    if (aExpression1.Get() == aExpression2.Get())
    {
        return true;
    }

    BigNumber n1 = aExpression1.Get().Number(aEnvironment.Precision());
    BigNumber n2 = aExpression2.Get().Number(aEnvironment.Precision());
    if (!(n1 == null && n2 == null) )
    {
        if (n1 == n2)
        {
            return true;
        }
        if (n1 == null) return false;
        if (n2 == null) return false;
        if (n1.Equals(n2)) return true;
        return false;
    }

    //Pointers to strings should be the same
    if (aExpression1.Get().String() != aExpression2.Get().String())
    {
        return false;
    }

    // Handle same sublists, or null
    if (aExpression1.Get().SubList() == aExpression2.Get().SubList())
    {
        return true;
    }

    // Now check the sublists
    if (aExpression1.Get().SubList() != null)
    {
        if (aExpression2.Get().SubList() == null)
        {
            return false;
        }
        LispIterator iter1 = new LispIterator(aExpression1.Get().SubList());
        LispIterator iter2 = new LispIterator(aExpression2.Get().SubList());

        while (iter1.GetObject() != null && iter2.GetObject() != null)
        {
            // compare two list elements
            if (!InternalEquals(aEnvironment, iter1.Ptr(),iter2.Ptr()))
            {
                return false;
            }
 
            // Step to next
            iter1.GoNext();
            iter2.GoNext();
        }
        // Lists don't have the same length
        if (iter1.GetObject() != iter2.GetObject())
            return false;

        // Same!
        return true;
    }

    // expressions sublists are not the same!
    return false;
  }





  public static void InternalSubstitute(LispPtr aTarget, LispPtr aSource, SubstBehaviourBase aBehaviour) throws Exception
  {
    LispObject object = aSource.Get();
    LispError.LISPASSERT(object != null);
    if (!aBehaviour.Matches(aTarget,aSource))
    {
      LispPtr oldList = object.SubList();
      if (oldList != null)
      {
        LispPtr newList = new LispPtr();
        LispPtr next = newList;
        while (oldList.Get() != null)
        {
          InternalSubstitute(next, oldList, aBehaviour);
          oldList = oldList.Get().Next();
          next = next.Get().Next();
        }
        aTarget.Set(LispSubList.New(newList.Get()));
      }
      else
      {
        aTarget.Set(object.Copy(false));
      }
    }
  }

  public static String InternalUnstringify(String aOriginal) throws Exception
  {
    LispError.Check(aOriginal != null,LispError.KLispErrInvalidArg);
    LispError.Check(aOriginal.charAt(0) == '\"',LispError.KLispErrInvalidArg);
    int nrc=aOriginal.length()-1;
    LispError.Check(aOriginal.charAt(nrc) == '\"',LispError.KLispErrInvalidArg);
    return aOriginal.substring(1,nrc);
  }




  public static void DoInternalLoad(LispEnvironment aEnvironment,LispInput aInput) throws Exception
  {
    LispInput previous = aEnvironment.iCurrentInput;
    try
    {
      aEnvironment.iCurrentInput = aInput;
      // TODO make "EndOfFile" a global thing
      // read-parse-eval to the end of file
      String eof = aEnvironment.HashTable().LookUp("EndOfFile");
      boolean endoffile = false;
      InfixParser parser = new InfixParser(new LispTokenizer(),
                         aEnvironment.iCurrentInput, aEnvironment,
                         aEnvironment.iPrefixOperators, aEnvironment.iInfixOperators,
                         aEnvironment.iPostfixOperators, aEnvironment.iBodiedOperators);
      LispPtr readIn =new LispPtr();
      while (!endoffile)
      {
        // Read expression
        parser.Parse(readIn);

        LispError.Check(readIn.Get() != null, LispError.KLispErrReadingFile);
        // Check for end of file
        if (readIn.Get().String() == eof)
        {
          endoffile = true;
        }
        // Else evaluate
        else
        {
          LispPtr result = new LispPtr();
          aEnvironment.iEvaluator.Eval(aEnvironment, result, readIn);
        }
      }
    }
    catch (Exception e)
    {
      throw e;
    }
    finally
    {
      aEnvironment.iCurrentInput = previous;
    }
  }



  public static void InternalLoad(LispEnvironment aEnvironment,String aFileName) throws Exception
  {
    String oper = InternalUnstringify(aFileName);

    String hashedname = aEnvironment.HashTable().LookUp(oper);

    InputStatus oldstatus = new InputStatus(aEnvironment.iInputStatus);
    aEnvironment.iInputStatus.SetTo(hashedname);
    try
    {
      // Open file
      LispInput newInput = // new StdFileInput(hashedname, aEnvironment.iInputStatus);
          OpenInputFile(aEnvironment, aEnvironment.iInputDirectories, hashedname, aEnvironment.iInputStatus);
 
      LispError.Check(newInput != null, LispError.KLispErrFileNotFound);
      DoInternalLoad(aEnvironment,newInput);
    }
    catch (Exception e)
    {
      throw e;
    }
    finally
    {
      aEnvironment.iInputStatus.RestoreFrom(oldstatus);
    }
  }
 
  public static void InternalUse(LispEnvironment aEnvironment,String aFileName) throws Exception
  {
    LispDefFile def = aEnvironment.iDefFiles.File(aFileName);
    if (!def.IsLoaded())
    {
      def.SetLoaded();
      InternalLoad(aEnvironment,aFileName);
    }
  }

  static String PrintExpression(LispPtr aExpression,
                      LispEnvironment aEnvironment,
                      int aMaxChars) throws Exception
  {
    StringBuffer result = new StringBuffer();
    StringOutput newOutput = new StringOutput(result);
    InfixPrinter infixprinter = new InfixPrinter(aEnvironment.iPrefixOperators,
                              aEnvironment.iInfixOperators,
                              aEnvironment.iPostfixOperators,
                              aEnvironment.iBodiedOperators);
    infixprinter.Print(aExpression, newOutput, aEnvironment);
    if (aMaxChars > 0 && result.length()>aMaxChars)
    {
      result.delete(aMaxChars,result.length());
      result.append((char)'.');
      result.append((char)'.');
      result.append((char)'.');
    }
    return result.toString();
  }


  public static LispInput OpenInputFile(String aFileName, InputStatus aInputStatus) throws Exception
  {
    try
    {
      if (zipFile != null)
      {
        java.util.zip.ZipEntry e = zipFile.getEntry(aFileName);
        if (e != null)
        {
          java.io.InputStream s = zipFile.getInputStream(e);
          return new StdFileInput(s, aInputStatus);
        }
      }

      if (aFileName.substring(0,4).equals("jar:"))
      {
        return new JarInputFile(aFileName, aInputStatus);
      }
      else
      {
        return new StdFileInput(aFileName, aInputStatus);
      }
    }
    catch (Exception e)
    {
    }
    return null;
  }

  public static LispInput OpenInputFile(LispEnvironment aEnvironment,
      InputDirectories aInputDirectories, String aFileName,
      InputStatus aInputStatus) throws Exception
  {
    String othername = aFileName;
    int i = 0;
    LispInput f = OpenInputFile(othername, aInputStatus);
    while (f == null && i<aInputDirectories.size())
    {
      othername = ((String)aInputDirectories.get(i)) + aFileName;
      f = OpenInputFile(othername, aInputStatus);
      i++;
    }
    return f;
  }


  public static String InternalFindFile(String aFileName, InputDirectories aInputDirectories) throws Exception
  {
    InputStatus inputStatus = new InputStatus();
    String othername = aFileName;
    int i = 0;
    LispInput f = OpenInputFile(othername, inputStatus);
    if (f != null) return othername;
    while (i<aInputDirectories.size())
    {
      othername = ((String)aInputDirectories.get(i)) + aFileName;
      f = OpenInputFile(othername, inputStatus);
      if (f != null) return othername;
      i++;
    }
    return "";
  }

  public static java.util.zip.ZipFile zipFile = null;


  private static void DoLoadDefFile(LispEnvironment aEnvironment, LispInput aInput,
                            LispDefFile def) throws Exception
  {
    LispInput previous = aEnvironment.iCurrentInput;
    try
    {
      aEnvironment.iCurrentInput = aInput;
      String eof = aEnvironment.HashTable().LookUp("EndOfFile");
      String end = aEnvironment.HashTable().LookUp("}");
      boolean endoffile = false;

      LispTokenizer tok = new LispTokenizer();

      while (!endoffile)
      {
        // Read expression
        String token = tok.NextToken(aEnvironment.iCurrentInput, aEnvironment.HashTable());

        // Check for end of file
        if (token == eof || token == end)
        {
            endoffile = true;
        }
        // Else evaluate
        else
        {
            String str = token;
            LispMultiUserFunction multiUser = aEnvironment.MultiUserFunction(str);
            if (multiUser.iFileToOpen!=null)
            {
              throw new Yacasexception("["+str+"]"+"] : def file already chosen: "+multiUser.iFileToOpen.iFileName);
            }
            multiUser.iFileToOpen = def;
        }
      }
    }
    catch (Exception e)
    {
      throw e;
    }
    finally
    {
      aEnvironment.iCurrentInput = previous;
    }
  }


  public static void LoadDefFile(LispEnvironment aEnvironment, String aFileName) throws Exception
  {
    LispError.LISPASSERT(aFileName!=null);

    String flatfile = InternalUnstringify(aFileName) + ".def";
    LispDefFile def = aEnvironment.iDefFiles.File(aFileName);

    String hashedname = aEnvironment.HashTable().LookUp(flatfile);

    InputStatus oldstatus = aEnvironment.iInputStatus;
    aEnvironment.iInputStatus.SetTo(hashedname);

    {
      LispInput newInput = // new StdFileInput(hashedname, aEnvironment.iInputStatus);
          OpenInputFile(aEnvironment, aEnvironment.iInputDirectories, hashedname, aEnvironment.iInputStatus);
      LispError.Check(newInput != null, LispError.KLispErrFileNotFound);
      DoLoadDefFile(aEnvironment, newInput,def);
    }
    aEnvironment.iInputStatus.RestoreFrom(oldstatus);
  }






















  //////////////////////////////////////////////////
  ///// bits_to_digits and digits_to_bits implementation
  //////////////////////////////////////////////////

  // lookup table for transforming the number of digits

  static int log2_table_size = 32;
  // report the table size
  int log2_table_range()
  {
    return log2_table_size;
  }

  // A lookup table of Ln(n)/Ln(2) for n = 1 .. 32.
  // With this we don't have to use math.h if all we need is to convert the number of digits from one base to another. This is also faster.
  // Generated by: PrintList(N(Ln(1 .. 32)/Ln(2)), ",") at precision 40
  static double log2_table[] =
    {
    0.,
    1.,
    1.5849625007211561814537389439478165087598,
    2.,
    2.3219280948873623478703194294893901758648,
    2.5849625007211561814537389439478165087598,
    2.807354922057604107441969317231830808641,
    3.,
    3.1699250014423123629074778878956330175196,
    3.3219280948873623478703194294893901758648,
    3.4594316186372972561993630467257929587032,
    3.5849625007211561814537389439478165087598,
    3.7004397181410921603968126542566947336284,
    3.807354922057604107441969317231830808641,
    3.9068905956085185293240583734372066846246,
    4.,
    4.0874628412503394082540660108104043540112,
    4.1699250014423123629074778878956330175196,
    4.2479275134435854937935194229068344226935,
    4.3219280948873623478703194294893901758648,
    4.3923174227787602888957082611796473174008,
    4.4594316186372972561993630467257929587032,
    4.5235619560570128722941482441626688444988,
    4.5849625007211561814537389439478165087598,
    4.6438561897747246957406388589787803517296,
    4.7004397181410921603968126542566947336284,
    4.7548875021634685443612168318434495262794,
    4.807354922057604107441969317231830808641,
    4.8579809951275721207197733246279847624768,
    4.9068905956085185293240583734372066846246,
    4.9541963103868752088061235991755544235489,
    5.
    };

  // table look-up of small integer logarithms, for converting the number of digits to binary and back
  static double log2_table_lookup(int n) throws Exception
  {
      if (n<=log2_table_size && n>=2)
        return log2_table[n-1];
      else
      {
        throw new Yacasexception("log2_table_lookup: error: invalid argument "+n);
      }
  }
  // convert the number of digits in given base to the number of bits, and back.
  // need to round the number of digits.
  // to make sure that there is no hysteresis, we round upwards on digits_to_bits but round down on bits_to_digits
  static long digits_to_bits(long digits, int base) throws Exception
  {
    return (long)Math.ceil(((double)digits)*log2_table_lookup(base));
  }

  static long bits_to_digits(long bits, int base) throws Exception
  {
    return (long)Math.floor(((double)bits)/log2_table_lookup(base));
  }




}


