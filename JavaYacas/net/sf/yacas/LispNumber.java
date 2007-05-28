package net.sf.yacas;


class LispNumber extends LispObject
{
    /// constructors:
    /// construct from another LispNumber
    public LispNumber(BigNumber aNumber,String aString)
  {
    iString = aString;
    iNumber = aNumber;
  }
    /// construct from a BigNumber; the string representation will be absent
    public LispNumber(BigNumber aNumber)
  {
    iString = null;
    iNumber =aNumber;
  }
  /// construct from a decimal string representation (also create a number object) and use aBasePrecision decimal digits
    public LispNumber(String aString, int aBasePrecision)
  {
    iString = aString;
    iNumber = null;  // purge whatever it was
    // create a new BigNumber object out of iString, set its precision in digits
//TODO FIXME enable this in the end    Number(aBasePrecision);
  }
  public LispObject Copy(boolean aRecursed)
  {
    return new LispNumber(iNumber, iString);
  }
    /// return a string representation in decimal with maximum decimal precision allowed by the inherent accuracy of the number
    public String String() throws Exception
     {
    if (iString == null)
    {
      LispError.LISPASSERT(iNumber != null);  // either the string is null or the number but not both
      iString = iNumber.ToString(0/*TODO FIXME*/,10);
    // export the current number to string and store it as LispNumber::iString
    }
    return iString;
  }
    /// give access to the BigNumber object; if necessary, will create a BigNumber object out of the stored string, at given precision (in decimal?)
    public BigNumber Number(int aPrecision) throws Exception
  {
    if (iNumber == null)
    {  // create and store a BigNumber out of string
      LispError.LISPASSERT(iString != null);
      String str;
      str = iString;
      // aBasePrecision is in digits, not in bits, ok
      iNumber = new BigNumber(str, aPrecision, 10/*TODO FIXME BASE10*/);
    }

    // check if the BigNumber object has enough precision, if not, extend it
    // (applies only to floats). Note that iNumber->GetPrecision() might be < 0

    else if (!iNumber.IsInt() && iNumber.GetPrecision() < aPrecision)
    {
      if (iString != null)
      {// have string representation, can extend precision
        iNumber.SetTo(iString,aPrecision, 10);
      }
      else
      {
      // do not have string representation, cannot extend precision!
      }
    }

    return iNumber;
  }
    /// annotate
    public LispObject SetExtraInfo(LispPtr aData)
  {
    /*TODO FIXME
    LispObject* result = NEW LispAnnotatedObject<LispNumber>(this);
    result->SetExtraInfo(aData);
    return result;
    */
    return null;
  }
    /// number object; NULL if not yet converted from string
    BigNumber iNumber;
    /// string representation in decimal; NULL if not yet converted from BigNumber
    String iString;
};
