package net.sf.yacas;

import java.math.*;

class BigNumber
{
  public static boolean NumericSupportForMantissa()
  {
    return true;
  }


  //constructors
  public BigNumber( String aString,int aBasePrecision,int aBase/*=10*/)
  {
    SetTo(aString, aBasePrecision, aBase);
  }
  /// copy constructor
  public BigNumber( BigNumber aOther)
  {
    SetTo(aOther);
  }
  // no constructors from int or double to avoid automatic conversions
  public BigNumber(int aPrecision/* = 20*/)
  {
    iPrecision = aPrecision;
    iTensExp = 0;
    integer = new BigInteger("0");
  }
  // assign from another number
  public void SetTo( BigNumber aOther)
  {
    iPrecision = aOther.GetPrecision();
    iTensExp = aOther.iTensExp;
    integer  = aOther.integer;
    decimal  = aOther.decimal;
  }
  boolean IsFloat(String aString,int aBase)
  {
    if (aString.indexOf('.')>=0)
      return true;
    if (aBase>10)
      return false;
    if (aString.indexOf('e')>=0)
      return true;
    if (aString.indexOf('E')>=0)
      return true;
    return false;
  }
  // assign from string, precision in base digits
  public void SetTo( String aString,int aPrecision,int aBase/*=10*/)
  {
    integer = null;
    decimal = null;
    boolean isFloat = IsFloat(aString,aBase);

    iPrecision = aPrecision;
    iTensExp = 0;
    if (isFloat)
    {
      int decimalPos;
      decimalPos = aString.indexOf("e");
      if (decimalPos < 0)
        decimalPos = aString.indexOf("E");
      if (decimalPos > 0) // will never be zero
      {
        iTensExp = Integer.parseInt(aString.substring(decimalPos+1,aString.length()));
        aString = aString.substring(0,decimalPos);
      }

      decimal = new BigDecimal(aString); //TODO FIXME does not listen to aBase!!!
      if (decimal.scale() > iPrecision)
        iPrecision = decimal.scale();
    }
    else
    {
      integer = new BigInteger(aString, aBase);
    }
  }
  // assign from a platform type
  public void SetTo(long value)
  {
    SetTo(""+value,iPrecision,10);
  }
  public void SetTo(int value)
  {
    SetTo((long)value);
  }
  public void SetTo(double value)
  {
    SetTo(""+value,iPrecision,10);
  }
  // Convert back to other types
  /// ToString : return string representation of number in aResult to given precision (base digits)
  public String ToString(int aPrecision, int aBase/*=10*/)
  {
    if (integer != null)
      return integer.toString(aBase);
    else
    {
      String result = decimal.toString();

      int extraExp = 0;
      // Parse out the exponent
      {
        int pos = result.indexOf("E");
        if (pos<0) pos = result.indexOf("e");
        if (pos > 0)
        {
          extraExp = Integer.parseInt(result.substring(pos+1));
          result = result.substring(0,pos);
        }
      }


      int dotPos = result.indexOf('.');
      if (dotPos >= 0)
      {
        int endpos = result.length();
        while (endpos>dotPos && result.charAt(endpos-1) == '0') endpos--;
        if (endpos > 1)
        {
          if (result.charAt(endpos-1) == '.' && result.charAt(endpos-2) >= '0' && result.charAt(endpos-2) <= '9')
          {
            endpos--;
          }
        }
        result = result.substring(0,endpos);
        if ((iTensExp+extraExp) != 0)
        {
          result = result + "e"+(iTensExp+extraExp);
        }
      }
      return result;
    }
  }
  /// Give approximate representation as a double number
  public double Double()
  {
    if (integer != null)
      return integer.doubleValue();
    else
      return decimal.doubleValue();
  }
  public long Long()
  {
    if (integer != null)
      return integer.longValue();
    else
      return decimal.longValue();
  }

  //basic object manipulation
  public boolean Equals( BigNumber aOther)
  {
    if (integer != null)
    {
      if (aOther.integer == null)
      {
        //hier
        BigDecimal x = GetDecimal(this);
        if (x.compareTo(aOther.decimal) == 0)
          return true;
        return false;
      }
      return (integer.compareTo(aOther.integer) == 0);
    }
    if (decimal != null)
    {
      BigDecimal thisd = decimal;
      BigDecimal otherd = aOther.decimal;
      if (otherd == null)
      {
        otherd = GetDecimal(aOther);
      }
      if (iTensExp > aOther.iTensExp)
      {
        thisd = thisd.movePointRight(iTensExp - aOther.iTensExp);
      }
      else if (iTensExp < aOther.iTensExp)
      {
        otherd = otherd.movePointRight(iTensExp - aOther.iTensExp);
      }
      return (thisd.compareTo(otherd) == 0);
    }
    return true;
  }
  public boolean IsInt()
  {
    return (integer != null && decimal == null);
  }
  public boolean IsSmall()
  {
    if (IsInt())
    {
      BigInteger i = integer.abs();
      return (i.compareTo(new BigInteger("65535"))<0);
    }
    else
    // a function to test smallness of a float is not present in ANumber, need to code a workaround to determine whether a number fits into double.
    {
      //TODO fixme
      return true;
/*
      LispInt tensExp = iNumber->iTensExp;
      if (tensExp<0)tensExp = -tensExp;
      return
      (
        iNumber->iPrecision <= 53  // standard float is 53 bits
        && tensExp<1021 // 306  // 1021 bits is about 306 decimals
      );
      // standard range of double precision is about 53 bits of mantissa and binary exponent of about 1021
*/
    }
  }
  public void BecomeInt()
  {
    if (decimal != null)
    {
      integer = decimal.toBigInteger();
      decimal = null;
    }
  }
  public void BecomeFloat(int aPrecision/*=0*/)
  {
    if (integer != null)
    {
      decimal = new BigDecimal(integer);
      iTensExp = 0;
      integer = null;
    }
  }
  public boolean LessThan( BigNumber aOther)
  {
    boolean floatResult = (decimal != null || aOther.decimal != null);
    if (floatResult)
    {
      BigDecimal dX = GetDecimal(this);
      BigDecimal dY = GetDecimal(aOther);
      return dX.compareTo(dY)<0;
    }
    else
    {
      return integer.compareTo(aOther.integer)<0;
    }
  }
  //arithmetic
  /// Multiply two numbers at given precision and put result in *this
  public void Multiply( BigNumber aX,  BigNumber aY, int aPrecision)
  {
    boolean floatResult = (aX.decimal != null || aY.decimal != null);
    if (floatResult)
    {
      BigDecimal dX = GetDecimal(aX);
      BigDecimal dY = GetDecimal(aY);
      integer = null;
      decimal = dX.multiply(dY);
      int newScale = iPrecision;
      if (newScale < decimal.scale())
        decimal = decimal.setScale(newScale,BigDecimal.ROUND_HALF_EVEN);
      iTensExp = aX.iTensExp + aY.iTensExp;
    }
    else
    {
      decimal = null;
      integer = aX.integer.multiply(aY.integer);
    }
  }
  /// Add two numbers at given precision and return result in *this
  public void Add( BigNumber aX,  BigNumber aY, int aPrecision)
  {
    boolean floatResult = (aX.decimal != null || aY.decimal != null);
    if (floatResult)
    {
      BigDecimal dX = GetDecimal(aX);
      BigDecimal dY = GetDecimal(aY);

      integer = null;
      if (aX.iTensExp > aY.iTensExp)
      {
        dY = dY.movePointLeft(aX.iTensExp-aY.iTensExp);
        iTensExp = aX.iTensExp;
      }
      else if (aX.iTensExp < aY.iTensExp)
      {
        dX = dX.movePointLeft(aY.iTensExp-aX.iTensExp);
        iTensExp = aY.iTensExp;
      }
      decimal = dX.add(dY);
    }
    else
    {
      decimal = null;
      integer = aX.integer.add(aY.integer);
    }
  }
  /// Negate the given number, return result in *this
  public void Negate( BigNumber aX)
  {
    if (aX.integer != null)
    {
      decimal = null;
      integer = aX.integer.negate();
    }
    if (aX.decimal != null)
    {
      integer = null;
      decimal = aX.decimal.negate();
      iTensExp = aX.iTensExp;
    }
  }
  /// Divide two numbers and return result in *this. Note: if the two arguments are integer, it should return an integer result!
  public void Divide( BigNumber aX,  BigNumber aY, int aPrecision)
  {
    boolean floatResult = (aX.decimal != null || aY.decimal != null);
    if (floatResult)
    {
      BigDecimal dX = GetDecimal(aX);
      BigDecimal dY = GetDecimal(aY);
      integer = null;
      int newScale = aPrecision+aY.GetPrecision();
      if (newScale > dX.scale())
        dX = dX.setScale(newScale);
      decimal = dX.divide(dY,BigDecimal.ROUND_HALF_EVEN);
      iPrecision = decimal.scale();
      iTensExp = aX.iTensExp-aY.iTensExp;
    }
    else
    {
      decimal = null;
      integer = aX.integer.divide(aY.integer);
    }
  }

  /// integer operation: *this = y mod z
  public void Mod( BigNumber aY,  BigNumber aZ) throws Exception
  {
    LispError.Check(aY.integer != null, LispError.KLispErrNotInteger);
    LispError.Check(aZ.integer != null, LispError.KLispErrNotInteger);
//TODO fixme    LispError.Check(!IsZero(aZ),LispError.KLispErrInvalidArg);
    integer = aY.integer.mod(aZ.integer);
    decimal = null;
  }

  /// For debugging purposes, dump internal state of this object into a string
  public void DumpDebugInfo(LispOutput aOutput) throws Exception
  {
    if (integer != null)
    {
      aOutput.Write("integer: "+integer.toString()+"\n");
    }
    else
    {
      aOutput.Write("decimal: "+decimal.unscaledValue()+" scale "+decimal.scale()+" x 10^("+iTensExp+")\n");
    }
  }

  /// assign self to Floor(aX) if possible
  public void Floor( BigNumber aX)
  {
    if (aX.decimal != null)
    {
      BigDecimal d = aX.decimal;
      if (aX.iTensExp != 0)
      {
        d = d.movePointRight(aX.iTensExp);
      }
      BigInteger rounded = d.toBigInteger();
      if (aX.decimal.signum()<0)
      {
        BigDecimal back =  new BigDecimal(rounded);
        BigDecimal difference = aX.decimal.subtract(back);
        if (difference.signum() != 0)
        {
          rounded = rounded.add(new BigInteger("-1"));
        }
      }
      integer = rounded;
    }
    else
    {
      integer = aX.integer;
    }
    decimal = null;
  }
  /// set precision (in bits)
  public void Precision(int aPrecision)
  {
    iPrecision = aPrecision;
    if (decimal != null)
    {
      if (decimal.scale() > aPrecision)
      {
        decimal = decimal.setScale(aPrecision, BigDecimal.ROUND_HALF_EVEN);
      }
    }
  }

  /// Bitwise operations, return result in *this.
  void ShiftLeft(  BigNumber aX, int aNrToShift) throws Exception
  {
    LispError.LISPASSERT(aX.integer != null);
    decimal = null;
    integer = aX.integer.shiftLeft(aNrToShift);
  }
  void ShiftRight(  BigNumber aX, int aNrToShift) throws Exception
  {
    LispError.LISPASSERT(aX.integer != null);
    decimal = null;
    integer = aX.integer.shiftRight(aNrToShift);
  }
 
  void Gcd( BigNumber aX,  BigNumber aY) throws Exception
  {
    LispError.LISPASSERT(aX.integer != null);
    LispError.LISPASSERT(aY.integer != null);
    integer = aX.integer.gcd(aY.integer);
    decimal = null;
  }
  void BitAnd( BigNumber aX,  BigNumber aY) throws Exception
  {
    LispError.LISPASSERT(aX.integer != null);
    LispError.LISPASSERT(aY.integer != null);
    integer = aX.integer.and(aY.integer);
    decimal = null;
  }
  void BitOr( BigNumber aX,  BigNumber aY) throws Exception
  {
    LispError.LISPASSERT(aX.integer != null);
    LispError.LISPASSERT(aY.integer != null);
    integer = aX.integer.or(aY.integer);
    decimal = null;
  }
  void BitXor( BigNumber aX,  BigNumber aY) throws Exception
  {
    LispError.LISPASSERT(aX.integer != null);
    LispError.LISPASSERT(aY.integer != null);
    integer = aX.integer.xor(aY.integer);
    decimal = null;
  }
  void BitNot( BigNumber aX) throws Exception
  {
    LispError.LISPASSERT(aX.integer != null);
    integer = aX.integer.not();
    decimal = null;
  }
  /// Bit count operation: return the number of significant bits if integer, return the binary exponent if float (shortcut for binary logarithm)
  /// give bit count as a platform integer
  private static BigDecimal zero = new BigDecimal("0");
  private static BigDecimal one = new BigDecimal("1");
  private static BigDecimal two = new BigDecimal("2");
  private static BigDecimal ten = new BigDecimal("10");
  public long BitCount()
  {
    //TODO fixme check that it works as needed
    if (integer != null)
      return integer.abs().bitLength();
    {
      BigDecimal d = decimal.abs();
      if (iTensExp != 0)
        d = d.movePointRight(iTensExp);
      if (d.compareTo(one)>0)
        return d.toBigInteger().bitLength();
      BigDecimal integerPart = new BigDecimal(d.toBigInteger());
      integerPart = integerPart.negate();
      d = d.add(integerPart);
      if (d.compareTo(zero) == 0)
        return 0;
      int bitCount = 0;
 
      //TODO OPTIMIZE
      d = d.multiply(two);
      while (d.compareTo(one)<0)
      {
        d = d.multiply(two);
        bitCount--;
      }
      return bitCount;
    }
  }
 
  /// Give sign (-1, 0, 1)
  public int Sign()
  {
    if (integer != null)
      return integer.signum();
    if (decimal != null)
      return decimal.signum();

    return 0;
  }

  public int GetPrecision()
  {
    return iPrecision;
  }

  int iPrecision;
  int iTensExp;

  BigDecimal GetDecimal(BigNumber aNumber)
  {
    if (aNumber.decimal != null)
      return aNumber.decimal;
    return new BigDecimal(aNumber.integer);
  }

   /// Internal library wrapper starts here.
  BigInteger integer = null;
  BigDecimal decimal= null;
  /// Internal library wrapper ends here.
}
