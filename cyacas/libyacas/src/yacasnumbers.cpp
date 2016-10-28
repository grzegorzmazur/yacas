/* Implementation of the number classes (the functionality used
 * by yacas any way
 */

#include "yacas/numbers.h"
#include "yacas/standard.h"
#include "yacas/anumber.h"
#include "yacas/platmath.h"
#include "yacas/lisperror.h"
#include "yacas/errors.h"

#include <iomanip>
#include <iostream>
#include <sstream>
#include <algorithm>

static LispObject* FloatToString(ANumber& aInt, LispEnvironment& aEnvironment, int aBase = 10);

/* Converting between internal formats and ascii format.
 * It is best done as little as possible. Usually, during calculations,
 * the ascii version of a number will not be required, so only the
 * internal version needs to be stored.
 */


LispObject* GcdInteger(LispObject* int1, LispObject* int2,
                         LispEnvironment& aEnvironment)
{
  BigNumber* i1 = int1->Number(0);
  BigNumber* i2 = int2->Number(0);

  if (i1->iNumber->iExp != 0 || i2->iNumber->iExp != 0)
      throw LispErrNotInteger();

  BigNumber* res = new BigNumber();
  BaseGcd(*res->iNumber,*i1->iNumber,*i2->iNumber);
  return new LispNumber(res);
}

LispObject* PowerFloat(LispObject* int1, LispObject* int2, LispEnvironment& aEnvironment,int aPrecision)
{
    if (int2->Number(aPrecision)->iNumber->iExp != 0)
        throw LispErrNotInteger();

    // Raising to the power of an integer can be done fastest by squaring
    // and bitshifting: x^(a+b) = x^a*x^b . Then, regarding each bit
    // in y (seen as a binary number) as added, the algorithm becomes:
    //
    ANumber x(*int1->Number(aPrecision)->iNumber);
    ANumber y(*int2->Number(aPrecision)->iNumber);
    bool neg = y.iNegative;
    y.iNegative=false;

    // result <- 1
    ANumber result("1",aPrecision);
    // base <- x
    ANumber base(aPrecision);
    base.CopyFrom(x);

    ANumber copy(aPrecision);

    // while (y!=0)
    while (!y.IsZero())
    {
        // if (y&1 != 0)
        if ( (y[0] & 1) != 0)
        {
            // result <- result*base
            copy.CopyFrom(result);
            Multiply(result,copy,base);
        }
        // base <- base*base
        copy.CopyFrom(base);
        Multiply(base,copy,copy);
        // y <- y>>1
        BaseShiftRight(y,1);
    }

    if (neg)
    {
        ANumber one("1",aPrecision);
        ANumber dummy(10);
        copy.CopyFrom(result);
        Divide(result,dummy,one,copy);
    }

    // result
    return FloatToString(result, aEnvironment);
}



LispObject* SqrtFloat(LispObject* int1, LispEnvironment& aEnvironment,int aPrecision)
{
    ANumber i1(*int1->Number(aPrecision)->iNumber);
    ANumber res(aPrecision);
    i1.ChangePrecision(aPrecision);
    Sqrt(res,i1);
    return FloatToString(res, aEnvironment);
}


LispObject* ShiftLeft( LispObject* int1, LispObject* int2, LispEnvironment& aEnvironment,int aPrecision)
{
  BigNumber *number = new BigNumber();
  int bits = InternalAsciiToInt(*int2->String());
  number->ShiftLeft(*int1->Number(aPrecision),bits);
  return new LispNumber(number);
}


LispObject* ShiftRight( LispObject* int1, LispObject* int2, LispEnvironment& aEnvironment,int aPrecision)
{
  BigNumber *number = new BigNumber();
  int bits = InternalAsciiToInt(*int2->String());
  number->ShiftRight(*int1->Number(aPrecision),bits);
  return new LispNumber(number);
}

static void DivideInteger(ANumber& aQuotient, ANumber& aRemainder,
                          const char* int1, const char* int2,
                          int aPrecision)
{
    ANumber a1(int1,aPrecision);
    ANumber a2(int2,aPrecision);

    if (a1.iExp != 0 || a2.iExp != 0)
          throw LispErrNotInteger();

    if (a2.IsZero())
          throw LispErrInvalidArg();

    IntegerDivide(aQuotient, aRemainder, a1, a2);
}

LispObject* ModFloat( LispObject* int1, LispObject* int2, LispEnvironment& aEnvironment,
                        int aPrecision)
{
    ANumber quotient(static_cast<int>(0));
    ANumber remainder(static_cast<int>(0));
    DivideInteger( quotient, remainder, int1->String()->c_str(), int2->String()->c_str(), aPrecision);
    return FloatToString(remainder, aEnvironment,10);

}


static LispObject* FloatToString(ANumber& aInt,
                            LispEnvironment& aEnvironment, int aBase)
{
    LispString result;
    ANumberToString(result, aInt, aBase);
    return LispAtom::New(aEnvironment, result);
}


LispObject* LispFactorial(LispObject* int1, LispEnvironment& aEnvironment,int aPrecision)
{
    int nr = InternalAsciiToInt(*int1->String());

    if (nr < 0)
        throw LispErrInvalidArg();

    ANumber fac("1",aPrecision);
    int i;
    for (i=2;i<=nr;i++)
    {
        BaseTimesInt(fac,i, WordBase);
    }
    return FloatToString(fac, aEnvironment);
}

/* This code will compute factorials faster when multiplication becomes better than quadratic time

// return old result*product of all integers from iLeft to iRight
void tree_factorial(ANumber& result, int iLeft, int iRight, int aPrecision)
{
  if (iRight == iLeft) BaseTimesInt(result, iLeft, WordBase);
  else if (iRight == iLeft + 1) BaseTimesInt(result, iLeft*iRight, WordBase);
  else if (iRight == iLeft + 2) BaseTimesInt(result, iLeft*iRight*(iLeft+1), WordBase);
  else
  {
      ANumber fac1("1", aPrecision), fac2("1", aPrecision);
      int i = (iLeft+iRight)>>1;
    tree_factorial(fac1, iLeft, i, aPrecision);
    tree_factorial(fac2, i+1, iRight, aPrecision);
    Multiply(result, fac1, fac2);
  }
}

LispString * LispFactorial(LispChar * int1, LispHashTable& aHashTable,int aPrecision)
{
    int nr = InternalAsciiToInt(int1);

    if (nr < 0)
        throw LispErrInvalidArg();

  ANumber fac("1",aPrecision);
  tree_factorial(fac, 1, nr, aPrecision);
    return FloatToString(fac, aEnvironment);
}

*/



// this will use the new BigNumber/BigInt/BigFloat scheme

BigNumber::BigNumber(const char* aString,int aBasePrecision,int aBase)
 : iReferenceCount(),iPrecision(0),iType(KInt),iNumber(nullptr)
{
  iNumber = nullptr;
  SetTo(aString, aBasePrecision, aBase);
}
BigNumber::BigNumber(const BigNumber& aOther)
 : iReferenceCount(),iPrecision(aOther.GetPrecision()),iType(KInt),iNumber(nullptr)
{
  iNumber = new ANumber(*aOther.iNumber);
  SetIsInteger(aOther.IsInt());
}
BigNumber::BigNumber(int aPrecision)
 : iReferenceCount(),iPrecision(aPrecision),iType(KInt),iNumber(nullptr)
{
  iNumber = new ANumber(bits_to_digits(aPrecision,10));
  SetIsInteger(true);
}

BigNumber::~BigNumber()
{
  delete iNumber;
}

void BigNumber::SetTo(const BigNumber& aOther)
{
  iPrecision = aOther.GetPrecision();
  if (!iNumber)
  {
    iNumber = new ANumber(*aOther.iNumber);
  }
  else
  {
    iNumber->CopyFrom(*aOther.iNumber);
  }
  SetIsInteger(aOther.IsInt());
}

/// Export a number to a string in given base to given base digits
// FIXME API breach: aPrecision is supposed to be in digits, not bits
void BigNumber::ToString(LispString& aResult, int aBasePrecision, int aBase) const
{
  ANumber num(*iNumber);
//  num.CopyFrom(*iNumber);

  //TODO this round-off code is not correct yet, but will work in most cases
  // This is a bit of a messy way to round off numbers. It is probably incorrect,
  // even. When precision drops one digit, it rounds off the last ten digits.
  // So the following code is probably only correct if aPrecision>=num.iPrecision
  // or if aPrecision < num.iPrecision-10
  if (aBasePrecision<num.iPrecision)
  {
    if (num.iExp > 1)
      num.RoundBits();
  }
  num.ChangePrecision(aBasePrecision);

  if (!IsInt())
  {
    for(;;)
    {

      const int ns = num.size();
      bool greaterOne = false;
      if (num.iExp >= int(ns)) break;
      for (int i=num.iExp;i<ns;i++)
      {
        if (num[i] != 0)
        {
          if (!(i==num.iExp && num[i]<10000 && num.iTensExp == 0))
          {
            greaterOne=true;
            break;
          }
        }
      }
      if (!greaterOne) break;
      PlatDoubleWord carry=0;
      BaseDivideInt(num,10, WordBase, carry);
      num.iTensExp++;
    }
  }

  ANumberToString(aResult, num, aBase,(iType == KFloat));
}
double BigNumber::Double() const
{
    LispString str;
    ANumber num(*iNumber);
    ANumberToString(str, num, 10);
    std::istringstream is(str.c_str());
    double d;
    is >> d;
    return d;
}










void BigNumber::Multiply(const BigNumber& aX, const BigNumber& aY, int aPrecision)
{
  SetIsInteger(aX.IsInt() && aY.IsInt());



  if (aPrecision<aX.GetPrecision()) aPrecision=aX.GetPrecision();
  if (aPrecision<aY.GetPrecision()) aPrecision=aY.GetPrecision();

  iNumber->ChangePrecision(bits_to_digits(aPrecision,10));


//  if (iNumber == aX.iNumber || iNumber == aY.iNumber)
  {
    ANumber a1(*aX.iNumber);
    ANumber a2(*aY.iNumber);
    :: Multiply(*iNumber,a1,a2);
  }
/*TODO I don't like that multiply is destructive, but alas... x=pi;x*0.5 demonstrates this. FIXME
  else
  {
    :: Multiply(*iNumber,*aX.iNumber,*aY.iNumber);
  }
*/

}
void BigNumber::MultiplyAdd(const BigNumber& aX, const BigNumber& aY, int aPrecision)
{//FIXME
  BigNumber mult;
  mult.Multiply(aX,aY,aPrecision);
  Add(*this,mult,aPrecision);
}
void BigNumber::Add(const BigNumber& aX, const BigNumber& aY, int aPrecision)
{
  SetIsInteger(aX.IsInt() && aY.IsInt());

  if (aPrecision<aX.GetPrecision()) aPrecision=aX.GetPrecision();
  if (aPrecision<aY.GetPrecision()) aPrecision=aY.GetPrecision();


  if (iNumber != aX.iNumber && iNumber != aY.iNumber &&
      aX.iNumber->iExp == aY.iNumber->iExp && aX.iNumber->iTensExp == aY.iNumber->iTensExp)
  {
    ::Add(*iNumber, *aX.iNumber, *aY.iNumber);
  }
  else
  {
    ANumber a1(*aX.iNumber );
    ANumber a2(*aY.iNumber );
    ::Add(*iNumber, a1, a2);
  }
  iNumber->SetPrecision(aPrecision);
/* */
}
void BigNumber::Negate(const BigNumber& aX)
{
  if (aX.iNumber != iNumber)
  {
    iNumber->CopyFrom(*aX.iNumber);
  }
  iNumber->Negate();
  SetIsInteger(aX.IsInt());
}


/*
int DividePrecision(const BigNumber& aX, const BigNumber& aY, int aPrecision)
{
  int p=1;
  if (aY.Sign()==0)
  {  // zero division, report and do nothing
    throw LispErrGeneric("BigNumber::Divide: zero division request ignored");
    p=0;
  }
  if (aX.IsInt())
  {
    // check for zero
    if (aX.Sign()==0)
    {  // divide 0 by something, set result to integer 0
      p = 1;
    }
    else if (aY.IsInt())
    {
    }
    else
    {  // divide nonzero integer by nonzero float, precision is unmodified
      p = MIN(aPrecision, aY.GetPrecision());
    }
  }
  else  // aX is a float, aY is nonzero
  {
  // check for a floating zero
  if (aX.Sign()==0)
  {
    // result is 0. with precision m-B(y)+1
    p = aX.GetPrecision()-aY.BitCount()+1;
  }
    else if (aY.IsInt())
    {  // aY is integer, must be promoted to float
      p = (MIN(aPrecision, aX.GetPrecision()));
    }
    else
    {  // both aX and aY are nonzero floats
      p = MIN(aX.GetPrecision(), aY.GetPrecision()) - DIST(aX.GetPrecision(), aY.GetPrecision());
      p = MIN((int)aPrecision, p);

      if (p<=0) p=1;

      if (p<=0)
        RaiseError("BigNumber::Divide: loss of precision with arguments %e (%d bits), %e (%d bits)", aX.Double(), aX.GetPrecision(), aY.Double(), aY.GetPrecision());
      return p;
    }
  }
  return p;
}
*/


void BigNumber::Divide(const BigNumber& aX, const BigNumber& aY, int aPrecision)
{


/*
  iPrecision = DividePrecision(aX, aY, aPrecision);
  int digitPrecision = bits_to_digits(iPrecision,10);
  iNumber->iPrecision = digitPrecision;
*/

/* */
  if (aPrecision<aX.GetPrecision()) aPrecision=aX.GetPrecision();
  if (aPrecision<aY.GetPrecision()) aPrecision=aY.GetPrecision();

  int digitPrecision = bits_to_digits(aPrecision,10);
  iPrecision = aPrecision;
  iNumber->iPrecision = digitPrecision;
/* */

  ANumber a1(*aX.iNumber);
//  a1.CopyFrom(*aX.iNumber);
  ANumber a2(*aY.iNumber);
//  a2.CopyFrom(*aY.iNumber);
  ANumber remainder(digitPrecision);

  if (a2.IsZero())
      throw LispErrInvalidArg();

  if (aX.IsInt() && aY.IsInt())
  {
      if (a1.iExp != 0 || a2.iExp != 0)
          throw LispErrNotInteger();

    SetIsInteger(true);
    ::IntegerDivide(*iNumber, remainder, a1, a2);
  }
  else
  {
    SetIsInteger(false);
    ::Divide(*iNumber,remainder,a1,a2);
  }
}
void BigNumber::ShiftLeft(const BigNumber& aX, int aNrToShift)
{
  if (aX.iNumber != iNumber)
  {
    iNumber->CopyFrom(*aX.iNumber);
  }
  ::BaseShiftLeft(*iNumber,aNrToShift);
}
void BigNumber::ShiftRight(const BigNumber& aX, int aNrToShift)
{
  if (aX.iNumber != iNumber)
  {
    iNumber->CopyFrom(*aX.iNumber);
  }
  ::BaseShiftRight(*iNumber,aNrToShift);
}
void BigNumber::BitAnd(const BigNumber& aX, const BigNumber& aY)
{
  int lenX=aX.iNumber->size(), lenY=aY.iNumber->size();
  int min=lenX,max=lenY;
  if (min>max)
  {
    int swap=min;
    min=max;
    max=swap;
  }
  iNumber->resize(min);
  int i;
  for (i=0;i<min;i++)  (*iNumber)[i] = (*aX.iNumber)[i] & (*aY.iNumber)[i];
}
void BigNumber::BitOr(const BigNumber& aX, const BigNumber& aY)
{
  int lenX=(*aX.iNumber).size(), lenY=(*aY.iNumber).size();
  int min=lenX,max=lenY;
  if (min>max)
  {
    int swap=min;
    min=max;
    max=swap;
  }

  iNumber->resize(max);

  int i;
  for (i=0;i<min;i++)    (*iNumber)[i] = (*aX.iNumber)[i] | (*aY.iNumber)[i];
  for (;i<lenY;i++)      (*iNumber)[i] = (*aY.iNumber)[i];
  for (;i<lenX;i++)      (*iNumber)[i] = (*aX.iNumber)[i];
}
void BigNumber::BitXor(const BigNumber& aX, const BigNumber& aY)
{
  int lenX=(*aX.iNumber).size(), lenY=(*aY.iNumber).size();
  int min=lenX,max=lenY;
  if (min>max)
  {
    int swap=min;
    min=max;
    max=swap;
  }

  iNumber->resize(max);

  int i;
  for (i=0;i<min;i++)  (*iNumber)[i] = (*aX.iNumber)[i] ^ (*aY.iNumber)[i];
  for (;i<lenY;i++)    (*iNumber)[i] = (*aY.iNumber)[i];
  for (;i<lenX;i++)    (*iNumber)[i] = (*aX.iNumber)[i];
}

void BigNumber::BitNot(const BigNumber& aX)
{// FIXME?
  int len=(*aX.iNumber).size();

  iNumber->resize(len);

  int i;
  for (i=0;i<len;i++)  (*iNumber)[i] = ~((*aX.iNumber)[i]);
}


/// Bit count operation: return the number of significant bits if integer, return the binary exponent if float (shortcut for binary logarithm)
// give BitCount as platform integer
signed long BigNumber::BitCount() const
{
/*
  int low=0;
  int high = 0;
  int i;
  for (i=0;i<iNumber->size();i++)
  {
    if ((*iNumber)[i] != 0)
    {
      high = i;
    }
  }
  if (low > high)
    low = high;
  int bits=(high-low)*sizeof(PlatWord)*8;
  return bits;
*/

  if (iNumber->IsZero()) return 0;//-(1L<<30);
  ANumber num(*iNumber);
//  num.CopyFrom(*iNumber);

  if (num.iTensExp < 0)
  {
    int digs = WordDigits(num.iPrecision, 10);
    PlatWord zero=0;
    while(num.iExp<digs)
    {
        num.insert(num.begin(),zero);
        num.iExp++;
    }
  }
  while (num.iTensExp < 0)
  {
    PlatDoubleWord carry=0;
    BaseDivideInt(num,10, WordBase, carry);
    num.iTensExp++;
  }
  while (num.iTensExp > 0)
  {
    BaseTimesInt(num,10, WordBase);
    num.iTensExp--;
  }

  int i,nr=num.size();
  for (i=nr-1;i>=0;i--)
  {
    if (num[i] != 0) break;
  }
  int bits=(i-num.iExp)*sizeof(PlatWord)*8;
  if (i>=0)
  {
    PlatWord w=num[i];
    while (w)
    {
      w>>=1;
      bits++;
    }
  }
  return (bits);
}
int BigNumber::Sign() const
{
  if (iNumber->iNegative) return -1;
  if (iNumber->IsZero()) return 0;
  return 1;
}


void BigNumber::DumpDebugInfo(std::ostream& os) const
{
    if (!iNumber)
        os << "No number representation\n";
    else
        iNumber->Print(os, "Number:");
}


/// integer operation: *this = y mod z
void BigNumber::Mod(const BigNumber& aY, const BigNumber& aZ)
{
    ANumber a1(*aY.iNumber);
    ANumber a2(*aZ.iNumber);
//    a1.CopyFrom(*aY.iNumber);
//    a2.CopyFrom(*aZ.iNumber);

    if (a1.iExp != 0 || a2.iExp != 0)
          throw LispErrNotInteger();

    if (a2.IsZero())
          throw LispErrInvalidArg();

    ANumber quotient(static_cast<int>(0));
    ::IntegerDivide(quotient, *iNumber, a1, a2);

    if (iNumber->iNegative)
    {
      ANumber a3(*iNumber);
//      a3.CopyFrom(*iNumber);
      ::Add(*iNumber, a3, a2);
    }
    SetIsInteger(true);
}

void BigNumber::Floor(const BigNumber& aX)
{
    iNumber->CopyFrom(*aX.iNumber);
    //  If iExp is zero, then we can not look at the decimals and determine the floor.
    // This number has to have digits (see code later in this routine that does a division).
    // Not extending the digits caused the MathFloor function to fail on n*10-m where n was an
    // integer. The code below divides away the 10^-m, but since iExp was zero, this resulted in a
    // premature truncation (seen when n<0)
    if (iNumber->iExp == 0)
      iNumber->ChangePrecision(iNumber->iPrecision);

    if (iNumber->iExp>1)
      iNumber->RoundBits();

    //TODO FIXME slow code! But correct
    if (iNumber->iTensExp > 0)
    {
      while (iNumber->iTensExp > 0)
      {
        BaseTimesInt(*iNumber,10, WordBase);
        iNumber->iTensExp--;
      }
    }
    else if (iNumber->iTensExp < 0)
    {
      while (iNumber->iTensExp < 0)
      {
        PlatDoubleWord carry;
        BaseDivideInt(*iNumber,10, WordBase, carry);
        iNumber->iTensExp++;
      }
    }
    iNumber->ChangePrecision(iNumber->iPrecision);
    int i=0;
    int fraciszero=true;
    while (i<iNumber->iExp && fraciszero)
    {
        PlatWord digit = (*iNumber)[i];
        if (digit != 0)
            fraciszero=false;
        i++;
    }
    iNumber->erase(iNumber->begin(),iNumber->begin()+iNumber->iExp);
    iNumber->iExp=0;

    if (iNumber->iNegative && !fraciszero)
    {
        ANumber orig(*iNumber);
//        orig.CopyFrom(*iNumber););
        ANumber minone("-1",10);
        ::Add(*iNumber,orig,minone);
    }
    SetIsInteger(true);
}


void BigNumber::Precision(int aPrecision)
{//FIXME
  if (aPrecision<0) aPrecision=0;
  if (aPrecision < iPrecision)
  {
  }
  else
  {
    iNumber->ChangePrecision(bits_to_digits(aPrecision,10));
  }
  SetIsInteger(iNumber->iExp == 0 && iNumber->iTensExp == 0);
  iPrecision = aPrecision;
}


//basic object manipulation
bool BigNumber::Equals(const BigNumber& aOther) const
{

  if (iNumber->iExp == aOther.iNumber->iExp)
  {
    iNumber->DropTrailZeroes();
    aOther.iNumber->DropTrailZeroes();

    if (iNumber->IsZero())
        iNumber->iNegative = false;
    if (aOther.iNumber->IsZero())
        aOther.iNumber->iNegative = false;
    if (iNumber->ExactlyEqual(*aOther.iNumber))
      return true;
    if (IsInt())
      return false;
    if (aOther.iNumber->iNegative != iNumber->iNegative)
      return false;
    }

  {
    //TODO optimize!!!!
    int precision = GetPrecision();
    if (precision<aOther.GetPrecision()) precision = aOther.GetPrecision();
/*For tiny numbers like 1e-600, the following seemed necessary to compare it with zero.
    if (precision< (35*-iNumber->iTensExp)/10)
      precision =  (35*-iNumber->iTensExp)/10;
    if (precision< (35*-aOther.iNumber->iTensExp)/10)
      precision =  (35*-aOther.iNumber->iTensExp)/10;
*/
    BigNumber diff;
    BigNumber otherNeg;
    otherNeg.Negate(aOther);
    diff.Add(*this,otherNeg,bits_to_digits(precision,10));

    // if the numbers are float, make sure they are normalized
    if (diff.iNumber->iExp || diff.iNumber->iTensExp)
    {
      int pr = diff.iNumber->iPrecision;
      if (pr<iPrecision)
        pr = iPrecision;
      if (pr<aOther.iPrecision)
        pr = aOther.iPrecision;
      NormalizeFloat(*diff.iNumber,WordDigits(pr, 10));
    }

    return !Significant(*diff.iNumber);
  }
}


bool BigNumber::IsInt() const
{
  return (iType == KInt);
}


bool BigNumber::IsIntValue() const
{
//FIXME I need to round first to get more reliable results.
  if (IsInt()) return true;
  iNumber->DropTrailZeroes();
  if (iNumber->iExp == 0 && iNumber->iTensExp == 0) return true;
  BigNumber num(iPrecision);
  num.Floor(*this);
  return Equals(num);

}


bool BigNumber::IsSmall() const
{
  if (IsInt())
  {
    PlatWord* ptr = &((*iNumber)[iNumber->size()-1]);
    int nr=iNumber->size();
    while (nr>1 && *ptr == 0) {ptr--;nr--;}
    return (nr <= iNumber->iExp+1);
  }
  else
  // a function to test smallness of a float is not present in ANumber, need to code a workaround to determine whether a number fits into double.
  {
    int tensExp = iNumber->iTensExp;
    if (tensExp<0)tensExp = -tensExp;
    return
    (
      iNumber->iPrecision <= 53  // standard float is 53 bits
      && tensExp<1021 // 306  // 1021 bits is about 306 decimals
    );
    // standard range of double precision is about 53 bits of mantissa and binary exponent of about 1021
  }
}


void BigNumber::BecomeInt()
{
  while (iNumber->iTensExp > 0)
  {
    BaseTimesInt(*iNumber,10, WordBase);
    iNumber->iTensExp--;
  }
  while (iNumber->iTensExp < 0)
  {
    PlatDoubleWord carry=0;
    BaseDivideInt(*iNumber,10, WordBase, carry);
    iNumber->iTensExp++;
  }

  iNumber->ChangePrecision(0);
  SetIsInteger(true);
}

/// Transform integer to float, setting a given bit precision.
/// Note that aPrecision=0 means automatic setting (just enough digits to represent the integer).
void BigNumber::BecomeFloat(int aPrecision)
{//FIXME: need to specify precision explicitly
  if (IsInt())
  {
    int precision = aPrecision;
    if (iPrecision > aPrecision)
      precision = iPrecision;
    iNumber->ChangePrecision(bits_to_digits(precision,10));  // is this OK or ChangePrecision means floating-point precision?
    SetIsInteger(false);
  }
}


bool BigNumber::LessThan(const BigNumber& aOther) const
{
  ANumber a1(*this->iNumber);
//  a1.CopyFrom(*this->iNumber);
  ANumber a2(*aOther.iNumber);
//  a2.CopyFrom(*aOther.iNumber);
  return ::LessThan(a1, a2);
}




// assign from a platform type
void BigNumber::SetTo(long aValue)
{
    std::ostringstream buf;
    buf << aValue;
    SetTo(buf.str().c_str(), iPrecision, BASE10);
    SetIsInteger(true);
}


void BigNumber::SetTo(double aValue)
{
    std::ostringstream buf;
    buf << std::setprecision(53) << aValue;
    SetTo(buf.str().c_str(), iPrecision, BASE10);
    SetIsInteger(false);
}

int CalculatePrecision(const char* aString,int aBasePrecision,int aBase, bool& aIsFloat)
{
  const char* ptr = aString;
  while (*ptr)
  {
    switch (*ptr)
    {
      case '.': goto FOUND_FLOAT_INDICATOR;
      case 'e':
      case 'E':
      case '@':
        if (aBase<=10) goto FOUND_FLOAT_INDICATOR;
        break;
    }
    ptr++;
  }
FOUND_FLOAT_INDICATOR:
  // decide whether the string is an integer or a float
  if (*ptr)
  {
    // converting to a float
    // estimate the number of bits we need to have
    // find the first significant digit:
    // initial zeros are not significant
    ptr = aString;
    while (*ptr == '.' || *ptr == '-' || *ptr == '0') ptr++;
    int digit1 = ptr-aString;
    // find the number of significant base digits (sig_digits)
    // trailing zeros and . *are* significant, do not include them in the sets
    int sig_digits;// = strcspn(aString+digit1, (aBase<=10) ? "-eE@" : "-@");

      while (*ptr)
      {
        switch (*ptr)
        {
          case '@': case '-':
            goto FND_1;
          case 'e': case 'E':
            if (aBase<=10) goto FND_1;
        }
        ptr++;
      }
FND_1:
      sig_digits = ptr - (aString+digit1);



    if (sig_digits<=0)
    {  // this is when we have "0." in various forms
      // the number of digits is the number of trailing 0s after .

      // the string cannot consist of only 0 and -, it must contain at least one of ".eE@"
      // for example, -0000000.000e10 has 4 significant digits
      // counting . as one of the digits, so that "0" will have 1 digit
      ptr = aString;
      while (*ptr == '-' || *ptr == '0') ptr++;
      sig_digits = ptr-aString;

      while (*ptr)
      {
        switch (*ptr)
        {
          case 'e': case 'E': case '@':
            goto FND_2;
        }
        ptr++;
      }
FND_2:
      sig_digits = ptr - (aString+sig_digits);

//      sig_digits = strcspn(aString+sig_digits, "eE@");
    }
    else
    {  // our number is nonzero
      ptr = aString+digit1;
      while (*ptr && *ptr != '.') ptr++;
      if (*ptr == '.')
        -- sig_digits;  // this is when we have "1.000001" where "." is not a digit, so need to decrement
    }
    // ok, so we need to represent MAX(aPrecision,sig_digits) digits in base aBase
    aIsFloat = true;
    return (int) digits_to_bits(std::max(aBasePrecision,sig_digits), aBase);
  }
  else
  {
    aIsFloat = false;
    return 0;
  }
}

// assign from string at given precision (the API says in base digits)
// FIXME: API breach: aPrecision is passed in digits but used as if it were bits
void BigNumber::SetTo(const char* aString,int aBasePrecision,int aBase)
{//FIXME -- what?
//  iPrecision = digits_to_bits(aBasePrecision,BASE10);
  bool isFloat = 0;
  int digits = aBasePrecision;
  iPrecision = CalculatePrecision(aString,aBasePrecision,aBase, isFloat);

/*
  const LispChar * ptr = aString;
  while (*ptr && *ptr != '.') ptr++;
  if (*ptr == '.')
  {
    isFloat = 1;
  }
*/
  if (!iNumber)   iNumber = new ANumber(digits);
  iNumber->SetPrecision(digits);
  iNumber->SetTo(aString,aBase);

  SetIsInteger(!isFloat && iNumber->iExp == 0 && iNumber->iTensExp == 0);
}
