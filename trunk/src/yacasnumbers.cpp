

/* Implementation of the number classes (the functionality used
 * by yacas any way
 */

#include "yacasprivate.h"
#include "yacasbase.h"
#include "numbers.h"
#include "standard.h"
#include "anumber.h"
#include "platmath.h"
#include "lisperror.h"
#include "errors.h"

#ifdef HAVE_CONFIG_H
  #include <config.h>
#endif

#ifdef HAVE_STDIO_H
  #include <stdio.h> // Safe, only included if HAVE_STDIO_H defined
#endif

#define BITS_TO_DIGITS(x,n) (bits_to_digits(x,n))

static LispObject* FloatToString(ANumber& aInt, LispEnvironment& aEnvironment, LispInt aBase = 10);

LispInt NumericSupportForMantissa()
{
  return LispTrue;
}



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
  Check(i1->iNumber->iExp == 0, KLispErrNotInteger);
  Check(i2->iNumber->iExp == 0, KLispErrNotInteger);
  BigNumber* res = NEW BigNumber();
  BaseGcd(*res->iNumber,*i1->iNumber,*i2->iNumber);
  return NEW LispNumber(res);
}



static void Trigonometry(ANumber& x,ANumber& i,ANumber& sum,ANumber& term)
{
  while (x.iTensExp<0)
  {
    PlatDoubleWord carry=0;
    BaseDivideInt(x,10, WordBase, carry);
    x.iTensExp++;
  }

    ANumber x2(sum.iPrecision);
    Multiply(x2,x,x);
    ANumber one("1",sum.iPrecision);
    ANumber dummy(10);

    LispInt requiredDigits = WordDigits(sum.iPrecision, 10)+
        x2.Size()-x2.iExp+1;
//    printf("WordDigits=%d\n",requiredDigits);
//    printf("[%d,%d]:",x.Size()-x.iExp,x.iExp);

    // While (term>epsilon)
    while (1 /* Significant(term)*/)
    {
 
#ifdef CORRECT_DIVISION

/*
        LispInt significantDigits = WordDigits(term.iPrecision, 10);
        NormalizeFloat(term,significantDigits);
        if ((-term.iTensExp) > term.iPrecision+2)
        {
          break;
        }
*/
        if (!Significant(term)) break;
#else
        if (!Significant(term)) break;
#endif
        ANumber orig(sum.iPrecision);

        //   term <- term*x^2/((i+1)(i+2))
        //   i <= i+2

        // added this: truncate digits to speed up the calculation
        {
            LispInt toDunk = term.iExp - requiredDigits;
            if (toDunk > 0)
            {
                term.Delete(0,toDunk);
                term.iExp = requiredDigits;
            }
        }

        orig.CopyFrom(term);

        Multiply(term,orig,x2);
//
        BaseAdd(i, one, WordBase);
        orig.CopyFrom(term);
        Divide(term, dummy, orig, i);
//
        BaseAdd(i, one, WordBase);
        orig.CopyFrom(term);
        Divide(term, dummy, orig, i);

        //   negate term
        Negate(term);
        //   sum <- sum+term
        orig.CopyFrom(sum);
        Add(sum, orig, term);


    }

//    printf("[%d,%d]:",sum.Size()-sum.iExp,sum.iExp);
}

static void SinFloat(ANumber& aResult, ANumber& x)
{
  // Sin(x)=Sum(i=0 to Inf) (-1)^i x^(2i+1) /(2i+1)!
  // Which incrementally becomes the algorithm:
  //
  // i <- 1
  ANumber i("1",aResult.iPrecision);
  // sum <- x
  aResult.CopyFrom(x);
  // term <- x
  ANumber term(aResult.iPrecision);
  term.CopyFrom(x);
  Trigonometry(x,i,aResult,term);
}


static void CosFloat(ANumber& aResult, ANumber& x)
{
    // i <- 0
    ANumber i("0",aResult.iPrecision);
    // sum <- 1
    aResult.SetTo("1.0");
    // term <- 1
    ANumber term("1.0",aResult.iPrecision);
    Trigonometry(x,i,aResult,term);
}


LispObject* SinFloat(LispObject* int1, LispEnvironment& aEnvironment,LispInt aPrecision)
{
//PrintNumber("Sin input: %s\n",*int1->Number(aPrecision)->iNumber);
  ANumber sum(aPrecision);
  ANumber x(*int1->Number(aPrecision)->iNumber);  // woof
  x.ChangePrecision(aPrecision);
  SinFloat(sum, x);
  return FloatToString(sum, aEnvironment);
}


LispObject* CosFloat(LispObject* int1, LispEnvironment& aEnvironment,LispInt aPrecision)
{
  ANumber sum(aPrecision);
  ANumber x(*int1->Number(aPrecision)->iNumber);
  x.ChangePrecision(aPrecision);
  CosFloat(sum, x);
  return FloatToString(sum, aEnvironment);
}

LispObject* TanFloat(LispObject* int1, LispEnvironment& aEnvironment,LispInt aPrecision)
{
  // Tan(x) = Sin(x)/Cos(x)
  ANumber s(aPrecision);
  {
    ANumber x(*int1->Number(aPrecision)->iNumber);
    x.ChangePrecision(aPrecision);
    SinFloat(s, x);
//      SinFloat(s,int1->String()->c_str());
  }
  ANumber c(aPrecision);
  {
    ANumber x(*int1->Number(aPrecision)->iNumber);
    x.ChangePrecision(aPrecision);
    CosFloat(c, x);
//      CosFloat(c,int1->String()->c_str());
  }
  ANumber result(aPrecision);
  ANumber dummy(aPrecision);
  Divide(result,dummy,s,c);
  return FloatToString(result, aEnvironment);
}


LispObject* ArcSinFloat(LispObject* int1, LispEnvironment& aEnvironment,LispInt aPrecision)
{
//PrintNumber("ArcSin input: \n",*int1->Number(aPrecision)->iNumber);

  // Use Newton's method to solve sin(x) = y by iteration:
    // x := x - (Sin(x) - y) / Cos(x)
  // this is similar to PiFloat()
  // we are using PlatArcSin() as the initial guess
  // maybe, for y very close to 1 or to -1 convergence will
  // suffer but seems okay in some tests
//printf("%s(%d)\n",__FILE__,__LINE__);
//printf("input: %s\n",int1->String()->c_str());
//PrintNumber("digits ",*int1->Number(aPrecision)->iNumber);
  RefPtr<LispObject> iResult(PlatArcSin(aEnvironment, int1,  0));
  ANumber result(*iResult->Number(aPrecision)->iNumber);  // hack, hack, hack
  result.ChangePrecision(aPrecision);

  // how else do I get an ANumber from the result of PlatArcSin()?
  ANumber x(aPrecision);  // dummy variable
//  ANumber q("10", aPrecision);  // initial value must be "significant"
 
  ANumber q( aPrecision);  // initial value must be "significant"
  {
    ANumber x(aPrecision);
    ANumber s(aPrecision);
    x.CopyFrom(result);
    SinFloat(s,x);
    ANumber orig(aPrecision);
    orig.CopyFrom(*int1->Number(aPrecision)->iNumber);
    Negate(orig);
    Add(q,s,orig);
  }

  ANumber s(aPrecision);
  ANumber c(aPrecision);
 
  while (Significant(q))
  {
    x.CopyFrom(result);
    SinFloat(s, x);
    Negate(s);
    x.CopyFrom(s);
    ANumber y(*int1->Number(aPrecision)->iNumber);
//PrintNumber("y = ",y);
    Add(s, x, y);
    // now s = y - Sin(x)
    x.CopyFrom(result);
    CosFloat(c, x);
    Divide(q,x,s,c);
    // now q = (y - Sin(x)) / Cos(x)

    // Calculate result:=result+q;
    x.CopyFrom(result);
    Add(result,x,q);
  }
  return FloatToString(result, aEnvironment);
}

static void ExpFloat(ANumber& aResult, ANumber& x)
{
    // Exp(x)=Sum(i=0 to Inf)  x^(i) /(i)!
    // Which incrementally becomes the algorithm:
    //
    ANumber one("1",aResult.iPrecision);
    // i <- 0
    ANumber i("0",aResult.iPrecision);
    // sum <- 1
    aResult.SetTo("1");
    // term <- 1
    ANumber term("1",aResult.iPrecision);
    ANumber dummy(10);

    LispInt requiredDigits = WordDigits(aResult.iPrecision, 10)+
        x.Size()-x.iExp+1;

    // While (term>epsilon)
    while (Significant(term))
    {
        ANumber orig(aResult.iPrecision);

        {
            LispInt toDunk = term.iExp - requiredDigits;
            if (toDunk > 0)
            {
                term.Delete(0,toDunk);
                term.iExp = requiredDigits;
            }
        }

 
        //   i <- i+1
        BaseAdd(i, one, WordBase);

        //   term <- term*x/(i)
        orig.CopyFrom(term);
        Multiply(term,orig,x);
        orig.CopyFrom(term);
        Divide(term, dummy, orig, i);

        //   sum <- sum+term
        orig.CopyFrom(aResult);
        Add(aResult, orig, term);
    }
}

LispObject* ExpFloat(LispObject* int1, LispEnvironment& aEnvironment,LispInt aPrecision)
{
    ANumber sum(aPrecision);
    ANumber x(*int1->Number(aPrecision)->iNumber);
    ExpFloat(sum, x);
    return FloatToString(sum, aEnvironment);
}

LispObject* PowerFloat(LispObject* int1, LispObject* int2, LispEnvironment& aEnvironment,LispInt aPrecision)
{
    // If is integer
    if (int2->Number(aPrecision)->iNumber->iExp == 0)
    {
        // Raising to the power of an integer can be done fastest by squaring
        // and bitshifting: x^(a+b) = x^a*x^b . Then, regarding each bit
        // in y (seen as a binary number) as added, the algorithm becomes:
        //
        ANumber x(*int1->Number(aPrecision)->iNumber);
        ANumber y(*int2->Number(aPrecision)->iNumber);
        LispBoolean neg = y.iNegative;
        y.iNegative=LispFalse;
 
        // result <- 1
        ANumber result("1",aPrecision);
        // base <- x
        ANumber base(aPrecision);
        base.CopyFrom(x);

        ANumber copy(aPrecision);

        // while (y!=0)
        while (!IsZero(y))
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
    Check(LispFalse, KLispErrNotInteger);
  return 0;
}



LispObject* SqrtFloat(LispObject* int1, LispEnvironment& aEnvironment,LispInt aPrecision)
{
    ANumber i1(*int1->Number(aPrecision)->iNumber);
    ANumber res(aPrecision);
    i1.ChangePrecision(aPrecision);
    Sqrt(res,i1);
    return FloatToString(res, aEnvironment);
}






LispObject* ShiftLeft( LispObject* int1, LispObject* int2, LispEnvironment& aEnvironment,LispInt aPrecision)
{
  BigNumber *number = NEW BigNumber();
  LispInt bits = InternalAsciiToInt(int2->String());
  number->ShiftLeft(*int1->Number(aPrecision),bits);
  return NEW LispNumber(number);
}


LispObject* ShiftRight( LispObject* int1, LispObject* int2, LispEnvironment& aEnvironment,LispInt aPrecision)
{
  BigNumber *number = NEW BigNumber();
  LispInt bits = InternalAsciiToInt(int2->String());
  number->ShiftRight(*int1->Number(aPrecision),bits);
  return NEW LispNumber(number);
}

static void DivideInteger( ANumber& aQuotient, ANumber& aRemainder,
                        LispChar * int1, LispChar * int2,
                        LispInt aPrecision)
{
    ANumber a1(int1,aPrecision);
    ANumber a2(int2,aPrecision);
 
    Check(a1.iExp == 0, KLispErrNotInteger);
    Check(a2.iExp == 0, KLispErrNotInteger);
    Check(!IsZero(a2),KLispErrInvalidArg);
    IntegerDivide(aQuotient, aRemainder, a1, a2);
}

LispObject* ModFloat( LispObject* int1, LispObject* int2, LispEnvironment& aEnvironment,
                        LispInt aPrecision)
{
    ANumber quotient(static_cast<LispInt>(0));
    ANumber remainder(static_cast<LispInt>(0));
    DivideInteger( quotient, remainder, int1->String()->c_str(), int2->String()->c_str(), aPrecision);
    return FloatToString(remainder, aEnvironment,10);

}


static LispObject* FloatToString(ANumber& aInt,
                            LispEnvironment& aEnvironment, LispInt aBase)
{
    LispString result;
    ANumberToString(result, aInt, aBase);
    return LispAtom::New(aEnvironment, result.c_str());
}


LispObject* LispFactorial(LispObject* int1, LispEnvironment& aEnvironment,LispInt aPrecision)
{
    LispInt nr = InternalAsciiToInt(int1->String());
    Check(nr>=0,KLispErrInvalidArg);
    ANumber fac("1",aPrecision);
    LispInt i;
    for (i=2;i<=nr;i++)
    {
        BaseTimesInt(fac,i, WordBase);
    }
    return FloatToString(fac, aEnvironment);
}

/* This code will compute factorials faster when multiplication becomes better than quadratic time

// return old result*product of all integers from iLeft to iRight
void tree_factorial(ANumber& result, LispInt iLeft, LispInt iRight, LispInt aPrecision)
{
  if (iRight == iLeft) BaseTimesInt(result, iLeft, WordBase);
  else if (iRight == iLeft + 1) BaseTimesInt(result, iLeft*iRight, WordBase);
  else if (iRight == iLeft + 2) BaseTimesInt(result, iLeft*iRight*(iLeft+1), WordBase);
  else
  {
      ANumber fac1("1", aPrecision), fac2("1", aPrecision);
      LispInt i = (iLeft+iRight)>>1;
    tree_factorial(fac1, iLeft, i, aPrecision);
    tree_factorial(fac2, i+1, iRight, aPrecision);
    Multiply(result, fac1, fac2);
  }
}

LispString * LispFactorial(LispChar * int1, LispHashTable& aHashTable,LispInt aPrecision)
{
    LispInt nr = InternalAsciiToInt(int1);
    Check(nr>=0,KLispErrInvalidArg);
  ANumber fac("1",aPrecision);
  tree_factorial(fac, 1, nr, aPrecision);
    return FloatToString(fac, aEnvironment);
}

*/



// this will use the new BigNumber/BigInt/BigFloat scheme

BigNumber::BigNumber(const LispChar * aString,LispInt aBasePrecision,LispInt aBase)
 : iReferenceCount(),iPrecision(0),iType(KInt),iNumber(NULL)
{
  iNumber = NULL;
  SetTo(aString, aBasePrecision, aBase);
}
BigNumber::BigNumber(const BigNumber& aOther)
 : iReferenceCount(),iPrecision(aOther.GetPrecision()),iType(KInt),iNumber(NULL)
{
  iNumber = NEW ANumber(*aOther.iNumber);
  SetIsInteger(aOther.IsInt());
}
BigNumber::BigNumber(LispInt aPrecision)
 : iReferenceCount(),iPrecision(aPrecision),iType(KInt),iNumber(NULL)
{
  iNumber = NEW ANumber(BITS_TO_DIGITS(aPrecision,10));
  SetIsInteger(LispTrue);
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
    iNumber = NEW ANumber(*aOther.iNumber);
  }
  else
  {
    iNumber->CopyFrom(*aOther.iNumber);
  }
  SetIsInteger(aOther.IsInt());
}

/// Export a number to a string in given base to given base digits
// FIXME API breach: aPrecision is supposed to be in digits, not bits
void BigNumber::ToString(LispString& aResult, LispInt aBasePrecision, LispInt aBase) const
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

#define ENABLE_SCI_NOTATION
#ifdef ENABLE_SCI_NOTATION
  if (!IsInt())
  {
    for(;;)
    {
      LispInt i;
      LispBoolean greaterOne = LispFalse;
      if (num.iExp >= num.Size()) break;
      for (i=num.iExp;i<num.Size();i++)
      {
        if (num[i] != 0)
        {
          if (!(i==num.iExp && num[i]<10000 && num.iTensExp == 0))
          {
            greaterOne=LispTrue;
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
#endif

  ANumberToString(aResult, num, aBase,(iType == KFloat));
}
double BigNumber::Double() const
{
// There are platforms that don't have strtod
#ifdef HAVE_STRTOD
  LispString str;
  ANumber num(*iNumber);
//  num.CopyFrom(*iNumber);
  ANumberToString(str, num, 10);
  char* endptr;
  return strtod(str.c_str(),&endptr);
#else
  //FIXME
  LISPASSERT(0);
  return 0.0;
#endif
}










void BigNumber::Multiply(const BigNumber& aX, const BigNumber& aY, LispInt aPrecision)
{
  SetIsInteger(aX.IsInt() && aY.IsInt());



  if (aPrecision<aX.GetPrecision()) aPrecision=aX.GetPrecision();
  if (aPrecision<aY.GetPrecision()) aPrecision=aY.GetPrecision();

  iNumber->ChangePrecision(BITS_TO_DIGITS(aPrecision,10));


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
void BigNumber::MultiplyAdd(const BigNumber& aX, const BigNumber& aY, LispInt aPrecision)
{//FIXME
  BigNumber mult;
  mult.Multiply(aX,aY,aPrecision);
  Add(*this,mult,aPrecision);
}
void BigNumber::Add(const BigNumber& aX, const BigNumber& aY, LispInt aPrecision)
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
  ::Negate(*iNumber);
  SetIsInteger(aX.IsInt());
}


/*
LispInt DividePrecision(const BigNumber& aX, const BigNumber& aY, LispInt aPrecision)
{
  LispInt p=1;
  if (aY.Sign()==0)
  {  // zero division, report and do nothing
    RaiseError("BigNumber::Divide: zero division request ignored\n");
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
      p = MIN((LispInt)aPrecision, p);

      if (p<=0) p=1;

      if (p<=0)
        RaiseError("BigNumber::Divide: loss of precision with arguments %e (%d bits), %e (%d bits)", aX.Double(), aX.GetPrecision(), aY.Double(), aY.GetPrecision());
      return p;
    }
  }
  return p;
}
*/


void BigNumber::Divide(const BigNumber& aX, const BigNumber& aY, LispInt aPrecision)
{


/*
  iPrecision = DividePrecision(aX, aY, aPrecision);
  LispInt digitPrecision = BITS_TO_DIGITS(iPrecision,10);
  iNumber->iPrecision = digitPrecision;
*/

/* */
  if (aPrecision<aX.GetPrecision()) aPrecision=aX.GetPrecision();
  if (aPrecision<aY.GetPrecision()) aPrecision=aY.GetPrecision();

  LispInt digitPrecision = BITS_TO_DIGITS(aPrecision,10);
  iPrecision = aPrecision;
  iNumber->iPrecision = digitPrecision;
/* */

  ANumber a1(*aX.iNumber);
//  a1.CopyFrom(*aX.iNumber);
  ANumber a2(*aY.iNumber);
//  a2.CopyFrom(*aY.iNumber);
  ANumber remainder(digitPrecision);

  Check(!IsZero(a2),KLispErrInvalidArg);
  if (aX.IsInt() && aY.IsInt())
  {
    Check(a1.iExp == 0, KLispErrNotInteger);
    Check(a2.iExp == 0, KLispErrNotInteger);
    SetIsInteger(LispTrue);
    ::IntegerDivide(*iNumber, remainder, a1, a2);
  }
  else
  {
    SetIsInteger(LispFalse);
    ::Divide(*iNumber,remainder,a1,a2);
  }
}
void BigNumber::ShiftLeft(const BigNumber& aX, LispInt aNrToShift)
{
  if (aX.iNumber != iNumber)
  {
    iNumber->CopyFrom(*aX.iNumber);
  }
  ::BaseShiftLeft(*iNumber,aNrToShift);
}
void BigNumber::ShiftRight(const BigNumber& aX, LispInt aNrToShift)
{
  if (aX.iNumber != iNumber)
  {
    iNumber->CopyFrom(*aX.iNumber);
  }
  ::BaseShiftRight(*iNumber,aNrToShift);
}
void BigNumber::BitAnd(const BigNumber& aX, const BigNumber& aY)
{
  LispInt lenX=aX.iNumber->Size(), lenY=aY.iNumber->Size();
  LispInt min=lenX,max=lenY;
  if (min>max)
  {
    LispInt swap=min;
    min=max;
    max=swap;
  }
  iNumber->ResizeTo(min);
  LispInt i;
  for (i=0;i<min;i++)  (*iNumber)[i] = (*aX.iNumber)[i] & (*aY.iNumber)[i];
}
void BigNumber::BitOr(const BigNumber& aX, const BigNumber& aY)
{
  LispInt lenX=(*aX.iNumber).Size(), lenY=(*aY.iNumber).Size();
  LispInt min=lenX,max=lenY;
  if (min>max)
  {
    LispInt swap=min;
    min=max;
    max=swap;
  }

  iNumber->ResizeTo(max);

  LispInt i;
  for (i=0;i<min;i++)    (*iNumber)[i] = (*aX.iNumber)[i] | (*aY.iNumber)[i];
  for (;i<lenY;i++)      (*iNumber)[i] = (*aY.iNumber)[i];
  for (;i<lenX;i++)      (*iNumber)[i] = (*aX.iNumber)[i];
}
void BigNumber::BitXor(const BigNumber& aX, const BigNumber& aY)
{
  LispInt lenX=(*aX.iNumber).Size(), lenY=(*aY.iNumber).Size();
  LispInt min=lenX,max=lenY;
  if (min>max)
  {
    LispInt swap=min;
    min=max;
    max=swap;
  }
 
  iNumber->ResizeTo(max);

  LispInt i;
  for (i=0;i<min;i++)  (*iNumber)[i] = (*aX.iNumber)[i] ^ (*aY.iNumber)[i];
  for (;i<lenY;i++)    (*iNumber)[i] = (*aY.iNumber)[i];
  for (;i<lenX;i++)    (*iNumber)[i] = (*aX.iNumber)[i];
}

void BigNumber::BitNot(const BigNumber& aX)
{// FIXME?
  LispInt len=(*aX.iNumber).Size();
 
  iNumber->ResizeTo(len);

  LispInt i;
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
  for (i=0;i<iNumber->Size();i++)
  {
    if ((*iNumber)[i] != 0)
    {
      high = i;
    }
  }
  if (low > high)
    low = high;
  LispInt bits=(high-low)*sizeof(PlatWord)*8;
  return bits;
*/

  if (IsZero(*iNumber)) return 0;//-(1L<<30);
  ANumber num(*iNumber);
//  num.CopyFrom(*iNumber);

  if (num.iTensExp < 0)
  {
    LispInt digs = WordDigits(num.iPrecision, 10);
    PlatWord zero=0;
    while(num.iExp<digs)
    {
        num.Insert(0,zero);
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

  LispInt i,nr=num.Size();
  for (i=nr-1;i>=0;i--)
  {
    if (num[i] != 0) break;
  }
  LispInt bits=(i-num.iExp)*sizeof(PlatWord)*8;
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
LispInt BigNumber::Sign() const
{
  if (iNumber->iNegative) return -1;
  if (IsZero(*iNumber)) return 0;
  return 1;
}


void BigNumber::DumpDebugInfo()
{
#ifdef HAVE_STDIO_H
  if (!iNumber)
  {
    printf("No number representation\n");
  }
  else
  {
    PrintNumber("Number:",*iNumber);
  }
#else
#endif
}


/// integer operation: *this = y mod z
void BigNumber::Mod(const BigNumber& aY, const BigNumber& aZ)
{
    ANumber a1(*aY.iNumber);
    ANumber a2(*aZ.iNumber);
//    a1.CopyFrom(*aY.iNumber);
//    a2.CopyFrom(*aZ.iNumber);
    Check(a1.iExp == 0, KLispErrNotInteger);
    Check(a2.iExp == 0, KLispErrNotInteger);
    Check(!IsZero(a2),KLispErrInvalidArg);

    ANumber quotient(static_cast<LispInt>(0));
    ::IntegerDivide(quotient, *iNumber, a1, a2);

    if (iNumber->iNegative)
    {
      ANumber a3(*iNumber);
//      a3.CopyFrom(*iNumber);
      ::Add(*iNumber, a3, a2);
    }
    SetIsInteger(LispTrue);
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
    LispInt i=0;
    LispInt fraciszero=LispTrue;
    while (i<iNumber->iExp && fraciszero)
    {
        PlatWord digit = (*iNumber)[i];
        if (digit != 0)
            fraciszero=LispFalse;
        i++;
    }
    iNumber->Delete(0,iNumber->iExp);
    iNumber->iExp=0;

    if (iNumber->iNegative && !fraciszero)
    {
        ANumber orig(*iNumber);
//        orig.CopyFrom(*iNumber););
        ANumber minone("-1",10);
        ::Add(*iNumber,orig,minone);
    }
    SetIsInteger(LispTrue);
}


void BigNumber::Precision(LispInt aPrecision)
{//FIXME
  if (aPrecision<0) aPrecision=0;
  if (aPrecision < iPrecision)
  {
  }
  else
  {
    iNumber->ChangePrecision(BITS_TO_DIGITS(aPrecision,10));
  }
  SetIsInteger(iNumber->iExp == 0 && iNumber->iTensExp == 0);
  iPrecision = aPrecision;
}


//basic object manipulation
LispBoolean BigNumber::Equals(const BigNumber& aOther) const
{

  if (iNumber->iExp == aOther.iNumber->iExp)
  {
    iNumber->DropTrailZeroes();
    aOther.iNumber->DropTrailZeroes();
 
    if (IsZero(*iNumber))
        iNumber->iNegative = LispFalse;
    if (IsZero(*aOther.iNumber))
        aOther.iNumber->iNegative = LispFalse;
    if (iNumber->ExactlyEqual(*aOther.iNumber))
      return LispTrue;
    if (IsInt())
      return LispFalse;
    if (aOther.iNumber->iNegative != iNumber->iNegative)
      return LispFalse;
    }

  {
    //TODO optimize!!!!
    LispInt precision = GetPrecision();
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
    diff.Add(*this,otherNeg,BITS_TO_DIGITS(precision,10));

#ifdef CORRECT_DIVISION
    // if the numbers are float, make sure they are normalized
    if (diff.iNumber->iExp || diff.iNumber->iTensExp)
    {
      LispInt pr = diff.iNumber->iPrecision;
      if (pr<iPrecision)
        pr = iPrecision;
      if (pr<aOther.iPrecision)
        pr = aOther.iPrecision;
      NormalizeFloat(*diff.iNumber,WordDigits(pr, 10));
    }
#endif // CORRECT_DIVISION


    return !Significant(*diff.iNumber);
  }
}


LispBoolean BigNumber::IsInt() const
{
  return (iType == KInt);
}


LispBoolean BigNumber::IsIntValue() const
{
//FIXME I need to round first to get more reliable results.
  if (IsInt()) return LispTrue;
  iNumber->DropTrailZeroes();
  if (iNumber->iExp == 0 && iNumber->iTensExp == 0) return LispTrue;
  BigNumber num(iPrecision);
  num.Floor(*this);
  return Equals(num);

}


LispBoolean BigNumber::IsSmall() const
{
  if (IsInt())
  {
    PlatWord* ptr = &((*iNumber)[iNumber->Size()-1]);
    LispInt nr=iNumber->Size();
    while (nr>1 && *ptr == 0) {ptr--;nr--;}
    return (nr <= iNumber->iExp+1);
  }
  else
  // a function to test smallness of a float is not present in ANumber, need to code a workaround to determine whether a number fits into double.
  {
    LispInt tensExp = iNumber->iTensExp;
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
  SetIsInteger(LispTrue);
}

/// Transform integer to float, setting a given bit precision.
/// Note that aPrecision=0 means automatic setting (just enough digits to represent the integer).
void BigNumber::BecomeFloat(LispInt aPrecision)
{//FIXME: need to specify precision explicitly
  if (IsInt())
  {
    LispInt precision = aPrecision;
    if (iPrecision > aPrecision)
      precision = iPrecision;
    iNumber->ChangePrecision(BITS_TO_DIGITS(precision,10));  // is this OK or ChangePrecision means floating-point precision?
    SetIsInteger(LispFalse);
  }
}


LispBoolean BigNumber::LessThan(const BigNumber& aOther) const
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
#ifdef HAVE_STDIO_H
  char dummy[150];
  //FIXME platform code
#ifdef HAVE_VSNPRINTF
  snprintf(dummy,150,"%ld",aValue);
#else
  sprintf(dummy,"%ld",aValue);
#endif
  SetTo(dummy,iPrecision,10);
  SetIsInteger(LispTrue);
#else
  //FIXME
  LISPASSERT(0);
#endif
}


void BigNumber::SetTo(double aValue)
{
#ifdef HAVE_STDIO_H
  iPrecision = 53;  // standard double has 53 bits
  char dummy[150];
  //FIXME platform code
  char format[20];
#ifdef HAVE_VSNPRINTF
  snprintf(format,20,"%%.%dg",iPrecision);
  snprintf(dummy,150,format,aValue);
#else
  sprintf(format,"%%.%dg",iPrecision);
  sprintf(dummy,format,aValue);
#endif
  SetTo(dummy,iPrecision,BASE10);
  SetIsInteger(LispFalse);
//  if (iNumber->iExp > 1)
//    iNumber->RoundBits();
#else
  //FIXME
  LISPASSERT(0);
#endif
}

LispInt CalculatePrecision(const LispChar * aString,LispInt aBasePrecision,LispInt aBase, LispBoolean& aIsFloat)
{
  const LispChar * ptr = aString;
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
    LispInt digit1 = ptr-aString;
    // find the number of significant base digits (sig_digits)
    // trailing zeros and . *are* significant, do not include them in the sets
    LispInt sig_digits;// = strcspn(aString+digit1, (aBase<=10) ? "-eE@" : "-@");

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
    aIsFloat = LispTrue;
    return (LispInt) digits_to_bits(MAX(aBasePrecision,sig_digits), aBase);
  }
  else
  {
    aIsFloat = LispFalse;
    return 0;
  }
}

// assign from string at given precision (the API says in base digits)
// FIXME: API breach: aPrecision is passed in digits but used as if it were bits
void BigNumber::SetTo(const LispChar * aString,LispInt aBasePrecision,LispInt aBase)
{//FIXME -- what?
//  iPrecision = digits_to_bits(aBasePrecision,BASE10);
  LispBoolean isFloat = 0;
  LispInt digits = aBasePrecision;
  iPrecision = CalculatePrecision(aString,aBasePrecision,aBase, isFloat);

/*
  const LispChar * ptr = aString;
  while (*ptr && *ptr != '.') ptr++;
  if (*ptr == '.')
  {
    isFloat = 1;
  }
*/
  if (!iNumber)   iNumber = NEW ANumber(digits);
  iNumber->SetPrecision(digits);
  iNumber->SetTo(aString,aBase);
 
  SetIsInteger(!isFloat && iNumber->iExp == 0 && iNumber->iTensExp == 0);
}




