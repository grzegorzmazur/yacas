

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
  #include <stdio.h>
#endif

#define BITS_TO_DIGITS(x,n) (bits_to_digits(x,n))

static LispObject* FloatToString(ANumber& aInt, LispEnvironment& aEnvironment, LispInt aBase = 10);

LispInt NumericSupportForMantissa()
{
  return LispTrue;
}

const LispCharPtr NumericLibraryName()
{
    return "Internal";
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
        x2.NrItems()-x2.iExp+1;
//    printf("WordDigits=%d\n",requiredDigits);
//    printf("[%d,%d]:",x.NrItems()-x.iExp,x.iExp);

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

//    printf("[%d,%d]:",sum.NrItems()-sum.iExp,sum.iExp);
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
  ANumber x(*int1->Number(aPrecision)->iNumber);
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
//      SinFloat(s,int1->String()->String());
  }
  ANumber c(aPrecision);
  {
    ANumber x(*int1->Number(aPrecision)->iNumber);
    x.ChangePrecision(aPrecision);
    CosFloat(c, x);
//      CosFloat(c,int1->String()->String());
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
//printf("input: %s\n",int1->String()->String());
//PrintNumber("digits ",*int1->Number(aPrecision)->iNumber);
  RefPtr<LispObject> iResult(PlatArcSin(aEnvironment, int1,  0));
	ANumber result(*iResult->Number(aPrecision)->iNumber);	// hack, hack, hack
  result.ChangePrecision(aPrecision);

	// how else do I get an ANumber from the result of PlatArcSin()?
  ANumber x(aPrecision);	// dummy variable
//  ANumber q("10", aPrecision);	// initial value must be "significant"
  
  ANumber q( aPrecision);	// initial value must be "significant"
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

// ArcCosFloat should be defined in scripts through ArcSinFloat
LispObject* ArcCosFloat(LispObject* int1, LispEnvironment& aEnvironment,LispInt aPrecision)
{
    //TODO
    return PlatArcCos(aEnvironment, int1, 0);
}

LispObject* ArcTanFloat(LispObject* int1, LispEnvironment& aEnvironment,LispInt aPrecision)
{
    //TODO
    return PlatArcTan(aEnvironment, int1, 0);
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
        x.NrItems()-x.iExp+1;

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


static void LnFloat(ANumber& aResult, ANumber& int1)
{
    // Optimization for convergence: the following taylor
    // series converges faster when x is close to zero.
    // So a trick is to first take the square root a couple
    // of times, until x is sufficiently close to 1.
    

    // Ln(y) = Ln(1+x) = Sum(i=1 to inf) (-1)^(i+1) * x^(i) / i
    // thus y=1+x => x = y-1
    //


    LispInt shifts=0;
    LispBoolean smallenough=LispFalse;
    LispInt precision = 2*aResult.iPrecision;
    ANumber y(int1);

    if (!Significant(y)) RaiseError("MathLog does not handle zero");
    if (y.iNegative) RaiseError("MathLog does not handle negative numbers");

    ANumber minusone("-1",precision);
    ANumber x(precision);

    ANumber delta("0.01",precision);
    while (!smallenough)
    {
        ANumber tosquare(precision);
        tosquare.CopyFrom(y);
        Sqrt(y,tosquare);
        shifts++;
        Add(x,y,minusone);
        if (BaseLessThan(x,delta))
            smallenough=LispTrue;
    }
    // i <- 0
    ANumber i("0",precision);
    // sum <- 1
    aResult.SetTo("0");
    // term <- 1
    ANumber term("-1",precision);
    ANumber dummy(10);

    ANumber one("1",precision);
    // While (term>epsilon)
    while (Significant(term))      
    {
        //   negate term
        Negate(term);

        ANumber orig(precision);

        orig.CopyFrom(term);
        Multiply(term,orig,x);
        //
        BaseAdd(i, one, WordBase);
        orig.CopyFrom(term);
        ANumber newTerm(precision);
        Divide(newTerm, dummy, orig, i);

        //   sum <- sum+term
        orig.CopyFrom(aResult);
        Add(aResult, orig, newTerm);
    }
    if (shifts)
        BaseShiftLeft(aResult,shifts);
}



LispObject* LnFloat(LispObject* int1, LispEnvironment& aEnvironment,LispInt aPrecision)
{
    ANumber sum(aPrecision);
    ANumber x(*int1->Number(aPrecision)->iNumber);
    LnFloat(sum, x);
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

    ANumber lnn(aPrecision);
    {
      ANumber x(*int1->Number(aPrecision)->iNumber);
      LnFloat(lnn, x);
    }
    ANumber exn(*int2->Number(aPrecision)->iNumber);

    ANumber x(aPrecision);
    Multiply(x,exn,lnn);
    ANumber result(aPrecision);
    ExpFloat(result, x);
    return FloatToString(result, aEnvironment);
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
  LispInt bits = InternalAsciiToInt(int2->String()->String());
  number->ShiftLeft(*int1->Number(aPrecision),bits);
  return NEW LispNumber(number);
}


LispObject* ShiftRight( LispObject* int1, LispObject* int2, LispEnvironment& aEnvironment,LispInt aPrecision)
{
  BigNumber *number = NEW BigNumber();
  LispInt bits = InternalAsciiToInt(int2->String()->String());
  number->ShiftRight(*int1->Number(aPrecision),bits);
  return NEW LispNumber(number);
}

static void DivideInteger( ANumber& aQuotient, ANumber& aRemainder,
                        LispCharPtr int1, LispCharPtr int2, 
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
    DivideInteger( quotient, remainder, int1->String()->String(), int2->String()->String(), aPrecision);
    return FloatToString(remainder, aEnvironment,10);

}

LispObject* PiFloat( LispEnvironment& aEnvironment, LispInt aPrecision)
{
    // Newton's method for finding pi:
    // x[0] := 3.1415926
    // x[n+1] := x[n] + Sin(x[n])

	LispInt initial_prec = aPrecision;	// target precision of first iteration, will be computed below
    LispInt cur_prec = 40;  // precision of the initial guess
    ANumber result("3.141592653589793238462643383279502884197169399",cur_prec + 3);    // initial guess is stored with 3 guard digits
    ANumber x(cur_prec);
	ANumber s(cur_prec);

	// optimize precision sequence
	while (initial_prec > cur_prec*3)
		initial_prec = int((initial_prec+2)/3);
	cur_prec = initial_prec;
    while (cur_prec <= aPrecision)
    {
 		// start of iteration code
		result.ChangePrecision(cur_prec);	// result has precision cur_prec now
//    NormalizeFloat(result,cur_prec);
        // Get Sin(result)
        x.CopyFrom(result);
		s.ChangePrecision(cur_prec);
//    NormalizeFloat(s,cur_prec);
        SinFloat(s, x);
        // Calculate new result: result := result + Sin(result);
        x.CopyFrom(result);	// precision cur_prec
        Add(result,x,s);
		// end of iteration code
		// decide whether we are at end of loop now
		if (cur_prec == aPrecision)	// if we are exactly at full precision, it's the last iteration
			cur_prec = aPrecision+1;	// terminate loop
		else {
			cur_prec *= 3;	// precision triples at each iteration
			// need to guard against overshooting precision
 			if (cur_prec > aPrecision)
				cur_prec = aPrecision;	// next will be the last iteration
		}
    }
	
//    return aHashTable.LookUp("3.14"); // Just kidding, Serge ;-)

//NormalizeFloat(result,WordDigits(result.iPrecision,10));
#ifdef CORRECT_DIVISION
    NormalizeFloat(result,WordDigits(result.iPrecision,10));
#endif

    return FloatToString(result, aEnvironment);
}



static LispObject* FloatToString(ANumber& aInt,
                            LispEnvironment& aEnvironment, LispInt aBase)
{
    LispString result;
    ANumberToString(result, aInt, aBase);
    return LispAtom::New(aEnvironment, result.String());
}


LispObject* LispFactorial(LispObject* int1, LispEnvironment& aEnvironment,LispInt aPrecision)
{
    LispInt nr = InternalAsciiToInt(int1->String()->String());
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

LispStringPtr LispFactorial(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision)
{
    LispInt nr = InternalAsciiToInt(int1);
    Check(nr>=0,KLispErrInvalidArg);
	ANumber fac("1",aPrecision);
	tree_factorial(fac, 1, nr, aPrecision);
    return FloatToString(fac, aEnvironment);
}

*/



// this will use the new BigNumber/BigInt/BigFloat scheme
#ifndef USE_NEW_BIGNUM

#ifndef USE_NATIVE


BigNumber::BigNumber(const LispCharPtr aString,LispInt aBasePrecision,LispInt aBase)
{
  iNumber = NULL;
  SetTo(aString, aBasePrecision, aBase);
}
BigNumber::BigNumber(const BigNumber& aOther)
{
  iPrecision = aOther.GetPrecision();
  iNumber = NEW ANumber(*aOther.iNumber);
  SetIsInteger(aOther.IsInt());
}
BigNumber::BigNumber(LispInt aPrecision)
{
  iPrecision = aPrecision;
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
  if (iNumber == NULL) 
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
      if (num.iExp >= num.NrItems()) break;
      for (i=num.iExp;i<num.NrItems();i++)
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
  return strtod(str.String(),&endptr);
#else
  //FIXME
  LISPASSERT(0);
  return 0.0;
#endif
}

const LispCharPtr BigNumber::NumericLibraryName()
{
  return "Internal Yacas numbers";
}


void BigNumber::Multiply(const BigNumber& aX, const BigNumber& aY, LispInt aPrecision)
{
  SetIsInteger(aX.IsInt() && aY.IsInt());

  if (aPrecision<aX.GetPrecision()) aPrecision=aX.GetPrecision();
  if (aPrecision<aY.GetPrecision()) aPrecision=aY.GetPrecision();

  iNumber->ChangePrecision(BITS_TO_DIGITS(aPrecision,10));

  if (iNumber == aX.iNumber || iNumber == aY.iNumber)
  {
    ANumber a1(*aX.iNumber);
    ANumber a2(*aY.iNumber);
    :: Multiply(*iNumber,a1,a2);
  }
  else
  {
    :: Multiply(*iNumber,*aX.iNumber,*aY.iNumber);
  }
/*TODO remove old? 
  ANumber a1(*aX.iNumber);
  ANumber a2(*aY.iNumber);
  :: Multiply(*iNumber,a1,a2);
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

  if (aX.iNumber->iExp == aY.iNumber->iExp && aX.iNumber->iTensExp == aY.iNumber->iTensExp)
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
void BigNumber::Divide(const BigNumber& aX, const BigNumber& aY, LispInt aPrecision)
{

  if (aPrecision<aX.GetPrecision()) aPrecision=aX.GetPrecision();
  if (aPrecision<aY.GetPrecision()) aPrecision=aY.GetPrecision();

  ANumber a1(*aX.iNumber);
//  a1.CopyFrom(*aX.iNumber);
  ANumber a2(*aY.iNumber);
//  a2.CopyFrom(*aY.iNumber);
  ANumber remainder(BITS_TO_DIGITS(aPrecision,10));

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
  LispInt len1=aX.iNumber->NrItems(), len2=aY.iNumber->NrItems();
  LispInt min=len1,max=len2;
  if (min>max)
  {
    LispInt swap=min;
    min=max;
    max=swap;
  }
  iNumber->GrowTo(min);
  LispInt i;
  for (i=0;i<len1 && i<len2;i++)
  {
    (*iNumber)[i] = (*aX.iNumber)[i] & (*aY.iNumber)[i];
  }
}
void BigNumber::BitOr(const BigNumber& aX, const BigNumber& aY)
{
  LispInt len1=(*aX.iNumber).NrItems(), len2=(*aY.iNumber).NrItems();
  LispInt min=len1,max=len2;
  if (min>max)
  {
    LispInt swap=min;
    min=max;
    max=swap;
  }
  
  iNumber->GrowTo(max);

  LispInt i;
  for (i=0;i<len1 && i<len2;i++)
  {
    (*iNumber)[i] = (*aX.iNumber)[i] | (*aY.iNumber)[i];
  }
  for (i=len1;i<len2;i++)
  {
    (*iNumber)[i] = (*aY.iNumber)[i];
  }
  for (i=len2;i<len1;i++)
  {
    (*iNumber)[i] = (*aX.iNumber)[i];
  }
}
void BigNumber::BitXor(const BigNumber& aX, const BigNumber& aY)
{
  LispInt len1=(*aX.iNumber).NrItems(), len2=(*aY.iNumber).NrItems();
  LispInt min=len1,max=len2;
  if (min>max)
  {
    LispInt swap=min;
    min=max;
    max=swap;
  }
  
  iNumber->GrowTo(max);

  LispInt i;
  for (i=0;i<len1 && i<len2;i++)
  {
    (*iNumber)[i] = (*aX.iNumber)[i] ^ (*aY.iNumber)[i];
  }
  for (i=len1;i<len2;i++)
  {
    (*iNumber)[i] = (*aY.iNumber)[i];
  }
  for (i=len2;i<len1;i++)
  {
    (*iNumber)[i] = (*aX.iNumber)[i];
  }
}

void BigNumber::BitNot(const BigNumber& aX)
{// FIXME?
  LispInt len=(*aX.iNumber).NrItems();
  
  iNumber->GrowTo(len);

  LispInt i;
  for (i=0;i<len;i++)
  {
    (*iNumber)[i] = ~((*aX.iNumber)[i]);
  }
}


/// Bit count operation: return the number of significant bits if integer, return the binary exponent if float (shortcut for binary logarithm)
// give BitCount as platform integer
signed long BigNumber::BitCount() const
{
  if (IsZero(*iNumber)) return -(1L<<30);
  ANumber num(*iNumber);
//  num.CopyFrom(*iNumber);
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

  LispInt i,nr=num.NrItems();
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
  if (iNumber == NULL)
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
//TODO FIXME slow code! But correct
    LispString str;
    iNumber->CopyFrom(*aX.iNumber);
    if (iNumber->iExp>1)
      iNumber->RoundBits();

//    aX.ToString(str,aX.GetPrecision());
//    iNumber->SetTo(str.String());

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
    }

  {
    //TODO optimize!!!!
    BigNumber diff;
    BigNumber otherNeg;
    otherNeg.Negate(aOther);
    LispInt precision = GetPrecision();
    if (precision<aOther.GetPrecision()) precision = aOther.GetPrecision();
    diff.Add(*this,otherNeg,BITS_TO_DIGITS(precision,10));

#ifdef CORRECT_DIVISION
    // if the numbers are float, make sure they are normalized
    if (diff.iNumber->iExp || diff.iNumber->iTensExp) NormalizeFloat(*diff.iNumber,WordDigits(diff.iNumber->iPrecision, 10));
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
  if (iNumber->iExp == 0 && iNumber->iTensExp == 0) return LispTrue;
  
  BigNumber num(iPrecision);
  num.Floor(*this);
  return Equals(num);

}


LispBoolean BigNumber::IsSmall() const
{
  if (IsInt())
  {
    PlatWord* ptr = &((*iNumber)[iNumber->NrItems()-1]);
    LispInt nr=iNumber->NrItems();
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
      iPrecision <= 53	// standard float is 53 bits
      && tensExp<306	// 1021 bits is about 306 decimals
    );
    // standard range of double precision is about 53 bits of mantissa and binary exponent of about 1021
  }
}


void BigNumber::BecomeInt()
{
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
    iNumber->ChangePrecision(BITS_TO_DIGITS(precision,10));	// is this OK or ChangePrecision means floating-point precision?
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
  iPrecision = 53;	// standard double has 53 bits
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


// assign from string at given precision (the API says in base digits)
// FIXME: API breach: aPrecision is passed in digits but used as if it were bits
void BigNumber::SetTo(const LispCharPtr aString,LispInt aBasePrecision,LispInt aBase)
{//FIXME -- what?
  iPrecision = digits_to_bits(aBasePrecision,BASE10);
  LispInt digits = aBasePrecision;
  LispBoolean isFloat = 0;
  const LispCharPtr ptr = aString;
  while (*ptr && *ptr != '.') ptr++;
  if (*ptr == '.')
  {
    isFloat = 1;
  }
  if (iNumber == NULL)   iNumber = NEW ANumber(digits);
  iNumber->SetPrecision(digits);
  iNumber->SetTo(aString,aBase);
  
  SetIsInteger(!isFloat && iNumber->iExp == 0 && iNumber->iTensExp == 0);
}


#endif	// ifndef USE_NATIVE

#endif	// ifndef USE_NEW_BIGNUM

#ifdef USE_NEW_BIGNUM

/// Implementation of BigFloat and BigInt through ANumber.


//////////////////////////////////////////////////
///// Start of BigFloat implementation
//////////////////////////////////////////////////


/// assign a float from given string, using exactly aPrecision *bits*
BigFloat::BigFloat(const LispCharPtr aString,LispInt aPrecision,LispInt aBase)
{
}

/// copy constructor
BigFloat::BigFloat(const BigFloat& aOther)
{
}


// no constructors from int or double to avoid automatic conversions
BigFloat::BigFloat(LispInt aPrecision)
{
}


BigFloat::~BigFloat()
{
}

/// set precision to a given # of bits, maybe reallocate number
void BigFloat::Precision(LispInt aPrecision)
{
}

// assign from another float number
void BigFloat::SetTo(const BigFloat& aOther)
{
}


// assign from another integer number
void BigFloat::SetTo(const BigInt& aOther, LispInt aPrecision)
{
}


// assign from string, using exactly aPrecision *bits*
void BigFloat::SetTo(const LispCharPtr aString,LispInt aPrecision,LispInt aBase)
{
	Precision(aPrecision);
}


// assign from a platform type
void BigFloat::SetTo(double value)
{
	Precision(53);	// 53 bits in a double
}


/// GetMantissaExp: return a string representation of the mantissa in aResult
/// to given precision (base digits), and the exponent in the same base into aExponent
void BigFloat::GetMantissaExp(LispCharPtr aBuffer, unsigned long aBufferSize, long* aExponent, LispInt aPrecision, LispInt aBase) const
{
}


/// Give approximate representation as a double number
double BigFloat::Double() const
{
	return 0;
}


/// Numeric library name
const LispCharPtr BigFloat::NumericLibraryName()
{
	return BigInt::NumericLibraryName();
}


/// Compare exactly as float numbers, bit-for-bit, no rounding
LispBoolean BigFloat::Equals(const BigFloat& aOther) const
{
	return 0;
}


/// check that a float has exactly an integer value
LispBoolean BigFloat::IsIntValue() const
{
	return 0;
}


/// check that a float is less than aOther, exactly as floats, no rounding
LispBoolean BigFloat::LessThan(const BigFloat& aOther) const
{
	return 0;
}


/// Multiply two numbers and put result in *this, result should have at least aPrecision correct digits
void BigFloat::Multiply(const BigFloat& aX, const BigFloat& aY, LispInt aPrecision)
{
	Precision(aPrecision);
}


/** Multiply two numbers, and add to *this (this is useful and generally efficient to implement).
* This is most likely going to be used by internal functions only, using aResult as an accumulator.
*/
void BigFloat::MultiplyAdd(const BigFloat& aX, const BigFloat& aY, LispInt aPrecision)
{
}


/// Add two numbers at given precision and return result in *this
void BigFloat::Add(const BigFloat& aX, const BigFloat& aY, LispInt aPrecision)
{
	Precision(aPrecision);
}


/// Negate the given number, return result in *this
void BigFloat::Negate(const BigFloat& aX)
{
	Precision(aX.GetPrecision());
}


/// Divide two numbers and return result in *this. Note: if the two arguments are integer, it should return an integer result!
void BigFloat::Divide(const BigFloat& aX, const BigFloat& aY, LispInt aPrecision)
{
	Precision(aPrecision);
}


/// return the integer part of the number (still as float value)
void BigFloat::Floor(const BigFloat& aX)
{
	Precision(aX.GetPrecision());
}


/// Multiplication by a power of 2, return result in *this.
void BigFloat::Multiply2exp(const BigFloat& aX, LispInt aNrToShift)
{
	Precision(aX.GetPrecision());
}


/// return the binary exponent (shortcut for binary logarithm)
long BigFloat::GetBinaryExp() const
{
	return 0;
}


/// Give sign (-1, 0, 1)
LispInt BigFloat::Sign() const
{
	return 0;
}



  /// Import/export underlying objects.
void BigFloat::ImportData(const void* aData)
{// assuming that aData is a pointer to a valid, initialized number object
}

const void* BigFloat::ExportData() const
{// export a pointer to iFloat
	return (const void*)&iNumber;
}

//////////////////////////////////////////////////
///// End of BigFloat implementation
//////////////////////////////////////////////////


//////////////////////////////////////////////////
///// Start of BigInt implementation
//////////////////////////////////////////////////

void BigInt::init()
{
}

/// assign an int from given string
BigInt::BigInt(const LispCharPtr aString, LispInt aBase)
{
}

/// copy constructor
BigInt::BigInt(const BigInt& aOther)
{
}

// no constructors from int or double to avoid automatic conversions
BigInt::BigInt()
{
}

BigInt::~BigInt()
{
}

// assign from another number
void BigInt::SetTo(const BigFloat& aOther)
{
}

void BigInt::SetTo(const BigInt& aOther)
{
}

// assign from string
void BigInt::SetTo(const LispCharPtr aString, LispInt aBase)
{
}

// assign from a platform type
void BigInt::SetTo(long value)
{
}


/// ToString: return a string representation in the given aBuffer
void BigInt::ToString(LispCharPtr aBuffer, unsigned long aBufferSize, LispInt aBase) const
{
}


/// Give approximate representation as a double number
double BigInt::Double() const
{
	return 0;
}


/// Numeric library name
const LispCharPtr BigInt::NumericLibraryName()
{
	return "Yacas";
}

/// check if integers are equal
LispBoolean BigInt::Equals(const BigInt& aOther) const
{
	return 0;
}


/// check if the integer fits into a platform long
LispBoolean BigInt::IsSmall() const
{
	return 0;
}


/// check that an integer is smaller
LispBoolean BigInt::LessThan(const BigInt& aOther) const
{
	return 0;
}


/// Multiply two integers and put result in *this
void BigInt::Multiply(const BigInt& aX, const BigInt& aY)
{
}

/** Multiply two numbers, and add to *this (this is useful and generally efficient to implement).
* This is most likely going to be used by internal functions only, using aResult as an accumulator.
*/
void BigInt::MultiplyAdd(const BigInt& aX, const BigInt& aY)
{
}

/// Add two integers and return result in *this
void BigInt::Add(const BigInt& aX, const BigInt& aY)
{
}

/// Negate the given number, return result in *this
void BigInt::Negate(const BigInt& aX)
{
}

/// Divide two numbers and return result in *this. (This is the integer division!)
void BigInt::Div(const BigInt& aX, const BigInt& aY)
{
}


/// integer operation: *this = y mod z
void BigInt::Mod(const BigInt& aX, const BigInt& aY)
{
}

/// Bitwise operations, return result in *this.
void BigInt::ShiftLeft(const BigInt& aX, LispInt aNrToShift)
{
      ShiftLeft(aX, aNrToShift);
}

void BigInt::ShiftRight(const BigInt& aX, LispInt aNrToShift)
{
      ShiftRight(aX, aNrToShift);
}

void BigInt::BitAnd(const BigInt& aX, const BigInt& aY)
{
}

void BigInt::BitOr(const BigInt& aX, const BigInt& aY)
{
}

void BigInt::BitXor(const BigInt& aX, const BigInt& aY)
{
}

void BigInt::BitNot(const BigInt& aX)
{
}

/// Bit count operation: return the number of significant bits,
/// give bit count as a platform integer
long BigInt::BitCount() const
{
	return 0;
}

/// Give sign (-1, 0, 1)
LispInt BigInt::Sign() const
{
	return 0;
}


  /// Import/export underlying objects.
void BigInt::ImportData(const void* aData)
{// assuming that aData is a pointer to a valid, initialized number object
}

const void* BigInt::ExportData() const
{// export a pointer to iNumber
	return (const void*)&iNumber;
}


//////////////////////////////////////////////////
///// End of BigInt implementation
//////////////////////////////////////////////////

#endif

