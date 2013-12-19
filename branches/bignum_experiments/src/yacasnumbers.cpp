#include <iostream>

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

#include <sstream>

static LispObject* FloatToString(ANumber& aInt, LispEnvironment& aEnvironment, LispInt aBase = 10);

LispInt NumericSupportForMantissa()
{
  return true;
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
    Check(i1->IsInt(), KLispErrNotInteger);
    Check(i2->IsInt(), KLispErrNotInteger);
    BigNumber* res = NEW BigNumber();

    mpz_gcd(res->_integer, i1->_integer, i2->_integer);

    return NEW LispNumber(res);
}



// static void Trigonometry(ANumber& x,ANumber& i,ANumber& sum,ANumber& term)
// {
//   while (x.iTensExp<0)
//   {
//     PlatDoubleWord carry=0;
//     BaseDivideInt(x,10, WordBase, carry);
//     x.iTensExp++;
//   }

//     ANumber x2(sum.iPrecision);
//     Multiply(x2,x,x);
//     ANumber one("1",sum.iPrecision);
//     ANumber dummy(10);

//     LispInt requiredDigits = WordDigits(sum.iPrecision, 10)+
//         x2.Size()-x2.iExp+1;
// //    printf("WordDigits=%d\n",requiredDigits);
// //    printf("[%d,%d]:",x.Size()-x.iExp,x.iExp);

//     // While (term>epsilon)
//     while (1 /* Significant(term)*/)
//     {
 
// #ifdef CORRECT_DIVISION

// /*
//         LispInt significantDigits = WordDigits(term.iPrecision, 10);
//         NormalizeFloat(term,significantDigits);
//         if ((-term.iTensExp) > term.iPrecision+2)
//         {
//           break;
//         }
// */
//         if (!Significant(term)) break;
// #else
//         if (!Significant(term)) break;
// #endif
//         ANumber orig(sum.iPrecision);

//         //   term <- term*x^2/((i+1)(i+2))
//         //   i <= i+2

//         // added this: truncate digits to speed up the calculation
//         {
//             LispInt toDunk = term.iExp - requiredDigits;
//             if (toDunk > 0)
//             {
//                 term.Delete(0,toDunk);
//                 term.iExp = requiredDigits;
//             }
//         }

//         orig.CopyFrom(term);

//         Multiply(term,orig,x2);
// //
//         BaseAdd(i, one, WordBase);
//         orig.CopyFrom(term);
//         Divide(term, dummy, orig, i);
// //
//         BaseAdd(i, one, WordBase);
//         orig.CopyFrom(term);
//         Divide(term, dummy, orig, i);

//         //   negate term
//         Negate(term);
//         //   sum <- sum+term
//         orig.CopyFrom(sum);
//         Add(sum, orig, term);


//     }

// //    printf("[%d,%d]:",sum.Size()-sum.iExp,sum.iExp);
// }

LispObject* SinFloat(LispObject* int1, LispEnvironment& aEnvironment,LispInt aPrecision)
{
    // //PrintNumber("Sin input: %s\n",*int1->Number(aPrecision)->iNumber);
    // std::cout << "sin" << std::endl;
    // ANumber sum(aPrecision);
    // ANumber x(*int1->Number(aPrecision)->iNumber);  // woof
    // x.ChangePrecision(aPrecision);
    // SinFloat(sum, x);
    // return FloatToString(sum, aEnvironment);
    std::cout << "sin" << std::endl;
    abort();
}


LispObject* CosFloat(LispObject* int1, LispEnvironment& aEnvironment,LispInt aPrecision)
{
  // ANumber sum(aPrecision);
  // ANumber x(*int1->Number(aPrecision)->iNumber);
  // x.ChangePrecision(aPrecision);
  // CosFloat(sum, x);
  // return FloatToString(sum, aEnvironment);
    std::cout << "cos" << std::endl;
    abort();
}

LispObject* TanFloat(LispObject* int1, LispEnvironment& aEnvironment,LispInt aPrecision)
{
//   // Tan(x) = Sin(x)/Cos(x)
//   ANumber s(aPrecision);
//   {
//     ANumber x(*int1->Number(aPrecision)->iNumber);
//     x.ChangePrecision(aPrecision);
//     SinFloat(s, x);
// //      SinFloat(s,int1->String()->c_str());
//   }
//   ANumber c(aPrecision);
//   {
//     ANumber x(*int1->Number(aPrecision)->iNumber);
//     x.ChangePrecision(aPrecision);
//     CosFloat(c, x);
// //      CosFloat(c,int1->String()->c_str());
//   }
//   ANumber result(aPrecision);
//   ANumber dummy(aPrecision);
//   Divide(result,dummy,s,c);
//   return FloatToString(result, aEnvironment);
    std::cout << "tan" << std::endl;
    abort();
}


LispObject* ArcSinFloat(LispObject* int1, LispEnvironment& aEnvironment,LispInt aPrecision)
{
// //PrintNumber("ArcSin input: \n",*int1->Number(aPrecision)->iNumber);

//   // Use Newton's method to solve sin(x) = y by iteration:
//     // x := x - (Sin(x) - y) / Cos(x)
//   // this is similar to PiFloat()
//   // we are using PlatArcSin() as the initial guess
//   // maybe, for y very close to 1 or to -1 convergence will
//   // suffer but seems okay in some tests
// //printf("%s(%d)\n",__FILE__,__LINE__);
// //printf("input: %s\n",int1->String()->c_str());
// //PrintNumber("digits ",*int1->Number(aPrecision)->iNumber);
//   RefPtr<LispObject> iResult(PlatArcSin(aEnvironment, int1,  0));
//   ANumber result(*iResult->Number(aPrecision)->iNumber);  // hack, hack, hack
//   result.ChangePrecision(aPrecision);

//   // how else do I get an ANumber from the result of PlatArcSin()?
//   ANumber x(aPrecision);  // dummy variable
// //  ANumber q("10", aPrecision);  // initial value must be "significant"
 
//   ANumber q( aPrecision);  // initial value must be "significant"
//   {
//     ANumber x(aPrecision);
//     ANumber s(aPrecision);
//     x.CopyFrom(result);
//     SinFloat(s,x);
//     ANumber orig(aPrecision);
//     orig.CopyFrom(*int1->Number(aPrecision)->iNumber);
//     Negate(orig);
//     Add(q,s,orig);
//   }

//   ANumber s(aPrecision);
//   ANumber c(aPrecision);
 
//   while (Significant(q))
//   {
//     x.CopyFrom(result);
//     SinFloat(s, x);
//     Negate(s);
//     x.CopyFrom(s);
//     ANumber y(*int1->Number(aPrecision)->iNumber);
// //PrintNumber("y = ",y);
//     Add(s, x, y);
//     // now s = y - Sin(x)
//     x.CopyFrom(result);
//     CosFloat(c, x);
//     Divide(q,x,s,c);
//     // now q = (y - Sin(x)) / Cos(x)

//     // Calculate result:=result+q;
//     x.CopyFrom(result);
//     Add(result,x,q);
//   }
//   return FloatToString(result, aEnvironment);
    std::cout << "asin" << std::endl;
    abort();
}

// static void ExpFloat(ANumber& aResult, ANumber& x)
// {
//     // // Exp(x)=Sum(i=0 to Inf)  x^(i) /(i)!
//     // // Which incrementally becomes the algorithm:
//     // //
//     // ANumber one("1",aResult.iPrecision);
//     // // i <- 0
//     // ANumber i("0",aResult.iPrecision);
//     // // sum <- 1
//     // aResult.SetTo("1");
//     // // term <- 1
//     // ANumber term("1",aResult.iPrecision);
//     // ANumber dummy(10);

//     // LispInt requiredDigits = WordDigits(aResult.iPrecision, 10)+
//     //     x.Size()-x.iExp+1;

//     // // While (term>epsilon)
//     // while (Significant(term))
//     // {
//     //     ANumber orig(aResult.iPrecision);

//     //     {
//     //         LispInt toDunk = term.iExp - requiredDigits;
//     //         if (toDunk > 0)
//     //         {
//     //             term.Delete(0,toDunk);
//     //             term.iExp = requiredDigits;
//     //         }
//     //     }

 
//     //     //   i <- i+1
//     //     BaseAdd(i, one, WordBase);

//     //     //   term <- term*x/(i)
//     //     orig.CopyFrom(term);
//     //     Multiply(term,orig,x);
//     //     orig.CopyFrom(term);
//     //     Divide(term, dummy, orig, i);

//     //     //   sum <- sum+term
//     //     orig.CopyFrom(aResult);
//     //     Add(aResult, orig, term);
//     // }
//     std::cout << "exp" << std::endl;
//     abort();
// }

LispObject* ExpFloat(LispObject* int1, LispEnvironment& aEnvironment,LispInt aPrecision)
{
    // ANumber sum(aPrecision);
    // ANumber x(*int1->Number(aPrecision)->iNumber);
    // ExpFloat(sum, x);
    // return FloatToString(sum, aEnvironment);
    std::cout << "exp" << std::endl;
    abort();
}

LispObject* PowerFloat(LispObject* int1, LispObject* int2, LispEnvironment& aEnvironment,LispInt aPrecision)
{
  //   // If is integer
  //   if (int2->Number(aPrecision)->iNumber->iExp == 0)
  //   {
  //       // Raising to the power of an integer can be done fastest by squaring
  //       // and bitshifting: x^(a+b) = x^a*x^b . Then, regarding each bit
  //       // in y (seen as a binary number) as added, the algorithm becomes:
  //       //
  //       ANumber x(*int1->Number(aPrecision)->iNumber);
  //       ANumber y(*int2->Number(aPrecision)->iNumber);
  //       bool neg = y.iNegative;
  //       y.iNegative=false;
 
  //       // result <- 1
  //       ANumber result("1",aPrecision);
  //       // base <- x
  //       ANumber base(aPrecision);
  //       base.CopyFrom(x);

  //       ANumber copy(aPrecision);

  //       // while (y!=0)
  //       while (!IsZero(y))
  //       {
  //           // if (y&1 != 0)
  //           if ( (y[0] & 1) != 0)
  //           {
  //               // result <- result*base
  //               copy.CopyFrom(result);
  //               Multiply(result,copy,base);
  //           }
  //           // base <- base*base
  //           copy.CopyFrom(base);
  //           Multiply(base,copy,copy);
  //           // y <- y>>1
  //           BaseShiftRight(y,1);
  //       }

  //       if (neg)
  //       {
  //           ANumber one("1",aPrecision);
  //           ANumber dummy(10);
  //           copy.CopyFrom(result);
  //           Divide(result,dummy,one,copy);
  //       }
 
  //       // result
  //       return FloatToString(result, aEnvironment);
  //   }
  //   Check(false, KLispErrNotInteger);
  // return 0;
    std::cout << "pow" << std::endl;
    abort();
}



LispObject* SqrtFloat(LispObject* int1, LispEnvironment& aEnvironment,LispInt aPrecision)
{
    // ANumber i1(*int1->Number(aPrecision)->iNumber);
    // ANumber res(aPrecision);
    // i1.ChangePrecision(aPrecision);
    // Sqrt(res,i1);
    // return FloatToString(res, aEnvironment);
    std::cout << "sqrt" << std::endl;
    abort();
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

static void DivideInteger(ANumber& aQuotient, ANumber& aRemainder,
                          const LispChar* int1, const LispChar* int2,
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
    : iReferenceCount(),iPrecision(0),iType(KInt)
{
    SetTo(aString, aBasePrecision, aBase);
}
BigNumber::BigNumber(const BigNumber& aOther)
 : iReferenceCount(),iPrecision(aOther.GetPrecision()),iType(KInt)
{
    SetIsInteger(aOther.IsInt());

    if (aOther.IsInt())
        mpz_init_set(_integer, aOther._integer);
    else {
        mpfr_init2(_float, iPrecision);
        mpfr_set(_float, aOther._float, MPFR_RNDN);
    }
}
BigNumber::BigNumber(LispInt aPrecision)
    : iReferenceCount(),iPrecision(aPrecision),iType(KInt)
{
    SetIsInteger(true);

    mpz_init(_integer);
}

BigNumber::~BigNumber()
{
    if (IsInt())
        mpz_clear(_integer);
    else
        mpfr_clear(_float);
}

void BigNumber::SetTo(const BigNumber& aOther)
{
  // iPrecision = aOther.GetPrecision();
  // if (!iNumber)
  // {
  //   iNumber = NEW ANumber(*aOther.iNumber);
  // }
  // else
  // {
  //   iNumber->CopyFrom(*aOther.iNumber);
  // }
  // SetIsInteger(aOther.IsInt());

  // if (aOther._integer) {
  //     if (_float) {
  //         mpfr_clear(_float);
  //         mpz_init(_integer);

  //         mpz_set(_integer, aOther._integer);
  //     } else {
  //         mpz_set(_integer, aOther._integer);
  //     }
  // } else {
  //     if (_float) {
  //         mpfr_set(_float, aOther._float, MPFR_RNDN);
  //     } else {
  //         mpz_clear(_integer);
  //         mpfr_init(_float);

  //         mpfr_set(_float, aOther._float, MPFR_RNDN);
  //     }
  // }
    iPrecision = aOther.GetPrecision();

    if (aOther.IsInt()) {
        BecomeInt();
        mpz_set(_integer, aOther._integer);
    } else {
        BecomeFloat();
        mpfr_set(_float, aOther._float, MPFR_RNDN);
    }
}

/// Export a number to a string in given base to given base digits
// FIXME API breach: aPrecision is supposed to be in digits, not bits
void BigNumber::ToString(LispString& aResult, LispInt aBasePrecision, LispInt aBase) const
{
//   ANumber num(*iNumber);
// //  num.CopyFrom(*iNumber);
 
//   //TODO this round-off code is not correct yet, but will work in most cases
//   // This is a bit of a messy way to round off numbers. It is probably incorrect,
//   // even. When precision drops one digit, it rounds off the last ten digits.
//   // So the following code is probably only correct if aPrecision>=num.iPrecision
//   // or if aPrecision < num.iPrecision-10
//   if (aBasePrecision<num.iPrecision)
//   {
//     if (num.iExp > 1)
//       num.RoundBits();
//   }
//   num.ChangePrecision(aBasePrecision);

//   if (!IsInt())
//   {
//     for(;;)
//     {
//       LispInt i;
//       bool greaterOne = false;
//       if (num.iExp >= num.Size()) break;
//       for (i=num.iExp;i<num.Size();i++)
//       {
//         if (num[i] != 0)
//         {
//           if (!(i==num.iExp && num[i]<10000 && num.iTensExp == 0))
//           {
//             greaterOne=true;
//             break;
//           }
//         }
//       }
//       if (!greaterOne) break;
//       PlatDoubleWord carry=0;
//       BaseDivideInt(num,10, WordBase, carry);
//       num.iTensExp++;
//     }
//   }

//   ANumberToString(aResult, num, aBase,(iType == KFloat));

    char* p = 0;

    if (IsInt()) {
        //gmp_asprintf(&p, "%Zd", _integer);
        p = mpz_get_str(0, aBase, _integer);
    } else {
        std::ostringstream os;
        os << "%." << bits_to_digits(iPrecision, 10) + 10 << "Rg";
        mpfr_asprintf(&p, os.str().c_str(), _float);
    }

    aResult = p;

    free(p);
}

double BigNumber::Double() const
{
    if (IsInt())
        return mpz_get_d(_integer);
    else
        return mpfr_get_d(_float, MPFR_RNDN);
}

void BigNumber::Multiply(const BigNumber& aX, const BigNumber& aY, LispInt aPrecision)
{
    const bool integer_result = aX.IsInt() && aY.IsInt();

    if (aPrecision<aX.GetPrecision())
        aPrecision=aX.GetPrecision();
    if (aPrecision<aY.GetPrecision())
        aPrecision=aY.GetPrecision();

    if (integer_result)
        BecomeInt();
    else
        BecomeFloat();

    if (IsInt())
        mpz_mul(_integer, aX._integer, aY._integer);
    else
        if (aX.IsInt())
            mpfr_mul_z(_float, aY._float, aX._integer, MPFR_RNDN);
        else if (aY.IsInt())
            mpfr_mul_z(_float, aX._float, aY._integer, MPFR_RNDN);
        else
            mpfr_mul(_float, aX._float, aY._float, MPFR_RNDN);
}

void BigNumber::MultiplyAdd(const BigNumber& aX, const BigNumber& aY, LispInt aPrecision)
{//FIXME
  BigNumber mult;
  mult.Multiply(aX,aY,aPrecision);
  Add(*this,mult,aPrecision);
}

void BigNumber::Add(const BigNumber& aX, const BigNumber& aY, LispInt aPrecision)
{
    const bool integer_result = aX.IsInt() && aY.IsInt();

    if (aPrecision < aX.GetPrecision())
        aPrecision = aX.GetPrecision();

    if (aPrecision < aY.GetPrecision())
        aPrecision = aY.GetPrecision();

    if (integer_result)
        BecomeInt();
    else
        BecomeFloat();

    if (IsInt())
        mpz_add(_integer, aX._integer, aY._integer);
    else {
        if (aX.IsInt())
            mpfr_add_z(_float, aY._float, aX._integer, MPFR_RNDN);
        else if (aY.IsInt())
            mpfr_add_z(_float, aX._float, aY._integer, MPFR_RNDN);
        else
            mpfr_add(_float, aX._float, aY._float, MPFR_RNDN);
    }
}


void BigNumber::Negate(const BigNumber& aX)
{
    if (this != &aX) {
        iPrecision = aX.iPrecision;

        if (aX.IsInt()) {
            BecomeInt();
            mpz_neg(_integer, aX._integer);
        } else {
            BecomeFloat();
            mpfr_neg(_float, aX._float, MPFR_RNDN);
        }
    } else {
        if (IsInt()) {
            mpz_t z;
            mpz_init_set(z, _integer);
            mpz_neg(_integer, z);
            mpz_clear(z);
        } else {
            mpfr_t r;
            mpfr_init2(r, iPrecision);
            mpfr_set(r, _float, MPFR_RNDN);
            mpfr_neg(_float, r, MPFR_RNDN);
            mpfr_clear(r);
        }
    }
}




void BigNumber::Divide(const BigNumber& aX, const BigNumber& aY, LispInt aPrecision)
{
    const bool integer_result = aX.IsInt() && aY.IsInt();

    if (aPrecision<aX.GetPrecision())
        aPrecision=aX.GetPrecision();

    if (aPrecision<aY.GetPrecision())
        aPrecision=aY.GetPrecision();

    if (integer_result)
        BecomeInt();
    else
        BecomeFloat();

    if (IsInt()) {
        //Check(!IsZero(aY._integer),KLispErrInvalidArg);
        mpz_div(_integer, aX._integer, aY._integer);
    } else {
        if (aX.IsInt()) {
            //Check(!IsZero(aY._float),KLispErrInvalidArg);
            mpfr_t x;
            mpfr_init2(x, aPrecision);
            mpfr_set_z(x, aX._integer, MPFR_RNDN);

            mpfr_div(_float, x, aY._float, MPFR_RNDN);;

            mpfr_clear(x);

        } else if (aY.IsInt()) {
            //Check(!IsZero(aY._integer),KLispErrInvalidArg);
            mpfr_div_z(_float, aX._float, aY._integer, MPFR_RNDN);
        } else {
            //Check(!IsZero(aY._float),KLispErrInvalidArg);
            mpfr_div(_float, aX._float, aY._float, MPFR_RNDN);
        }
    }
}


void BigNumber::ShiftLeft(const BigNumber& aX, LispInt aNrToShift)
{
    BecomeInt();

    mpz_mul_2exp(_integer, aX._integer, aNrToShift);
}

void BigNumber::ShiftRight(const BigNumber& aX, LispInt aNrToShift)
{
  // if (aX.iNumber != iNumber)
  // {
  //   iNumber->CopyFrom(*aX.iNumber);
  // }
  // ::BaseShiftRight(*iNumber,aNrToShift);

    BecomeInt();

    if (mpz_sgn(aX._integer) >= 0)
        mpz_tdiv_q_2exp(_integer, aX._integer, aNrToShift);
    else
        mpz_fdiv_q_2exp(_integer, aX._integer, aNrToShift);
}

void BigNumber::BitAnd(const BigNumber& aX, const BigNumber& aY)
{
  // LispInt lenX=aX.iNumber->Size(), lenY=aY.iNumber->Size();
  // LispInt min=lenX,max=lenY;
  // if (min>max)
  // {
  //   LispInt swap=min;
  //   min=max;
  //   max=swap;
  // }
  // iNumber->ResizeTo(min);
  // LispInt i;
  // for (i=0;i<min;i++)  (*iNumber)[i] = (*aX.iNumber)[i] & (*aY.iNumber)[i];
//    std::cerr << "BigNumber::BitAnd" << std::endl;
//    abort();

    BecomeInt();
    mpz_and(_integer, aX._integer, aY._integer);
}

void BigNumber::BitOr(const BigNumber& aX, const BigNumber& aY)
{
  // LispInt lenX=(*aX.iNumber).Size(), lenY=(*aY.iNumber).Size();
  // LispInt min=lenX,max=lenY;
  // if (min>max)
  // {
  //   LispInt swap=min;
  //   min=max;
  //   max=swap;
  // }

  // iNumber->ResizeTo(max);

  // LispInt i;
  // for (i=0;i<min;i++)    (*iNumber)[i] = (*aX.iNumber)[i] | (*aY.iNumber)[i];
  // for (;i<lenY;i++)      (*iNumber)[i] = (*aY.iNumber)[i];
  // for (;i<lenX;i++)      (*iNumber)[i] = (*aX.iNumber)[i];
    std::cerr << "BigNumber::BitOr" << std::endl;
    abort();
}

void BigNumber::BitXor(const BigNumber& aX, const BigNumber& aY)
{
  // LispInt lenX=(*aX.iNumber).Size(), lenY=(*aY.iNumber).Size();
  // LispInt min=lenX,max=lenY;
  // if (min>max)
  // {
  //   LispInt swap=min;
  //   min=max;
  //   max=swap;
  // }
 
  // iNumber->ResizeTo(max);

  // LispInt i;
  // for (i=0;i<min;i++)  (*iNumber)[i] = (*aX.iNumber)[i] ^ (*aY.iNumber)[i];
  // for (;i<lenY;i++)    (*iNumber)[i] = (*aY.iNumber)[i];
  // for (;i<lenX;i++)    (*iNumber)[i] = (*aX.iNumber)[i];
    std::cerr << "BigNumber::BitXor" << std::endl;
    abort();
}

void BigNumber::BitNot(const BigNumber& aX)
{// FIXME?
  // LispInt len=(*aX.iNumber).Size();
 
  // iNumber->ResizeTo(len);

  // LispInt i;
  // for (i=0;i<len;i++)  (*iNumber)[i] = ~((*aX.iNumber)[i]);
    std::cerr << "BigNumber::BitNot" << std::endl;
    abort();
}


/// Bit count operation: return the number of significant bits if integer, return the binary exponent if float (shortcut for binary logarithm)
// give BitCount as platform integer
signed long BigNumber::BitCount() const
{
// /*
//   int low=0;
//   int high = 0;
//   int i;
//   for (i=0;i<iNumber->Size();i++)
//   {
//     if ((*iNumber)[i] != 0)
//     {
//       high = i;
//     }
//   }
//   if (low > high)
//     low = high;
//   LispInt bits=(high-low)*sizeof(PlatWord)*8;
//   return bits;
// */

//   if (IsZero(*iNumber)) return 0;//-(1L<<30);
//   ANumber num(*iNumber);
// //  num.CopyFrom(*iNumber);

//   if (num.iTensExp < 0)
//   {
//     LispInt digs = WordDigits(num.iPrecision, 10);
//     PlatWord zero=0;
//     while(num.iExp<digs)
//     {
//         num.Insert(0,zero);
//         num.iExp++;
//     }
//   }
//   while (num.iTensExp < 0)
//   {
//     PlatDoubleWord carry=0;
//     BaseDivideInt(num,10, WordBase, carry);
//     num.iTensExp++;
//   }
//   while (num.iTensExp > 0)
//   {
//     BaseTimesInt(num,10, WordBase);
//     num.iTensExp--;
//   }

//   LispInt i,nr=num.Size();
//   for (i=nr-1;i>=0;i--)
//   {
//     if (num[i] != 0) break;
//   }
//   LispInt bits=(i-num.iExp)*sizeof(PlatWord)*8;
//   if (i>=0)
//   {
//     PlatWord w=num[i];
//     while (w)
//     {
//       w>>=1;
//       bits++;
//     }
//   }
//   return (bits);

    if (IsInt()) {
        const int sgn = mpz_sgn(_integer);
        return sgn * mpz_sizeinbase(_integer, 2);
    }

    const int sgn = mpfr_sgn(_float);

    if (sgn == 0)
        return 0;

    mpfr_t k;
    mpfr_init2(k, iPrecision);
    mpfr_abs(k, _float, MPFR_RNDN);

    mpfr_t l;
    mpfr_init2(l, iPrecision);
    mpfr_log2(l, k, MPFR_RNDN);

    mpz_t m;
    mpz_init(m);
    mpfr_get_z(m, l, MPFR_RNDD);
    const long int n = mpz_get_si(m);

    mpfr_clear(k);
    mpfr_clear(l);
    mpz_clear(m);

    return sgn * (n + 1);
}

LispInt BigNumber::Sign() const
{
    if (IsInt())
        return mpz_sgn(_integer);

    return mpfr_sgn(_float);
  // if (iNumber->iNegative) return -1;
  // if (IsZero(*iNumber)) return 0;
  // return 1;
}

void BigNumber::DumpDebugInfo() const
{
    std::cout << "integer: " << IsInt() << std::endl;
    LispString s;
    ToString(s, iPrecision, 10);
    std::cout << "value:" << s.c_str()  << std::endl;

    // if (IsInt()) {
    //     char* p = 0;
    //     p = mpz_get_str(0, 10, _integer);
    //     std::cout << "value: " << p << std::endl;
    //     free(p);
    // } else {
    //     char* p = 0;
    //     mp_exp_t exp;
    //     p = mpfr_get_str(0, &exp, 10, 0, _float, MPFR_RNDN);
    //     std::cout << "value: " << p << std::endl;
    //     std::cout << "point: " << exp << std::endl;
    //     free(p);
    // }
}


/// integer operation: *this = y mod z
void BigNumber::Mod(const BigNumber& aY, const BigNumber& aZ)
{
//     ANumber a1(*aY.iNumber);
//     ANumber a2(*aZ.iNumber);
// //    a1.CopyFrom(*aY.iNumber);
// //    a2.CopyFrom(*aZ.iNumber);
//     Check(a1.iExp == 0, KLispErrNotInteger);
//     Check(a2.iExp == 0, KLispErrNotInteger);
//     Check(!IsZero(a2),KLispErrInvalidArg);

//     ANumber quotient(static_cast<LispInt>(0));
//     ::IntegerDivide(quotient, *iNumber, a1, a2);

//     if (iNumber->iNegative)
//     {
//       ANumber a3(*iNumber);
// //      a3.CopyFrom(*iNumber);
//       ::Add(*iNumber, a3, a2);
//     }
//     SetIsInteger(true);
    std::cerr << "BigNumber::Mod" << std::endl;
    abort();
}

void BigNumber::Floor(const BigNumber& aX)
{
    // iNumber->CopyFrom(*aX.iNumber);
    // //  If iExp is zero, then we can not look at the decimals and determine the floor.
    // // This number has to have digits (see code later in this routine that does a division).
    // // Not extending the digits caused the MathFloor function to fail on n*10-m where n was an
    // // integer. The code below divides away the 10^-m, but since iExp was zero, this resulted in a
    // // premature truncation (seen when n<0)
    // if (iNumber->iExp == 0)
    //   iNumber->ChangePrecision(iNumber->iPrecision);

    // if (iNumber->iExp>1)
    //   iNumber->RoundBits();

    // //TODO FIXME slow code! But correct
    // if (iNumber->iTensExp > 0)
    // {
    //   while (iNumber->iTensExp > 0)
    //   {
    //     BaseTimesInt(*iNumber,10, WordBase);
    //     iNumber->iTensExp--;
    //   }
    // }
    // else if (iNumber->iTensExp < 0)
    // {
    //   while (iNumber->iTensExp < 0)
    //   {
    //     PlatDoubleWord carry;
    //     BaseDivideInt(*iNumber,10, WordBase, carry);
    //     iNumber->iTensExp++;
    //   }
    // }
    // iNumber->ChangePrecision(iNumber->iPrecision);
    // LispInt i=0;
    // LispInt fraciszero=true;
    // while (i<iNumber->iExp && fraciszero)
    // {
    //     PlatWord digit = (*iNumber)[i];
    //     if (digit != 0)
    //         fraciszero=false;
    //     i++;
    // }
    // iNumber->Delete(0,iNumber->iExp);
    // iNumber->iExp=0;

    // if (iNumber->iNegative && !fraciszero)
    // {
    //     ANumber orig(*iNumber);
    //     ANumber minone("-1",10);
    //     ::Add(*iNumber,orig,minone);
    // }
    // SetIsInteger(true);

    if (this != &aX) {
        if (!IsInt()) {
            mpz_init(_integer);
            mpfr_clear(_float);
            SetIsInteger(true);
        }

        if (aX.IsInt())
            mpz_set(_integer, aX._integer);
        else
            mpfr_get_z(_integer, aX._float, MPFR_RNDD);

    } else {
        if (IsInt())
            return;

        mpz_init(_integer);
        mpfr_get_z(_integer, _float, MPFR_RNDD);
        mpfr_clear(_float);
        SetIsInteger(true);
    }
}

void BigNumber::Precision(LispInt aPrecision)
{
    if (aPrecision<0)
        aPrecision=0;

    // if (aPrecision < iPrecision)
    // {
    // }
    // else
    // {
    //     iNumber->ChangePrecision(bits_to_digits(aPrecision,10));
    // }
    // SetIsInteger(iNumber->iExp == 0 && iNumber->iTensExp == 0);

    iPrecision = aPrecision;
}


//basic object manipulation
bool BigNumber::Equals(const BigNumber& aOther) const
{
    if (IsInt() && aOther.IsInt())
        return mpz_cmp(_integer, aOther._integer) == 0;
    else if (IsInt() && !aOther.IsInt())
        return mpfr_cmp_z (aOther._float, _integer) == 0;
    else if (!IsInt() && aOther.IsInt())
        return mpfr_cmp_z (_float, aOther._integer) == 0;
    else
        return mpfr_cmp(_float, aOther._float) == 0;
}


bool BigNumber::IsInt() const
{
    return iType == KInt;
}


bool BigNumber::IsIntValue() const
{
// //FIXME I need to round first to get more reliable results.
//   if (IsInt()) return true;
//   iNumber->DropTrailZeroes();
//   if (iNumber->iExp == 0 && iNumber->iTensExp == 0) return true;
//   BigNumber num(iPrecision);
//   num.Floor(*this);
//   return Equals(num);
    std::cerr << "BigNumber::IsIntValue" << std::endl;
    abort();
}


bool BigNumber::IsSmall() const
{
  // if (IsInt())
  // {
  //   PlatWord* ptr = &((*iNumber)[iNumber->Size()-1]);
  //   LispInt nr=iNumber->Size();
  //   while (nr>1 && *ptr == 0) {ptr--;nr--;}
  //   return (nr <= iNumber->iExp+1);
  // }
  // else
  // // a function to test smallness of a float is not present in ANumber, need to code a workaround to determine whether a number fits into double.
  // {
  //   LispInt tensExp = iNumber->iTensExp;
  //   if (tensExp<0)tensExp = -tensExp;
  //   return
  //   (
  //     iNumber->iPrecision <= 53  // standard float is 53 bits
  //     && tensExp<1021 // 306  // 1021 bits is about 306 decimals
  //   );
  //   // standard range of double precision is about 53 bits of mantissa and binary exponent of about 1021
  // }

    if (IsInt()) {
        return mpz_fits_slong_p(_integer);
    }

    return true;
}


void BigNumber::BecomeInt()
{
  // while (iNumber->iTensExp > 0)
  // {
  //   BaseTimesInt(*iNumber,10, WordBase);
  //   iNumber->iTensExp--;
  // }
  // while (iNumber->iTensExp < 0)
  // {
  //   PlatDoubleWord carry=0;
  //   BaseDivideInt(*iNumber,10, WordBase, carry);
  //   iNumber->iTensExp++;
  // }
 
  // iNumber->ChangePrecision(0);
  // SetIsInteger(true);
    if (IsInt())
        return;

    mpz_init(_integer);
    mpfr_get_z(_integer, _float, MPFR_RNDZ);
    mpfr_clear(_float);

    SetIsInteger(true);
}

/// Transform integer to float, setting a given bit precision.
/// Note that aPrecision=0 means automatic setting (just enough digits to represent the integer).
void BigNumber::BecomeFloat(LispInt aPrecision)
{//FIXME: need to specify precision explicitly
  // if (IsInt())
  // {
  //   LispInt precision = aPrecision;
  //   if (iPrecision > aPrecision)
  //     precision = iPrecision;
  //   iNumber->ChangePrecision(bits_to_digits(precision,10));  // is this OK or ChangePrecision means floating-point precision?
  //   SetIsInteger(false);
  // }
    if (!IsInt())
        return;

    LispInt precision = aPrecision;
    if (iPrecision > aPrecision)
        precision = iPrecision;

    mpfr_init2(_float, precision);
    mpfr_set_z(_float, _integer, MPFR_RNDN);
    mpz_clear(_integer);
    SetIsInteger(false);
}


bool BigNumber::LessThan(const BigNumber& aOther) const
{
    if (IsInt() && aOther.IsInt())
        return mpz_cmp(_integer, aOther._integer) < 0;
    else if (IsInt() && !aOther.IsInt())
        return mpfr_cmp_z (aOther._float, _integer) > 0;
    else if (!IsInt() && aOther.IsInt())
        return mpfr_cmp_z (_float, aOther._integer) < 0;
    else
        return mpfr_cmp(_float, aOther._float) < 0;
}




// assign from a platform type
void BigNumber::SetTo(long aValue)
{
    BecomeInt();

    mpz_set_si(_integer, aValue);
}


void BigNumber::SetTo(double aValue)
{
// #ifdef HAVE_STDIO_H
//   iPrecision = 53;  // standard double has 53 bits
//   char dummy[150];
//   //FIXME platform code
//   char format[20];
// #ifdef HAVE_VSNPRINTF
//   snprintf(format,20,"%%.%dg",iPrecision);
//   snprintf(dummy,150,format,aValue);
// #else
//   sprintf(format,"%%.%dg",iPrecision);
//   sprintf(dummy,format,aValue);
// #endif
//   SetTo(dummy,iPrecision,BASE10);
//   SetIsInteger(false);
// //  if (iNumber->iExp > 1)
// //    iNumber->RoundBits();
// #else
//   //FIXME
//   LISPASSERT(0);
// #endif
    iPrecision = bits_to_digits(53, 10);
    BecomeFloat();
    mpfr_set_d(_float, aValue, MPFR_RNDN);
}

LispInt CalculatePrecision(const LispChar * aString,LispInt aBasePrecision,LispInt aBase, bool& aIsFloat)
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
    aIsFloat = true;
    return (LispInt) digits_to_bits(MAX(aBasePrecision,sig_digits), aBase);
  }
  else
  {
    aIsFloat = false;
    return 0;
  }
}

// assign from string at given precision (the API says in base digits)
// FIXME: API breach: aPrecision is passed in digits but used as if it were bits
void BigNumber::SetTo(const LispChar* aString, LispInt aBasePrecision, LispInt aBase)
{//FIXME -- what?
// //  iPrecision = digits_to_bits(aBasePrecision,BASE10);

    const LispChar* ptr = aString;
    while (*ptr && *ptr != '.')
        ptr++;

    bool is_float = *ptr == '.';

    iPrecision = CalculatePrecision(aString, aBasePrecision, aBase, is_float);

    if (is_float) {
        SetIsInteger(false);
        mpfr_init2(_float, iPrecision);
        mpfr_set_str(_float, aString, aBase, MPFR_RNDN);
    } else {
        SetIsInteger(true);
        mpz_init(_integer);
        mpz_set_str(_integer, aString, aBase);
    }

// */
//   if (!iNumber)   iNumber = NEW ANumber(digits);
//   iNumber->SetPrecision(digits);
//   iNumber->SetTo(aString,aBase);
 
//   SetIsInteger(!isFloat && iNumber->iExp == 0 && iNumber->iTensExp == 0);
//    std::cerr << "BigNumber::SetToString: " << aString << std::endl;
}
