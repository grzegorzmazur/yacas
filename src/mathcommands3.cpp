
#include "yacasprivate.h"

#include "yacasbase.h"
#include "lispenvironment.h"
#include "standard.h"
#include "lispeval.h"
#include "lispatom.h"
#include "lispparser.h"
#include "stdfileio.h"
#include "stringio.h"
#include "lisperror.h"
#include "infixparser.h"
#include "lispuserfunc.h"
#include "mathuserfunc.h"
#include "platmath.h"
#include "numbers.h"
#include "anumber.h"
#include "arrayclass.h"
#include "patternclass.h"
#include "substitute.h"
#include "errors.h"
#include "patcher.h"
#include "platdll.h"
#include "unipoly.h"

#ifdef HAVE_CONFIG_H
#include "../config.h"
#endif

#ifdef HAVE_MATH_H
  #include <math.h>
#endif

#ifdef VERSION
#undef VERSION
#endif //VERSION

//#define VERSION "Windows latest"
#include "version.h"


/*TODO

 - Make a separate Number object, for fast calculations.

 - as an alternative, I *could* implement MathFac ;-)

 - Add Number and CopiedNumber methods to LispObject
 - implement Number and CopiedNumber in LispAtom
 - define a LispNumber that can cache a number. This would already
   speed up twofold.
 - implement at least addition and multiplication so they
   can use these.
 - IsNumber and IsInteger should Check ->Number() before doing ->String()

 

 The trick will obviously be to make sure no conversions are made to
 ascii until needed.

 
 */






#define InternalEval aEnvironment.iEvaluator->Eval



#ifndef NO_USE_BIGFLOAT
void GetNumber(RefPtr<BigNumber>& x, LispEnvironment& aEnvironment, LispPtr& aArguments, LispInt aArgNr)
{
    LispPtr result;
    InternalEval(aEnvironment, result, Argument(aArguments,aArgNr));
    RefPtr<BigNumber> num; num = result.Get()->Number(aEnvironment.Precision());
    CHK_ARG(num.Ptr() != NULL,aArgNr);
    x = num;
}
#endif // USE_BIGFLOAT

//FIXME remove these
void LispArithmetic2(LispEnvironment& aEnvironment, LispPtr& aResult,
                     LispPtr& aArguments,
                     LispStringPtr (*func)(LispCharPtr f1, LispCharPtr f2,LispHashTable& aHashTable,LispInt aPrecision),
                    LispBoolean arbbase=LispFalse);

void LispArithmetic1(LispEnvironment& aEnvironment, LispPtr& aResult,
                     LispPtr& aArguments,
                     LispStringPtr (*func)(LispCharPtr f1, LispHashTable& aHashTable,LispInt aPrecision));






//FIXME remove these
void LispArithmetic1(LispEnvironment& aEnvironment, LispPtr& aResult,
                     LispPtr& aArguments,
                     LispStringPtr (*func)(LispCharPtr f1, LispHashTable& aHashTable,LispInt aPrecision))
{
    TESTARGS(2);

    LispStringPtr str1;

    LispPtr result1;
    InternalEval(aEnvironment, result1, Argument(aArguments,1));

    str1 = result1.Get()->String();
    CHK_ARG(str1 != NULL,1);
    CHK_ARG(IsNumber(str1->String(),LispTrue),1);
    aResult.Set(LispAtom::New(aEnvironment,func(str1->String(),
                                   aEnvironment.HashTable(),
                                   aEnvironment.Precision())));
}


//FIXME remove these
void LispArithmetic2(LispEnvironment& aEnvironment, LispPtr& aResult,
                     LispPtr& aArguments,
                     LispStringPtr (*func)(LispCharPtr f1, LispCharPtr f2,LispHashTable& aHashTable,LispInt aPrecision),
                    LispBoolean arbbase)
{
    TESTARGS(3);

    LispStringPtr str1;
    LispStringPtr str2;

    LispPtr result1;
    LispPtr result2;
    InternalEval(aEnvironment, result1, Argument(aArguments,1));
    InternalEval(aEnvironment, result2, Argument(aArguments,2));

    str1 = result1.Get()->String();
    str2 = result2.Get()->String();
    CHK_ARG(str1 != NULL,1);
    CHK_ARG(str2 != NULL,2);
    if (!arbbase)
    {
        CHK_ARG(IsNumber(str1->String(),LispTrue) ,1);
        CHK_ARG(IsNumber(str2->String(),LispTrue) ,2);
    }

    aResult.Set(LispAtom::New(aEnvironment,func(str1->String(),str2->String(),
                                   aEnvironment.HashTable(),
                                   aEnvironment.Precision())));
}


void LispMultiply(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
#ifndef NO_USE_BIGFLOAT
      RefPtr<BigNumber> x;
      RefPtr<BigNumber> y;
      GetNumber(x,aEnvironment, aArguments, 1);
      GetNumber(y,aEnvironment, aArguments, 2);
      BigNumber *z = NEW BigNumber(aEnvironment.Precision());
      z->Multiply(*x.Ptr(),*y.Ptr(),aEnvironment.Precision());
      aResult.Set(NEW LispNumber(aEnvironment.HashTable(),z));
      return;
#endif // USE_BIGFLOAT
    LispArithmetic2(aEnvironment, aResult, aArguments, MultiplyFloat);
}

void LispAdd(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispInt length = InternalListLength(aArguments);
    if (length == 2)
    {
#ifndef NO_USE_BIGFLOAT
      RefPtr<BigNumber> x;
      GetNumber(x,aEnvironment, aArguments, 1);
      aResult.Set(NEW LispNumber(aEnvironment.HashTable(),x.Ptr()));
      return;
#endif
        LispArithmetic1(aEnvironment, aResult, aArguments, PlusFloat);
    }
    else
    {
#ifndef NO_USE_BIGFLOAT
      RefPtr<BigNumber> x;
      RefPtr<BigNumber> y;
      GetNumber(x,aEnvironment, aArguments, 1);
      GetNumber(y,aEnvironment, aArguments, 2);
      BigNumber *z = NEW BigNumber(aEnvironment.Precision());
      z->Add(*x.Ptr(),*y.Ptr(),aEnvironment.Precision());
      aResult.Set(NEW LispNumber(aEnvironment.HashTable(),z));
      return;
#endif // USE_BIGFLOAT

        LispArithmetic2(aEnvironment, aResult, aArguments, AddFloat);
    }
}

void LispSubtract(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispInt length = InternalListLength(aArguments);
    if (length == 2)
    {
#ifndef NO_USE_BIGFLOAT
      RefPtr<BigNumber> x;
      GetNumber(x,aEnvironment, aArguments, 1);
      BigNumber *z = NEW BigNumber(aEnvironment.Precision());
      z->Negate(*x.Ptr());
      aResult.Set(NEW LispNumber(aEnvironment.HashTable(),z));
      return;
#endif
        LispArithmetic1(aEnvironment, aResult, aArguments, NegateFloat);
    }
    else
    {
#ifndef NO_USE_BIGFLOAT
      RefPtr<BigNumber> x;
      RefPtr<BigNumber> y;
      GetNumber(x,aEnvironment, aArguments, 1);
      GetNumber(y,aEnvironment, aArguments, 2);
      BigNumber yneg;
      yneg.Negate(*y.Ptr());
      BigNumber *z = NEW BigNumber(aEnvironment.Precision());
      z->Add(*x.Ptr(),yneg,aEnvironment.Precision());
      aResult.Set(NEW LispNumber(aEnvironment.HashTable(),z));
      return;
#endif // USE_BIGFLOAT
        LispArithmetic2(aEnvironment, aResult, aArguments, SubtractFloat);
    }
}


void LispDivide(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
// Serge, what was the deal again with divide, floats and integers mixed in the same function?
//	yes, divide works differently on integers and on floats -- see new.chapt -- Serge
#ifndef NO_USE_BIGFLOAT
      RefPtr<BigNumber> x;
      RefPtr<BigNumber> y;
      GetNumber(x,aEnvironment, aArguments, 1);
      GetNumber(y,aEnvironment, aArguments, 2);
      BigNumber *z = NEW BigNumber(aEnvironment.Precision());
	  // if both arguments are integers, then BigNumber::Divide would perform an integer divide, but we want a float divide here.
	  if (x.Ptr()->IsInt() && y.Ptr()->IsInt())
	  {
		  // why can't we just say BigNumber temp; ?
		  BigNumber *temp = NEW BigNumber(aEnvironment.Precision());
		  temp->SetTo(*x.Ptr());
		  temp->BecomeFloat();	// coerce x to float
     	  z->Divide(*temp, *y.Ptr(),aEnvironment.Precision());
	  }
	  else
	  {
		  z->Divide(*x.Ptr(), *y.Ptr(),aEnvironment.Precision());
	  }
	  aResult.Set(NEW LispNumber(aEnvironment.HashTable(),z));
      return;
#endif // USE_BIGFLOAT
    LispArithmetic2(aEnvironment, aResult, aArguments, DivideFloat);
}


void LispSin(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{//FIXME move to scripts
    LispArithmetic1(aEnvironment, aResult, aArguments, SinFloat);
}

void LispCos(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{//FIXME move to scripts
    LispArithmetic1(aEnvironment, aResult, aArguments, CosFloat);
}

void LispTan(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{//FIXME move to scripts
    LispArithmetic1(aEnvironment, aResult, aArguments, TanFloat);
}

void LispArcSin(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{//FIXME move to scripts
    LispArithmetic1(aEnvironment, aResult, aArguments, ArcSinFloat);
}

void LispArcCos(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{//FIXME move to scripts
    LispArithmetic1(aEnvironment, aResult, aArguments, ArcCosFloat);
}

void LispArcTan(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{//FIXME move to scripts
    LispArithmetic1(aEnvironment, aResult, aArguments, ArcTanFloat);
}

void LispSqrt(LispEnvironment& aEnvironment, LispPtr& aResult,
              LispPtr& aArguments)
{//FIXME move to scripts
    LispArithmetic1(aEnvironment, aResult, aArguments, SqrtFloat);
}


#ifndef NO_USE_BIGFLOAT
#define UNARYFUNCTION(LispName, BigNumName, OldName) \
void LispName(LispEnvironment& aEnvironment, LispPtr& aResult, LispPtr& aArguments) \
{ \
      RefPtr<BigNumber> x; \
      GetNumber(x,aEnvironment, aArguments, 1); \
      BigNumber *z = NEW BigNumber(aEnvironment.Precision()); \
      z->BigNumName(*x.Ptr()); \
      aResult.Set(NEW LispNumber(aEnvironment.HashTable(),z)); \
}
#define BINARYFUNCTION(LispName, BigNumName, OldName) \
void LispName(LispEnvironment& aEnvironment, LispPtr& aResult, LispPtr& aArguments) \
{ \
      RefPtr<BigNumber> x; \
      RefPtr<BigNumber> y; \
      GetNumber(x,aEnvironment, aArguments, 1); \
      GetNumber(y,aEnvironment, aArguments, 2); \
      BigNumber *z = NEW BigNumber(aEnvironment.Precision()); \
      z->BigNumName(*x.Ptr(), *y.Ptr()); \
      aResult.Set(NEW LispNumber(aEnvironment.HashTable(),z)); \
}
#else
#define UNARYFUNCTION(LispName, BigNumName, OldName) \
void LispName(LispEnvironment& aEnvironment, LispPtr& aResult, LispPtr& aArguments) \
{ \
    LispArithmetic1(aEnvironment, aResult, aArguments, OldName); \
}
#define BINARYFUNCTION(LispName, BigNumName, OldName) \
void LispName(LispEnvironment& aEnvironment, LispPtr& aResult, LispPtr& aArguments) \
{ \
    LispArithmetic2(aEnvironment, aResult, aArguments, OldName); \
}
#endif

UNARYFUNCTION(LispFloor, Floor, FloorFloat)
// these functions are not yet in yacasapi.cpp but should be eventually
//UNARYFUNCTION(LispBitCount, BitCount, MathBitCount)
//UNARYFUNCTION(LispSign, Sign, MathSign)
//UNARYFUNCTION(LispNegate, Negate, MathNegate)

/** this produces the following equivalent code for a unary function:
void LispFloor(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
#ifndef NO_USE_BIGFLOAT
      RefPtr<BigNumber> x;
      GetNumber(x,aEnvironment, aArguments, 1);
      BigNumber *z = NEW BigNumber(aEnvironment.Precision());
      z->Floor(*x.Ptr());
      aResult.Set(NEW LispNumber(aEnvironment.HashTable(),z));
#else
    LispArithmetic1(aEnvironment, aResult, aArguments, FloorFloat);
#endif
}
*/

void LispCeil(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
#ifndef NO_USE_BIGFLOAT
      RefPtr<BigNumber> x;
      GetNumber(x,aEnvironment, aArguments, 1);
      BigNumber *z = NEW BigNumber(aEnvironment.Precision());
      z->Negate(*x.Ptr());
      z->Floor(*z);
      z->Negate(*z);
      aResult.Set(NEW LispNumber(aEnvironment.HashTable(),z));
#else
   LispArithmetic1(aEnvironment, aResult, aArguments, CeilFloat);
#endif
}

void LispAbs(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
#ifndef NO_USE_BIGFLOAT
      RefPtr<BigNumber> x;
      GetNumber(x,aEnvironment, aArguments, 1);
      BigNumber *z = NEW BigNumber(aEnvironment.Precision());
      z->SetTo(*x.Ptr());
      if (x.Ptr()->Sign()<0)
	      z->Negate(*x.Ptr());
      aResult.Set(NEW LispNumber(aEnvironment.HashTable(),z));
#else
   LispArithmetic1(aEnvironment, aResult, aArguments, AbsFloat);
#endif
}
//BINARYFUNCTION(LispMod, Mod, ModFloat)
/* this will be gone */
void LispMod(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{//FIXME
    LispArithmetic2(aEnvironment, aResult, aArguments, ModFloat);
}
/* up to here */

void LispDiv(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
#ifndef NO_USE_BIGFLOAT
      RefPtr<BigNumber> x;
      RefPtr<BigNumber> y;
      GetNumber(x,aEnvironment, aArguments, 1);
      GetNumber(y,aEnvironment, aArguments, 2);
	  if (x.Ptr()->IsInt() && y.Ptr()->IsInt())
	  {	// both integer, perform integer division
    	  BigNumber *z = NEW BigNumber(aEnvironment.Precision());
    	  z->Divide(*x.Ptr(),*y.Ptr(),aEnvironment.Precision());
    	  aResult.Set(NEW LispNumber(aEnvironment.HashTable(),z));
    	  return;
	  }
	  else
	  {// FIXME: either need to report error that one or both of the arguments are not integer, or coerce them to integers
#ifdef HAVE_STDIO_H
	  	fprintf(stderr, "LispDiv: error: both arguments must be integer\n");
#endif
		  return;
	  }
	  
#endif // USE_BIGFLOAT
    LispArithmetic2(aEnvironment, aResult, aArguments, DivFloat);
}

void LispLog(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{//FIXME move to scripts
    LispArithmetic1(aEnvironment, aResult, aArguments, LnFloat);
}

void LispExp(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{//FIXME move to scripts
    LispArithmetic1(aEnvironment, aResult, aArguments, ExpFloat);
}

void LispPower(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{//FIXME move to scripts
    LispArithmetic2(aEnvironment, aResult, aArguments, PowerFloat);
}



void LispFac(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{//FIXME move to scripts
    LispArithmetic1(aEnvironment, aResult, aArguments, LispFactorial);
}


// platform functions, taking/returning a platform int/float

void LispFastIsPrime(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{//FIXME
#ifndef NO_USE_BIGFLOAT
      RefPtr<BigNumber> x;
      GetNumber(x,aEnvironment, aArguments, 1);
      LispInt result = primes_table_check((unsigned long)(x->Double()));
      BigNumber *z = NEW BigNumber(aEnvironment.Precision());
      z->SetTo(result);
      aResult.Set(NEW LispNumber(aEnvironment.HashTable(),z));
#else
    LispArithmetic1(aEnvironment, aResult, aArguments, PlatIsPrime);
#endif
}

// define a macro to replace all platform math functions
#ifndef NO_USE_BIGFLOAT

  #ifndef HAVE_MATH_H
  // this warning is here just to be sure what we are compiling
    #warning do not have math.h
    #define PLATFORM_UNARY(LispName, PlatformName, LispBackupName, OldName) \
void LispName(LispEnvironment& aEnvironment, LispPtr& aResult, LispPtr& aArguments) \
{ \
      LispBackupName(aEnvironment, aResult, aArguments); \
}
    #define PLATFORM_BINARY(LispName, PlatformName, LispBackupName, OldName) \
void LispName(LispEnvironment& aEnvironment, LispPtr& aResult, LispPtr& aArguments) \
{ \
      LispBackupName(aEnvironment, aResult, aArguments); \
}
  #else	// HAVE_MATH_H
    #define PLATFORM_UNARY(LispName, PlatformName, LispBackupName, OldName) \
void LispName(LispEnvironment& aEnvironment, LispPtr& aResult, LispPtr& aArguments) \
{ \
      RefPtr<BigNumber> x; \
      GetNumber(x,aEnvironment, aArguments, 1); \
      double result = PlatformName(x->Double()); \
      BigNumber *z = NEW BigNumber(aEnvironment.Precision()); \
      z->SetTo(result); \
      aResult.Set(NEW LispNumber(aEnvironment.HashTable(),z)); \
}
    #define PLATFORM_BINARY(LispName, PlatformName, LispBackupName, OldName) \
void LispName(LispEnvironment& aEnvironment, LispPtr& aResult, LispPtr& aArguments) \
{ \
      RefPtr<BigNumber> x, y; \
      GetNumber(x,aEnvironment, aArguments, 1); \
      GetNumber(y,aEnvironment, aArguments, 2); \
      double result = PlatformName(x->Double(), y->Double()); \
      BigNumber *z = NEW BigNumber(aEnvironment.Precision()); \
      z->SetTo(result); \
      aResult.Set(NEW LispNumber(aEnvironment.HashTable(),z)); \
}
  #endif
#else	// NO_USE_BIGFLOAT
  // this warning is here just to be sure what we are compiling
  #warning not using BigNumber:: functions
  #define PLATFORM_UNARY(LispName, PlatformName, LispBackupName, OldName) \
void LispName(LispEnvironment& aEnvironment, LispPtr& aResult, LispPtr& aArguments) \
{ \
    LispArithmetic1(aEnvironment, aResult, aArguments, OldName);
}
  #define PLATFORM_BINARY(LispName, PlatformName, LispBackupName, OldName) \
void LispName(LispEnvironment& aEnvironment, LispPtr& aResult, LispPtr& aArguments) \
{ \
    LispArithmetic2(aEnvironment, aResult, aArguments, OldName);
}
#endif

// now we can define all such functions, e.g.:
//	PLATFORM_UNARY(LispFastSin, sin, LispSin, PlatSin)
// this will generate the following equivalent code:
/*

void LispFastSin(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
#ifndef NO_USE_BIGFLOAT
#ifdef HAVE_MATH_H
      RefPtr<BigNumber> x;
      GetNumber(x,aEnvironment, aArguments, 1);
      double result = sin(x->Double());
      BigNumber *z = NEW BigNumber(aEnvironment.Precision());
      z->SetTo(result);
      aResult.Set(NEW LispNumber(aEnvironment.HashTable(),z));
#else
      LispSin(aEnvironment, aResult, aArguments);
#endif
      return;
#endif

    LispArithmetic1(aEnvironment, aResult, aArguments, PlatSin);
}

*/

// some or all of these functions should be moved to scripts
	PLATFORM_UNARY(LispFastSin, sin, LispSin, PlatSin)
	PLATFORM_UNARY(LispFastCos, cos, LispCos, PlatCos)
	PLATFORM_UNARY(LispFastTan, tan, LispTan, PlatTan)
	PLATFORM_UNARY(LispFastArcSin, asin, LispArcSin, PlatArcSin)
	PLATFORM_UNARY(LispFastArcCos, acos, LispArcCos, PlatArcCos)
	PLATFORM_UNARY(LispFastArcTan, atan, LispArcTan, PlatArcTan)
	PLATFORM_UNARY(LispFastExp, exp, LispExp, PlatExp)
	PLATFORM_UNARY(LispFastLog, log, LispLn, PlatLn)
	PLATFORM_UNARY(LispFastAbs, fabs, LispAbs, PlatAbs)
	PLATFORM_UNARY(LispFastFloor, floor, LispFloor, PlatFloor)
	PLATFORM_UNARY(LispFastCeil, ceil, LispCeil, PlatCeil)
	PLATFORM_UNARY(LispFastSqrt, sqrt, LispSqrt, PlatSqrt)
	PLATFORM_BINARY(LispFastPower, pow, LispPower, PlatPower)
	PLATFORM_BINARY(LispFastMod, fmod, LispMod, PlatMod)

/* this will be gone 
void LispFastCos(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{//FIXME
    LispArithmetic1(aEnvironment, aResult, aArguments, PlatCos);
}
void LispFastTan(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{//FIXME
    LispArithmetic1(aEnvironment, aResult, aArguments, PlatTan);
}

void LispFastArcSin(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{//FIXME
    LispArithmetic1(aEnvironment, aResult, aArguments, PlatArcSin);
}
void LispFastArcCos(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{//FIXME
    LispArithmetic1(aEnvironment, aResult, aArguments, PlatArcCos);
}
void LispFastArcTan(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{//FIXME
    LispArithmetic1(aEnvironment, aResult, aArguments, PlatArcTan);
}
void LispFastExp(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{//FIXME
    LispArithmetic1(aEnvironment, aResult, aArguments, PlatExp);
}
void LispFastLog(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{//FIXME
    LispArithmetic1(aEnvironment, aResult, aArguments, PlatLn);
}

void LispFastPower(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{//FIXME
    LispArithmetic2(aEnvironment, aResult, aArguments, PlatPower);
}

void LispFastSqrt(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{//FIXME
    LispArithmetic1(aEnvironment, aResult, aArguments, PlatSqrt);
}

void LispFastFloor(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{//FIXME
    LispArithmetic1(aEnvironment, aResult, aArguments, PlatFloor);
}

void LispFastCeil(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{//FIXME
    LispArithmetic1(aEnvironment, aResult, aArguments, PlatCeil);
}

void LispFastMod(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{//FIXME
    LispArithmetic2(aEnvironment, aResult, aArguments, PlatMod);
}

void LispFastAbs(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{//FIXME
    LispArithmetic1(aEnvironment, aResult, aArguments, PlatAbs);
}
up to here */

/*
BINARYFUNCTION(LispShiftLeft, ShiftLeft, ShiftLeft)
BINARYFUNCTION(LispShiftRight, ShiftRight, ShiftRight)

BINARYFUNCTION(LispBitAnd, BitAnd, BitAnd)
BINARYFUNCTION(LispBitOr, BitOr, BitOr)
BINARYFUNCTION(LispBitXor, BitXor, BitXor)
// BitNot not yet in yacasapi etc.
//BINARYFUNCTION(LispBitNot, BitNot, BitNot)
*/
/* this will be gone */
void LispShiftLeft(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{//FIXME
    LispArithmetic2(aEnvironment, aResult, aArguments, ShiftLeft);
}
void LispShiftRight(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{//FIXME
    LispArithmetic2(aEnvironment, aResult, aArguments, ShiftRight);
}

void LispBitAnd(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{//FIXME
    LispArithmetic2(aEnvironment, aResult, aArguments, BitAnd);
}
void LispBitOr(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{//FIXME
    LispArithmetic2(aEnvironment, aResult, aArguments, BitOr);
}
void LispBitXor(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{//FIXME
    LispArithmetic2(aEnvironment, aResult, aArguments, BitXor);
}
/* up to here */


void LispFromBase(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    TESTARGS(3);

    // Get the base to convert to:
    // Evaluate first argument, and store result in oper
    LispPtr oper;
    InternalEval(aEnvironment, oper, Argument(aArguments,1));
    // Check that result is a number, and that it is in fact an integer
    RefPtr<BigNumber> num; num = oper.Get()->Number(aEnvironment.Precision());
    CHK_ARG(num.Ptr() != NULL,1);
    CHK_ARG(num->IsInt(),1);

    // Get a short platform integer from the first argument
    LispStringPtr str1;
    str1 = oper.Get()->String();
    CHK_ARG(str1 != NULL,1);
    LispInt base = InternalAsciiToInt(str1->String());

    // Get the number to convert
    LispPtr fromNum;
    InternalEval(aEnvironment, fromNum, Argument(aArguments,2));
    LispStringPtr str2;
    str2 = fromNum.Get()->String();
    CHK_ARG(str2 != NULL,2);

    // convert using correct base
    BigNumber *z = NEW BigNumber(str2->String(),aEnvironment.Precision(),base);
    aResult.Set(NEW LispNumber(aEnvironment.HashTable(),z));

//TODO remove old code    LispArithmetic2(aEnvironment, aResult, aArguments, FromBase,LispTrue);
}
void LispToBase(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    TESTARGS(3);

    // Get the base to convert to:
    // Evaluate first argument, and store result in oper
    LispPtr oper;
    InternalEval(aEnvironment, oper, Argument(aArguments,1));
    // Check that result is a number, and that it is in fact an integer
    RefPtr<BigNumber> num; num = oper.Get()->Number(aEnvironment.Precision());
    CHK_ARG(num.Ptr() != NULL,1);
    CHK_ARG(num->IsInt(),1);

    // Get a short platform integer from the first argument
    LispStringPtr str1;
    str1 = oper.Get()->String();
    CHK_ARG(str1 != NULL,1);
    LispInt base = InternalAsciiToInt(str1->String());

    // Get the number to convert
    RefPtr<BigNumber> x;
    GetNumber(x,aEnvironment, aArguments, 2);

    // convert using correct base
    LispString str;
    x->ToString(str,aEnvironment.Precision(),base);
    // Get unique string from hash table, and create an atom from it.
    aResult.Set(LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUp(str.String())));

//TODO remove old code?    LispArithmetic2(aEnvironment, aResult, aArguments, ToBase);
}



void LispApplyPure(LispEnvironment& aEnvironment, LispPtr& aResult,
                   LispPtr& aArguments)
{
    TESTARGS(3);

    LispPtr oper;
    InternalEval(aEnvironment, oper, Argument(aArguments,1));
    LispPtr args;
    InternalEval(aEnvironment, args, Argument(aArguments,2));

    CHK_ARG(args.Get()->SubList() != NULL,2);
    CHK(args.Get()->SubList()->Get() != NULL,2);

    

    // Apply a pure string
    if (oper.Get()->String() != NULL)
    {
        InternalApplyString(aEnvironment, aResult,
                    oper.Get()->String(),
                    args.Get()->SubList()->Get()->Next());
    }
    else
    {   // Apply a pure function {args,body}.
        LispPtr args2;
        args2.Set(args.Get()->SubList()->Get()->Next().Get());
        CHK_ARG(oper.Get()->SubList() != NULL,1);
        CHK_ARG(oper.Get()->SubList()->Get() != NULL,1);
        InternalApplyPure(oper,args2,aResult,aEnvironment);
    }
}


void LispPrettyPrinter(LispEnvironment& aEnvironment, LispPtr& aResult,
                       LispPtr& aArguments)
{
    LispInt nrArguments = InternalListLength(aArguments);

    if (nrArguments == 1)
    {
        aEnvironment.SetPrettyPrinter(NULL);
    }
    else
    {
        CHK(nrArguments == 2,KLispErrWrongNumberOfArgs);
        LispPtr oper;
        InternalEval(aEnvironment, oper, Argument(aArguments,1));
        CHK_ISSTRING(oper,1);
        aEnvironment.SetPrettyPrinter(oper.Get()->String());
    }
    InternalTrue(aEnvironment,aResult);
}


void LispGarbageCollect(LispEnvironment& aEnvironment, LispPtr& aResult,
                        LispPtr& aArguments)
{
    TESTARGS(1);
    aEnvironment.HashTable().GarbageCollect();
    
    InternalTrue(aEnvironment,aResult);
}

void LispLazyGlobal(LispEnvironment& aEnvironment, LispPtr& aResult,
                    LispPtr& aArguments)
{
    TESTARGS(2);
    LispStringPtr string = Argument(aArguments,1).Get()->String();
    CHK_ARG(string != NULL, 1);
    aEnvironment.SetGlobalEvaluates(string);
    InternalTrue(aEnvironment,aResult);

}

void LispPatchLoad(LispEnvironment& aEnvironment, LispPtr& aResult,
                   LispPtr& aArguments)
{
    TESTARGS(2);

    LispPtr evaluated;
    InternalEval(aEnvironment, evaluated, Argument(aArguments,1));

    LispStringPtr string = evaluated.Get()->String();
    CHK_ARG(string != NULL, 1);

    LispString oper;
    InternalUnstringify(oper, string);
    LispStringPtr hashedname = aEnvironment.HashTable().LookUp(oper.String());

    InputStatus oldstatus = aEnvironment.iInputStatus;
    aEnvironment.iInputStatus.SetTo(hashedname->String());

    
    LispLocalFile localFP(aEnvironment, oper.String(),LispTrue,
                          aEnvironment.iInputDirectories);
    Check(localFP.iOpened != 0, KLispErrFileNotFound);
    FILEINPUT newInput(localFP,aEnvironment.iInputStatus);

    PatchLoad(newInput.StartPtr(),
              *aEnvironment.CurrentOutput(),
              aEnvironment);
    aEnvironment.iInputStatus.RestoreFrom(oldstatus);
    InternalTrue(aEnvironment,aResult);
}

void LispPatchString(LispEnvironment& aEnvironment, LispPtr& aResult,
                     LispPtr& aArguments)
{
    TESTARGS(2);

    LispPtr evaluated;
    InternalEval(aEnvironment, evaluated, Argument(aArguments,1));
    
    LispStringPtr string = evaluated.Get()->String();
    CHK_ARG(string != NULL, 1);
    LispString oper;
    InternalUnstringify(oper, string);

    LispString str;
    StringOutput newOutput(str);

    LispLocalOutput localOutput(aEnvironment, &newOutput);

    PatchLoad(&oper[0], newOutput, aEnvironment);

    aResult.Set(LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUpStringify(str.String())));
}

void LispDllLoad(LispEnvironment& aEnvironment, LispPtr& aResult,
                   LispPtr& aArguments)
{
    TESTARGS(2);

    LispPtr evaluated;
    InternalEval(aEnvironment, evaluated, Argument(aArguments,1));

    LispStringPtr string = evaluated.Get()->String();
    CHK_ARG(string != NULL, 1);

    LispString oper;
    InternalUnstringify(oper, string);
    LispDllBase *dll = NEW DLLCLASS;
    Check(dll != NULL,KLispErrNotEnoughMemory);
    LispInt opened;

//    printf("file is [%s]\n"&(oper[0]));
    
    opened = dll->Open(&oper[0],aEnvironment);
    if (!opened) delete dll;
    Check(opened,KLispErrLibraryNotFound);
    /*TODO remove?
    LispPluginBase* plugin = dll->Plugin();
    if (plugin == NULL)
    {
        delete dll;
        Check(plugin != NULL,KLispErrLibraryNotFound);
    }
    */
    aEnvironment.iDlls.Append(dll);
//TODO remove?    plugin->Add(aEnvironment);

    InternalTrue(aEnvironment,aResult);
}

void LispDllUnload(LispEnvironment& aEnvironment, LispPtr& aResult,
                   LispPtr& aArguments)
{
    TESTARGS(2);
    LispPtr evaluated;
    InternalEval(aEnvironment, evaluated, Argument(aArguments,1));

    LispStringPtr string = evaluated.Get()->String();
    CHK_ARG(string != NULL, 1);

    LispString oper;
    InternalUnstringify(oper, string);
    aEnvironment.iDlls.DeleteNamed(&oper[0],aEnvironment);

    InternalTrue(aEnvironment,aResult);
}

void LispDllEnumerate(LispEnvironment& aEnvironment, LispPtr& aResult,
                      LispPtr& aArguments)
{
    TESTARGS(1);
    LispInt i;
    LispObject *res = NULL;
    for (i=aEnvironment.iDlls.NrItems()-1;i>=0;i--)
    {
        LispString orig;
        orig = aEnvironment.iDlls[i]->DllFileName();
        LispString oper;
        InternalStringify(oper, &orig);

        res = LA(ATOML(&oper[0])) + LA(res);
    }
    aResult.Set(LIST(LA(ATOML("List")) + LA(res)));
}

void LispSetExtraInfo(LispEnvironment& aEnvironment, LispPtr& aResult,
                   LispPtr& aArguments)
{
    TESTARGS(3);

    LispPtr object;
    InternalEval(aEnvironment, object, Argument(aArguments,1));

    LispPtr info;
    InternalEval(aEnvironment, info, Argument(aArguments,2));

    aResult.Set( object.Get()->SetExtraInfo(info) );
}


void LispGetExtraInfo(LispEnvironment& aEnvironment, LispPtr& aResult,
                   LispPtr& aArguments)
{
    TESTARGS(2);

    LispPtr object;
    InternalEval(aEnvironment, object, Argument(aArguments,1));

    LispPtr* result = object.Get()->ExtraInfo();
    if (result == NULL)
    {
        InternalFalse(aEnvironment,aResult);
    }
    else if (result->Get() == NULL)
    {
        InternalFalse(aEnvironment,aResult);
    }
    else
    {
        aResult.Set(result->Get());
    }
}

void LispBerlekamp(LispEnvironment& aEnvironment, LispPtr& aResult,
                   LispPtr& aArguments)
{
    TESTARGS(3);
    LispPtr polyobj;
    InternalEval(aEnvironment, polyobj , Argument(aArguments,1));
    LispPtr primeobj;
    InternalEval(aEnvironment, primeobj, Argument(aArguments,2));

    LispInt prime = InternalAsciiToInt(primeobj.Get()->String()->String());

    ZZPoly poly;
    CHK_ARG(polyobj.Get()->SubList() != NULL, 1);
    LispPtr hold;
    hold.Set(polyobj.Get());
    polyobj.Set(polyobj.Get()->SubList()->Get());

    CHK_ARG(polyobj.Get() != NULL, 1);
    polyobj.Set(polyobj.Get()->Next().Get());

    while (polyobj.Get() != NULL)
    {
        ZZ fact;
        CHK_ARG(polyobj.Get()->String() != NULL, 1);
        fact = InternalAsciiToInt(polyobj.Get()->String()->String());
        poly.Append(fact);
        polyobj.Set(polyobj.Get()->Next().Get());
    }
    ZZPolyList result;
    Berlekamp(result, poly, prime);

    LispInt i;
    LispObject *res = NULL;
    for (i=result.NrItems()-1;i>=0;i--)
    {
        LispInt j;
        LispObject *sub = NULL;
        for (j=result[i]->Degree();j>=0;j--)
        {
            LispChar s[20];
            InternalIntToAscii(s,(*result[i])[j]);
            sub = LA(ATOML(s)) + LA(sub);
        }
        res = LIST(LA(ATOML("List")) + LA(sub)) + LA(res);
    }
    aResult.Set(LIST(LA(ATOML("List")) + LA(res)));
}

void LispDefaultTokenizer(LispEnvironment& aEnvironment, LispPtr& aResult,
                   LispPtr& aArguments)
{
    TESTARGS(1);
    aEnvironment.iCurrentTokenizer = &aEnvironment.iDefaultTokenizer;
    InternalTrue(aEnvironment,aResult);
}

void LispCommonLispTokenizer(LispEnvironment& aEnvironment, LispPtr& aResult,
                   LispPtr& aArguments)
{
    TESTARGS(1);
    aEnvironment.iCurrentTokenizer = &aEnvironment.iCommonLispTokenizer;
    InternalTrue(aEnvironment,aResult);
}
void LispCTokenizer(LispEnvironment& aEnvironment, LispPtr& aResult,
                   LispPtr& aArguments)
{
    TESTARGS(1);
    aEnvironment.iCurrentTokenizer = &aEnvironment.iCTokenizer;
    InternalTrue(aEnvironment,aResult);
}

void LispXmlTokenizer(LispEnvironment& aEnvironment, LispPtr& aResult,
                      LispPtr& aArguments)
{
    TESTARGS(1);
    aEnvironment.iCurrentTokenizer = &aEnvironment.iXmlTokenizer;
    InternalTrue(aEnvironment,aResult);
}

void LispExplodeTag(LispEnvironment& aEnvironment, LispPtr& aResult,
                   LispPtr& aArguments)
{
    TESTARGS(2);
    LispPtr out;
    InternalEval(aEnvironment, out, Argument(aArguments,1));
    CHK_ISSTRING(out,1);

    LispCharPtr str = out.Get()->String()->String();
    str++;
    if (str[0] != '<')
    {
        aResult.Set(out.Get());
        return;
    }
    CHK_ARG(str[0] == '<',1);
    str++;
    LispCharPtr type = "\"Open\"";

    if (str[0] == '/')
    {
        type = "\"Close\"";
        str++;
    }
    LispString tag;
    tag.SetNrItems(0);
    
    tag.Append('\"');
    while (IsAlpha(*str))
    {
        LispChar c = *str++;
        if (c >= 'a' && c <= 'z')
            c = c + ('A'-'a');
        tag.Append(c);
    }
    tag.Append('\"');
    tag.Append('\0');

    LispObject* info = NULL;

    while (*str == ' ') str++;
    while (*str != '>')
    {
        LispString name;
        name.SetNrItems(0);
        name.Append('\"');
        
//TODO remove?        LispCharPtr start = str;
        while (IsAlpha(*str))
        {
            LispChar c = *str++;
            if (c >= 'a' && c <= 'z')
                c = c + ('A'-'a');
            name.Append(c);
        }
        name.Append('\"');
        name.Append('\0');
//printf("Should be =, is %c\n",str[0]);
        CHK_ARG(str[0] == '=',1);
        str++;
//printf("Should be \", is %c\n",str[0]);
        CHK_ARG(str[0] == '\"',1);
        LispString value;
        value.SetNrItems(0);
        value.Append(*str++);
        while (*str != '\"')
        {
            value.Append(*str++);
        }
        value.Append(*str++);
        value.Append('\0');
//printf("[%s], [%s]\n",name.String(),value.String());
        info =  LIST(LA(ATOML("List")) + LA(ATOML(name.String())) + LA(ATOML(value.String()))) + LA(info);
        while (*str == ' ') str++;

//printf("End is %c\n",str[0]);
        if (*str == '/')
        {
            type = "\"OpenClose\"";
            str++;
            while (*str == ' ') str++;
        }
    }
    
    info = LIST(LA(ATOML("List")) + LA(info));
    aResult.Set(
                LIST(
                     LA(ATOML("XmlTag")) +
                     LA(ATOML(tag.String())) +
                     LA(info) +
                     LA(ATOML(type))
                    )
               );
}


void LispFastAssoc(LispEnvironment& aEnvironment, LispPtr& aResult,
                   LispPtr& aArguments)
{
    // Check that we have two arguments.
    TESTARGS(3);

    // key to find
    LispPtr key;
    InternalEval(aEnvironment, key , Argument(aArguments,1));
    
    // assoc-list to find it in
    LispPtr list;
    InternalEval(aEnvironment, list , Argument(aArguments,2));

    LispObject* t;

    //Check that it is a compound object
    CHK_ARG(list.Get()->SubList() != NULL, 2);
    t = list.Get()->SubList()->Get();
    CHK_ARG(t != NULL, 2);
    t = t->Next().Get();

    while (t != NULL)
    {
        if (t->SubList())
        {
            LispObject* sub = t->SubList()->Get();
            if (sub)
            {
                sub = sub->Next().Get();
                LispPtr temp;
                temp.Set(sub);
                if(InternalEquals(aEnvironment,key,temp))
                {
                    aResult.Set(t);
                    return;
                }
                
            }
        }
        t = t->Next().Get();
    }

    
    aResult.Set(ATOML("Empty"));
}

/*TODO remove!
void LispSetCRemarkReceiver(LispEnvironment& aEnvironment, LispPtr& aResult,
                            LispPtr& aArguments)
{
}
*/


void LispCurrentFile(LispEnvironment& aEnvironment, LispPtr& aResult,
                     LispPtr& aArguments)
{
    // Check that we have zero arguments.
    TESTARGS(1);
    aResult.Set(LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUpStringify(aEnvironment.iInputStatus.FileName())));
}

void LispCurrentLine(LispEnvironment& aEnvironment, LispPtr& aResult,
                     LispPtr& aArguments)
{
    // Check that we have zero arguments.
    TESTARGS(1);
    LispChar s[30];
    InternalIntToAscii(s, aEnvironment.iInputStatus.LineNumber());
    aResult.Set(LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUp(s)));
}

void LispBackQuote(LispEnvironment& aEnvironment, LispPtr& aResult,
                     LispPtr& aArguments)
{
    // Check that we have one argument.
    TESTARGS(2);
    BackQuoteBehaviour behaviour(aEnvironment);
    LispPtr result;
    InternalSubstitute(result, Argument(aArguments, 1), behaviour);
    InternalEval(aEnvironment, aResult, result);
}

// this function is declared in yacasapi.cpp
void LispVersion(LispEnvironment& aEnvironment, LispPtr& aResult,
                 LispPtr& aArguments)
{
    aResult.Set(LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUp("\"" VERSION "\"")));
}


