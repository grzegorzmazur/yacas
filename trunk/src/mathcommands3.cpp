
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
//TODO remove #include "anumber.h"
#include "arrayclass.h"
#include "patternclass.h"
#include "substitute.h"
#include "errors.h"
#include "patcher.h"
#include "platdll.h"
#include "exedll.h"
//TODO remove #include "unipoly.h"

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
#define RESULT aEnvironment.iStack.GetElement(aStackTop)
#define ARGUMENT(i) aEnvironment.iStack.GetElement(aStackTop+i)



#ifndef NO_USE_BIGFLOAT

/// Construct a BigNumber from one of the arguments.
/// \param x (on output) the constructed bignumber
/// \param aEnvironment the current environment
/// \param aStackTop the index of the top of the stack
/// \param aArgNr the index of the argument to be converted
void GetNumber(RefPtr<BigNumber>& x, LispEnvironment& aEnvironment, LispInt aStackTop, LispInt aArgNr)
{
    /*??? */
    x = ARGUMENT(aArgNr).Get()->Number(aEnvironment.Precision());
    CHK_ARG_CORE(x.Ptr() != NULL,aArgNr);
/*TODO remove old?
    RefPtr<BigNumber> num; 
    num = ARGUMENT(aArgNr).Get()->Number(aEnvironment.Precision());
    CHK_ARG_CORE(num.Ptr() != NULL,aArgNr);
    x = num;
*/
}
#endif // USE_BIGFLOAT

//FIXME remove these
void LispArithmetic2(LispEnvironment& aEnvironment, LispInt aStackTop,
                     LispObject* (*func)(LispObject* f1, LispObject* f2,LispEnvironment& aEnvironment,LispInt aPrecision),
                    LispBoolean arbbase=LispFalse);

void LispArithmetic1(LispEnvironment& aEnvironment, LispInt aStackTop,
                     LispObject* (*func)(LispObject* f1, LispEnvironment& aEnvironment,LispInt aPrecision));






//FIXME remove these
void LispArithmetic1(LispEnvironment& aEnvironment, LispInt aStackTop,
                     LispObject* (*func)(LispObject* f1, LispEnvironment& aEnvironment,LispInt aPrecision))
{
    CHK_ARG_CORE(ARGUMENT(1).Get()->Number(0),1);
    RESULT.Set(func(ARGUMENT(1).Get(), aEnvironment, aEnvironment.Precision())); 
}


//FIXME remove these
void LispArithmetic2(LispEnvironment& aEnvironment, LispInt aStackTop,
                     LispObject* (*func)(LispObject* f1, LispObject* f2,LispEnvironment& aEnvironment,LispInt aPrecision),
                    LispBoolean arbbase)
{
    if (!arbbase)
    {
        CHK_ARG_CORE(ARGUMENT(1).Get()->Number(0) ,1);
        CHK_ARG_CORE(ARGUMENT(2).Get()->Number(0) ,2);
    }
    RESULT.Set(func(ARGUMENT(1).Get(),ARGUMENT(2).Get(),
                                   aEnvironment,
                                   aEnvironment.Precision())); 
}


void LispDumpBigNumberDebugInfo(LispEnvironment& aEnvironment, LispInt aStackTop)
{
#ifndef NO_USE_BIGFLOAT
  RefPtr<BigNumber> x;
  GetNumber(x,aEnvironment, aStackTop, 1);
  x->DumpDebugInfo();
#endif
  InternalTrue(aEnvironment,RESULT);
}

void LispMultiply(LispEnvironment& aEnvironment, LispInt aStackTop)
{
#ifndef NO_USE_BIGFLOAT
      RefPtr<BigNumber> x;
      RefPtr<BigNumber> y;
      GetNumber(x,aEnvironment, aStackTop, 1);
      GetNumber(y,aEnvironment, aStackTop, 2);
      BigNumber *z = NEW BigNumber(aEnvironment.BinaryPrecision());
      z->Multiply(*x.Ptr(),*y.Ptr(),aEnvironment.BinaryPrecision());
      RESULT.Set(NEW LispNumber(z));
      return;
#endif // USE_BIGFLOAT
}

//TODO we need to have Gcd in BigNumber!
void LispGcd(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    CHK_ARG_CORE(ARGUMENT(1).Get()->Number(0) ,1);
    CHK_ARG_CORE(ARGUMENT(1).Get()->Number(0) ,2);
    RESULT.Set(GcdInteger(ARGUMENT(1).Get(),ARGUMENT(2).Get(),aEnvironment));
}


/// Corresponds to the Yacas function \c MathAdd.
/// If called with one argument (unary plus), this argument is
/// converted to BigNumber. If called with two arguments (binary plus),
/// both argument are converted to a BigNumber, and these are added
/// together at the current precision. The sum is returned.
/// \sa GetNumber(), BigNumber::Add()
void LispAdd(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispInt length = InternalListLength(ARGUMENT(0));
    if (length == 2)
    {
#ifndef NO_USE_BIGFLOAT
      RefPtr<BigNumber> x;
      GetNumber(x,aEnvironment, aStackTop, 1);
      RESULT.Set(NEW LispNumber(x.Ptr()));
      return;
#endif
    }
    else
    {
#ifndef NO_USE_BIGFLOAT
      RefPtr<BigNumber> x;
      RefPtr<BigNumber> y;
      GetNumber(x,aEnvironment, aStackTop, 1);
      GetNumber(y,aEnvironment, aStackTop, 2);
      LispInt bin = aEnvironment.BinaryPrecision();
      BigNumber *z = NEW BigNumber(bin);
      z->Add(*x.Ptr(),*y.Ptr(),aEnvironment.BinaryPrecision());
      RESULT.Set(NEW LispNumber(z));
      return;
#endif // USE_BIGFLOAT
    }
}

void LispSubtract(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispInt length = InternalListLength(ARGUMENT(0));
    if (length == 2)
    {
#ifndef NO_USE_BIGFLOAT
      RefPtr<BigNumber> x;
      GetNumber(x,aEnvironment, aStackTop, 1);
      BigNumber *z = NEW BigNumber(*x.Ptr()/*aEnvironment.BinaryPrecision()*/);
      z->Negate(*z /* *x.Ptr() */);
      RESULT.Set(NEW LispNumber(z));
      return;
#endif
    }
    else
    {
#ifndef NO_USE_BIGFLOAT
      RefPtr<BigNumber> x;
      RefPtr<BigNumber> y;
      GetNumber(x,aEnvironment, aStackTop, 1);
      GetNumber(y,aEnvironment, aStackTop, 2);
      BigNumber yneg(*y.Ptr());
      yneg.Negate(yneg);
      BigNumber *z = NEW BigNumber(aEnvironment.BinaryPrecision());
      z->Add(*x.Ptr(),yneg,aEnvironment.BinaryPrecision());
      RESULT.Set(NEW LispNumber(z));
      return;
#endif // USE_BIGFLOAT
    }
}


void LispDivide(LispEnvironment& aEnvironment, LispInt aStackTop)
{
// Serge, what was the deal again with divide, floats and integers mixed in the same function?
//	yes, divide works differently on integers and on floats -- see new.chapt -- Serge
#ifndef NO_USE_BIGFLOAT
      RefPtr<BigNumber> x;
      RefPtr<BigNumber> y;
      GetNumber(x,aEnvironment, aStackTop, 1);
      GetNumber(y,aEnvironment, aStackTop, 2);
      BigNumber *z = NEW BigNumber(aEnvironment.BinaryPrecision());
	  // if both arguments are integers, then BigNumber::Divide would perform an integer divide, but we want a float divide here.
	  if (x.Ptr()->IsInt() && y.Ptr()->IsInt())
	  {
		  // why can't we just say BigNumber temp; ?
		  BigNumber tempx(aEnvironment.BinaryPrecision());
		  tempx.SetTo(*x.Ptr());
		  tempx.BecomeFloat(aEnvironment.BinaryPrecision());	// coerce x to float
		  BigNumber tempy(aEnvironment.BinaryPrecision());
		  tempy.SetTo(*y.Ptr());
		  tempy.BecomeFloat(aEnvironment.BinaryPrecision());	// coerce x to float
      z->Divide(tempx, tempy,aEnvironment.BinaryPrecision());
	  }
	  else
	  {
		  z->Divide(*x.Ptr(), *y.Ptr(),aEnvironment.BinaryPrecision());
	  }
	  RESULT.Set(NEW LispNumber(z));
      return;
#endif // USE_BIGFLOAT
}


void LispArcCos(LispEnvironment& aEnvironment, LispInt aStackTop)
{//FIXME move to scripts
    LispArithmetic1(aEnvironment, aStackTop, ArcCosFloat);
}

void LispArcTan(LispEnvironment& aEnvironment, LispInt aStackTop)
{//FIXME move to scripts
    LispArithmetic1(aEnvironment, aStackTop, ArcTanFloat);
}

void LispSqrt(LispEnvironment& aEnvironment, LispInt aStackTop)
{//FIXME move to scripts
    LispArithmetic1(aEnvironment, aStackTop, SqrtFloat);
}


#ifndef NO_USE_BIGFLOAT
#define UNARYFUNCTION(LispName, BigNumName, OldName) \
void LispName(LispEnvironment& aEnvironment, LispInt aStackTop) \
{ \
      RefPtr<BigNumber> x; \
      GetNumber(x,aEnvironment, aStackTop, 1); \
      BigNumber *z = NEW BigNumber( /*???*/ *x.Ptr() /*TODO remove? aEnvironment.BinaryPrecision()*/); \
      z->BigNumName(/*???*/ *z /*TODO remove? *x.Ptr() */); \
      RESULT.Set(NEW LispNumber(z)); \
}
#define BINARYFUNCTION(LispName, BigNumName, OldName) \
void LispName(LispEnvironment& aEnvironment, LispInt aStackTop) \
{ \
      RefPtr<BigNumber> x; \
      RefPtr<BigNumber> y; \
      GetNumber(x,aEnvironment, aStackTop, 1); \
      GetNumber(y,aEnvironment, aStackTop, 2); \
      BigNumber *z = NEW BigNumber(aEnvironment.BinaryPrecision()); \
      z->BigNumName(*x.Ptr(), *y.Ptr()); \
      RESULT.Set(NEW LispNumber(z)); \
}
#else
#define UNARYFUNCTION(LispName, BigNumName, OldName) \
void LispName(LispEnvironment& aEnvironment, LispInt aStackTop) \
{ \
    LispArithmetic1(aEnvironment, aStackTop, OldName); \
}
#define BINARYFUNCTION(LispName, BigNumName, OldName) \
void LispName(LispEnvironment& aEnvironment, LispInt aStackTop) \
{ \
    LispArithmetic2(aEnvironment, aStackTop, OldName); \
}
#endif

UNARYFUNCTION(LispFloor, Floor, FloorFloat)
UNARYFUNCTION(LispMathNegate, Negate, NegateFloat)

/** the macro
	UNARYFUNCTION(LispFloor, Floor, FloorFloat)
is used to help interface Yacas with BigNumber. Suppose we need to access a unary function named 'Floor' in BigNumber and 'LispFloor' here, with 'FloorFloat' the backup function for no BigNumber support.
The macro produces the following equivalent code for the unary function:
void LispFloor(LispEnvironment& aEnvironment, LispInt aStackTop)
{
#ifndef NO_USE_BIGFLOAT
      RefPtr<BigNumber> x;
      GetNumber(x,aEnvironment, aStackTop, 1);
      BigNumber *z = NEW BigNumber(aEnvironment.BinaryPrecision());
      z->Floor(*x.Ptr());
      RESULT.Set(NEW LispNumber(z));
#else
    LispArithmetic1(aEnvironment, aStackTop, FloorFloat);
#endif
}
*/
/* FIXME Eventually the BigNumber support will be stable and we can remove old code and simplify these macros */

/// obtain internal precision data on a number object.
void LispGetExactBits(LispEnvironment& aEnvironment, LispInt aStackTop)
{
#ifndef NO_USE_BIGFLOAT
      RefPtr<BigNumber> x;
      GetNumber(x,aEnvironment, aStackTop, 1);
      BigNumber *z = NEW BigNumber(aEnvironment.BinaryPrecision());
      z->SetTo(
	    (x.Ptr()->IsInt())
		? x.Ptr()->BitCount()	// for integers, return the bit count
	  	: x.Ptr()->GetPrecision() 	// for floats, return the precision
      );
      RESULT.Set(NEW LispNumber(z));
#else	// this is not defined without BigNumber, so return something
    RaiseError("Function MathGetExactBits is not available without BigNumber support");
    LispArithmetic1(aEnvironment, aStackTop, FloorFloat);
#endif
}
/// set internal precision data on a number object.
void LispSetExactBits(LispEnvironment& aEnvironment, LispInt aStackTop)
{
#ifndef NO_USE_BIGFLOAT
      RefPtr<BigNumber> x;
      RefPtr<BigNumber> y;
      GetNumber(x,aEnvironment, aStackTop, 1);
      GetNumber(y,aEnvironment, aStackTop, 2);
      BigNumber *z = NEW BigNumber(aEnvironment.BinaryPrecision());
      z->SetTo(*x.Ptr());
	  // do nothing for integers
	  if (!(z->IsInt()))
	    z->Precision((long)(y->Double()));	// segfaults unless y is defined?
      RESULT.Set(NEW LispNumber(z));
#else	// this is not defined without BigNumber, so return something
    RaiseError("Function MathSetExactBits is not available without BigNumber support");
    LispArithmetic1(aEnvironment, aStackTop, FloorFloat);
#endif
}


/// obtain the bit count of a number object.
void LispBitCount(LispEnvironment& aEnvironment, LispInt aStackTop)
{
#ifndef NO_USE_BIGFLOAT
      RefPtr<BigNumber> x;
      GetNumber(x,aEnvironment, aStackTop, 1);
      BigNumber *z = NEW BigNumber(aEnvironment.BinaryPrecision());
      z->SetTo(x.Ptr()->BitCount());
      RESULT.Set(NEW LispNumber(z));
#else	// this is not defined without BigNumber, so return something
    RaiseError("Function MathBitCount is not available without BigNumber support");
    LispArithmetic1(aEnvironment, aStackTop, FloorFloat);
#endif
}

/// obtain the sign of a number object.
void LispMathSign(LispEnvironment& aEnvironment, LispInt aStackTop)
{
#ifndef NO_USE_BIGFLOAT
      RefPtr<BigNumber> x;
      GetNumber(x,aEnvironment, aStackTop, 1);
      BigNumber *z = NEW BigNumber(aEnvironment.BinaryPrecision());
      z->SetTo(x.Ptr()->Sign());
      RESULT.Set(NEW LispNumber(z));
#else	// this is not defined without BigNumber, so return something
    RaiseError("Function MathSign is not available without BigNumber support");
    LispArithmetic1(aEnvironment, aStackTop, FloorFloat);
#endif
}

/// check whether a number object fits into a platform type.
void LispMathIsSmall(LispEnvironment& aEnvironment, LispInt aStackTop)
{
#ifndef NO_USE_BIGFLOAT
      RefPtr<BigNumber> x;
      GetNumber(x,aEnvironment, aStackTop, 1);
      if(x.Ptr()->IsSmall())
	InternalTrue(aEnvironment,RESULT);
      else
	InternalFalse(aEnvironment,RESULT);      
#else	// this is not defined without BigNumber, so return False
    InternalFalse(aEnvironment,RESULT);
#endif
}


void LispCeil(LispEnvironment& aEnvironment, LispInt aStackTop)
{
#ifndef NO_USE_BIGFLOAT
      RefPtr<BigNumber> x;
      GetNumber(x,aEnvironment, aStackTop, 1);
      BigNumber *z = NEW BigNumber(aEnvironment.BinaryPrecision());
      z->Negate(*x.Ptr());
      z->Floor(*z);	// danger: possible exception raised in Floor() leads to a memory leak because z is not destroyed
      z->Negate(*z);
      RESULT.Set(NEW LispNumber(z));
#else
   LispArithmetic1(aEnvironment, aStackTop, CeilFloat);
#endif
}

void LispAbs(LispEnvironment& aEnvironment, LispInt aStackTop)
{
#ifndef NO_USE_BIGFLOAT
      RefPtr<BigNumber> x;
      GetNumber(x,aEnvironment, aStackTop, 1);
      BigNumber *z = NEW BigNumber(aEnvironment.BinaryPrecision());
      z->SetTo(*x.Ptr());
      if (x.Ptr()->Sign()<0)
	      z->Negate(*x.Ptr());
      RESULT.Set(NEW LispNumber(z));
#else
   LispArithmetic1(aEnvironment, aStackTop, AbsFloat);
#endif
}
//BINARYFUNCTION(LispMod, Mod, ModFloat)
/* this will be gone */
void LispMod(LispEnvironment& aEnvironment, LispInt aStackTop)
{//FIXME
    LispArithmetic2(aEnvironment, aStackTop, ModFloat);
}
/* up to here */

void LispDiv(LispEnvironment& aEnvironment, LispInt aStackTop)
{
#ifndef NO_USE_BIGFLOAT
      RefPtr<BigNumber> x;
      RefPtr<BigNumber> y;
      GetNumber(x,aEnvironment, aStackTop, 1);
      GetNumber(y,aEnvironment, aStackTop, 2);
	  if (x.Ptr()->IsInt() && y.Ptr()->IsInt())
	  {	// both integer, perform integer division
    	  BigNumber *z = NEW BigNumber(aEnvironment.BinaryPrecision());
    	  z->Divide(*x.Ptr(),*y.Ptr(),aEnvironment.BinaryPrecision());
    	  RESULT.Set(NEW LispNumber(z));
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
}

void LispLog(LispEnvironment& aEnvironment, LispInt aStackTop)
{//FIXME move to scripts
    LispArithmetic1(aEnvironment, aStackTop, LnFloat);
}


void LispPower(LispEnvironment& aEnvironment, LispInt aStackTop)
{//FIXME move to scripts
    LispArithmetic2(aEnvironment, aStackTop, PowerFloat);
}



void LispFac(LispEnvironment& aEnvironment, LispInt aStackTop)
{//FIXME move to scripts
    LispArithmetic1(aEnvironment, aStackTop, LispFactorial);
}


// platform functions, taking/returning a platform int/float

void LispFastIsPrime(LispEnvironment& aEnvironment, LispInt aStackTop)
{//FIXME
#ifndef NO_USE_BIGFLOAT
      RefPtr<BigNumber> x;
      GetNumber(x,aEnvironment, aStackTop, 1);
      long result = primes_table_check((unsigned long)(x->Double()));
      BigNumber *z = NEW BigNumber(aEnvironment.BinaryPrecision());
      z->SetTo(result);
      RESULT.Set(NEW LispNumber(z));
#else
    LispArithmetic1(aEnvironment, aStackTop, PlatIsPrime);
#endif
}

// define a macro to replace all platform math functions
#ifndef NO_USE_BIGFLOAT

  #ifndef HAVE_MATH_H
  // this warning is here just to be sure what we are compiling
    #warning do not have math.h
    #define PLATFORM_UNARY(LispName, PlatformName, LispBackupName, OldName) \
void LispName(LispEnvironment& aEnvironment, LispInt aStackTop) \
{ \
      LispBackupName(aEnvironment, aStackTop); \
}
    #define PLATFORM_BINARY(LispName, PlatformName, LispBackupName, OldName) \
void LispName(LispEnvironment& aEnvironment, LispInt aStackTop) \
{ \
      LispBackupName(aEnvironment, aStackTop); \
}
  #else	// HAVE_MATH_H
    #define PLATFORM_UNARY(LispName, PlatformName, LispBackupName, OldName) \
void LispName(LispEnvironment& aEnvironment, LispInt aStackTop) \
{ \
      RefPtr<BigNumber> x; \
      GetNumber(x,aEnvironment, aStackTop, 1); \
      double result = PlatformName(x->Double()); \
      BigNumber *z = NEW BigNumber(aEnvironment.BinaryPrecision()); \
      z->SetTo(result); \
      RESULT.Set(NEW LispNumber(z)); \
}
    #define PLATFORM_BINARY(LispName, PlatformName, LispBackupName, OldName) \
void LispName(LispEnvironment& aEnvironment, LispInt aStackTop) \
{ \
      RefPtr<BigNumber> x, y; \
      GetNumber(x,aEnvironment, aStackTop, 1); \
      GetNumber(y,aEnvironment, aStackTop, 2); \
      double result = PlatformName(x->Double(), y->Double()); \
      BigNumber *z = NEW BigNumber(aEnvironment.BinaryPrecision()); \
      z->SetTo(result); \
      RESULT.Set(NEW LispNumber(z)); \
}
  #endif
#else	// NO_USE_BIGFLOAT
  // this warning is here just to be sure what we are compiling
  #warning not using BigNumber:: functions
  #define PLATFORM_UNARY(LispName, PlatformName, LispBackupName, OldName) \
void LispName(LispEnvironment& aEnvironment, LispInt aStackTop) \
{ \
    LispArithmetic1(aEnvironment, aStackTop, OldName);
}
  #define PLATFORM_BINARY(LispName, PlatformName, LispBackupName, OldName) \
void LispName(LispEnvironment& aEnvironment, LispInt aStackTop) \
{ \
    LispArithmetic2(aEnvironment, aStackTop, OldName);
}
#endif

// now we can define all such functions, e.g.:
//	PLATFORM_UNARY(LispFastSin, sin, LispSin, PlatSin)
// this will generate the following equivalent code:
/*

void LispFastSin(LispEnvironment& aEnvironment, LispInt aStackTop)
{
#ifndef NO_USE_BIGFLOAT
#ifdef HAVE_MATH_H
      RefPtr<BigNumber> x;
      GetNumber(x,aEnvironment, aStackTop, 1);
      double result = sin(x->Double());
      BigNumber *z = NEW BigNumber(aEnvironment.BinaryPrecision());
      z->SetTo(result);
      RESULT.Set(NEW LispNumber(z));
#else
      LispSin(aEnvironment, aStackTop);
#endif
      return;
#endif

    LispArithmetic1(aEnvironment, aStackTop, PlatSin);
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
void LispFastCos(LispEnvironment& aEnvironment, LispInt aStackTop)
{//FIXME
    LispArithmetic1(aEnvironment, aStackTop, PlatCos);
}
void LispFastTan(LispEnvironment& aEnvironment, LispInt aStackTop)
{//FIXME
    LispArithmetic1(aEnvironment, aStackTop, PlatTan);
}

void LispFastArcSin(LispEnvironment& aEnvironment, LispInt aStackTop)
{//FIXME
    LispArithmetic1(aEnvironment, aStackTop, PlatArcSin);
}
void LispFastArcCos(LispEnvironment& aEnvironment, LispInt aStackTop)
{//FIXME
    LispArithmetic1(aEnvironment, aStackTop, PlatArcCos);
}
void LispFastArcTan(LispEnvironment& aEnvironment, LispInt aStackTop)
{//FIXME
    LispArithmetic1(aEnvironment, aStackTop, PlatArcTan);
}
void LispFastExp(LispEnvironment& aEnvironment, LispInt aStackTop)
{//FIXME
    LispArithmetic1(aEnvironment, aStackTop, PlatExp);
}
void LispFastLog(LispEnvironment& aEnvironment, LispInt aStackTop)
{//FIXME
    LispArithmetic1(aEnvironment, aStackTop, PlatLn);
}

void LispFastPower(LispEnvironment& aEnvironment, LispInt aStackTop)
{//FIXME
    LispArithmetic2(aEnvironment, aStackTop, PlatPower);
}

void LispFastSqrt(LispEnvironment& aEnvironment, LispInt aStackTop)
{//FIXME
    LispArithmetic1(aEnvironment, aStackTop, PlatSqrt);
}

void LispFastFloor(LispEnvironment& aEnvironment, LispInt aStackTop)
{//FIXME
    LispArithmetic1(aEnvironment, aStackTop, PlatFloor);
}

void LispFastCeil(LispEnvironment& aEnvironment, LispInt aStackTop)
{//FIXME
    LispArithmetic1(aEnvironment, aStackTop, PlatCeil);
}

void LispFastMod(LispEnvironment& aEnvironment, LispInt aStackTop)
{//FIXME
    LispArithmetic2(aEnvironment, aStackTop, PlatMod);
}

void LispFastAbs(LispEnvironment& aEnvironment, LispInt aStackTop)
{//FIXME
    LispArithmetic1(aEnvironment, aStackTop, PlatAbs);
}
up to here */

/*
BINARYFUNCTION(LispShiftLeft, ShiftLeft, ShiftLeft)
BINARYFUNCTION(LispShiftRight, ShiftRight, ShiftRight)
*/

BINARYFUNCTION(LispBitAnd, BitAnd, BitAnd)
BINARYFUNCTION(LispBitOr, BitOr, BitOr)
BINARYFUNCTION(LispBitXor, BitXor, BitXor)
/*
// BitNot not yet in yacasapi etc.
//BINARYFUNCTION(LispBitNot, BitNot, BitNot)
*/
/* this will be gone */
void LispShiftLeft(LispEnvironment& aEnvironment, LispInt aStackTop)
{//FIXME
    LispArithmetic2(aEnvironment, aStackTop, ShiftLeft);
}
void LispShiftRight(LispEnvironment& aEnvironment, LispInt aStackTop)
{//FIXME
    LispArithmetic2(aEnvironment, aStackTop, ShiftRight);
}


/* up to here */


void LispFromBase(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    //TESTARGS(3);

    // Get the base to convert to:
    // Evaluate first argument, and store result in oper
    LispPtr oper;
    oper.Set(ARGUMENT(1).Get());
    // Check that result is a number, and that it is in fact an integer
    RefPtr<BigNumber> num; num = oper.Get()->Number(aEnvironment.BinaryPrecision());
    CHK_ARG_CORE(num.Ptr() != NULL,1);
	// check that the base is an integer between 2 and 32
    CHK_ARG_CORE(num->IsInt()
        && num->Double() >= BASE2 && num->Double() <= log2_table_range(), 1);

    // Get a short platform integer from the first argument
    LispInt base = (LispInt)(num->Double());

    // Get the number to convert
    LispPtr fromNum;
    fromNum.Set(ARGUMENT(2).Get());
    LispStringPtr str2;
    str2 = fromNum.Get()->String();
    CHK_ARG_CORE(str2 != NULL,2);

    // Added, unquote a string
    CHK_ARG_CORE(InternalIsString(str2),2);
    str2 = aEnvironment.HashTable().LookUpUnStringify(str2->String());

    // convert using correct base
	// FIXME: API breach, must pass precision in base digits and not in bits!
	// if converting an integer, the precision argument is ignored,
	// but if converting a float, need to use bits_to_digits(BinaryPrecision, base)
    BigNumber *z = NEW BigNumber(str2->String(),aEnvironment.BinaryPrecision(),base);
    RESULT.Set(NEW LispNumber(z));
}
void LispToBase(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    //TESTARGS(3);

    // Get the base to convert to:
    // Evaluate first argument, and store result in oper
    LispPtr oper;
    oper.Set(ARGUMENT(1).Get());
    // Check that result is a number, and that it is in fact an integer
    RefPtr<BigNumber> num; num = oper.Get()->Number(aEnvironment.BinaryPrecision());
    CHK_ARG_CORE(num.Ptr() != NULL,1);
	// check that the base is an integer between 2 and 32
    CHK_ARG_CORE(num->IsInt()
        && num->Double() >= BASE2 && num->Double() <= log2_table_range(), 1);

    // Get a short platform integer from the first argument
    LispInt base = (LispInt)(num->Double());

    // Get the number to convert
    RefPtr<BigNumber> x;
    GetNumber(x,aEnvironment, aStackTop, 2);

    // convert using correct base
    LispString str;
	// FIXME: API breach, must pass precision in base digits and not in bits!
	// if converting an integer, the precision argument is ignored,
	// but if converting a float, need to use bits_to_digits(BinaryPrecision, base)
    x->ToString(str,aEnvironment.BinaryPrecision(),base);
    // Get unique string from hash table, and create an atom from it.

    RESULT.Set(LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUpStringify(str.String())->String()));
}



void LispApplyPure(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    //TESTARGS(3);

    LispPtr oper;
    oper.Set(ARGUMENT(1).Get());
    LispPtr args;
    args.Set(ARGUMENT(2).Get());

    CHK_ARG_CORE(args.Get()->SubList() != NULL,2);
    CHK_CORE(args.Get()->SubList()->Get() != NULL,2);

    

    // Apply a pure string
    if (oper.Get()->String() != NULL)
    {
        InternalApplyString(aEnvironment, RESULT,
                    oper.Get()->String(),
                    args.Get()->SubList()->Get()->Next());
    }
    else
    {   // Apply a pure function {args,body}.
        LispPtr args2;
        args2.Set(args.Get()->SubList()->Get()->Next().Get());
        CHK_ARG_CORE(oper.Get()->SubList() != NULL,1);
        CHK_ARG_CORE(oper.Get()->SubList()->Get() != NULL,1);
        InternalApplyPure(oper,args2,RESULT,aEnvironment);
    }
}


void LispPrettyPrinter(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispInt nrArguments = InternalListLength(ARGUMENT(0));

    if (nrArguments == 1)
    {
        aEnvironment.SetPrettyPrinter(NULL);
    }
    else
    {
        CHK_CORE(nrArguments == 2,KLispErrWrongNumberOfArgs);
        LispPtr oper;
        oper.Set(Argument(ARGUMENT(0),1).Get());
        CHK_ISSTRING_CORE(oper,1);
        aEnvironment.SetPrettyPrinter(oper.Get()->String());
    }
    InternalTrue(aEnvironment,RESULT);
}

void LispGetPrettyPrinter(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  if (aEnvironment.PrettyPrinter() == NULL)
    RESULT.Set(LispAtom::New(aEnvironment,"\"\""));
  else
    RESULT.Set(LispAtom::New(aEnvironment,aEnvironment.PrettyPrinter()->String()));
}



void LispGarbageCollect(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    //TESTARGS(1);
    aEnvironment.HashTable().GarbageCollect();
    
    InternalTrue(aEnvironment,RESULT);
}

void LispLazyGlobal(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    //TESTARGS(2);
    LispStringPtr string = ARGUMENT(1).Get()->String();
    CHK_ARG_CORE(string != NULL, 1);
    aEnvironment.SetGlobalEvaluates(string);
    InternalTrue(aEnvironment,RESULT);

}

void LispPatchLoad(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    //TESTARGS(2);

    LispPtr evaluated;
    evaluated.Set(ARGUMENT(1).Get());

    LispStringPtr string = evaluated.Get()->String();
    CHK_ARG_CORE(string != NULL, 1);

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
    InternalTrue(aEnvironment,RESULT);
}

void LispPatchString(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    //TESTARGS(2);

    LispPtr evaluated;
    evaluated.Set(ARGUMENT(1).Get());
    
    LispStringPtr string = evaluated.Get()->String();
    CHK_ARG_CORE(string != NULL, 1);
    LispString oper;
    InternalUnstringify(oper, string);

    LispString str;
    StringOutput newOutput(str);

    LispLocalOutput localOutput(aEnvironment, &newOutput);

    PatchLoad(&oper[0], newOutput, aEnvironment);

    RESULT.Set(LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUpStringify(str.String())->String()));
}

void LispDllLoad(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  //TESTARGS(2);

  LispPtr evaluated;
  evaluated.Set(ARGUMENT(1).Get());

  LispStringPtr string = evaluated.Get()->String();
  CHK_ARG_CORE(string != NULL, 1);

  LispString oper;
  InternalUnstringify(oper, string);
  LispDllBase *dll;
  LispInt opened = LispFalse;

  ExePluginMaker maker = FindExePlugin(&oper[0]);
  if (maker != NULL)
  {
    dll  = NEW ExeDll(maker);
    Check(dll != NULL,KLispErrNotEnoughMemory);
  }
  else
  {
    dll  = NEW DLLCLASS;
    Check(dll != NULL,KLispErrNotEnoughMemory);
  }
  opened = dll->Open(&oper[0],aEnvironment);
  if (!opened) delete dll;
  Check(opened,KLispErrLibraryNotFound);
  aEnvironment.iDlls.Append(dll);
  InternalTrue(aEnvironment,RESULT);
}

void LispDllUnload(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    //TESTARGS(2);
    LispPtr evaluated;
    evaluated.Set(ARGUMENT(1).Get());

    LispStringPtr string = evaluated.Get()->String();
    CHK_ARG_CORE(string != NULL, 1);

    LispString oper;
    InternalUnstringify(oper, string);
    aEnvironment.iDlls.DeleteNamed(&oper[0],aEnvironment);

    InternalTrue(aEnvironment,RESULT);
}

void LispDllEnumerate(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    //TESTARGS(1);
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
    RESULT.Set(LIST(LA(ATOML("List")) + LA(res)));
}

void LispSetExtraInfo(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    //TESTARGS(3);

    LispPtr object;
    object.Set(ARGUMENT(1).Get());

    LispPtr info;
    info.Set(ARGUMENT(2).Get());

    RESULT.Set( object.Get()->SetExtraInfo(info) );
}


void LispGetExtraInfo(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    //TESTARGS(2);

    LispPtr object;
    object.Set(ARGUMENT(1).Get());

    LispPtr* result = object.Get()->ExtraInfo();
    if (result == NULL)
    {
        InternalFalse(aEnvironment,RESULT);
    }
    else if (result->Get() == NULL)
    {
        InternalFalse(aEnvironment,RESULT);
    }
    else
    {
        RESULT.Set(result->Get());
    }
}

/*TODO remove
void LispBerlekamp(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    //TESTARGS(3);
    LispPtr polyobj;
    polyobj.Set(ARGUMENT(1).Get());
    LispPtr primeobj;
    primeobj.Set(ARGUMENT(2).Get());

    LispInt prime = InternalAsciiToInt(primeobj.Get()->String()->String());

    ZZPoly poly;
    CHK_ARG_CORE(polyobj.Get()->SubList() != NULL, 1);
    LispPtr hold;
    hold.Set(polyobj.Get());
    polyobj.Set(polyobj.Get()->SubList()->Get());

    CHK_ARG_CORE(polyobj.Get() != NULL, 1);
    polyobj.Set(polyobj.Get()->Next().Get());

    while (polyobj.Get() != NULL)
    {
        ZZ fact;
        CHK_ARG_CORE(polyobj.Get()->String() != NULL, 1);
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
    RESULT.Set(LIST(LA(ATOML("List")) + LA(res)));
}
*/


void LispDefaultTokenizer(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    //TESTARGS(1);
    aEnvironment.iCurrentTokenizer = &aEnvironment.iDefaultTokenizer;
    InternalTrue(aEnvironment,RESULT);
}

void LispCommonLispTokenizer(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    //TESTARGS(1);
    aEnvironment.iCurrentTokenizer = &aEnvironment.iCommonLispTokenizer;
    InternalTrue(aEnvironment,RESULT);
}
void LispCTokenizer(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    //TESTARGS(1);
    aEnvironment.iCurrentTokenizer = &aEnvironment.iCTokenizer;
    InternalTrue(aEnvironment,RESULT);
}

void LispXmlTokenizer(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    //TESTARGS(1);
    aEnvironment.iCurrentTokenizer = &aEnvironment.iXmlTokenizer;
    InternalTrue(aEnvironment,RESULT);
}

void LispExplodeTag(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    //TESTARGS(2);
    LispPtr out;
    out.Set(ARGUMENT(1).Get());
    CHK_ISSTRING_CORE(out,1);

    LispCharPtr str = out.Get()->String()->String();
    str++;
    if (str[0] != '<')
    {
        RESULT.Set(out.Get());
        return;
    }
    CHK_ARG_CORE(str[0] == '<',1);
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
    while (*str != '>' && *str != '/')
    {
        LispString name;
        name.SetNrItems(0);
        name.Append('\"');

        while (IsAlpha(*str))
        {
            LispChar c = *str++;
            if (c >= 'a' && c <= 'z')
                c = c + ('A'-'a');
            name.Append(c);
        }
        name.Append('\"');
        name.Append('\0');
        CHK_ARG_CORE(str[0] == '=',1);
        str++;
        CHK_ARG_CORE(str[0] == '\"',1);
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
    }
    if (*str == '/')
    {
      type = "\"OpenClose\"";
      str++;
      while (*str == ' ') str++;
    }
    
    info = LIST(LA(ATOML("List")) + LA(info));
    RESULT.Set(
                LIST(
                     LA(ATOML("XmlTag")) +
                     LA(ATOML(tag.String())) +
                     LA(info) +
                     LA(ATOML(type))
                    )
               );
}


void LispFastAssoc(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    // Check that we have two arguments.
    //TESTARGS(3);

    // key to find
    LispPtr key;
    key.Set(ARGUMENT(1).Get());
    
    // assoc-list to find it in
    LispPtr list;
    list.Set(ARGUMENT(2).Get());

    LispObject* t;

    //Check that it is a compound object
    CHK_ARG_CORE(list.Get()->SubList() != NULL, 2);
    t = list.Get()->SubList()->Get();
    CHK_ARG_CORE(t != NULL, 2);
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
                    RESULT.Set(t);
                    return;
                }
                
            }
        }
        t = t->Next().Get();
    }

    
    RESULT.Set(ATOML("Empty"));
}

void LispCurrentFile(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    // Check that we have zero arguments.
    //TESTARGS(1);
    RESULT.Set(LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUpStringify(aEnvironment.iInputStatus.FileName())->String()));
}

void LispCurrentLine(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    // Check that we have zero arguments.
    //TESTARGS(1);
    LispChar s[30];
    InternalIntToAscii(s, aEnvironment.iInputStatus.LineNumber());
    RESULT.Set(LispAtom::New(aEnvironment,s));
}

void LispBackQuote(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    // Check that we have one argument.
    //TESTARGS(2);
    BackQuoteBehaviour behaviour(aEnvironment);
    LispPtr result;
    InternalSubstitute(result, ARGUMENT( 1), behaviour);
    InternalEval(aEnvironment, RESULT, result);
}

// this function is declared in yacasapi.cpp
void LispVersion(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    RESULT.Set(LispAtom::New(aEnvironment,"\"" VERSION "\""));
}

/// convert bits to digits. Use the kernel function bits_to_digits. Arguments must be small integers.
void LispBitsToDigits(LispEnvironment& aEnvironment, LispInt aStackTop)
{
#ifndef NO_USE_BIGFLOAT
      RefPtr<BigNumber> x;
      RefPtr<BigNumber> y;
      GetNumber(x,aEnvironment, aStackTop, 1);
      GetNumber(y,aEnvironment, aStackTop, 2);
	  long result = 0;	// initialize just in case
	  if (x->IsInt() && x->IsSmall() && y->IsInt() && y->IsSmall())
	  {
		  // bits_to_digits uses unsigned long, see numbers.h
			unsigned base = unsigned(y->Double());
			result = bits_to_digits((unsigned long)(x->Double()), base);
	  }
	  else
	  {
		  RaiseError("BitsToDigits: error: arguments (%f, %f) must be small integers", x->Double(), y->Double());		  
	  }
      BigNumber *z = NEW BigNumber();
      z->SetTo((long)result);
      RESULT.Set(NEW LispNumber(z));

#else	// this is not defined without BigNumber, so return something
    RaiseError("Function BitsToDigits is not available without BigNumber support");
    LispArithmetic1(aEnvironment, aStackTop, FloorFloat);
#endif
}

/// convert digits to bits. Use the kernel function digits_to_bits. Arguments must be small integers.
void LispDigitsToBits(LispEnvironment& aEnvironment, LispInt aStackTop)
{
#ifndef NO_USE_BIGFLOAT
      RefPtr<BigNumber> x;
      RefPtr<BigNumber> y;
      GetNumber(x,aEnvironment, aStackTop, 1);
      GetNumber(y,aEnvironment, aStackTop, 2);
	  long result = 0;	// initialize just in case
	  if (x->IsInt() && x->IsSmall() && y->IsInt() && y->IsSmall())
	  {
		  // bits_to_digits uses unsigned long, see numbers.h
			unsigned base = unsigned(y->Double());
			result = digits_to_bits((unsigned long)(x->Double()), base);
	  }
	  else
	  {
		  RaiseError("DigitsToBits: error: arguments (%f, %f) must be small integers", x->Double(), y->Double());
	  }
      BigNumber *z = NEW BigNumber();
      z->SetTo((long)result);
      RESULT.Set(NEW LispNumber(z));

#else	// this is not defined without BigNumber, so return something
    RaiseError("Function BitsToDigits is not available without BigNumber support");
    LispArithmetic1(aEnvironment, aStackTop, FloorFloat);
#endif
}

