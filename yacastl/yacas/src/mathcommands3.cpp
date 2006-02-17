
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
#include "codecomment.h"
//TODO remove #include "unipoly.h"

#ifdef HAVE_MATH_H
  #include <math.h>
#endif

#undef VERSION

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
    x = ARGUMENT(aArgNr)->Number(aEnvironment.Precision());
    CHK_ARG_CORE(x,aArgNr);	// was: x.Ptr()
/*TODO remove old?
    RefPtr<BigNumber> num; 
    num = ARGUMENT(aArgNr)->Number(aEnvironment.Precision());
    CHK_ARG_CORE(num.Ptr(),aArgNr);
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
    CHK_ARG_CORE(ARGUMENT(1)->Number(0),1);
    RESULT = (func(ARGUMENT(1), aEnvironment, aEnvironment.Precision())); 
}


//FIXME remove these
void LispArithmetic2(LispEnvironment& aEnvironment, LispInt aStackTop,
                     LispObject* (*func)(LispObject* f1, LispObject* f2,LispEnvironment& aEnvironment,LispInt aPrecision),
                    LispBoolean arbbase)
{
    if (!arbbase)
    {
        CHK_ARG_CORE(ARGUMENT(1)->Number(0) ,1);
        CHK_ARG_CORE(ARGUMENT(2)->Number(0) ,2);
    }
    RESULT = (func(ARGUMENT(1),ARGUMENT(2),
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
    //z->Multiply(*x.ptr(),*y.ptr(),aEnvironment.BinaryPrecision());
      z->Multiply(*x, *y, aEnvironment.BinaryPrecision());
      RESULT = (NEW LispNumber(z));
      return;
#endif // USE_BIGFLOAT
}

//TODO we need to have Gcd in BigNumber!
void LispGcd(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    CHK_ARG_CORE(ARGUMENT(1)->Number(0) ,1);
    CHK_ARG_CORE(ARGUMENT(1)->Number(0) ,2);
    RESULT = (GcdInteger(ARGUMENT(1),ARGUMENT(2),aEnvironment));
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
      RESULT = (NEW LispNumber(x.ptr()));
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
      z->Add(*x.ptr(),*y.ptr(),aEnvironment.BinaryPrecision());
      RESULT = (NEW LispNumber(z));
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
      BigNumber *z = NEW BigNumber(*x/*aEnvironment.BinaryPrecision()*/);
      z->Negate(*z /* *x.Ptr() */);
      RESULT = (NEW LispNumber(z));
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
      BigNumber yneg(*y);
      yneg.Negate(yneg);
      BigNumber *z = NEW BigNumber(aEnvironment.BinaryPrecision());
      z->Add(*x,yneg,aEnvironment.BinaryPrecision());
      RESULT = (NEW LispNumber(z));
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
	  if (x->IsInt() && y->IsInt())
	  {
		  // why can't we just say BigNumber temp; ?
		  BigNumber tempx(aEnvironment.BinaryPrecision());
		  tempx.SetTo(*x);	// was: *x.Ptr()
		  tempx.BecomeFloat(aEnvironment.BinaryPrecision());	// coerce x to float
		  BigNumber tempy(aEnvironment.BinaryPrecision());
		  tempy.SetTo(*y);
		  tempy.BecomeFloat(aEnvironment.BinaryPrecision());	// coerce x to float
      z->Divide(tempx, tempy,aEnvironment.BinaryPrecision());
	  }
	  else
	  {
		  z->Divide(*x, *y,aEnvironment.BinaryPrecision());
	  }
	  RESULT = (NEW LispNumber(z));
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
      BigNumber *z = NEW BigNumber( /*???*/ *x /*TODO remove? aEnvironment.BinaryPrecision()*/); \
      z->BigNumName(/*???*/ *z /*TODO remove? *x.Ptr() */); \
      RESULT = (NEW LispNumber(z)); \
}
#define BINARYFUNCTION(LispName, BigNumName, OldName) \
void LispName(LispEnvironment& aEnvironment, LispInt aStackTop) \
{ \
      RefPtr<BigNumber> x; \
      RefPtr<BigNumber> y; \
      GetNumber(x,aEnvironment, aStackTop, 1); \
      GetNumber(y,aEnvironment, aStackTop, 2); \
      BigNumber *z = NEW BigNumber(aEnvironment.BinaryPrecision()); \
      z->BigNumName(*x, *y); \
      RESULT = (NEW LispNumber(z)); \
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
      RESULT = (NEW LispNumber(z));
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
	    (x->IsInt())
		? x->BitCount()	// for integers, return the bit count
	  	: x->GetPrecision() 	// for floats, return the precision
      );
      RESULT = (NEW LispNumber(z));
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
      z->SetTo(*x);
	  // do nothing for integers
	  if (!(z->IsInt()))
	    z->Precision((long)(y->Double()));	// segfaults unless y is defined?
      RESULT = (NEW LispNumber(z));
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
      z->SetTo(x->BitCount());
      RESULT = (NEW LispNumber(z));
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
      z->SetTo(x->Sign());
      RESULT = (NEW LispNumber(z));
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
      if(x->IsSmall())
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
/*
      LispInt prec = aEnvironment.Precision();
      if (x->iNumber->TensExp > x->iNumber->iPrecision)
      {
        aEnvironment.SetPrecision(x->iNumber->iTensExp);
        GetNumber(x,aEnvironment, aStackTop, 1);
      }
*/
      BigNumber *z = NEW BigNumber(aEnvironment.BinaryPrecision());
      z->Negate(*x);
      z->Floor(*z);	// danger: possible exception raised in Floor() leads to a memory leak because z is not destroyed
      z->Negate(*z);
//      aEnvironment.SetPrecision(prec);
      RESULT = (NEW LispNumber(z));
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
      z->SetTo(*x);
      if (x->Sign()<0)
	      z->Negate(*x);
      RESULT = (NEW LispNumber(z));
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
	  if (x->IsInt() && y->IsInt())
	  {	// both integer, perform integer division
    	  BigNumber *z = NEW BigNumber(aEnvironment.BinaryPrecision());
    	  z->Divide(*x,*y,aEnvironment.BinaryPrecision());
    	  RESULT = (NEW LispNumber(z));
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
/*TODO remove
void LispLog(LispEnvironment& aEnvironment, LispInt aStackTop)
{//FIXME move to scripts
    LispArithmetic1(aEnvironment, aStackTop, LnFloat);
}
*/

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
      RESULT = (NEW LispNumber(z));
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
      RESULT = (NEW LispNumber(z)); \
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
      RESULT = (NEW LispNumber(z)); \
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
      RESULT = (NEW LispNumber(z));
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
    LispPtr oper(ARGUMENT(1));
    // Check that result is a number, and that it is in fact an integer
    RefPtr<BigNumber> num; num = oper->Number(aEnvironment.BinaryPrecision());
    CHK_ARG_CORE(num,1);	// was: num.Ptr()
	// check that the base is an integer between 2 and 32
    CHK_ARG_CORE(num->IsInt()
        && num->Double() >= BASE2 && num->Double() <= log2_table_range(), 1);

    // Get a short platform integer from the first argument
    LispInt base = (LispInt)(num->Double());

    // Get the number to convert
    LispPtr fromNum(ARGUMENT(2));
    LispString * str2 = fromNum->String();
    CHK_ARG_CORE(str2,2);

    // Added, unquote a string
    CHK_ARG_CORE(InternalIsString(str2),2);
    str2 = aEnvironment.HashTable().LookUpUnStringify(str2->c_str());

    // convert using correct base
	// FIXME: API breach, must pass precision in base digits and not in bits!
	// if converting an integer, the precision argument is ignored,
	// but if converting a float, need to use bits_to_digits(BinaryPrecision, base)
    BigNumber *z = NEW BigNumber(str2->c_str(),aEnvironment.BinaryPrecision(),base);
    RESULT = (NEW LispNumber(z));
}
void LispToBase(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    //TESTARGS(3);

    // Get the base to convert to:
    // Evaluate first argument, and store result in oper
    LispPtr oper(ARGUMENT(1));
    // Check that result is a number, and that it is in fact an integer
    RefPtr<BigNumber> num; num = oper->Number(aEnvironment.BinaryPrecision());
    CHK_ARG_CORE(num,1);
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

    RESULT = (LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUpStringify(str.c_str())->c_str()));
}



void LispApplyPure(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    //TESTARGS(3);

    LispPtr oper(ARGUMENT(1));
    LispPtr args(ARGUMENT(2));

    CHK_ARG_CORE(args->SubList(),2);
    CHK_CORE((*args->SubList()),2);

    

    // Apply a pure string
    if (oper->String())
    {
        InternalApplyString(aEnvironment, RESULT,
                    oper->String(),
                    (*args->SubList())->Nixed());
    }
    else
    {   // Apply a pure function {args,body}.
        LispPtr args2((*args->SubList())->Nixed());
        CHK_ARG_CORE(oper->SubList(),1);
        CHK_ARG_CORE((*oper->SubList()),1);
        InternalApplyPure(oper,args2,RESULT,aEnvironment);
    }
}


void LispPrettyReader(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  LispInt nrArguments = InternalListLength(ARGUMENT(0));

  if (nrArguments == 1)
  {
    aEnvironment.SetPrettyReader(NULL);
  }
  else
  {
    CHK_CORE(nrArguments == 2,KLispErrWrongNumberOfArgs);
    LispPtr oper(ARGUMENT(0));
    oper = oper->Nixed(); // oper.GoNext();	// woof woof woof
    CHK_ISSTRING_CORE(oper,1);
    aEnvironment.SetPrettyReader(oper->String());
  }
  InternalTrue(aEnvironment,RESULT);
}

void LispGetPrettyReader(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  if (!aEnvironment.PrettyReader())
    RESULT = (LispAtom::New(aEnvironment,"\"\""));
  else
    RESULT = (LispAtom::New(aEnvironment,aEnvironment.PrettyReader()->c_str()));
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
    LispPtr oper(ARGUMENT(0));
    oper = oper->Nixed(); // oper.GoNext();	// woof woof woof
    CHK_ISSTRING_CORE(oper,1);
    aEnvironment.SetPrettyPrinter(oper->String());
  }
  InternalTrue(aEnvironment,RESULT);
}

void LispGetPrettyPrinter(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  if (!aEnvironment.PrettyPrinter())
    RESULT = (LispAtom::New(aEnvironment,"\"\""));
  else
    RESULT = (LispAtom::New(aEnvironment,aEnvironment.PrettyPrinter()->c_str()));
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
    LispString * string = ARGUMENT(1)->String();
    CHK_ARG_CORE(string, 1);
    aEnvironment.SetGlobalEvaluates(string);
    InternalTrue(aEnvironment,RESULT);

}

void LispPatchLoad(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    //TESTARGS(2);

    LispPtr evaluated(ARGUMENT(1));

    LispString * string = evaluated->String();
    CHK_ARG_CORE(string, 1);

    LispString oper;
    InternalUnstringify(oper, string);
    LispString * hashedname = aEnvironment.HashTable().LookUp(oper.c_str());

    InputStatus oldstatus = aEnvironment.iInputStatus;
    aEnvironment.iInputStatus.SetTo(hashedname->c_str());

    
    LispLocalFile localFP(aEnvironment, oper.c_str(), LispTrue,
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

    LispPtr evaluated(ARGUMENT(1));
    
    LispString * string = evaluated->String();
    CHK_ARG_CORE(string, 1);
    LispString oper;
    InternalUnstringify(oper, string);

    LispString str;
    StringOutput newOutput(str);

    LispLocalOutput localOutput(aEnvironment, &newOutput);

    PatchLoad(oper.c_str(), newOutput, aEnvironment);

    RESULT = (LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUpStringify(str.c_str())->c_str()));
}

void LispDllLoad(LispEnvironment& aEnvironment, LispInt aStackTop)
{
	//TESTARGS(2);

	LispPtr evaluated(ARGUMENT(1));

	LispString * string = evaluated->String();
	CHK_ARG_CORE(string, 1);

	LispString oper;
	InternalUnstringify(oper, string);
	LispDllBase *dll=NULL;
	LispInt opened = LispFalse;

	ExePluginMaker maker = FindExePlugin(oper.c_str());
	if (maker)
	{
		dll  = NEW ExeDll(maker);
	}
	#ifndef DISABLE_DYNAMIC
	else
	{
		dll  = NEW DLLCLASS;
	}
	#endif	// DISABLE_DYNAMIC
	Check(dll,KLispErrNotEnoughMemory);

	DBG_printf("DLL allocated -- %s\n",oper.c_str());
	aEnvironment.iDlls.Append(dll);	// store ptr now, in case Open never returns!
	opened = dll->Open(oper.c_str(),aEnvironment);
	DBG_printf("DLL opened, opened = %d\n", opened);
	if (!opened) aEnvironment.iDlls.DeleteNamed(oper.c_str(), aEnvironment);
	Check(opened,KLispErrLibraryNotFound);

	DBG_printf("DLL added, %d DLLs loaded\n",aEnvironment.iDlls.Size());

	InternalTrue(aEnvironment,RESULT);
}

void LispDllUnload(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    //TESTARGS(2);
    LispPtr evaluated(ARGUMENT(1));

    LispString * string = evaluated->String();
    CHK_ARG_CORE(string, 1);

    LispString oper;
    InternalUnstringify(oper, string);
    aEnvironment.iDlls.DeleteNamed(oper.c_str(),aEnvironment);

    InternalTrue(aEnvironment,RESULT);
}

void LispDllEnumerate(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    //TESTARGS(1);
    LispInt i;
    LispObject *res = NULL;
    for (i=aEnvironment.iDlls.Size()-1;i>=0;i--)
    {
        LispString orig;
        orig = aEnvironment.iDlls[i]->DllFileName();
        LispString oper;
        InternalStringify(oper, &orig);
        res = LA(ATOML(oper.c_str())) + LA(res);
    }
    RESULT = (LIST(LA(ATOML("List")) + LA(res)));
}

void LispSetExtraInfo(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    //TESTARGS(3);

    LispPtr object(ARGUMENT(1));
    LispPtr info(ARGUMENT(2));
    RESULT = ( object->SetExtraInfo(info) );
}


void LispGetExtraInfo(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    //TESTARGS(2);

    LispPtr object(ARGUMENT(1));

    LispObject* result = object->ExtraInfo();
    if (!result)
    {
        InternalFalse(aEnvironment,RESULT);
    }
    else
    {
        RESULT = ((result));
    }
}

/*TODO remove
void LispBerlekamp(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    //TESTARGS(3);
    LispPtr polyobj(ARGUMENT(1));
    LispPtr primeobj(ARGUMENT(2));

    LispInt prime = InternalAsciiToInt(primeobj->String());

    ZZPoly poly;
    CHK_ARG_CORE(polyobj->SubList(), 1);
    LispPtr hold(polyobj);
    polyobj = ((*polyobj->SubList()));

    CHK_ARG_CORE(polyobj, 1);
    polyobj = (polyobj->Nixed());

    while (polyobj)
    {
        ZZ fact;
        CHK_ARG_CORE(polyobj->String(), 1);
        fact = InternalAsciiToInt(polyobj->String());
        poly.Append(fact);
        polyobj = (polyobj->Nixed());
    }
    ZZPolyList result;
    Berlekamp(result, poly, prime);

    LispInt i;
    LispObject *res = NULL;
    for (i=result.Size()-1;i>=0;i--)
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
    RESULT = (LIST(LA(ATOML("List")) + LA(res)));
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
    LispPtr out(ARGUMENT(1));
    CHK_ISSTRING_CORE(out,1);

    LispChar * str = out->String()->c_str();
    str++;
    if (str[0] != '<')
    {
        RESULT = (out);
        return;
    }
    str++;
#if 0
    LispChar * type = "\"Open\"";
    if (str[0] == '/')
    {
        type = "\"Close\"";
        str++;
    }
#else
	const LispChar * type = (str[0] == '/') ? (str++, "\"Close\"") : "\"Open\"";
#endif
    LispString tag;
    tag.Resize(0);

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
        name.Resize(0);
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
        value.Resize(0);
        value.Append(*str++);
        while (*str != '\"')
        {
            value.Append(*str++);
        }
        value.Append(*str++);
        value.Append('\0');
//printf("[%s], [%s]\n",name.String(),value.String());
        info =  LIST(LA(ATOML("List")) + LA(ATOML(name.c_str())) + LA(ATOML(value.c_str()))) + LA(info);
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
    RESULT = (
                LIST(
                     LA(ATOML("XmlTag")) +
                     LA(ATOML(tag.c_str())) +
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
    LispPtr key(ARGUMENT(1));
    
    // assoc-list to find it in
    LispPtr list(ARGUMENT(2));

    LispObject* t;

    //Check that it is a compound object
    CHK_ARG_CORE(list->SubList(), 2);
    t = (*list->SubList());
    CHK_ARG_CORE(t, 2);
    t = t->Nixed();

    while (t)
    {
        if (t->SubList())
        {
            LispObject* sub = (*t->SubList());
            if (sub)
            {
                sub = sub->Nixed();
                LispPtr temp(sub);
                if(InternalEquals(aEnvironment,key,temp))
                {
                    RESULT = (t);
                    return;
                }
                
            }
        }
        t = t->Nixed();
    }

    
    RESULT = (ATOML("Empty"));
}

void LispCurrentFile(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    // Check that we have zero arguments.
    //TESTARGS(1);
    RESULT = (LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUpStringify(aEnvironment.iInputStatus.FileName())->c_str()));
}

void LispCurrentLine(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    // Check that we have zero arguments.
    //TESTARGS(1);
    LispChar s[30];
    InternalIntToAscii(s, aEnvironment.iInputStatus.LineNumber());
    RESULT = (LispAtom::New(aEnvironment,s));
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
    RESULT = (LispAtom::New(aEnvironment,"\"" VERSION "\""));
}

CodeComment::Type * CodeComment::pCodeComments = 0;

// this function is declared in yacasapi.cpp
void LispCodeComment(LispEnvironment& aEnvironment, LispInt aStackTop)
{
	LispPtr head;
	LispIterator tail(head);
	LispObject* pObj;

	pObj = aEnvironment.iList;
	(*tail) = pObj;
	tail = pObj->Nixed();

	CodeComment::Type * pArrStr = CodeComment::pCodeComments;
	if (pArrStr)
    for (int ii = 0, nn = pArrStr->Size(); ii < nn; ii++)
	{
		LispString orig((*pArrStr)[ii]);
		LispString stringified;
		InternalStringify(stringified, &orig);
		LispObject* pObj = LispAtom::New(aEnvironment,stringified.c_str());
		(*tail) = pObj;
		tail = pObj->Nixed();
	}
    RESULT = (LispSubList::New(head));
}
//namespace{CodeComment varname("CodeComment support added 12/08/2005.");}

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
      z->SetTo(result);
      RESULT = (NEW LispNumber(z));

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
      z->SetTo(result);
      RESULT = (NEW LispNumber(z));

#else	// this is not defined without BigNumber, so return something
    RaiseError("Function BitsToDigits is not available without BigNumber support");
    LispArithmetic1(aEnvironment, aStackTop, FloorFloat);
#endif
}

