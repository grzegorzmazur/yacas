
#include "yacas/yacasprivate.h"

#include "yacas/yacasbase.h"
#include "yacas/lispenvironment.h"
#include "yacas/standard.h"
#include "yacas/lispeval.h"
#include "yacas/lispatom.h"
#include "yacas/lispparser.h"
#include "yacas/stdfileio.h"
#include "yacas/stringio.h"
#include "yacas/lisperror.h"
#include "yacas/infixparser.h"
#include "yacas/lispuserfunc.h"
#include "yacas/mathuserfunc.h"
#include "yacas/platmath.h"
#include "yacas/numbers.h"
#include "yacas/arrayclass.h"
#include "yacas/patternclass.h"
#include "yacas/substitute.h"
#include "yacas/errors.h"
#include "yacas/patcher.h"
#include "yacas/string_utils.h"

#include <cmath>

#include "yacas/yacas_version.h"

#include <sstream>


#define InternalEval aEnvironment.iEvaluator->Eval
#define RESULT aEnvironment.iStack.GetElement(aStackTop)
#define ARGUMENT(i) aEnvironment.iStack.GetElement(aStackTop+i)




/// Construct a BigNumber from one of the arguments.
/// \param x (on output) the constructed bignumber
/// \param aEnvironment the current environment
/// \param aStackTop the index of the top of the stack
/// \param aArgNr the index of the argument to be converted
void GetNumber(RefPtr<BigNumber>& x, LispEnvironment& aEnvironment, LispInt aStackTop, LispInt aArgNr)
{
    x = ARGUMENT(aArgNr)->Number(aEnvironment.Precision());
    CheckArg(x, aArgNr, aEnvironment, aStackTop);
}

//FIXME remove these
void LispArithmetic2(LispEnvironment& aEnvironment, LispInt aStackTop,
                     LispObject* (*func)(LispObject* f1, LispObject* f2,LispEnvironment& aEnvironment,LispInt aPrecision),
                    bool arbbase=false);

void LispArithmetic1(LispEnvironment& aEnvironment, LispInt aStackTop,
                     LispObject* (*func)(LispObject* f1, LispEnvironment& aEnvironment,LispInt aPrecision));






//FIXME remove these
void LispArithmetic1(LispEnvironment& aEnvironment, LispInt aStackTop,
                     LispObject* (*func)(LispObject* f1, LispEnvironment& aEnvironment,LispInt aPrecision))
{
    CheckArg(ARGUMENT(1)->Number(0), 1, aEnvironment, aStackTop);
    RESULT = (func(ARGUMENT(1), aEnvironment, aEnvironment.Precision()));
}


//FIXME remove these
void LispArithmetic2(LispEnvironment& aEnvironment, LispInt aStackTop,
                     LispObject* (*func)(LispObject* f1, LispObject* f2,LispEnvironment& aEnvironment,LispInt aPrecision),
                    bool arbbase)
{
    if (!arbbase)
    {
        CheckArg(ARGUMENT(1)->Number(0), 1, aEnvironment, aStackTop);
        CheckArg(ARGUMENT(2)->Number(0), 2, aEnvironment, aStackTop);
    }
    RESULT = (func(ARGUMENT(1),ARGUMENT(2),
                                   aEnvironment,
                                   aEnvironment.Precision()));
}


void LispDumpBigNumberDebugInfo(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  RefPtr<BigNumber> x;
  GetNumber(x,aEnvironment, aStackTop, 1);
  x->DumpDebugInfo();
  InternalTrue(aEnvironment,RESULT);
}

void LispMultiply(LispEnvironment& aEnvironment, LispInt aStackTop)
{
      RefPtr<BigNumber> x;
      RefPtr<BigNumber> y;
      GetNumber(x,aEnvironment, aStackTop, 1);
      GetNumber(y,aEnvironment, aStackTop, 2);
      BigNumber *z = NEW BigNumber(aEnvironment.BinaryPrecision());
      z->Multiply(*x, *y, aEnvironment.BinaryPrecision());
      RESULT = (NEW LispNumber(z));
      return;
}

//TODO we need to have Gcd in BigNumber!
void LispGcd(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    CheckArg(ARGUMENT(1)->Number(0), 1, aEnvironment, aStackTop);
    CheckArg(ARGUMENT(2)->Number(0), 2, aEnvironment, aStackTop);

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
      RefPtr<BigNumber> x;
      GetNumber(x,aEnvironment, aStackTop, 1);
      RESULT = (NEW LispNumber(x.ptr()));
      return;
    }
    else
    {
      RefPtr<BigNumber> x;
      RefPtr<BigNumber> y;
      GetNumber(x,aEnvironment, aStackTop, 1);
      GetNumber(y,aEnvironment, aStackTop, 2);
      LispInt bin = aEnvironment.BinaryPrecision();
      BigNumber *z = NEW BigNumber(bin);
      z->Add(*x.ptr(),*y.ptr(),aEnvironment.BinaryPrecision());
      RESULT = (NEW LispNumber(z));
      return;
    }
}

void LispSubtract(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispInt length = InternalListLength(ARGUMENT(0));
    if (length == 2)
    {
      RefPtr<BigNumber> x;
      GetNumber(x,aEnvironment, aStackTop, 1);
      BigNumber *z = NEW BigNumber(*x/*aEnvironment.BinaryPrecision()*/);
      z->Negate(*z /* *x.Ptr() */);
      RESULT = (NEW LispNumber(z));
      return;
    }
    else
    {
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
    }
}


void LispDivide(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  // Serge, what was the deal again with divide, floats and integers mixed in the same function?
  //  yes, divide works differently on integers and on floats -- see new.chapt -- Serge
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
    tempx.SetTo(*x);  // was: *x.Ptr()
    tempx.BecomeFloat(aEnvironment.BinaryPrecision());  // coerce x to float
    BigNumber tempy(aEnvironment.BinaryPrecision());
    tempy.SetTo(*y);
    tempy.BecomeFloat(aEnvironment.BinaryPrecision());  // coerce x to float
    z->Divide(tempx, tempy,aEnvironment.BinaryPrecision());
  }
  else
  {
    z->Divide(*x, *y,aEnvironment.BinaryPrecision());
  }
  RESULT = (NEW LispNumber(z));
  return;
}

void LispSqrt(LispEnvironment& aEnvironment, LispInt aStackTop)
{//FIXME move to scripts
    LispArithmetic1(aEnvironment, aStackTop, SqrtFloat);
}


#define UNARYFUNCTION(LispName, BigNumName, OldName) \
void LispName(LispEnvironment& aEnvironment, LispInt aStackTop) \
{ \
      RefPtr<BigNumber> x; \
      GetNumber(x,aEnvironment, aStackTop, 1); \
      BigNumber *z = NEW BigNumber(  *x ); \
      z->BigNumName( *z ); \
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

UNARYFUNCTION(LispFloor, Floor, FloorFloat)
UNARYFUNCTION(LispMathNegate, Negate, NegateFloat)

/** the macro
  UNARYFUNCTION(LispFloor, Floor, FloorFloat)
is used to help interface Yacas with BigNumber. Suppose we need to access a unary function named 'Floor' in BigNumber and 'LispFloor' here, with 'FloorFloat' the backup function for no BigNumber support.
The macro produces the following equivalent code for the unary function:
void LispFloor(LispEnvironment& aEnvironment, LispInt aStackTop)
{
      RefPtr<BigNumber> x;
      GetNumber(x,aEnvironment, aStackTop, 1);
      BigNumber *z = NEW BigNumber(aEnvironment.BinaryPrecision());
      z->Floor(*x.Ptr());
      RESULT = (NEW LispNumber(z));
}
*/
/* FIXME Eventually the BigNumber support will be stable and we can remove old code and simplify these macros */

/// obtain internal precision data on a number object.
void LispGetExactBits(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  RefPtr<BigNumber> x;
  GetNumber(x,aEnvironment, aStackTop, 1);
  BigNumber *z = NEW BigNumber(aEnvironment.BinaryPrecision());
  z->SetTo(
  (x->IsInt())
    ? x->BitCount()  // for integers, return the bit count
    : x->GetPrecision()   // for floats, return the precision
  );
  RESULT = (NEW LispNumber(z));
}
/// set internal precision data on a number object.
void LispSetExactBits(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  RefPtr<BigNumber> x;
  RefPtr<BigNumber> y;
  GetNumber(x,aEnvironment, aStackTop, 1);
  GetNumber(y,aEnvironment, aStackTop, 2);
  BigNumber *z = NEW BigNumber(aEnvironment.BinaryPrecision());
  z->SetTo(*x);
  // do nothing for integers
  if (!(z->IsInt()))
    z->Precision((long)(y->Double()));  // segfaults unless y is defined?
  RESULT = (NEW LispNumber(z));
}


/// obtain the bit count of a number object.
void LispBitCount(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  RefPtr<BigNumber> x;
  GetNumber(x,aEnvironment, aStackTop, 1);
  BigNumber *z = NEW BigNumber(aEnvironment.BinaryPrecision());
  z->SetTo(x->BitCount());
  RESULT = (NEW LispNumber(z));
}

/// obtain the sign of a number object.
void LispMathSign(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  RefPtr<BigNumber> x;
  GetNumber(x,aEnvironment, aStackTop, 1);
  BigNumber *z = NEW BigNumber(aEnvironment.BinaryPrecision());
  z->SetTo(x->Sign());
  RESULT = (NEW LispNumber(z));
}

/// check whether a number object fits into a platform type.
void LispMathIsSmall(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  RefPtr<BigNumber> x;
  GetNumber(x,aEnvironment, aStackTop, 1);
  InternalBoolean(aEnvironment, RESULT, x->IsSmall());
}


void LispCeil(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  RefPtr<BigNumber> x;
  GetNumber(x,aEnvironment, aStackTop, 1);
  BigNumber *z = NEW BigNumber(aEnvironment.BinaryPrecision());
  z->Negate(*x);
  z->Floor(*z);  // danger: possible exception raised in Floor() leads to a memory leak because z is not destroyed
  z->Negate(*z);
  RESULT = (NEW LispNumber(z));
}

void LispAbs(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  RefPtr<BigNumber> x;
  GetNumber(x,aEnvironment, aStackTop, 1);
  BigNumber *z = NEW BigNumber(aEnvironment.BinaryPrecision());
  z->SetTo(*x);
  if (x->Sign()<0)
    z->Negate(*x);
  RESULT = (NEW LispNumber(z));
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
  RefPtr<BigNumber> x;
  RefPtr<BigNumber> y;
  GetNumber(x,aEnvironment, aStackTop, 1);
  GetNumber(y,aEnvironment, aStackTop, 2);
  if (x->IsInt() && y->IsInt())
  {  // both integer, perform integer division
    BigNumber *z = NEW BigNumber(aEnvironment.BinaryPrecision());
    z->Divide(*x,*y,aEnvironment.BinaryPrecision());
    RESULT = (NEW LispNumber(z));
    return;
  }
  else
  {//TODO FIXME: either need to report error that one or both of the arguments are not integer, or coerce them to integers
#ifdef HAVE_STDIO_H
    fprintf(stderr, "LispDiv: error: both arguments must be integer\n");
#endif
    return;
  }
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
{//TODO FIXME
  RefPtr<BigNumber> x;
  GetNumber(x,aEnvironment, aStackTop, 1);
  long result = primes_table_check((unsigned long)(x->Double()));
  BigNumber *z = NEW BigNumber(aEnvironment.BinaryPrecision());
  z->SetTo(result);
  RESULT = (NEW LispNumber(z));
}

// define a macro to replace all platform math functions

#define PLATFORM_UNARY(LispName, PlatformName, LispBackupName, OldName) \
    void LispName(LispEnvironment& aEnvironment, LispInt aStackTop)     \
    {                                                                   \
        RefPtr<BigNumber> x;                                            \
        GetNumber(x,aEnvironment, aStackTop, 1);                        \
        double result = PlatformName(x->Double());                      \
        BigNumber *z = NEW BigNumber(aEnvironment.BinaryPrecision());   \
        z->SetTo(result);                                               \
        RESULT = (NEW LispNumber(z));                                   \
    }

#define PLATFORM_BINARY(LispName, PlatformName, LispBackupName, OldName) \
    void LispName(LispEnvironment& aEnvironment, LispInt aStackTop)     \
    {                                                                   \
        RefPtr<BigNumber> x, y;                                         \
        GetNumber(x,aEnvironment, aStackTop, 1);                        \
        GetNumber(y,aEnvironment, aStackTop, 2);                        \
        double result = PlatformName(x->Double(), y->Double());         \
        BigNumber *z = NEW BigNumber(aEnvironment.BinaryPrecision());   \
        z->SetTo(result);                                               \
        RESULT = (NEW LispNumber(z));                                   \
    }

// now we can define all such functions, e.g.:

// some or all of these functions should be moved to scripts
PLATFORM_UNARY(LispFastArcSin, std::asin, LispArcSin, PlatArcSin)
PLATFORM_UNARY(LispFastLog, std::log, LispLn, PlatLn)
PLATFORM_BINARY(LispFastPower, std::pow, LispPower, PlatPower)



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
    // Get the base to convert to:
    // Evaluate first argument, and store result in oper
    LispPtr oper(ARGUMENT(1));
    // Check that result is a number, and that it is in fact an integer
    RefPtr<BigNumber> num; num = oper->Number(aEnvironment.BinaryPrecision());
    CheckArg(num, 1, aEnvironment, aStackTop);
  // check that the base is an integer between 2 and 32
    CheckArg(num->IsInt() && num->Double() >= BASE2 && num->Double() <= log2_table_range(), 1, aEnvironment, aStackTop);

    // Get a short platform integer from the first argument
    LispInt base = (LispInt)(num->Double());

    // Get the number to convert
    LispPtr fromNum(ARGUMENT(2));
    const LispString* str2 = fromNum->String();
    CheckArg(str2, 2, aEnvironment, aStackTop);

    // Added, unquote a string
    CheckArg(InternalIsString(str2), 2, aEnvironment, aStackTop);
    str2 = aEnvironment.HashTable().LookUp(str2->substr(1, str2->length() - 2));

    // convert using correct base
  // FIXME: API breach, must pass precision in base digits and not in bits!
  // if converting an integer, the precision argument is ignored,
  // but if converting a float, need to use bits_to_digits(BinaryPrecision, base)
    BigNumber *z = NEW BigNumber(str2->c_str(),aEnvironment.BinaryPrecision(),base);
    RESULT = (NEW LispNumber(z));
}
void LispToBase(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    // Get the base to convert to:
    // Evaluate first argument, and store result in oper
    LispPtr oper(ARGUMENT(1));
    // Check that result is a number, and that it is in fact an integer
    RefPtr<BigNumber> num; num = oper->Number(aEnvironment.BinaryPrecision());
    CheckArg(num, 1, aEnvironment, aStackTop);
  // check that the base is an integer between 2 and 32
    CheckArg(num->IsInt() && num->Double() >= BASE2 && num->Double() <= log2_table_range(), 1, aEnvironment, aStackTop);

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

    RESULT = LispAtom::New(aEnvironment, stringify(str));
}



void LispApplyPure(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispPtr oper(ARGUMENT(1));
    LispPtr args(ARGUMENT(2));

    CheckArg(args->SubList(), 2, aEnvironment, aStackTop);
    CheckArg(*args->SubList(), 2, aEnvironment, aStackTop);



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
        CheckArg(oper->SubList(), 1, aEnvironment, aStackTop);
        CheckArg(*oper->SubList(), 1, aEnvironment, aStackTop);
        InternalApplyPure(oper,args2,RESULT,aEnvironment);
    }
}


void YacasPrettyReaderSet(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  LispInt nrArguments = InternalListLength(ARGUMENT(0));

  if (nrArguments == 1)
  {
    aEnvironment.SetPrettyReader(nullptr);
  }
  else
  {
    CheckNrArgs(2, ARGUMENT(0), aEnvironment);
    LispPtr oper(ARGUMENT(0));
    oper = oper->Nixed(); // oper.GoNext();  // woof woof woof
    CheckArgIsString(oper, 1, aEnvironment, aStackTop);
    aEnvironment.SetPrettyReader(oper->String());
  }
  InternalTrue(aEnvironment,RESULT);
}

void YacasPrettyReaderGet(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  if (!aEnvironment.PrettyReader())
    RESULT = LispAtom::New(aEnvironment, "\"\"");
  else
    RESULT = LispAtom::New(aEnvironment, *aEnvironment.PrettyReader());
}

void YacasPrettyPrinterSet(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  LispInt nrArguments = InternalListLength(ARGUMENT(0));

  if (nrArguments == 1)
  {
    aEnvironment.SetPrettyPrinter(nullptr);
  }
  else
  {
    CheckNrArgs(2, ARGUMENT(0), aEnvironment);
    LispPtr oper(ARGUMENT(0));
    oper = oper->Nixed(); // oper.GoNext();  // woof woof woof
    CheckArgIsString(oper, 1, aEnvironment, aStackTop);
    aEnvironment.SetPrettyPrinter(oper->String());
  }
  InternalTrue(aEnvironment,RESULT);
}

void YacasPrettyPrinterGet(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  if (!aEnvironment.PrettyPrinter())
    RESULT = LispAtom::New(aEnvironment, "\"\"");
  else
    RESULT = LispAtom::New(aEnvironment, *aEnvironment.PrettyPrinter());
}

void LispGarbageCollect(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  aEnvironment.HashTable().GarbageCollect();
  InternalTrue(aEnvironment,RESULT);
}

void LispPatchLoad(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispPtr evaluated(ARGUMENT(1));
    const LispString* string = evaluated->String();
    CheckArg(string, 1, aEnvironment, aStackTop);
    const std::string fname = InternalUnstringify(*string);
    InputStatus oldstatus = aEnvironment.iInputStatus;
    aEnvironment.iInputStatus.SetTo(fname);
    LispLocalFile localFP(aEnvironment, fname, true,
                          aEnvironment.iInputDirectories);

    if (!localFP.stream.is_open())
        throw LispErrFileNotFound();

    std::string content(std::istreambuf_iterator<char>(localFP.stream), std::istreambuf_iterator<char>());

    PatchLoad(content.c_str(), aEnvironment.CurrentOutput(), aEnvironment);

    aEnvironment.iInputStatus.RestoreFrom(oldstatus);
    InternalTrue(aEnvironment, RESULT);
}

void LispPatchString(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  LispPtr evaluated(ARGUMENT(1));
  const LispString* string = evaluated->String();
  CheckArg(string, 1, aEnvironment, aStackTop);
  const std::string oper = InternalUnstringify(*string);

  std::ostringstream os;
  LispLocalOutput localOutput(aEnvironment, os);
  PatchLoad(oper.c_str(), os, aEnvironment);
  RESULT = LispAtom::New(aEnvironment, stringify(os.str()));
}

void YacasExtraInfoSet(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispPtr object(ARGUMENT(1));
    LispPtr info(ARGUMENT(2));
    RESULT = ( object->SetExtraInfo(info) );
}


void YacasExtraInfoGet(LispEnvironment& aEnvironment, LispInt aStackTop)
{
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


void LispDefaultTokenizer(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  aEnvironment.iCurrentTokenizer = &aEnvironment.iDefaultTokenizer;
  InternalTrue(aEnvironment,RESULT);
}


void LispXmlTokenizer(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  aEnvironment.iCurrentTokenizer = &aEnvironment.iXmlTokenizer;
  InternalTrue(aEnvironment,RESULT);
}

void LispExplodeTag(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  LispPtr out(ARGUMENT(1));
  CheckArgIsString(1, aEnvironment, aStackTop);

  const LispChar* str = out->String()->c_str();
  str++;
  if (str[0] != '<')
  {
    RESULT = (out);
    return;
  }
  str++;
  const LispChar * type = (str[0] == '/') ? (str++, "\"Close\"") : "\"Open\"";
  std::string tag;

  tag.push_back('\"');
  while (IsAlpha(*str))
  {
    LispChar c = *str++;
    if (c >= 'a' && c <= 'z')
      c = c + ('A'-'a');
    tag.push_back(c);
  }
  tag.push_back('\"');

  LispObject* info = nullptr;

  while (*str == ' ') str++;
  while (*str != '>' && *str != '/')
  {
    std::string name;
    name.push_back('\"');

    while (IsAlpha(*str))
    {
      LispChar c = *str++;
      if (c >= 'a' && c <= 'z')
        c = c + ('A'-'a');
      name.push_back(c);
    }
    name.push_back('\"');

    CheckArg(str[0] == '=', 1, aEnvironment, aStackTop);
    str++;
    CheckArg(str[0] == '\"', 1, aEnvironment, aStackTop);

    std::string value;
    value.push_back(*str++);
    while (*str != '\"')
      value.push_back(*str++);

    value.push_back(*str++);

    info =  LispSubList::New(LispObjectAdder(aEnvironment.iList->Copy()) + LispObjectAdder(LispAtom::New(aEnvironment, name)) + LispObjectAdder(LispAtom::New(aEnvironment, value))) + LispObjectAdder(info);
    while (*str == ' ') str++;
  }
  if (*str == '/')
  {
    type = "\"OpenClose\"";
    str++;
    while (*str == ' ')
        ++str;
  }

  info = LispSubList::New(LispObjectAdder(aEnvironment.iList->Copy()) + LispObjectAdder(info));
  RESULT = (
              LispSubList::New(
                   LispObjectAdder(LispAtom::New(aEnvironment, "XmlTag")) +
                   LispObjectAdder(LispAtom::New(aEnvironment, tag)) +
                   LispObjectAdder(info) +
                   LispObjectAdder(LispAtom::New(aEnvironment, type))
                  )
             );
}


void YacasBuiltinAssoc(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    // Check that we have two arguments.

    // key to find
    LispPtr key(ARGUMENT(1));

    // assoc-list to find it in
    LispPtr list(ARGUMENT(2));

    LispObject* t;

    //Check that it is a compound object
    CheckArg(list->SubList(), 2, aEnvironment, aStackTop);
    t = (*list->SubList());
    CheckArg(t, 2, aEnvironment, aStackTop);
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

    RESULT = (LispAtom::New(aEnvironment, "Empty"));
}

void LispCurrentFile(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    RESULT = LispAtom::New(aEnvironment, stringify(aEnvironment.iInputStatus.FileName()));
}

void LispCurrentLine(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    RESULT = LispAtom::New(aEnvironment, std::to_string(aEnvironment.iInputStatus.LineNumber()));
}

void LispBackQuote(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    BackQuoteBehaviour behaviour(aEnvironment);
    LispPtr result;
    InternalSubstitute(result, ARGUMENT( 1), behaviour);
    InternalEval(aEnvironment, RESULT, result);
}

void LispInterpreter(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    RESULT = (LispAtom::New(aEnvironment,"\"yacas\""));
}

void LispVersion(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    RESULT = (LispAtom::New(aEnvironment,"\"" YACAS_VERSION "\""));
}


/// convert bits to digits. Use the kernel function bits_to_digits. Arguments must be small integers.
void LispBitsToDigits(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  RefPtr<BigNumber> x;
  RefPtr<BigNumber> y;
  GetNumber(x,aEnvironment, aStackTop, 1);
  GetNumber(y,aEnvironment, aStackTop, 2);
  long result = 0;  // initialize just in case
  if (x->IsInt() && x->IsSmall() && y->IsInt() && y->IsSmall())
  {
    // bits_to_digits uses unsigned long, see numbers.h
    unsigned base = unsigned(y->Double());
    result = bits_to_digits((unsigned long)(x->Double()), base);
  }
  else
  {
      std::ostringstream buf;
      buf << "BitsToDigits: error: arguments (" << x->Double() << ", " << y->Double() << " must be small integers";
      throw LispErrGeneric(buf.str());
  }
  BigNumber *z = NEW BigNumber();
  z->SetTo(result);
  RESULT = (NEW LispNumber(z));
}

/// convert digits to bits. Use the kernel function digits_to_bits. Arguments must be small integers.
void LispDigitsToBits(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  RefPtr<BigNumber> x;
  RefPtr<BigNumber> y;
  GetNumber(x,aEnvironment, aStackTop, 1);
  GetNumber(y,aEnvironment, aStackTop, 2);
  long result = 0;  // initialize just in case
  if (x->IsInt() && x->IsSmall() && y->IsInt() && y->IsSmall())
  {
    // bits_to_digits uses unsigned long, see numbers.h
    unsigned base = unsigned(y->Double());
    result = digits_to_bits((unsigned long)(x->Double()), base);
  }
  else
  {
      std::ostringstream buf;
      buf << "BitsToDigits: error: arguments (" << x->Double() << ", " << y->Double() << " must be small integers";
      throw LispErrGeneric(buf.str());
  }
  BigNumber *z = NEW BigNumber();
  z->SetTo(result);
  RESULT = (NEW LispNumber(z));
}
