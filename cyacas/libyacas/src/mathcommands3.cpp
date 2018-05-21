#include "yacas/arrayclass.h"
#include "yacas/errors.h"
#include "yacas/infixparser.h"
#include "yacas/lispatom.h"
#include "yacas/lispenvironment.h"
#include "yacas/lisperror.h"
#include "yacas/lispeval.h"
#include "yacas/lispparser.h"
#include "yacas/lispuserfunc.h"
#include "yacas/mathuserfunc.h"
#include "yacas/numbers.h"
#include "yacas/patcher.h"
#include "yacas/patternclass.h"
#include "yacas/platfileio.h"
#include "yacas/platmath.h"
#include "yacas/standard.h"
#include "yacas/string_utils.h"
#include "yacas/stringio.h"
#include "yacas/substitute.h"

#include <cmath>

#include "yacas/yacas_version.h"

#include <iomanip>
#include <sstream>

#define InternalEval aEnvironment.iEvaluator->Eval
#define RESULT aEnvironment.iStack[aStackTop]
#define ARGUMENT(i) aEnvironment.iStack[aStackTop + i]

/// Construct a BigNumber from one of the arguments.
/// \param x (on output) the constructed bignumber
/// \param aEnvironment the current environment
/// \param aStackTop the index of the top of the stack
/// \param aArgNr the index of the argument to be converted
void GetNumber(RefPtr<BigNumber>& x,
               LispEnvironment& aEnvironment,
               int aStackTop,
               int aArgNr)
{
    x = ARGUMENT(aArgNr)->Number(aEnvironment.Precision());
    CheckArg(x, aArgNr, aEnvironment, aStackTop);
}

// FIXME remove these
void LispArithmetic2(LispEnvironment& aEnvironment,
                     int aStackTop,
                     LispObject* (*func)(LispObject* f1,
                                         LispObject* f2,
                                         LispEnvironment& aEnvironment,
                                         int aPrecision),
                     bool arbbase = false);

void LispArithmetic1(LispEnvironment& aEnvironment,
                     int aStackTop,
                     LispObject* (*func)(LispObject* f1,
                                         LispEnvironment& aEnvironment,
                                         int aPrecision));

// FIXME remove these
void LispArithmetic1(LispEnvironment& aEnvironment,
                     int aStackTop,
                     LispObject* (*func)(LispObject* f1,
                                         LispEnvironment& aEnvironment,
                                         int aPrecision))
{
    CheckArg(ARGUMENT(1)->Number(0), 1, aEnvironment, aStackTop);
    RESULT = (func(ARGUMENT(1), aEnvironment, aEnvironment.Precision()));
}

// FIXME remove these
void LispArithmetic2(LispEnvironment& aEnvironment,
                     int aStackTop,
                     LispObject* (*func)(LispObject* f1,
                                         LispObject* f2,
                                         LispEnvironment& aEnvironment,
                                         int aPrecision),
                     bool arbbase)
{
    if (!arbbase) {
        CheckArg(ARGUMENT(1)->Number(0), 1, aEnvironment, aStackTop);
        CheckArg(ARGUMENT(2)->Number(0), 2, aEnvironment, aStackTop);
    }
    RESULT = (func(
        ARGUMENT(1), ARGUMENT(2), aEnvironment, aEnvironment.Precision()));
}

void LispDumpBigNumberDebugInfo(LispEnvironment& aEnvironment, int aStackTop)
{
    RefPtr<BigNumber> x;
    GetNumber(x, aEnvironment, aStackTop, 1);
    x->DumpDebugInfo(aEnvironment.CurrentOutput());
    InternalTrue(aEnvironment, RESULT);
}

void LispMultiply(LispEnvironment& aEnvironment, int aStackTop)
{
    RefPtr<BigNumber> x;
    RefPtr<BigNumber> y;
    GetNumber(x, aEnvironment, aStackTop, 1);
    GetNumber(y, aEnvironment, aStackTop, 2);
    BigNumber* z = new BigNumber("0", aEnvironment.BinaryPrecision());
    z->Precision(aEnvironment.BinaryPrecision());
    z->Multiply(*x, *y, aEnvironment.BinaryPrecision());
    RESULT = new LispNumber(z);
    return;
}

// TODO we need to have Gcd in BigNumber!
void LispGcd(LispEnvironment& aEnvironment, int aStackTop)
{
    CheckArg(ARGUMENT(1)->Number(0), 1, aEnvironment, aStackTop);
    CheckArg(ARGUMENT(2)->Number(0), 2, aEnvironment, aStackTop);

    RESULT = (GcdInteger(ARGUMENT(1), ARGUMENT(2), aEnvironment));
}

/// Corresponds to the Yacas function \c MathAdd.
/// If called with one argument (unary plus), this argument is
/// converted to BigNumber. If called with two arguments (binary plus),
/// both argument are converted to a BigNumber, and these are added
/// together at the current precision. The sum is returned.
/// \sa GetNumber(), BigNumber::Add()
void LispAdd(LispEnvironment& aEnvironment, int aStackTop)
{
    int length = InternalListLength(ARGUMENT(0));
    if (length == 2) {
        RefPtr<BigNumber> x;
        GetNumber(x, aEnvironment, aStackTop, 1);
        RESULT = (new LispNumber(x.ptr()));
        return;
    } else {
        RefPtr<BigNumber> x;
        RefPtr<BigNumber> y;
        GetNumber(x, aEnvironment, aStackTop, 1);
        GetNumber(y, aEnvironment, aStackTop, 2);
        BigNumber* z = new BigNumber("0", aEnvironment.BinaryPrecision());
        z->Precision(aEnvironment.BinaryPrecision());
        z->Add(*x.ptr(), *y.ptr(), aEnvironment.BinaryPrecision());
        RESULT = (new LispNumber(z));
        return;
    }
}

void LispSubtract(LispEnvironment& aEnvironment, int aStackTop)
{
    int length = InternalListLength(ARGUMENT(0));
    if (length == 2) {
        RefPtr<BigNumber> x;
        GetNumber(x, aEnvironment, aStackTop, 1);
        BigNumber* z = new BigNumber(*x);
        z->Negate(*z);
        RESULT = (new LispNumber(z));
        return;
    } else {
        RefPtr<BigNumber> x;
        RefPtr<BigNumber> y;
        GetNumber(x, aEnvironment, aStackTop, 1);
        GetNumber(y, aEnvironment, aStackTop, 2);
        BigNumber yneg(*y);
        yneg.Negate(yneg);
        BigNumber* z = new BigNumber("0", aEnvironment.BinaryPrecision());
        z->Precision(aEnvironment.BinaryPrecision());
        z->Add(*x, yneg, aEnvironment.BinaryPrecision());
        RESULT = (new LispNumber(z));
        return;
    }
}

void LispDivide(LispEnvironment& aEnvironment, int aStackTop)
{
    // Serge, what was the deal again with divide, floats and integers mixed in
    // the same function?
    //  yes, divide works differently on integers and on floats -- see new.chapt
    //  -- Serge
    RefPtr<BigNumber> x;
    RefPtr<BigNumber> y;
    GetNumber(x, aEnvironment, aStackTop, 1);
    GetNumber(y, aEnvironment, aStackTop, 2);
    BigNumber* z = new BigNumber("0", aEnvironment.BinaryPrecision());
    z->Precision(aEnvironment.BinaryPrecision());
    // if both arguments are integers, then BigNumber::Divide would perform an
    // integer divide, but we want a float divide here.
    if (x->IsInt() && y->IsInt()) {
        // why can't we just say BigNumber temp; ?
        BigNumber tempx(*x);
        tempx.BecomeFloat(aEnvironment.BinaryPrecision()); // coerce x to float
        BigNumber tempy(*y);
        tempy.BecomeFloat(aEnvironment.BinaryPrecision()); // coerce x to float
        z->Divide(tempx, tempy, aEnvironment.BinaryPrecision());
    } else {
        z->Divide(*x, *y, aEnvironment.BinaryPrecision());
    }
    RESULT = (new LispNumber(z));
    return;
}

void LispSqrt(LispEnvironment& aEnvironment, int aStackTop)
{ // FIXME move to scripts
    LispArithmetic1(aEnvironment, aStackTop, SqrtFloat);
}

#define UNARYFUNCTION(LispName, BigNumName)                                    \
    void LispName(LispEnvironment& aEnvironment, int aStackTop)                \
    {                                                                          \
        RefPtr<BigNumber> x;                                                   \
        GetNumber(x, aEnvironment, aStackTop, 1);                              \
        BigNumber* z = new BigNumber(*x);                                      \
        z->BigNumName(*z);                                                     \
        RESULT = (new LispNumber(z));                                          \
    }
#define BINARYFUNCTION(LispName, BigNumName)                                   \
    void LispName(LispEnvironment& aEnvironment, int aStackTop)                \
    {                                                                          \
        RefPtr<BigNumber> x;                                                   \
        RefPtr<BigNumber> y;                                                   \
        GetNumber(x, aEnvironment, aStackTop, 1);                              \
        GetNumber(y, aEnvironment, aStackTop, 2);                              \
        BigNumber* z = new BigNumber("0", 0);                                  \
        z->BigNumName(*x, *y);                                                 \
        RESULT = (new LispNumber(z));                                          \
    }

UNARYFUNCTION(LispFloor, Floor)
UNARYFUNCTION(LispMathNegate, Negate)

/** the macro
  UNARYFUNCTION(LispFloor, Floor, FloorFloat)
is used to help interface Yacas with BigNumber. Suppose we need to access a
unary function named 'Floor' in BigNumber and 'LispFloor' here, with
'FloorFloat' the backup function for no BigNumber support. The macro produces
the following equivalent code for the unary function: void
LispFloor(LispEnvironment& aEnvironment, int aStackTop)
{
      RefPtr<BigNumber> x;
      GetNumber(x,aEnvironment, aStackTop, 1);
      BigNumber *z = new BigNumber(aEnvironment.BinaryPrecision());
      z->Floor(*x.Ptr());
      RESULT = (new LispNumber(z));
}
*/
/* FIXME Eventually the BigNumber support will be stable and we can remove old
 * code and simplify these macros */

/// obtain internal precision data on a number object.
void LispGetExactBits(LispEnvironment& aEnvironment, int aStackTop)
{
    RefPtr<BigNumber> x;
    GetNumber(x, aEnvironment, aStackTop, 1);
    BigNumber* z = new BigNumber(
        std::to_string(x->IsInt() ? x->BitCount() : x->GetPrecision()),
        aEnvironment.BinaryPrecision());
    RESULT = (new LispNumber(z));
}
/// set internal precision data on a number object.
void LispSetExactBits(LispEnvironment& aEnvironment, int aStackTop)
{
    RefPtr<BigNumber> x;
    RefPtr<BigNumber> y;
    GetNumber(x, aEnvironment, aStackTop, 1);
    GetNumber(y, aEnvironment, aStackTop, 2);
    BigNumber* z = new BigNumber(*x);
    // do nothing for integers
    if (!(z->IsInt()))
        z->Precision((long)(y->Double())); // segfaults unless y is defined?
    RESULT = (new LispNumber(z));
}

/// obtain the bit count of a number object.
void LispBitCount(LispEnvironment& aEnvironment, int aStackTop)
{
    RefPtr<BigNumber> x;
    GetNumber(x, aEnvironment, aStackTop, 1);
    BigNumber* z = new BigNumber(std::to_string(x->BitCount()),
                                 aEnvironment.BinaryPrecision());
    RESULT = (new LispNumber(z));
}

/// obtain the sign of a number object.
void LispMathSign(LispEnvironment& aEnvironment, int aStackTop)
{
    RefPtr<BigNumber> x;
    GetNumber(x, aEnvironment, aStackTop, 1);
    BigNumber* z = new BigNumber(std::to_string(x->Sign()),
                                 aEnvironment.BinaryPrecision());
    RESULT = (new LispNumber(z));
}

/// check whether a number object fits into a platform type.
void LispMathIsSmall(LispEnvironment& aEnvironment, int aStackTop)
{
    RefPtr<BigNumber> x;
    GetNumber(x, aEnvironment, aStackTop, 1);
    InternalBoolean(aEnvironment, RESULT, x->IsSmall());
}

void LispCeil(LispEnvironment& aEnvironment, int aStackTop)
{
    RefPtr<BigNumber> x;
    GetNumber(x, aEnvironment, aStackTop, 1);
    BigNumber* z = new BigNumber("0", aEnvironment.BinaryPrecision());
    z->Negate(*x);
    z->Floor(*z); // danger: possible exception raised in Floor() leads to a
                  // memory leak because z is not destroyed
    z->Negate(*z);
    RESULT = (new LispNumber(z));
}

void LispAbs(LispEnvironment& aEnvironment, int aStackTop)
{
    RefPtr<BigNumber> x;
    GetNumber(x, aEnvironment, aStackTop, 1);
    BigNumber* z = new BigNumber(*x);
    if (x->Sign() < 0)
        z->Negate(*x);
    RESULT = (new LispNumber(z));
}
// BINARYFUNCTION(LispMod, Mod, ModFloat)
/* this will be gone */
void LispMod(LispEnvironment& aEnvironment, int aStackTop)
{ // FIXME
    LispArithmetic2(aEnvironment, aStackTop, ModFloat);
}
/* up to here */

void LispDiv(LispEnvironment& aEnvironment, int aStackTop)
{
    RefPtr<BigNumber> x;
    RefPtr<BigNumber> y;
    GetNumber(x, aEnvironment, aStackTop, 1);
    GetNumber(y, aEnvironment, aStackTop, 2);

    // TODO FIXME: either need to report error that one or both of the arguments
    // are not integer, or coerce them to integers
    CheckArg(x->IsInt(), 1, aEnvironment, aStackTop);
    CheckArg(y->IsInt(), 2, aEnvironment, aStackTop);

    BigNumber* z = new BigNumber("0", aEnvironment.BinaryPrecision());
    z->Precision(aEnvironment.BinaryPrecision());
    z->Divide(*x, *y, aEnvironment.BinaryPrecision());
    RESULT = (new LispNumber(z));
}

void LispPower(LispEnvironment& aEnvironment, int aStackTop)
{ // FIXME move to scripts
    LispArithmetic2(aEnvironment, aStackTop, PowerFloat);
}

void LispFac(LispEnvironment& aEnvironment, int aStackTop)
{ // FIXME move to scripts
    LispArithmetic1(aEnvironment, aStackTop, LispFactorial);
}

// platform functions, taking/returning a platform int/float

void LispFastIsPrime(LispEnvironment& aEnvironment, int aStackTop)
{ // TODO FIXME
    RefPtr<BigNumber> x;
    GetNumber(x, aEnvironment, aStackTop, 1);
    long result = primes_table_check((unsigned long)(x->Double()));
    BigNumber* z =
        new BigNumber(std::to_string(result), aEnvironment.BinaryPrecision());
    RESULT = (new LispNumber(z));
}

// define a macro to replace all platform math functions

#define PLATFORM_UNARY(LispName, PlatformName, LispBackupName, OldName)        \
    void LispName(LispEnvironment& aEnvironment, int aStackTop)                \
    {                                                                          \
        RefPtr<BigNumber> x;                                                   \
        GetNumber(x, aEnvironment, aStackTop, 1);                              \
        std::ostringstream buf;                                                \
        buf << std::setprecision(53) << PlatformName(x->Double());             \
        BigNumber* z =                                                         \
            new BigNumber(buf.str(), aEnvironment.BinaryPrecision());          \
        RESULT = (new LispNumber(z));                                          \
    }

#define PLATFORM_BINARY(LispName, PlatformName, LispBackupName, OldName)       \
    void LispName(LispEnvironment& aEnvironment, int aStackTop)                \
    {                                                                          \
        RefPtr<BigNumber> x, y;                                                \
        GetNumber(x, aEnvironment, aStackTop, 1);                              \
        GetNumber(y, aEnvironment, aStackTop, 2);                              \
        std::ostringstream buf;                                                \
        buf << std::setprecision(53)                                           \
            << PlatformName(x->Double(), y->Double());                         \
        BigNumber* z =                                                         \
            new BigNumber(buf.str(), aEnvironment.BinaryPrecision());          \
        RESULT = (new LispNumber(z));                                          \
    }

// now we can define all such functions, e.g.:

// some or all of these functions should be moved to scripts
PLATFORM_UNARY(LispFastArcSin, std::asin, LispArcSin, PlatArcSin)
PLATFORM_UNARY(LispFastLog, std::log, LispLn, PlatLn)
PLATFORM_BINARY(LispFastPower, std::pow, LispPower, PlatPower)

BINARYFUNCTION(LispBitAnd, BitAnd)
BINARYFUNCTION(LispBitOr, BitOr)
BINARYFUNCTION(LispBitXor, BitXor)
/*
// BitNot not yet in yacasapi etc.
//BINARYFUNCTION(LispBitNot, BitNot, BitNot)
*/
/* this will be gone */
void LispShiftLeft(LispEnvironment& aEnvironment, int aStackTop)
{ // FIXME
    LispArithmetic2(aEnvironment, aStackTop, ShiftLeft);
}
void LispShiftRight(LispEnvironment& aEnvironment, int aStackTop)
{ // FIXME
    LispArithmetic2(aEnvironment, aStackTop, ShiftRight);
}

/* up to here */

void LispFromBase(LispEnvironment& aEnvironment, int aStackTop)
{
    // Get the base to convert to:
    // Evaluate first argument, and store result in oper
    LispPtr oper(ARGUMENT(1));
    // Check that result is a number, and that it is in fact an integer
    RefPtr<BigNumber> num;
    num = oper->Number(aEnvironment.BinaryPrecision());
    CheckArg(num, 1, aEnvironment, aStackTop);
    // check that the base is an integer between 2 and 32
    CheckArg(num->IsInt() && num->Double() >= BASE2 &&
                 num->Double() <= log2_table_range(),
             1,
             aEnvironment,
             aStackTop);

    // Get a short platform integer from the first argument
    int base = (int)(num->Double());

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
    // but if converting a float, need to use bits_to_digits(BinaryPrecision,
    // base)
    BigNumber* z = new BigNumber(*str2, aEnvironment.BinaryPrecision(), base);
    RESULT = (new LispNumber(z));
}
void LispToBase(LispEnvironment& aEnvironment, int aStackTop)
{
    // Get the base to convert to:
    // Evaluate first argument, and store result in oper
    LispPtr oper(ARGUMENT(1));
    // Check that result is a number, and that it is in fact an integer
    RefPtr<BigNumber> num;
    num = oper->Number(aEnvironment.BinaryPrecision());
    CheckArg(num, 1, aEnvironment, aStackTop);
    // check that the base is an integer between 2 and 32
    CheckArg(num->IsInt() && num->Double() >= BASE2 &&
                 num->Double() <= log2_table_range(),
             1,
             aEnvironment,
             aStackTop);

    // Get a short platform integer from the first argument
    int base = (int)(num->Double());

    // Get the number to convert
    RefPtr<BigNumber> x;
    GetNumber(x, aEnvironment, aStackTop, 2);

    // convert using correct base
    LispString str;
    // FIXME: API breach, must pass precision in base digits and not in bits!
    // if converting an integer, the precision argument is ignored,
    // but if converting a float, need to use bits_to_digits(BinaryPrecision,
    // base)
    x->ToString(str, aEnvironment.BinaryPrecision(), base);
    // Get unique string from hash table, and create an atom from it.

    RESULT = LispAtom::New(aEnvironment, stringify(str));
}

void LispApplyPure(LispEnvironment& aEnvironment, int aStackTop)
{
    LispPtr oper(ARGUMENT(1));
    LispPtr args(ARGUMENT(2));

    CheckArg(args->SubList(), 2, aEnvironment, aStackTop);
    CheckArg(*args->SubList(), 2, aEnvironment, aStackTop);

    // Apply a pure string
    if (oper->String()) {
        InternalApplyString(
            aEnvironment, RESULT, oper->String(), (*args->SubList())->Nixed());
    } else { // Apply a pure function {args,body}.
        LispPtr args2((*args->SubList())->Nixed());
        CheckArg(oper->SubList(), 1, aEnvironment, aStackTop);
        CheckArg(*oper->SubList(), 1, aEnvironment, aStackTop);
        InternalApplyPure(oper, args2, RESULT, aEnvironment);
    }
}

void YacasPrettyReaderSet(LispEnvironment& aEnvironment, int aStackTop)
{
    int nrArguments = InternalListLength(ARGUMENT(0));

    if (nrArguments == 1) {
        aEnvironment.SetPrettyReader(nullptr);
    } else {
        CheckNrArgs(2, ARGUMENT(0), aEnvironment);
        LispPtr oper(ARGUMENT(0));
        oper = oper->Nixed(); // oper.GoNext();  // woof woof woof
        CheckArgIsString(oper, 1, aEnvironment, aStackTop);
        aEnvironment.SetPrettyReader(oper->String());
    }
    InternalTrue(aEnvironment, RESULT);
}

void YacasPrettyReaderGet(LispEnvironment& aEnvironment, int aStackTop)
{
    if (!aEnvironment.PrettyReader())
        RESULT = LispAtom::New(aEnvironment, "\"\"");
    else
        RESULT = LispAtom::New(aEnvironment, *aEnvironment.PrettyReader());
}

void YacasPrettyPrinterSet(LispEnvironment& aEnvironment, int aStackTop)
{
    int nrArguments = InternalListLength(ARGUMENT(0));

    if (nrArguments == 1) {
        aEnvironment.SetPrettyPrinter(nullptr);
    } else {
        CheckNrArgs(2, ARGUMENT(0), aEnvironment);
        LispPtr oper(ARGUMENT(0));
        oper = oper->Nixed(); // oper.GoNext();  // woof woof woof
        CheckArgIsString(oper, 1, aEnvironment, aStackTop);
        aEnvironment.SetPrettyPrinter(oper->String());
    }
    InternalTrue(aEnvironment, RESULT);
}

void YacasPrettyPrinterGet(LispEnvironment& aEnvironment, int aStackTop)
{
    if (!aEnvironment.PrettyPrinter())
        RESULT = LispAtom::New(aEnvironment, "\"\"");
    else
        RESULT = LispAtom::New(aEnvironment, *aEnvironment.PrettyPrinter());
}

void LispGarbageCollect(LispEnvironment& aEnvironment, int aStackTop)
{
    aEnvironment.HashTable().GarbageCollect();
    InternalTrue(aEnvironment, RESULT);
}

void LispPatchLoad(LispEnvironment& aEnvironment, int aStackTop)
{
    LispPtr evaluated(ARGUMENT(1));
    const LispString* string = evaluated->String();
    CheckArg(string, 1, aEnvironment, aStackTop);
    const std::string fname = InternalUnstringify(*string);
    InputStatus oldstatus = aEnvironment.iInputStatus;
    aEnvironment.iInputStatus.SetTo(fname);
    LispLocalFile localFP(
        aEnvironment, fname, true, aEnvironment.iInputDirectories);

    if (!localFP.stream.is_open())
        throw LispErrFileNotFound();

    std::string content(std::istreambuf_iterator<char>(localFP.stream),
                        std::istreambuf_iterator<char>());

    PatchLoad(content, aEnvironment.CurrentOutput(), aEnvironment);

    aEnvironment.iInputStatus.RestoreFrom(oldstatus);
    InternalTrue(aEnvironment, RESULT);
}

void LispPatchString(LispEnvironment& aEnvironment, int aStackTop)
{
    LispPtr evaluated(ARGUMENT(1));
    const LispString* string = evaluated->String();
    CheckArg(string, 1, aEnvironment, aStackTop);
    const std::string oper = InternalUnstringify(*string);

    std::ostringstream os;
    LispLocalOutput localOutput(aEnvironment, os);
    PatchLoad(oper, os, aEnvironment);
    RESULT = LispAtom::New(aEnvironment, stringify(os.str()));
}

void LispDefaultTokenizer(LispEnvironment& aEnvironment, int aStackTop)
{
    aEnvironment.iCurrentTokenizer = &aEnvironment.iDefaultTokenizer;
    InternalTrue(aEnvironment, RESULT);
}

void LispXmlTokenizer(LispEnvironment& aEnvironment, int aStackTop)
{
    aEnvironment.iCurrentTokenizer = &aEnvironment.iXmlTokenizer;
    InternalTrue(aEnvironment, RESULT);
}

void LispExplodeTag(LispEnvironment& aEnvironment, int aStackTop)
{
    LispPtr out(ARGUMENT(1));
    CheckArgIsString(1, aEnvironment, aStackTop);

    const char* str = out->String()->c_str();
    str++;
    if (str[0] != '<') {
        RESULT = (out);
        return;
    }
    str++;
    const char* type = (str[0] == '/') ? (str++, "\"Close\"") : "\"Open\"";
    std::string tag;

    tag.push_back('\"');
    while (IsAlpha(*str)) {
        char c = *str++;
        if (c >= 'a' && c <= 'z')
            c = c + ('A' - 'a');
        tag.push_back(c);
    }
    tag.push_back('\"');

    LispObject* info = nullptr;

    while (*str == ' ')
        str++;
    while (*str != '>' && *str != '/') {
        std::string name;
        name.push_back('\"');

        while (IsAlpha(*str)) {
            char c = *str++;
            if (c >= 'a' && c <= 'z')
                c = c + ('A' - 'a');
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

        info = LispSubList::New(
                   LispObjectAdder(aEnvironment.iList->Copy()) +
                   LispObjectAdder(LispAtom::New(aEnvironment, name)) +
                   LispObjectAdder(LispAtom::New(aEnvironment, value))) +
               LispObjectAdder(info);
        while (*str == ' ')
            str++;
    }
    if (*str == '/') {
        type = "\"OpenClose\"";
        str++;
        while (*str == ' ')
            ++str;
    }

    info = LispSubList::New(LispObjectAdder(aEnvironment.iList->Copy()) +
                            LispObjectAdder(info));
    RESULT = (LispSubList::New(
        LispObjectAdder(LispAtom::New(aEnvironment, "XmlTag")) +
        LispObjectAdder(LispAtom::New(aEnvironment, tag)) +
        LispObjectAdder(info) +
        LispObjectAdder(LispAtom::New(aEnvironment, type))));
}

void YacasBuiltinAssoc(LispEnvironment& aEnvironment, int aStackTop)
{
    // Check that we have two arguments.

    // key to find
    LispPtr key(ARGUMENT(1));

    // assoc-list to find it in
    LispPtr list(ARGUMENT(2));

    LispObject* t;

    // Check that it is a compound object
    CheckArg(list->SubList(), 2, aEnvironment, aStackTop);
    t = (*list->SubList());
    CheckArg(t, 2, aEnvironment, aStackTop);
    t = t->Nixed();

    while (t) {
        if (t->SubList()) {
            LispObject* sub = (*t->SubList());
            if (sub) {
                sub = sub->Nixed();
                LispPtr temp(sub);
                if (InternalEquals(aEnvironment, key, temp)) {
                    RESULT = (t);
                    return;
                }
            }
        }
        t = t->Nixed();
    }

    RESULT = (LispAtom::New(aEnvironment, "Empty"));
}

void LispCurrentFile(LispEnvironment& aEnvironment, int aStackTop)
{
    RESULT = LispAtom::New(aEnvironment,
                           stringify(aEnvironment.iInputStatus.FileName()));
}

void LispCurrentLine(LispEnvironment& aEnvironment, int aStackTop)
{
    RESULT = LispAtom::New(
        aEnvironment, std::to_string(aEnvironment.iInputStatus.LineNumber()));
}

void LispBackQuote(LispEnvironment& aEnvironment, int aStackTop)
{
    BackQuoteBehaviour behaviour(aEnvironment);
    LispPtr result;
    InternalSubstitute(result, ARGUMENT(1), behaviour);
    InternalEval(aEnvironment, RESULT, result);
}

void interpreter(LispEnvironment& aEnvironment, int aStackTop)
{
    RESULT = (LispAtom::New(aEnvironment, "\"yacas\""));
}

void LispVersion(LispEnvironment& aEnvironment, int aStackTop)
{
    RESULT = (LispAtom::New(aEnvironment, "\"" YACAS_VERSION "\""));
}

/// convert bits to digits. Use the kernel function bits_to_digits. Arguments
/// must be small integers.
void LispBitsToDigits(LispEnvironment& aEnvironment, int aStackTop)
{
    RefPtr<BigNumber> x;
    RefPtr<BigNumber> y;
    GetNumber(x, aEnvironment, aStackTop, 1);
    GetNumber(y, aEnvironment, aStackTop, 2);
    long result = 0; // initialize just in case
    if (x->IsInt() && x->IsSmall() && y->IsInt() && y->IsSmall()) {
        // bits_to_digits uses unsigned long, see numbers.h
        unsigned base = unsigned(y->Double());
        result = bits_to_digits((unsigned long)(x->Double()), base);
    } else {
        std::ostringstream buf;
        buf << "BitsToDigits: error: arguments (" << x->Double() << ", "
            << y->Double() << " must be small integers";
        throw LispErrGeneric(buf.str());
    }
    BigNumber* z =
        new BigNumber(std::to_string(result), aEnvironment.BinaryPrecision());
    RESULT = (new LispNumber(z));
}

/// convert digits to bits. Use the kernel function digits_to_bits. Arguments
/// must be small integers.
void LispDigitsToBits(LispEnvironment& aEnvironment, int aStackTop)
{
    RefPtr<BigNumber> x;
    RefPtr<BigNumber> y;
    GetNumber(x, aEnvironment, aStackTop, 1);
    GetNumber(y, aEnvironment, aStackTop, 2);
    long result = 0; // initialize just in case
    if (x->IsInt() && x->IsSmall() && y->IsInt() && y->IsSmall()) {
        // bits_to_digits uses unsigned long, see numbers.h
        unsigned base = unsigned(y->Double());
        result = digits_to_bits((unsigned long)(x->Double()), base);
    } else {
        std::ostringstream buf;
        buf << "BitsToDigits: error: arguments (" << x->Double() << ", "
            << y->Double() << " must be small integers";
        throw LispErrGeneric(buf.str());
    }
    BigNumber* z =
        new BigNumber(std::to_string(result), aEnvironment.BinaryPrecision());
    RESULT = (new LispNumber(z));
}
