#ifndef YACAS_NUMBERS_H
#define YACAS_NUMBERS_H

#include "lispenvironment.h"
#include "anumber.h"
#include "refcount.h"
#include "yacas/mp/zz.hpp"

#include <memory>

using namespace yacas;

LispObject* GcdInteger(LispObject* int1, LispObject* int2, LispEnvironment& aEnvironment);
LispObject* SqrtFloat(LispObject* int1, LispEnvironment& aEnvironment,int aPrecision);
LispObject* ModFloat( LispObject* int1, LispObject* int2, LispEnvironment& aEnvironment,
                        int aPrecision);

LispObject* PowerFloat(LispObject* int1, LispObject* int2,
                         LispEnvironment& aEnvironment,int aPrecision);
LispObject* ShiftLeft( LispObject* int1, LispObject* int2, LispEnvironment& aEnvironment,int aPrecision);
LispObject* ShiftRight( LispObject* int1, LispObject* int2, LispEnvironment& aEnvironment,int aPrecision);
LispObject* LispFactorial(LispObject* int1, LispEnvironment& aEnvironment,int aPrecision);

/** Base number class.
 */


/// Main class for multiple-precision arithmetic.
/// All calculations are done at given precision. Integers grow as needed, floats don't grow beyond given precision.
class BigNumber: public RefCount {
public: //constructors
    BigNumber(const std::string& aString,int aPrecision,int aBase=10);
    explicit BigNumber(const mp::ZZ& zz);
    /// copy constructor
    explicit BigNumber(const BigNumber& aOther);

    BigNumber& operator = (const BigNumber&);

    /// ToString : return string representation of number in aResult to given precision (base digits)
    void ToString(std::string& aResult, int aPrecision, int aBase=10) const;
    /// Give approximate representation as a double number
    double Double() const;

    //basic object manipulation
    bool Equals(const BigNumber& aOther) const;
    bool IsInt() const;
    bool IsSmall() const;
    void BecomeInt();
    void BecomeFloat(int aPrecision=0);
    bool LessThan(const BigNumber& aOther) const;
    //arithmetic
    /// Multiply two numbers at given precision and put result in *this
    void Multiply(const BigNumber& aX, const BigNumber& aY, int aPrecision);
    /// Add two numbers at given precision and return result in *this
    void Add(const BigNumber& aX, const BigNumber& aY, int aPrecision);
    /// Negate the given number, return result in *this
    void Negate(const BigNumber& aX);
    /// Divide two numbers and return result in *this. Note: if the two arguments are integer, it should return an integer result!
    void Divide(const BigNumber& aX, const BigNumber& aY, int aPrecision);

    /// For debugging purposes, dump internal state of this object into a string
    void DumpDebugInfo(std::ostream&) const;

    /// assign self to Floor(aX) if possible
    void Floor(const BigNumber& aX);
    /// set precision (in bits)
    void Precision(int aPrecision);

    /// Bitwise operations, return result in *this.
    void ShiftLeft( const BigNumber& aX, int aNrToShift);
    void ShiftRight( const BigNumber& aX, int aNrToShift);
    void BitAnd(const BigNumber& aX, const BigNumber& aY);
    void BitOr(const BigNumber& aX, const BigNumber& aY);
    void BitXor(const BigNumber& aX, const BigNumber& aY);
    void BitNot(const BigNumber& aX);
    /// Bit count operation: return the number of significant bits if integer, return the binary exponent if float (shortcut for binary logarithm)
    /// give bit count as a platform integer
    signed long BitCount() const;

    /// Give sign (-1, 0, 1)
    int Sign() const;

    inline int GetPrecision() const {return iPrecision;};

private:
    int iPrecision;

    friend LispObject* GcdInteger(LispObject* int1, LispObject* int2, LispEnvironment& aEnvironment);
    friend LispObject* SqrtFloat(LispObject* int1, LispEnvironment& aEnvironment,int aPrecision);
    friend LispObject* PowerFloat(LispObject* int1, LispObject* int2, LispEnvironment& aEnvironment,int aPrecision);

    std::unique_ptr<ANumber> iNumber;
    std::unique_ptr<mp::ZZ> _zz;
};

/// bits_to_digits and digits_to_bits, utility functions
/// to convert the number of digits in some base (usually 10) to bits and back

// lookup table for Ln(n)/Ln(2)
// table range is from 2 to this value:
unsigned log2_table_range();
// convert the number of digits in given base to the number of bits, and back.
// need to round the number of digits.
// These functions only work for aBase inside the allowed table range.
unsigned long digits_to_bits(unsigned long aDigits, unsigned aBase);
unsigned long bits_to_digits(unsigned long abits, unsigned aBase);


#endif
