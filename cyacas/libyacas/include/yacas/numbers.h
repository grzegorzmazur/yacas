#ifndef YACAS_NUMBERS_H
#define YACAS_NUMBERS_H

#include "lispenvironment.h"

/// Whether the numeric library supports 1.0E-10 and such.
int NumericSupportForMantissa();

LispObject* GcdInteger(LispObject* int1, LispObject* int2, LispEnvironment& aEnvironment);

LispObject* SinFloat(LispObject* int1, LispEnvironment& aEnvironment,int aPrecision);
LispObject* CosFloat(LispObject* int1, LispEnvironment& aEnvironment,int aPrecision);
LispObject* TanFloat(LispObject* int1, LispEnvironment& aEnvironment,int aPrecision);
LispObject* ArcSinFloat(LispObject* int1, LispEnvironment& aEnvironment,int aPrecision);
LispObject* ExpFloat(LispObject* int1, LispEnvironment& aEnvironment,int aPrecision);
LispObject* LnFloat(LispObject* int1, LispEnvironment& aEnvironment,int aPrecision);

LispObject* SqrtFloat(LispObject* int1, LispEnvironment& aEnvironment,int aPrecision);
LispObject* ModFloat( LispObject* int1, LispObject* int2, LispEnvironment& aEnvironment,
                        int aPrecision);

LispObject* PowerFloat(LispObject* int1, LispObject* int2,
                         LispEnvironment& aEnvironment,int aPrecision);
LispObject* ShiftLeft( LispObject* int1, LispObject* int2, LispEnvironment& aEnvironment,int aPrecision);
LispObject* ShiftRight( LispObject* int1, LispObject* int2, LispEnvironment& aEnvironment,int aPrecision);
LispObject* LispFactorial(LispObject* int1, LispEnvironment& aEnvironment,int aPrecision);



// methods generally useful for all numeric libraries
const unsigned GUARD_BITS = 8;  // we leave this many guard bits untruncated in various situations when we need to truncate precision by hand

const long DIST_BITS = 1;  // at least this many bits of difference - used in precision tracking

/// DIST(x, y) returns 1 if abs(x-y) >= DIST_BITS. See documentation for precision tracking.
template<class T> inline T DIST(T x, T y) { return (x>=y+DIST_BITS || y>=x+DIST_BITS) ? 0 : 1; }


/** Base number class.
 */


class ANumber;



/// Main class for multiple-precision arithmetic.
/// All calculations are done at given precision. Integers grow as needed, floats don't grow beyond given precision.
class BigNumber {
public: //constructors
  BigNumber(const char* aString,int aPrecision,int aBase=10);
/// copy constructor
  BigNumber(const BigNumber& aOther);
  // no constructors from int or double to avoid automatic conversions
  BigNumber(int aPrecision = 20);
  ~BigNumber();
  // assign from another number
  void SetTo(const BigNumber& aOther);
  // assign from string, precision in base digits
  void SetTo(const char* aString,int aPrecision,int aBase=10);
    // assign from a platform type
  void SetTo(long value);
  inline void SetTo(int value) { SetTo(long(value)); };
  void SetTo(double value);
public: // Convert back to other types
  /// ToString : return string representation of number in aResult to given precision (base digits)
  void ToString(LispString& aResult, int aPrecision, int aBase=10) const;
  /// Give approximate representation as a double number
  double Double() const;

public://basic object manipulation
  bool Equals(const BigNumber& aOther) const;
  bool IsInt() const;
  bool IsIntValue() const;
  bool IsSmall() const;
  void BecomeInt();
  void BecomeFloat(int aPrecision=0);
  bool LessThan(const BigNumber& aOther) const;
public://arithmetic
  /// Multiply two numbers at given precision and put result in *this
  void Multiply(const BigNumber& aX, const BigNumber& aY, int aPrecision);
  /** Multiply two numbers, and add to *this (this is useful and generally efficient to implement).
   * This is most likely going to be used by internal functions only, using aResult as an accumulator.
   */
  void MultiplyAdd(const BigNumber& aX, const BigNumber& aY, int aPrecision);
  /// Add two numbers at given precision and return result in *this
  void Add(const BigNumber& aX, const BigNumber& aY, int aPrecision);
  /// Negate the given number, return result in *this
  void Negate(const BigNumber& aX);
  /// Divide two numbers and return result in *this. Note: if the two arguments are integer, it should return an integer result!
  void Divide(const BigNumber& aX, const BigNumber& aY, int aPrecision);

  /// integer operation: *this = y mod z
  void Mod(const BigNumber& aY, const BigNumber& aZ);

  /// For debugging purposes, dump internal state of this object into a string
  void DumpDebugInfo(std::ostream&) const;

public:
  /// assign self to Floor(aX) if possible
  void Floor(const BigNumber& aX);
  /// set precision (in bits)
  void Precision(int aPrecision);

public:/// Bitwise operations, return result in *this.
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

public:
  inline int GetPrecision() const {return iPrecision;};

private:
  BigNumber& operator=(const BigNumber& aOther)
  {
    // copy constructor not written yet, hence the assert
    assert(0);
    return *this;
  }
public:
  unsigned iReferenceCount;
private:
  int iPrecision;

public:
  /// Internal library wrapper starts here.
    inline void SetIsInteger(bool aIsInteger) {iType = (aIsInteger ? KInt : KFloat);}
private:
    enum ENumType
    {
      KInt = 0,
      KFloat
    };
    ENumType iType;

    friend LispObject* GcdInteger(LispObject* int1, LispObject* int2, LispEnvironment& aEnvironment);
    friend LispObject* SinFloat(LispObject* int1, LispEnvironment& aEnvironment,int aPrecision);
    friend LispObject* CosFloat(LispObject* int1, LispEnvironment& aEnvironment,int aPrecision);
    friend LispObject* TanFloat(LispObject* int1, LispEnvironment& aEnvironment,int aPrecision);
    friend LispObject* ArcSinFloat(LispObject* int1, LispEnvironment& aEnvironment,int aPrecision);
    friend LispObject* ExpFloat(LispObject* int1, LispEnvironment& aEnvironment,int aPrecision);
    friend LispObject* LnFloat(LispObject* int1, LispEnvironment& aEnvironment,int aPrecision);
    friend LispObject* SqrtFloat(LispObject* int1, LispEnvironment& aEnvironment,int aPrecision);
    friend LispObject* PowerFloat(LispObject* int1, LispObject* int2,
                           LispEnvironment& aEnvironment,int aPrecision);
    ANumber* iNumber;
  /// Internal library wrapper ends here.
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













