
#ifndef __platmath_h__
#define __platmath_h__

// Beware the use of these functions! They cannot be guaranteed to be
// supported on any platform.
double GetDouble(LispCharPtr aString);
LispStringPtr Double(double aValue, LispHashTable& aHashTable);

LispStringPtr PlatSin(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision);
LispStringPtr PlatCos(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision);
LispStringPtr PlatTan(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision);
LispStringPtr PlatArcSin(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision);
LispStringPtr PlatArcCos(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision);
LispStringPtr PlatArcTan(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision);
LispStringPtr PlatExp(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision);
LispStringPtr PlatLn(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision);
LispStringPtr PlatPower(LispCharPtr int1, LispCharPtr int2,
                        LispHashTable& aHashTable,LispInt aPrecision);

LispStringPtr PlatSqrt(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision);
LispStringPtr PlatPi(LispHashTable& aHashTable,LispInt aPrecision);
LispStringPtr PlatFloor(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision);
LispStringPtr PlatCeil(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision);
LispStringPtr PlatMod(LispCharPtr int1, LispCharPtr int2,LispHashTable& aHashTable,LispInt aPrecision);
LispStringPtr PlatDiv(LispCharPtr int1, LispCharPtr int2,LispHashTable& aHashTable,LispInt aPrecision);
LispStringPtr PlatAbs(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision);

LispStringPtr PlatIsPrime(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision);

// table lookup for small prime numbers
unsigned primes_table_check(unsigned long p);
unsigned primes_table_range();

// lookup table for Ln(n)/Ln(2). This works whether or not we have math.h.
// table range is from 2 to this value:
unsigned log2_table_range();
// convert the number of digits in given base to the number of bits, and back.
// need to round the number of digits.
// These functions only work for aBase inside the allowed table range.
unsigned long digits_to_bits(unsigned long aDigits, unsigned aBase);
unsigned long bits_to_digits(unsigned long abits, unsigned aBase);



/*
class NativeNumber : public NumberBase
{
public: //constructors
  NativeNumber(LispCharPtr aString,LispInt aPrecision,LispInt aBase=10);
public: //constructors
  virtual void SetTo(const NumberBase&);
  virtual void ToString(LispString& aResult, LispInt aBase) const;
public: //information retrieval on library used  
  virtual const LispCharPtr NumericLibraryName() const;
public://arithmetic
  virtual void Multiply(const NumberBase& aX, const NumberBase& aY, LispInt aPrecision);
  virtual void MultiplyAdd(NumberBase& aResult,
                const NumberBase& aX, 
                const NumberBase& aY, 
                LispInt aPrecision);
  virtual void Add(const NumberBase& aX, const NumberBase& aY, LispInt aPrecision);
  virtual void Negate(const NumberBase& aX);
  virtual void Divide(const NumberBase& aX, const NumberBase& aY, LispInt aPrecision);
  virtual LispInt BitCount() const;
  virtual double Double() const;
  virtual LispInt Sign() const;
public://bitwise operations  
  virtual void ShiftLeft( const NumberBase& aX, LispInt aNrToShift);
  virtual void ShiftRight( const NumberBase& aX, LispInt aNrToShift);
  virtual void BitAnd(const NumberBase& aX, const NumberBase& aY);
  virtual void BitOr(const NumberBase& aX, const NumberBase& aY);
  virtual void BitXor(const NumberBase& aX, const NumberBase& aY);
public:
};
*/

#endif

