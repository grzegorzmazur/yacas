
#ifndef __numbers_h__
#define __numbers_h__

#include "lispenvironment.h"
#include "yacasbase.h"

//TODO FIXME : this define is just temporary, to be able to easily enable/disable new numerics
//TODO FIXME : code, but still be able to revert back to stable code. But this has to be removed
//TODO FIXME : at a certain stage.
#define noNO_USE_BIGFLOAT

// this will switch to the new BigNumber/BigInt/BigFloat scheme
#define noUSE_NEW_BIGNUM


/// Whether the numeric library supports 1.0E-10 and such.
LispInt NumericSupportForMantissa();

/// Numeric library name
const LispCharPtr NumericLibraryName();

LispObject* GcdInteger(LispObject* int1, LispObject* int2, LispEnvironment& aEnvironment);

LispObject* SinFloat(LispObject* int1, LispEnvironment& aEnvironment,LispInt aPrecision);
LispObject* CosFloat(LispObject* int1, LispEnvironment& aEnvironment,LispInt aPrecision);
LispObject* TanFloat(LispObject* int1, LispEnvironment& aEnvironment,LispInt aPrecision);
LispObject* ArcSinFloat(LispObject* int1, LispEnvironment& aEnvironment,LispInt aPrecision);
LispObject* ArcCosFloat(LispObject* int1, LispEnvironment& aEnvironment,LispInt aPrecision);
LispObject* ArcTanFloat(LispObject* int1, LispEnvironment& aEnvironment,LispInt aPrecision);
LispObject* ExpFloat(LispObject* int1, LispEnvironment& aEnvironment,LispInt aPrecision);
LispObject* LnFloat(LispObject* int1, LispEnvironment& aEnvironment,LispInt aPrecision);

LispObject* SqrtFloat(LispObject* int1, LispEnvironment& aEnvironment,LispInt aPrecision);
LispObject* ModFloat( LispObject* int1, LispObject* int2, LispEnvironment& aEnvironment,
                        LispInt aPrecision);

//TODO remove LispObject* PiFloat( LispEnvironment& aEnvironment, LispInt aPrecision);
LispObject* PowerFloat(LispObject* int1, LispObject* int2,
                         LispEnvironment& aEnvironment,LispInt aPrecision);
LispObject* ShiftLeft( LispObject* int1, LispObject* int2, LispEnvironment& aEnvironment,LispInt aPrecision);
LispObject* ShiftRight( LispObject* int1, LispObject* int2, LispEnvironment& aEnvironment,LispInt aPrecision);
LispObject* LispFactorial(LispObject* int1, LispEnvironment& aEnvironment,LispInt aPrecision);



// methods generally useful for all numeric libraries
const unsigned GUARD_BITS = 8;	// we leave this many guard bits untruncated in various situations when we need to truncate precision by hand

template<class T> inline T MAX(T x, T y) { if (x<y) return y; else return x; }
template<class T> inline T MIN(T x, T y) { if (x>y) return y; else return x; }

const long DIST_BITS = 1;	// at least this many bits of difference - used in precision tracking

/// DIST(x, y) returns 1 if abs(x-y) >= DIST_BITS. See documentation for precision tracking.
template<class T> inline T DIST(T x, T y) { return (x>=y+DIST_BITS || y>=x+DIST_BITS) ? 0 : 1; }


/** Base number class. 
 */
 
// You can enable this define to have a BigNumber implementation based on 'double' and 'long'
#define noUSE_NATIVE

#ifndef USE_NATIVE
  #ifdef USE_GMP
	#include <gmp.h>
  #else
    class ANumber;
  #endif
#endif

#ifdef USE_NEW_BIGNUM
/// Low-level wrapper classes for multiple-precision arithmetic.
/// BigInt and BigFloat will be impemented in separate c++ files, e.g. gmpnumbers.cpp, yacasnumbers.cpp, etc.

class BigInt;

class BigFloat
{
friend class BigInt;
public: //constructors
/// assign a float from given string, using exactly aPrecision *bits*
  BigFloat(const LispCharPtr aString,LispInt aPrecision,LispInt aBase=10);
/// copy constructor
  BigFloat(const BigFloat& aOther);
  // no constructors from int or double to avoid automatic conversions
  BigFloat(LispInt aPrecision = 32);
  ~BigFloat();
  // assign from another number
  void SetTo(const BigFloat& aOther);
  void SetTo(const BigInt& aOther, LispInt aPrecision);
  // assign from string, using exactly aPrecision *digits*
  void SetTo(const LispCharPtr aString,LispInt aPrecision,LispInt aBase=10);
    // assign from a platform type
  void SetTo(double value);
public: // Convert back to other types
  /// GetMantissaExp: return a string representation of the mantissa in aResult to given precision (base digits), and the exponent in the same base into aExponent
  void GetMantissaExp(LispCharPtr aBuffer, unsigned long aBufferSize, long* aExponent, LispInt aPrecision, LispInt aBase=10) const;
  /// Give approximate representation as a double number
  double Double() const;

  /// Numeric library name
static const LispCharPtr NumericLibraryName();

public://basic object manipulation
  LispBoolean Equals(const BigFloat& aOther) const;
  LispBoolean IsIntValue() const;
  LispBoolean LessThan(const BigFloat& aOther) const;
public://arithmetic
  /// Multiply two numbers and put result in *this, result should have at least aPrecision correct digits
  void Multiply(const BigFloat& aX, const BigFloat& aY, LispInt aPrecision);
  /** Multiply two numbers, and add to *this (this is useful and generally efficient to implement).
   * This is most likely going to be used by internal functions only, using aResult as an accumulator.
   */
  void MultiplyAdd(const BigFloat& aX, const BigFloat& aY, LispInt aPrecision);
  /// Add two numbers at given precision and return result in *this
  void Add(const BigFloat& aX, const BigFloat& aY, LispInt aPrecision);
  /// Negate the given number, return result in *this
  void Negate(const BigFloat& aX);
  /// Divide two numbers and return result in *this. Note: if the two arguments are integer, it should return an integer result!
  void Divide(const BigFloat& aX, const BigFloat& aY, LispInt aPrecision);

public:
/// return the integer part of the number (still as float value)
  void Floor(const BigFloat& aX);
/// set internal precision to this many bits
  void Precision(LispInt aPrecision);

public:
/// Multiplication by a power of 2, return result in *this.
  void Multiply2exp(const BigFloat& aX, LispInt aNrToShift);
  /// return the binary exponent (shortcut for binary logarithm)
  long GetBinaryExp() const;
  
  /// Give sign (-1, 0, 1)
  LispInt Sign() const;
  /// Import/export underlying objects.
  void ImportData(const void* aData);
  const void* ExportData() const;

public:
/// manipulate internal precision (in bits)
  inline LispInt GetPrecision() const {return iPrecision;};

private: 
  LispInt iPrecision;

private:
#ifdef USE_GMP
	// gmpnumbers
	mpf_t iFloat;
	void init(LispInt aPrecision = 32);	
#else
	// yacasnumbers
	ANumber* iNumber;
#endif
};

class BigInt
{
friend class BigFloat;
public:
/// assign an int from given string
  BigInt(const LispCharPtr aString, LispInt aBase=10);
/// copy constructor
  BigInt(const BigInt& aOther);
  // no constructors from int or double to avoid automatic conversions
  BigInt();
  ~BigInt();
  // assign from another number
  void SetTo(const BigFloat& aOther);
  void SetTo(const BigInt& aOther);
  // assign from string
  void SetTo(const LispCharPtr aString, LispInt aBase=10);
    // assign from a platform type
  void SetTo(long value);
public: // Convert back to other types
  /// ToString: return a string representation in the given aBuffer
  void ToString(LispCharPtr aBuffer, unsigned long aBufferSize, LispInt aBase=10) const;
  /// Give approximate representation as a double number
  double Double() const;

  /// Numeric library name
static const LispCharPtr NumericLibraryName();

public://basic object manipulation
  LispBoolean Equals(const BigInt& aOther) const;
  LispBoolean IsSmall() const;
  LispBoolean LessThan(const BigInt& aOther) const;
public://arithmetic
  /// Multiply two integers and put result in *this
  void Multiply(const BigInt& aX, const BigInt& aY);
  /** Multiply two numbers, and add to *this (this is useful and generally efficient to implement).
   * This is most likely going to be used by internal functions only, using aResult as an accumulator.
   */
  void MultiplyAdd(const BigInt& aX, const BigInt& aY);
  /// Add two integers and return result in *this
  void Add(const BigInt& aX, const BigInt& aY);
  /// Negate the given number, return result in *this
  void Negate(const BigInt& aX);
  /// Divide two numbers and return result in *this. (This is the integer division!)
  void Div(const BigInt& aX, const BigInt& aY);

  /// integer operation: *this = y mod z
  void Mod(const BigInt& aY, const BigInt& aZ);

public:/// Bitwise operations, return result in *this.
  void ShiftLeft(const BigInt& aX, LispInt aNrToShift);
  void ShiftRight(const BigInt& aX, LispInt aNrToShift);
  void BitAnd(const BigInt& aX, const BigInt& aY);
  void BitOr(const BigInt& aX, const BigInt& aY);
  void BitXor(const BigInt& aX, const BigInt& aY);
  void BitNot(const BigInt& aX);
  /// Bit count operation: return the number of significant bits,
  /// give bit count as a platform integer
  signed long BitCount() const;
  
  /// Give sign (-1, 0, 1)
  LispInt Sign() const;
  /// Import/export underlying objects.
  void ImportData(const void* aData);
  const void* ExportData() const;
private:
#ifdef USE_GMP
	// gmpnumbers
	mpz_t iInt;
	void init();	
#else
	// yacasnumbers
	ANumber* iNumber;
#endif

};

#endif // USE_NEW_BIGNUM
		
/// Main class for multiple-precision arithmetic.
/// All calculations are done at given precision. Integers grow as needed, floats don't grow beyond given precision.
class BigNumber : public RefCountedObject
{
public: //constructors
  BigNumber(const LispCharPtr aString,LispInt aPrecision,LispInt aBase=10);
/// copy constructor
  BigNumber(const BigNumber& aOther);
  // no constructors from int or double to avoid automatic conversions
  BigNumber(LispInt aPrecision = 20);
  ~BigNumber();
  // assign from another number
  void SetTo(const BigNumber& aOther);
  // assign from string, precision in base digits
  void SetTo(const LispCharPtr aString,LispInt aPrecision,LispInt aBase=10);
    // assign from a platform type
  void SetTo(long value);
  inline void SetTo(LispInt value) { SetTo(long(value)); };
  void SetTo(double value);
public: // Convert back to other types
  /// ToString : return string representation of number in aResult to given precision (base digits)
  void ToString(LispString& aResult, LispInt aPrecision, LispInt aBase=10) const;
  /// Give approximate representation as a double number
  double Double() const;

  /// Numeric library name
static const LispCharPtr NumericLibraryName();

public://basic object manipulation
  LispBoolean Equals(const BigNumber& aOther) const;
  LispBoolean IsInt() const;
  LispBoolean IsIntValue() const;
  LispBoolean IsSmall() const;
  void BecomeInt();
  void BecomeFloat(LispInt aPrecision=0);
  LispBoolean LessThan(const BigNumber& aOther) const;
public://arithmetic
  /// Multiply two numbers at given precision and put result in *this
  void Multiply(const BigNumber& aX, const BigNumber& aY, LispInt aPrecision);
  /** Multiply two numbers, and add to *this (this is useful and generally efficient to implement).
   * This is most likely going to be used by internal functions only, using aResult as an accumulator.
   */
  void MultiplyAdd(const BigNumber& aX, const BigNumber& aY, LispInt aPrecision);
  /// Add two numbers at given precision and return result in *this
  void Add(const BigNumber& aX, const BigNumber& aY, LispInt aPrecision);
  /// Negate the given number, return result in *this
  void Negate(const BigNumber& aX);
  /// Divide two numbers and return result in *this. Note: if the two arguments are integer, it should return an integer result!
  void Divide(const BigNumber& aX, const BigNumber& aY, LispInt aPrecision);

  /// integer operation: *this = y mod z
  void Mod(const BigNumber& aY, const BigNumber& aZ);

  /// For debugging purposes, dump internal state of this object into a string
  void DumpDebugInfo();

public:
  /// assign self to Floor(aX) if possible
  void Floor(const BigNumber& aX);
  /// set precision (in bits)
  void Precision(LispInt aPrecision);

public:/// Bitwise operations, return result in *this.
  void ShiftLeft( const BigNumber& aX, LispInt aNrToShift);
  void ShiftRight( const BigNumber& aX, LispInt aNrToShift);
  void BitAnd(const BigNumber& aX, const BigNumber& aY);
  void BitOr(const BigNumber& aX, const BigNumber& aY);
  void BitXor(const BigNumber& aX, const BigNumber& aY);
  void BitNot(const BigNumber& aX);
  /// Bit count operation: return the number of significant bits if integer, return the binary exponent if float (shortcut for binary logarithm)
  /// give bit count as a platform integer
  signed long BitCount() const;
  
  /// Give sign (-1, 0, 1)
  LispInt Sign() const;

public:
  inline LispInt GetPrecision() const {return iPrecision;};
private: 
  LispInt iPrecision;

#ifdef USE_NEW_BIGNUM
public:
  /// Import and export constituent objects.
  void ImportBigInt(const BigInt &);
  void ImportBigFloat(const BigFloat &);
  void ExportBigInt(BigInt &) const;
  void ExportBigFloat(BigFloat &) const;
private:
	enum EType
	{ /// bit masks: KFloatNAN includes KFloat.
	  KInt = 1,
	  KFloat = 2,
	  KFloatNAN =  2 | 4
	};
	BigInt int_;
	BigFloat float_;
  /// Auxiliary internal private functions.
  /// Initialize all objects.
	void init(LispInt aPrecision = 32);	
  /// Change types to int and float but do not convert any values.
	void turn_float();
	void turn_int();
	unsigned type_;
#else
#ifdef USE_NATIVE

private:
  enum EType
  {
    KInteger = 0,
    KDouble=1,
  };
  union UValue
  {
    long i;
    double  d;
  };
  EType type;
  UValue value;

#else 
  #ifdef USE_GMP
  /// direct GMP wrapper starts here
  public:
  /// These functions will enable us to use arbitrary GMP functions without changing this class definition.
  /// Copy from gmp objects (which must have values).
  void import_gmp(mpz_t gmp_int);
  void import_gmp(mpf_t gmp_float);
  // Copy to gmp objects (which must be already initialized and of correct type).
  void export_gmp(mpz_t gmp_int) const;
  void export_gmp(mpf_t gmp_float) const;
  private:
  /// Auxiliary internal private functions.
  /// Initialize all gmp objects.
  void init();
  /// Change types to int and float but do not convert any values.
  void turn_float();
  void turn_int();
  /// This type gives masks to check the current type of the BigNumber.
  enum EType
  { /// bit masks: KFloatNAN includes KFloat.
	  KInt = 1,
	  KFloat = 2,
	  KFloatNAN =  2 | 4
  };
  mpz_t int_;	// these two are not in a union
  mpf_t float_;	// because we want to avoid excessive memory reallocation.
  /// Type flag (a bit mask).
  unsigned type_;
  
  /// direct GMP wrapper ends here.
  #else
  public:
  /// Internal library wrapper starts here.
    inline void SetIsInteger(LispBoolean aIsInteger) {iType = (aIsInteger ? KInt : KFloat);}
    enum ENumType
    {
      KInt = 0,
      KFloat
    };
    ENumType iType;
    ANumber* iNumber;
  /// Internal library wrapper ends here.
  #endif	// USE_GMP
#endif	// USE_NATIVE
#endif // USE_NEW_BIGNUM
};

/// bits_to_digits and digits_to_bits, utility functions
/// to convert the number of digits in some base (usually 10) to bits and back

// lookup table for Ln(n)/Ln(2). This works whether or not we have math.h.
// table range is from 2 to this value:
unsigned log2_table_range();
// convert the number of digits in given base to the number of bits, and back.
// need to round the number of digits.
// These functions only work for aBase inside the allowed table range.
unsigned long digits_to_bits(unsigned long aDigits, unsigned aBase);
unsigned long bits_to_digits(unsigned long abits, unsigned aBase);


#endif













