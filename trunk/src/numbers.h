
#ifndef __numbers_h__
#define __numbers_h__

#include "lispenvironment.h"
#include "yacasbase.h"

//TODO FIXME : this define is just temporary, to be able to easily enable/disable new numerics
//TODO FIXME : code, but still be able to revert back to stable code. But this has to be removed
//TODO FIXME : at a certain stage.
#define noNO_USE_BIGFLOAT



/// Create a internal number object from an ascii string.
void* AsciiToNumber(LispCharPtr aString,LispInt aPrecision);

/// Convert from internal number format to ascii format
LispStringPtr NumberToAscii(void* aNumber,LispHashTable& aHashTable,
                           LispInt aBase);

/// Whether the numeric library supports 1.0E-10 and such.
LispInt NumericSupportForMantissa();

/// Numeric library name
const LispCharPtr NumericLibraryName();

/// Copy a number class
void* NumberCopy(void* aOriginal);

/// Delete a number object.
void NumberDestroy(void* aNumber);


LispStringPtr GcdInteger(LispCharPtr int1, LispCharPtr int2,
                         LispHashTable& aHashTable);

LispStringPtr MultiplyFloat(LispCharPtr int1, LispCharPtr int2,
                            LispHashTable& aHashTable,LispInt aPrecision);
LispStringPtr AddFloat(LispCharPtr int1, LispCharPtr int2,
                       LispHashTable& aHashTable,LispInt aPrecision);
LispStringPtr PlusFloat(LispCharPtr int1,LispHashTable& aHashTable,LispInt aPrecision);
LispStringPtr SubtractFloat(LispCharPtr int1, LispCharPtr int2,
                            LispHashTable& aHashTable,LispInt aPrecision);
LispStringPtr NegateFloat(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision);
LispStringPtr DivideFloat(LispCharPtr int1, LispCharPtr int2,
                          LispHashTable& aHashTable,LispInt aPrecision);
LispStringPtr PowerFloat(LispCharPtr int1, LispCharPtr int2,
                         LispHashTable& aHashTable,LispInt aPrecision);

LispStringPtr SinFloat(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision);
LispStringPtr CosFloat(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision);
LispStringPtr TanFloat(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision);
LispStringPtr ArcSinFloat(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision);
LispStringPtr ArcCosFloat(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision);
LispStringPtr ArcTanFloat(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision);
LispStringPtr ExpFloat(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision);
LispStringPtr LnFloat(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision);

LispStringPtr SqrtFloat(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision);
LispStringPtr AbsFloat( LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision);

LispBoolean GreaterThan(LispCharPtr int1, LispCharPtr int2,
                       LispHashTable& aHashTable,LispInt aPrecision);
LispBoolean LessThan(LispCharPtr int1, LispCharPtr int2,
                       LispHashTable& aHashTable,LispInt aPrecision);

LispStringPtr ShiftLeft( LispCharPtr int1, LispCharPtr int2, LispHashTable& aHashTable,LispInt aPrecision);
LispStringPtr ShiftRight( LispCharPtr int1, LispCharPtr int2, LispHashTable& aHashTable,LispInt aPrecision);

LispStringPtr FromBase( LispCharPtr int1, LispCharPtr int2, LispHashTable& aHashTable,
                        LispInt aPrecision);
LispStringPtr ToBase( LispCharPtr int1, LispCharPtr int2, LispHashTable& aHashTable,
                      LispInt aPrecision);

LispStringPtr FloorFloat( LispCharPtr int1, LispHashTable& aHashTable,
                        LispInt aPrecision);
LispStringPtr CeilFloat( LispCharPtr int1, LispHashTable& aHashTable,
                        LispInt aPrecision);
LispStringPtr ModFloat( LispCharPtr int1, LispCharPtr int2, LispHashTable& aHashTable,
                        LispInt aPrecision);
LispStringPtr DivFloat( LispCharPtr int1, LispCharPtr int2, LispHashTable& aHashTable,
                        LispInt aPrecision);
LispStringPtr PiFloat( LispHashTable& aHashTable, LispInt aPrecision);

LispStringPtr BitAnd(LispCharPtr int1, LispCharPtr int2,
                     LispHashTable& aHashTable,LispInt aPrecision);
LispStringPtr BitOr(LispCharPtr int1, LispCharPtr int2,
                     LispHashTable& aHashTable,LispInt aPrecision);
LispStringPtr BitXor(LispCharPtr int1, LispCharPtr int2,
                     LispHashTable& aHashTable,LispInt aPrecision);


LispStringPtr LispFactorial(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision);



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


/// Base class for low-level multiple-precision arithmetic.
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
  // assign from string
  void SetTo(const LispCharPtr aString,LispInt aPrecision,LispInt aBase=10);
    // assign from a platform type
  void SetTo(LispInt value);
  void SetTo(double value);
public: // Convert back to other types
  /// ToString : return string representation of number in aResult to given precision (base digits)
  void ToString(LispString& aResult, LispInt aPrecision, LispInt aBase=10) const;
  /// Give approximate representation as a double number
  double Double() const;
public: //information retrieval on library used  
  /// Numeric library name
static const LispCharPtr NumericLibraryName();

public://basic object manipulation
  bool Equals(const BigNumber& aOther) const;
  bool IsInt() const;
  bool IsIntValue() const;
  bool IsSmall() const;
  void BecomeInt();
  void BecomeFloat();
  bool LessThan(const BigNumber& aOther) const;
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

public:
  void Floor(const BigNumber& aX);
  void Precision(LispInt aPrecision);

public:/// Bitwise operations, return result in *this.
  void ShiftLeft( const BigNumber& aX, LispInt aNrToShift);
  void ShiftRight( const BigNumber& aX, LispInt aNrToShift);
  void ShiftLeft( const BigNumber& aX, const BigNumber& aNrToShift);
  void ShiftRight( const BigNumber& aX, const BigNumber& aNrToShift);
  void BitAnd(const BigNumber& aX, const BigNumber& aY);
  void BitOr(const BigNumber& aX, const BigNumber& aY);
  void BitXor(const BigNumber& aX, const BigNumber& aY);
  void BitNot(const BigNumber& aX);
  /// Bit count operation: return the number of significant bits if integer, return the binary exponent if float (shortcut for binary logarithm)
  void BitCount(const BigNumber& aX);
  
  /// Give sign (-1, 0, 1)
  LispInt Sign() const;

public:
  inline LispInt GetPrecision() const {return iPrecision;};
private: 
  LispInt iPrecision;

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
  /// GMP wrapper starts here
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
  { /// bit masks: KExpFloat includes KFloat.
	  KInt = 1,
	  KFloat = 2,
	  KExpFloat = 6,
  };
  mpz_t int_;	// these two are not in a union
  mpf_t float_;	// because we want to avoid excessive memory reallocation.
  /// Type flag (a bit mask).
  unsigned type_;
  mpz_t exponent_; 	// this is only used for exp-floats when the exponent is out of range for GMP.
  /// Check whether we are of exp-float type.
  bool IsExpFloat() const;
  
  /// GMP wrapper ends here.
  #else
    ANumber* iNumber;
  #endif
#endif
};



#endif













