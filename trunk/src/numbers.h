
#ifndef __numbers_h__
#define __numbers_h__

#include "lispenvironment.h"
#include "yacasbase.h"

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



/** Base number class. It is recommended that the derived classes add the following 
 *  static method to construct a number type:
 *
 *  static YacasBigNumber* Make(LispCharPtr aString,LispInt aPrecision,LispInt aBase=10);
 *
 */
 
//TODO remove!!!
#if 0
// reference-counting through a smart pointer, so we can do automatic clean up on numbers
class YacasBigNumber;
typedef const RefPtr<YacasBigNumber> YacasBigNumberPtr;
// pure abstract class representing a number
class YacasBigNumber : public RefCountedObject
{
public: //constructors
  /// Copy a number class
  virtual YacasBigNumber* Copy() const = 0;
  /// ToString : return string representation of number in aResult 
  virtual void ToString(LispString& aResult, LispInt aBase) = 0;
public: //information retrieval on library used  
  /// Numeric library name
  virtual const LispCharPtr NumericLibraryName() = 0;
public://arithmetic
  /// Multiply two numbers, and return result in aResult
  virtual YacasBigNumber* Multiply(YacasBigNumberPtr& aX, YacasBigNumberPtr& aY, LispInt aPrecision) = 0;
  /** Multiply two numbers, and add to aResult (this is useful and generally efficient to implement).
   * This is most likely going to be used by internal functions only, using aResult as an accumulator.
   */
  virtual YacasBigNumber* MultiplyAdd(YacasBigNumber* aResult,
                YacasBigNumberPtr& aX, 
                YacasBigNumberPtr& aY, 
                LispInt aPrecision) = 0;
  /// Add two numbers, and return result in aResult
  virtual YacasBigNumber* Add(YacasBigNumberPtr& aX, YacasBigNumberPtr& aY, LispInt aPrecision) = 0;
  /// Negate the current number, and return it
  virtual YacasBigNumber* Negate() = 0;
  /// Divide, and return result. Note: if the two arguments are integer, it should return an integer result!
  virtual YacasBigNumber* Divide(YacasBigNumberPtr& aX, YacasBigNumberPtr& aY, LispInt aPrecision) = 0;

public://bitwise operations  
  virtual YacasBigNumber* ShiftLeft( YacasBigNumberPtr& aX, LispInt aNrToShift) = 0;
  virtual YacasBigNumber* ShiftRight( YacasBigNumberPtr& aX, LispInt aNrToShift) = 0;
  virtual YacasBigNumber* BitAnd(YacasBigNumberPtr& aX, YacasBigNumberPtr& aY) = 0;
  virtual YacasBigNumber* BitOr(YacasBigNumberPtr& aX, YacasBigNumberPtr& aY) = 0;
  virtual YacasBigNumber* BitXor(YacasBigNumberPtr& aX, YacasBigNumberPtr& aY) = 0;

protected: // I don't want any one to construct this class any other way!
  YacasBigNumber(){};


};
#endif //0

/// Virtual base class for low-level multiple-precision arithmetic.
/// All calculations are done at given precision. Integers grow as needed, floats don't grow beyond given precision.
class NumberBase : public YacasBase
{
public: //constructors
// default constructor
  NumberBase() { };
// copy constructor
  NumberBase(const NumberBase& ) { };
// construct from string
  NumberBase(const LispString& number, LispInt aPrecision, LispInt aBase=10) {};
// construct from a double value
  NumberBase(double number, LispInt aPrecision) {};
  // assign to another number
 virtual void SetTo(const NumberBase& ) = 0;
  /// ToString : return string representation of number in aResult 
  virtual void ToString(LispString& aResult, LispInt aBase) const = 0;
public: //information retrieval on library used  
  /// Numeric library name
  virtual const LispCharPtr NumericLibraryName() const = 0;
public://arithmetic
  /// Multiply two numbers at given precision and put result in *this
  virtual void Multiply(const NumberBase& aX, const NumberBase& aY, LispInt aPrecision) = 0;
  /** Multiply two numbers, and add to aResult (this is useful and generally efficient to implement).
   * This is most likely going to be used by internal functions only, using aResult as an accumulator.
   */
  virtual void MultiplyAdd(NumberBase& aResult, const NumberBase& aX, const NumberBase& aY, LispInt aPrecision) = 0;
  /// Add two numbers at given precision and return result in *this
  virtual void Add(const NumberBase& aX, const NumberBase& aY, LispInt aPrecision) = 0;
  /// Negate the given number, return result in *this
  virtual void Negate(const NumberBase& aX) = 0;
  /// Divide two numbers and return result in *this. Note: if the two arguments are integer, it should return an integer result!
  virtual void Divide(const NumberBase& aX, const NumberBase& aY, LispInt aPrecision) = 0;

public:/// Bitwise operations, return result in *this.
  virtual void ShiftLeft( const NumberBase& aX, LispInt aNrToShift) = 0;
  virtual void ShiftRight( const NumberBase& aX, LispInt aNrToShift) = 0;
  virtual void BitAnd(const NumberBase& aX, const NumberBase& aY) = 0;
  virtual void BitOr(const NumberBase& aX, const NumberBase& aY) = 0;
  virtual void BitXor(const NumberBase& aX, const NumberBase& aY) = 0;
  /// Bit count operation: return the number of significant bits if integer, return the binary exponent if float (shortcut for binary logarithm)
  virtual LispInt BitCount() const = 0;
  /// Give approximate representation as a double number
  virtual double Double() const = 0;
  /// Give sign (-1, 0, 1)
  virtual LispInt Sign() const = 0;
};




/*DEPRECATED, TODO remove
public: //base conversions
  virtual YacasBigNumber* FromBase( const YacasBigNumberPtr& aX, const YacasBigNumberPtr& aY,  LispInt aPrecision) = 0;
  virtual YacasBigNumber* ToBase( const YacasBigNumberPtr& aX, const YacasBigNumberPtr& aY,  LispInt aPrecision) = 0;

  /// Calculate GCD of this object with another, and return result in aResult
  virtual YacasBigNumber* Gcd(YacasBigNumberPtr aX, YacasBigNumberPtr aY) = 0;

  /// Raise power, and return result
  virtual YacasBigNumber* Power(const YacasBigNumberPtr& aX, const YacasBigNumberPtr& aY,LispInt aPrecision) = 0;

public: //trigonometric functions
  virtual YacasBigNumber* Sin(const YacasBigNumberPtr& aX,LispInt aPrecision) = 0;
  virtual YacasBigNumber* Cos(const YacasBigNumberPtr& aX,LispInt aPrecision) = 0;
  virtual YacasBigNumber* Tan(const YacasBigNumberPtr& aX,LispInt aPrecision) = 0;
  virtual YacasBigNumber* ArcSin(const YacasBigNumberPtr& aX,LispInt aPrecision) = 0;
  virtual YacasBigNumber* ArcCos(const YacasBigNumberPtr& aX,LispInt aPrecision) = 0;
  virtual YacasBigNumber* ArcTan(const YacasBigNumberPtr& aX,LispInt aPrecision) = 0;
  virtual YacasBigNumber* Exp(const YacasBigNumberPtr& aX,LispInt aPrecision) = 0;
  virtual YacasBigNumber* Ln(const YacasBigNumberPtr& aX,LispInt aPrecision) = 0;
public://other functions  
  virtual YacasBigNumber* Sqrt(const YacasBigNumberPtr& aX,LispInt aPrecision) = 0;
  virtual YacasBigNumber* Abs( const YacasBigNumberPtr& aX,LispInt aPrecision) = 0;
  virtual YacasBigNumber* Factorial(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision) = 0;
  virtual YacasBigNumber* Pi(LispInt aPrecision) = 0;
  virtual YacasBigNumber* Mod( const YacasBigNumberPtr& aX, const YacasBigNumberPtr& aY, LispInt aPrecision) = 0;
  virtual YacasBigNumber* Div( const YacasBigNumberPtr& aX, const YacasBigNumberPtr& aY, LispInt aPrecision) = 0;
  virtual LispBoolean GreaterThan(const YacasBigNumberPtr& aX, const YacasBigNumberPtr& aY, LispInt aPrecision) = 0;

  virtual YacasBigNumber* Floor(const YacasBigNumberPtr& aX, LispInt aPrecision) = 0;
  virtual YacasBigNumber* Ceil( const YacasBigNumberPtr& aX,LispInt aPrecision) = 0;
public://comparisons  
  virtual LispBoolean LessThan(const YacasBigNumberPtr& aX, const YacasBigNumberPtr& aY, LispInt aPrecision) = 0;

*/






/*
YacasBigNumberPtr adder = MakeBigNum();
YacasBigNumberPtr result =  adder->Add(
                              MakeBigNum("23",10),
                              MakeBigNum("45.23e3",10)
                                      );
  LispString str;
  result->ToString(str,10);
  aResult.Set(LispAtom::New(iEnvironment.HashTable().Lookup(str.String())));
*/


#endif













