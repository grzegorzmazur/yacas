
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


class NativeNumber : public NumberBase
{
public: //constructors
  /// Construct a number class
  NativeNumber(LispCharPtr aString,LispInt aPrecision,LispInt aBase=10);
public: //constructors
  virtual void SetTo(const NumberBase&);
  /// ToString : return string representation of number in aResult 
  virtual void ToString(LispString& aResult, LispInt aBase) const;
public: //information retrieval on library used  
  /// Numeric library name
  virtual const LispCharPtr NumericLibraryName() const;
public://arithmetic
  /// Multiply two numbers, and return result in aResult
  virtual void Multiply(const NumberBase& aX, const NumberBase& aY, LispInt aPrecision);
  /** Multiply two numbers, and add to aResult (this is useful and generally efficient to implement).
   * This is most likely going to be used by internal functions only, using aResult as an accumulator.
   */
  virtual void MultiplyAdd(NumberBase& aResult,
                const NumberBase& aX, 
                const NumberBase& aY, 
                LispInt aPrecision);
  /// Add two numbers, and return result in aResult
  virtual void Add(const NumberBase& aX, const NumberBase& aY, LispInt aPrecision);
  /// Negate the current number, and return it
  virtual void Negate(const NumberBase& aX);
  /// Divide, and return result. Note: if the two arguments are integer, it should return an integer result!
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
  inline NativeNumber(const NativeNumber& aOther)
  {
    type  = aOther.type;
    value = aOther.value;
  }
  inline NativeNumber(const NumberBase& aOther)
  {
    type  = ((const NativeNumber*)(&aOther))->type;
    value = ((const NativeNumber*)(&aOther))->value;
  }

  inline NativeNumber() {type = KDouble;value.d = 0.0;}
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
};


#endif

