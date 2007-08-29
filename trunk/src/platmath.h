
#ifndef __platmath_h__
#define __platmath_h__

// Beware the use of these functions! They cannot be guaranteed to be
// supported on any platform.
double GetDouble(LispObject* aInteger);
LispObject* Double(LispEnvironment& aEnvironment,double aValue);

LispObject* PlatArcSin(LispEnvironment& aEnvironment,LispObject* int1, LispInt aPrecision);
LispObject* PlatArcCos(LispEnvironment& aEnvironment,LispObject* int1, LispInt aPrecision);
LispObject* PlatArcTan(LispEnvironment& aEnvironment,LispObject* int1, LispInt aPrecision);
LispObject* PlatExp(LispEnvironment& aEnvironment,LispObject* int1, LispInt aPrecision);
LispObject* PlatLn(LispEnvironment& aEnvironment,LispObject* int1, LispInt aPrecision);
LispObject* PlatPower(LispEnvironment& aEnvironment,LispObject* int1, LispObject* int2,
                        LispInt aPrecision);

LispObject* PlatSqrt(LispEnvironment& aEnvironment,LispObject* int1, LispInt aPrecision);
LispObject* PlatPi(LispEnvironment& aEnvironment,LispInt aPrecision);
LispObject* PlatFloor(LispEnvironment& aEnvironment,LispObject* int1, LispInt aPrecision);
LispObject* PlatCeil(LispEnvironment& aEnvironment,LispObject* int1, LispInt aPrecision);
LispObject* PlatMod(LispEnvironment& aEnvironment,LispObject* int1, LispObject* int2,LispInt aPrecision);
LispObject* PlatDiv(LispEnvironment& aEnvironment,LispObject* int1, LispObject* int2,LispInt aPrecision);
LispObject* PlatAbs(LispEnvironment& aEnvironment,LispObject* int1, LispInt aPrecision);

LispObject* PlatIsPrime(LispEnvironment& aEnvironment,LispObject* int1, LispInt aPrecision);

// table lookup for small prime numbers
unsigned primes_table_check(unsigned long p);
unsigned primes_table_range();



/*
class NativeNumber : public NumberBase
{
public: //constructors
  NativeNumber(LispEnvironment& aEnvironment,LispChar * aString,LispInt aPrecision,LispInt aBase=10);
public: //constructors
  virtual void SetTo(LispEnvironment& aEnvironment,const NumberBase&);
  virtual void ToString(LispEnvironment& aEnvironment,LispString& aResult, LispInt aBase) const;
public: //information retrieval on library used
  virtual const LispChar * NumericLibraryName(LispEnvironment& aEnvironment,) const;
public://arithmetic
  virtual void Multiply(LispEnvironment& aEnvironment,const NumberBase& aX, const NumberBase& aY, LispInt aPrecision);
  virtual void MultiplyAdd(LispEnvironment& aEnvironment,NumberBase& aResult,
                const NumberBase& aX,
                const NumberBase& aY,
                LispInt aPrecision);
  virtual void Add(LispEnvironment& aEnvironment,const NumberBase& aX, const NumberBase& aY, LispInt aPrecision);
  virtual void Negate(LispEnvironment& aEnvironment,const NumberBase& aX);
  virtual void Divide(LispEnvironment& aEnvironment,const NumberBase& aX, const NumberBase& aY, LispInt aPrecision);
  virtual LispInt BitCount(LispEnvironment& aEnvironment,) const;
  virtual double Double(LispEnvironment& aEnvironment,) const;
  virtual LispInt Sign(LispEnvironment& aEnvironment,) const;
public://bitwise operations
  virtual void ShiftLeft(LispEnvironment& aEnvironment, const NumberBase& aX, LispInt aNrToShift);
  virtual void ShiftRight(LispEnvironment& aEnvironment, const NumberBase& aX, LispInt aNrToShift);
  virtual void BitAnd(LispEnvironment& aEnvironment,const NumberBase& aX, const NumberBase& aY);
  virtual void BitOr(LispEnvironment& aEnvironment,const NumberBase& aX, const NumberBase& aY);
  virtual void BitXor(LispEnvironment& aEnvironment,const NumberBase& aX, const NumberBase& aY);
public:
};
*/

#endif

