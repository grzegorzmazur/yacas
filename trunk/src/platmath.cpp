/* Math using the standard library, if the precision is less than 13 */
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "yacasbase.h"
#include "lispobject.h"
#include "lispatom.h"
#include "lispenvironment.h"
#include "numbers.h"


double GetDouble(LispCharPtr aString)
{
    return strtod(aString,NULL); //TODO!
}

LispStringPtr Double(double aValue, LispHashTable& aHashTable)
{
    char dummy[150];
//    sprintf(dummy,"%.24g",aValue);
    sprintf(dummy,"%f",aValue);
    return aHashTable.LookUp(dummy);
}

LispStringPtr PlatSin(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision)
{
    return Double(sin(GetDouble(int1)),aHashTable);
}

LispStringPtr PlatCos(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision)
{
    return Double(cos(GetDouble(int1)),aHashTable);
}

LispStringPtr PlatTan(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision)
{
    return Double(tan(GetDouble(int1)),aHashTable);
}

LispStringPtr PlatArcSin(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision)
{
    return Double(asin(GetDouble(int1)),aHashTable);
}

LispStringPtr PlatArcCos(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision)
{
    return Double(acos(GetDouble(int1)),aHashTable);
}

LispStringPtr PlatArcTan(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision)
{
    return Double(atan(GetDouble(int1)),aHashTable);
}

LispStringPtr PlatExp(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision)
{
    return Double(exp(GetDouble(int1)),aHashTable);
}

LispStringPtr PlatLn(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision)
{
    return Double(log(GetDouble(int1)),aHashTable);
}

LispStringPtr PlatPower(LispCharPtr int1, LispCharPtr int2,
                        LispHashTable& aHashTable,LispInt aPrecision)
{
    return Double(pow(GetDouble(int1),GetDouble(int2)),aHashTable);
}



LispStringPtr PlatSqrt(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision)
{
    return Double(sqrt(GetDouble(int1)),aHashTable);
}
LispStringPtr PlatPi(LispHashTable& aHashTable,LispInt aPrecision)
{
    return Double(acos(-1.0),aHashTable);
}
LispStringPtr PlatFloor(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision)
{
    return Double(floor(GetDouble(int1)),aHashTable);
}
LispStringPtr PlatCeil(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision)
{
    return Double(ceil(GetDouble(int1)),aHashTable);
}
LispStringPtr PlatMod(LispCharPtr int1, LispCharPtr int2,LispHashTable& aHashTable,LispInt aPrecision)
{
    return Double(fmod(GetDouble(int1),GetDouble(int2)),aHashTable);
}
LispStringPtr PlatDiv(LispCharPtr int1, LispCharPtr int2,LispHashTable& aHashTable,LispInt aPrecision)
{
    return Double(((long)GetDouble(int1))/((long)GetDouble(int2)),aHashTable);
}
LispStringPtr PlatAbs(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision)
{
    return Double(fabs(GetDouble(int1)),aHashTable);
}









/* And here is a reference implementation of YacasBigNumber: one using native types.
*/

#define NATIVE_CAST(x)  ((const YacasNativeNumber*)(x.Ptr()))

class YacasNativeNumber : public YacasBigNumber
{
public: //constructors
  /// Copy a number class
  static YacasNativeNumber* Make(LispCharPtr aString,LispInt aPrecision,LispInt aBase=10);
  virtual YacasBigNumber* Copy() const;
  /// ToString : return string representation of number in aResult 
  virtual void ToString(LispString& aResult, LispInt aBase) ;
public: //base conversions
  virtual YacasBigNumber* FromBase( const YacasBigNumberPtr& aX, const YacasBigNumberPtr& aY,  LispInt aPrecision) ;
  virtual YacasBigNumber* ToBase( const YacasBigNumberPtr& aX, const YacasBigNumberPtr& aY,  LispInt aPrecision) ;
public: //information retrieval on library used  
  /// Whether the numeric library supports 1.0E-10 and such.
  virtual LispInt NumericSupportForMantissa() ;
  /// Numeric library name
  virtual const LispCharPtr NumericLibraryName() ;
  
public://arithmetic
  /// Calculate GCD of this object with another, and return result in aResult
  virtual YacasBigNumber* Gcd(YacasBigNumberPtr aX, YacasBigNumberPtr aY) ;
  /// Multiply two numbers, and return result in aResult
  virtual YacasBigNumber* Multiply(const YacasBigNumberPtr& aX, const YacasBigNumberPtr& aY, LispInt aPrecision) ;
  /** Multiply two numbers, and add to aResult (this is useful and generally efficient to implement).
   * This is most likely going to be used by internal functions only, using aResult as an accumulator.
   */
  virtual YacasBigNumber* MultiplyAdd(YacasBigNumber* aResult,
                const YacasBigNumberPtr& aX, 
                const YacasBigNumberPtr& aY, 
                LispInt aPrecision) ;
  /// Add two numbers, and return result in aResult
  virtual YacasBigNumber* Add(const YacasBigNumberPtr& aX, const YacasBigNumberPtr& aY, LispInt aPrecision) ;
  /// Negate the current number, and return it
  virtual YacasBigNumber* Negate() ;
  /// Divide, and return result  
  virtual YacasBigNumber* Divide(const YacasBigNumberPtr& aX, const YacasBigNumberPtr& aY, LispInt aPrecision) ;
  /// Raise power, and return result
  virtual YacasBigNumber* Power(const YacasBigNumberPtr& aX, const YacasBigNumberPtr& aY,LispInt aPrecision) ;
public: //trigonometric functions
  virtual YacasBigNumber* Sin(const YacasBigNumberPtr& aX,LispInt aPrecision) ;
  virtual YacasBigNumber* Cos(const YacasBigNumberPtr& aX,LispInt aPrecision) ;
  virtual YacasBigNumber* Tan(const YacasBigNumberPtr& aX,LispInt aPrecision) ;
  virtual YacasBigNumber* ArcSin(const YacasBigNumberPtr& aX,LispInt aPrecision) ;
  virtual YacasBigNumber* ArcCos(const YacasBigNumberPtr& aX,LispInt aPrecision) ;
  virtual YacasBigNumber* ArcTan(const YacasBigNumberPtr& aX,LispInt aPrecision) ;
  virtual YacasBigNumber* Exp(const YacasBigNumberPtr& aX,LispInt aPrecision) ;
  virtual YacasBigNumber* Ln(const YacasBigNumberPtr& aX,LispInt aPrecision) ;
public://other functions  
  virtual YacasBigNumber* Sqrt(const YacasBigNumberPtr& aX,LispInt aPrecision) ;
  virtual YacasBigNumber* Abs( const YacasBigNumberPtr& aX,LispInt aPrecision) ;
  virtual YacasBigNumber* Factorial(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision) ;
  virtual YacasBigNumber* Floor(const YacasBigNumberPtr& aX, LispInt aPrecision) ;
  virtual YacasBigNumber* Ceil( const YacasBigNumberPtr& aX,LispInt aPrecision) ;
  virtual YacasBigNumber* Mod( const YacasBigNumberPtr& aX, const YacasBigNumberPtr& aY, LispInt aPrecision) ;
  virtual YacasBigNumber* Div( const YacasBigNumberPtr& aX, const YacasBigNumberPtr& aY, LispInt aPrecision) ;
  virtual YacasBigNumber* Pi(LispInt aPrecision) ;
public://comparisons  
  virtual LispBoolean GreaterThan(const YacasBigNumberPtr& aX, const YacasBigNumberPtr& aY, LispInt aPrecision) ;
  virtual LispBoolean LessThan(const YacasBigNumberPtr& aX, const YacasBigNumberPtr& aY, LispInt aPrecision) ;

public://bitwise operations  
  virtual YacasBigNumber* ShiftLeft( LispCharPtr int1, LispCharPtr int2, LispHashTable& aHashTable,LispInt aPrecision) ;
  virtual YacasBigNumber* ShiftRight( LispCharPtr int1, LispCharPtr int2, LispHashTable& aHashTable,LispInt aPrecision) ;
  virtual YacasBigNumber* BitAnd(const YacasBigNumberPtr& aX, const YacasBigNumberPtr& aY, LispInt aPrecision) ;
  virtual YacasBigNumber* BitOr(const YacasBigNumberPtr& aX, const YacasBigNumberPtr& aY, LispInt aPrecision) ;
  virtual YacasBigNumber* BitXor(const YacasBigNumberPtr& aX, const YacasBigNumberPtr& aY, LispInt aPrecision) ;

private:
  inline YacasNativeNumber() {type = KDouble;value.d = 0.0;}
  inline YacasNativeNumber(const YacasNativeNumber& aOther)
  {
    type  = aOther.type;
    value = aOther.value;
  }
  inline YacasNativeNumber(const YacasBigNumberPtr& aOther)
  {
    type  = NATIVE_CAST(aOther)->type;
    value = NATIVE_CAST(aOther)->value;
  }
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

const YacasBigNumber* MakeNativeNumber(LispCharPtr aString,LispInt aPrecision,LispInt aBase)
{
  return YacasNativeNumber::Make(aString,aPrecision,aBase);
}














/*
YacasNativeNumber(const YacasBigNumberPtr& aPtr)
{
}
*/





YacasNativeNumber* YacasNativeNumber::Make(LispCharPtr aString,LispInt aPrecision,LispInt aBase=10)
{
  YacasNativeNumber* result = NEW YacasNativeNumber();
  result->type  = KDouble;
  result->value.d = GetDouble(aString);
  return result;
}
YacasBigNumber* YacasNativeNumber::Copy() const
{
  YacasNativeNumber* result = NEW YacasNativeNumber();
  result->type  = type;
  result->value = value;
  return result;
}
void YacasNativeNumber::ToString(LispString& aResult, LispInt aBase) 
{
  char dummy[150];
  dummy[0] = '\0';
  switch (type)
  {
  case KInteger:
    sprintf(dummy,"%ld",value.i);
    break;
  case KDouble:
    sprintf(dummy,"%f",value.d);
    break;
  }
  aResult.SetStringCounted(dummy,PlatStrLen(dummy));
}
YacasBigNumber* YacasNativeNumber::FromBase( const YacasBigNumberPtr& aX, const YacasBigNumberPtr& aY,  LispInt aPrecision) 
{
  return NULL;//TODO
}
YacasBigNumber* YacasNativeNumber::ToBase( const YacasBigNumberPtr& aX, const YacasBigNumberPtr& aY,  LispInt aPrecision) 
{
  return NULL;//TODO
}
LispInt YacasNativeNumber:: NumericSupportForMantissa() 
{
  return LispTrue;
}
const LispCharPtr YacasNativeNumber::NumericLibraryName() 
{
  return "Native types";
}

YacasBigNumber* YacasNativeNumber::Gcd(YacasBigNumberPtr aX, YacasBigNumberPtr aY) 
{
  return NULL;//TODO
}

YacasBigNumber* YacasNativeNumber::Multiply(const YacasBigNumberPtr& aX, const YacasBigNumberPtr& aY, LispInt aPrecision) 
{
  LISPASSERT(!StrCompare(aX.NumericLibraryName(), aY.NumericLibraryName()));
  YacasNativeNumber* result = NEW YacasNativeNumber();


  switch (NATIVE_CAST(aX)->type)
  {
    case KInteger:
      switch (NATIVE_CAST(aY)->type)
      {
        case KInteger:
          result->type = KInteger;
          result->value.i = ((YacasNativeNumber)aX).value.i * ((YacasNativeNumber)aY).value.i;
          break;
        case KDouble:
          result->type = KDouble;
          result->value.d = ((YacasNativeNumber)aX).value.i * ((YacasNativeNumber)aY).value.d;
          break;
      }
      break;

    case KDouble:
      switch (NATIVE_CAST(aY)->type)
      {
        case KInteger:
          result->type = KDouble;
          result->value.d = ((YacasNativeNumber)aX).value.d * ((YacasNativeNumber)aY).value.i;
          break;
        case KDouble:
          result->type = KDouble;
          result->value.d = ((YacasNativeNumber)aX).value.d * ((YacasNativeNumber)aY).value.d;
          break;
      }
      break;
  }
  return result;
}

YacasBigNumber* YacasNativeNumber::MultiplyAdd(YacasBigNumber* aResult,
              const YacasBigNumberPtr& aX, 
              const YacasBigNumberPtr& aY, 
              LispInt aPrecision) 
{
  return NULL;//TODO
}
YacasBigNumber* YacasNativeNumber::Add(const YacasBigNumberPtr& aX, const YacasBigNumberPtr& aY, LispInt aPrecision) 
{
  return NULL;//TODO
}
YacasBigNumber* YacasNativeNumber::Negate() 
{
  return NULL;//TODO
}
YacasBigNumber* YacasNativeNumber::Divide(const YacasBigNumberPtr& aX, const YacasBigNumberPtr& aY, LispInt aPrecision) 
{
  return NULL;//TODO
}
YacasBigNumber* YacasNativeNumber::Power(const YacasBigNumberPtr& aX, const YacasBigNumberPtr& aY,LispInt aPrecision) 
{
  return NULL;//TODO
}

#define ONE_FUN(fn)                                                   \
  {                                                                   \
    YacasNativeNumber* result = NEW YacasNativeNumber();              \
    result->type = KDouble;                                           \
    double dvalue = 0;                                                \
    switch (NATIVE_CAST(aX)->type)                                    \
    {                                                                 \
      case KInteger: dvalue = NATIVE_CAST(aX)->value.i; break;        \
      case KDouble:  dvalue = NATIVE_CAST(aX)->value.d; break;        \
    }                                                                 \
    result->value.d = fn(dvalue);                                    \
    return result;                                                    \
  }


YacasBigNumber* YacasNativeNumber::Sin(const YacasBigNumberPtr& aX,LispInt aPrecision) 
ONE_FUN(sin)

YacasBigNumber* YacasNativeNumber::Cos(const YacasBigNumberPtr& aX,LispInt aPrecision) 
ONE_FUN(cos)

YacasBigNumber* YacasNativeNumber::Tan(const YacasBigNumberPtr& aX,LispInt aPrecision) 
ONE_FUN(tan)

YacasBigNumber* YacasNativeNumber::ArcSin(const YacasBigNumberPtr& aX,LispInt aPrecision) 
ONE_FUN(asin)

YacasBigNumber* YacasNativeNumber::ArcCos(const YacasBigNumberPtr& aX,LispInt aPrecision) 
ONE_FUN(acos)

YacasBigNumber* YacasNativeNumber::ArcTan(const YacasBigNumberPtr& aX,LispInt aPrecision) 
ONE_FUN(atan)

YacasBigNumber* YacasNativeNumber::Exp(const YacasBigNumberPtr& aX,LispInt aPrecision) 
ONE_FUN(exp)

YacasBigNumber* YacasNativeNumber::Ln(const YacasBigNumberPtr& aX,LispInt aPrecision) 
ONE_FUN(log)

YacasBigNumber* YacasNativeNumber::Sqrt(const YacasBigNumberPtr& aX,LispInt aPrecision) 
{
  return NULL;//TODO
}
YacasBigNumber* YacasNativeNumber::Abs( const YacasBigNumberPtr& aX,LispInt aPrecision) 
{
  return NULL;//TODO
}
YacasBigNumber* YacasNativeNumber::Factorial(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision) 
{
  return NULL;//TODO
}
YacasBigNumber* YacasNativeNumber::Floor(const YacasBigNumberPtr& aX, LispInt aPrecision) 
{
  return NULL;//TODO
}
YacasBigNumber* YacasNativeNumber::Ceil( const YacasBigNumberPtr& aX,LispInt aPrecision) 
{
  return NULL;//TODO
}
YacasBigNumber* YacasNativeNumber::Mod( const YacasBigNumberPtr& aX, const YacasBigNumberPtr& aY, LispInt aPrecision) 
{
  return NULL;//TODO
}
YacasBigNumber* YacasNativeNumber::Div( const YacasBigNumberPtr& aX, const YacasBigNumberPtr& aY, LispInt aPrecision) 
{
  return NULL;//TODO
}
YacasBigNumber* YacasNativeNumber::Pi(LispInt aPrecision) 
{
  return NULL;//TODO
}
LispBoolean YacasNativeNumber::GreaterThan(const YacasBigNumberPtr& aX, const YacasBigNumberPtr& aY, LispInt aPrecision) 
{
  return NULL;//TODO
}
LispBoolean YacasNativeNumber::LessThan(const YacasBigNumberPtr& aX, const YacasBigNumberPtr& aY, LispInt aPrecision) 
{
  return NULL;//TODO
}
YacasBigNumber* YacasNativeNumber::ShiftLeft( LispCharPtr int1, LispCharPtr int2, LispHashTable& aHashTable,LispInt aPrecision) 
{
  return NULL;//TODO
}
YacasBigNumber* YacasNativeNumber::ShiftRight( LispCharPtr int1, LispCharPtr int2, LispHashTable& aHashTable,LispInt aPrecision) 
{
  return NULL;//TODO
}
YacasBigNumber* YacasNativeNumber::BitAnd(const YacasBigNumberPtr& aX, const YacasBigNumberPtr& aY, LispInt aPrecision) 
{
  return NULL;//TODO
}
YacasBigNumber* YacasNativeNumber::BitOr(const YacasBigNumberPtr& aX, const YacasBigNumberPtr& aY, LispInt aPrecision) 
{
  return NULL;//TODO
}
YacasBigNumber* YacasNativeNumber::BitXor(const YacasBigNumberPtr& aX, const YacasBigNumberPtr& aY, LispInt aPrecision) 
{
  return NULL;//TODO
}
