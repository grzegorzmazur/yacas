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

#define NATIVE_CAST(x)  ((YacasNativeNumber*)(x.Ptr()))

class YacasNativeNumber : public YacasBigNumber
{
public: //constructors
  /// Copy a number class
  static YacasNativeNumber* Make(LispCharPtr aString,LispInt aPrecision,LispInt aBase=10);
public: //constructors
  /// Copy a number class
  virtual YacasBigNumber* Copy() const;
  /// ToString : return string representation of number in aResult 
  virtual void ToString(LispString& aResult, LispInt aBase);
public: //information retrieval on library used  
  /// Numeric library name
  virtual const LispCharPtr NumericLibraryName();
public://arithmetic
  /// Multiply two numbers, and return result in aResult
  virtual YacasBigNumber* Multiply(YacasBigNumberPtr& aX, YacasBigNumberPtr& aY, LispInt aPrecision);
  /** Multiply two numbers, and add to aResult (this is useful and generally efficient to implement).
   * This is most likely going to be used by internal functions only, using aResult as an accumulator.
   */
  virtual YacasBigNumber* MultiplyAdd(YacasBigNumber* aResult,
                YacasBigNumberPtr& aX, 
                YacasBigNumberPtr& aY, 
                LispInt aPrecision);
  /// Add two numbers, and return result in aResult
  virtual YacasBigNumber* Add(YacasBigNumberPtr& aX, YacasBigNumberPtr& aY, LispInt aPrecision);
  /// Negate the current number, and return it
  virtual YacasBigNumber* Negate();
  /// Divide, and return result. Note: if the two arguments are integer, it should return an integer result!
  virtual YacasBigNumber* Divide(YacasBigNumberPtr& aX, YacasBigNumberPtr& aY, LispInt aPrecision);

public://bitwise operations  
  virtual YacasBigNumber* ShiftLeft( YacasBigNumberPtr& aX, LispInt aNrToShift);
  virtual YacasBigNumber* ShiftRight( YacasBigNumberPtr& aX, LispInt aNrToShift);
  virtual YacasBigNumber* BitAnd(YacasBigNumberPtr& aX, YacasBigNumberPtr& aY);
  virtual YacasBigNumber* BitOr(YacasBigNumberPtr& aX, YacasBigNumberPtr& aY);
  virtual YacasBigNumber* BitXor(YacasBigNumberPtr& aX, YacasBigNumberPtr& aY);

public:
  inline YacasNativeNumber(const YacasNativeNumber& aOther)
  {
    type  = aOther.type;
    value = aOther.value;
  }
  inline YacasNativeNumber(YacasBigNumberPtr& aOther)
  {
    type  = NATIVE_CAST(aOther)->type;
    value = NATIVE_CAST(aOther)->value;
  }

private:
  inline YacasNativeNumber() {type = KDouble;value.d = 0.0;}
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

YacasBigNumber* MakeNativeNumber(LispCharPtr aString,LispInt aPrecision,LispInt aBase)
{
  return YacasNativeNumber::Make(aString,aPrecision,aBase);
}

YacasNativeNumber* YacasNativeNumber::Make(LispCharPtr aString,LispInt aPrecision,LispInt aBase)
{
  YacasNativeNumber* result = NEW YacasNativeNumber();
  if (strchr(aString,'.'))
  {
    result->type  = KDouble;
    result->value.d = GetDouble(aString);
  }
  else
  {
    result->type  = KInteger;
    result->value.i = atoi(aString);
  }
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
const LispCharPtr YacasNativeNumber::NumericLibraryName() 
{
  return "Native types";
}

//          result->value.i assign ((YacasNativeNumber)aX).value.i op ((YacasNativeNumber)aY).value.i; 


#define TWO_FUNC_WITH(result,assign,op) \
{ \
YacasNativeNumber* x = (YacasNativeNumber*)aX.Ptr();\
YacasNativeNumber* y = (YacasNativeNumber*)aY.Ptr();\
  switch (NATIVE_CAST(aX)->type) \
  { \
    case KInteger: \
      switch (NATIVE_CAST(aY)->type) \
      { \
        case KInteger: \
          result->type = KInteger; \
          result->value.i assign ((x->value.i) op (y->value.i)); \
          break; \
        case KDouble: \
          result->type = KDouble; \
          result->value.d assign ((x->value.i) op (y->value.d)); \
          break; \
      } \
      break; \
    case KDouble: \
      switch (NATIVE_CAST(aY)->type) \
      { \
        case KInteger: \
          result->type = KDouble; \
          result->value.d assign ((x->value.d) op (y->value.i)); \
          break; \
        case KDouble: \
          result->type = KDouble; \
          result->value.d assign ((x->value.d) op (y->value.d)); \
          break; \
      } \
      break; \
  } \
  return result; \
}

#define TWO_FUNC(assign,op) \
{ \
  LISPASSERT(!StrCompare(aX.NumericLibraryName(), aY.NumericLibraryName())); \
  LISPASSERT(!StrCompare(aX.NumericLibraryName(), NumericLibraryName())); \
  YacasNativeNumber* result = NEW YacasNativeNumber(); \
  TWO_FUNC_WITH(result,assign,op) \
}


YacasBigNumber* YacasNativeNumber::Multiply(YacasBigNumberPtr& aX, YacasBigNumberPtr& aY, LispInt aPrecision) 
TWO_FUNC(=,*)

YacasBigNumber* YacasNativeNumber::MultiplyAdd(YacasBigNumber* aResult,
              YacasBigNumberPtr& aX, 
              YacasBigNumberPtr& aY, 
              LispInt aPrecision) 
TWO_FUNC_WITH(((YacasNativeNumber*)aResult),+=,*)

YacasBigNumber* YacasNativeNumber::Add(YacasBigNumberPtr& aX, YacasBigNumberPtr& aY, LispInt aPrecision) 
TWO_FUNC(=,+)

YacasBigNumber* YacasNativeNumber::Negate() 
{
  switch (type)
  {
    case KInteger: value.i = -value.i; break;
    case KDouble:  value.d = -value.d; break;
  }
  return this;
}
YacasBigNumber* YacasNativeNumber::Divide(YacasBigNumberPtr& aX, YacasBigNumberPtr& aY, LispInt aPrecision) 
TWO_FUNC(=,/)

YacasBigNumber* YacasNativeNumber::ShiftLeft( YacasBigNumberPtr& aX, LispInt aNrToShift)
{
  LISPASSERT(!StrCompare(aX.NumericLibraryName(), NumericLibraryName())); 
  YacasNativeNumber* x = (YacasNativeNumber*)aX.Ptr();
  if (x->type != KInteger) return NULL;
  YacasNativeNumber *result = (YacasNativeNumber*)x->Copy();
  result->value.i <<= aNrToShift;
  return result;
}
YacasBigNumber* YacasNativeNumber::ShiftRight( YacasBigNumberPtr& aX, LispInt aNrToShift)
{
  LISPASSERT(!StrCompare(aX.NumericLibraryName(), NumericLibraryName())); 
  YacasNativeNumber* x = (YacasNativeNumber*)aX.Ptr();
  if (x->type != KInteger) return NULL;
  YacasNativeNumber *result = (YacasNativeNumber*)x->Copy();
  result->value.i >>= aNrToShift;
  return result;
}
YacasBigNumber* YacasNativeNumber::BitAnd(YacasBigNumberPtr& aX, YacasBigNumberPtr& aY)
{
  LISPASSERT(!StrCompare(aX.NumericLibraryName(), aY.NumericLibraryName())); 
  LISPASSERT(!StrCompare(aX.NumericLibraryName(), NumericLibraryName())); 
  YacasNativeNumber* x = (YacasNativeNumber*)aX.Ptr();
  YacasNativeNumber* y = (YacasNativeNumber*)aY.Ptr();
  if (x->type != KInteger) return NULL;
  if (y->type != KInteger) return NULL;
  YacasNativeNumber *result = (YacasNativeNumber*)x->Copy();
  result->value.i = x->value.i & y->value.i;
  return result;
}
YacasBigNumber* YacasNativeNumber::BitOr(YacasBigNumberPtr& aX, YacasBigNumberPtr& aY)
{
  LISPASSERT(!StrCompare(aX.NumericLibraryName(), aY.NumericLibraryName())); 
  LISPASSERT(!StrCompare(aX.NumericLibraryName(), NumericLibraryName())); 
  YacasNativeNumber* x = (YacasNativeNumber*)aX.Ptr();
  YacasNativeNumber* y = (YacasNativeNumber*)aY.Ptr();
  if (x->type != KInteger) return NULL;
  if (y->type != KInteger) return NULL;
  YacasNativeNumber *result = (YacasNativeNumber*)x->Copy();
  result->value.i = x->value.i | y->value.i;
  return result;
}
YacasBigNumber* YacasNativeNumber::BitXor(YacasBigNumberPtr& aX, YacasBigNumberPtr& aY)
{
  LISPASSERT(!StrCompare(aX.NumericLibraryName(), aY.NumericLibraryName())); 
  LISPASSERT(!StrCompare(aX.NumericLibraryName(), NumericLibraryName())); 
  YacasNativeNumber* x = (YacasNativeNumber*)aX.Ptr();
  YacasNativeNumber* y = (YacasNativeNumber*)aY.Ptr();
  if (x->type != KInteger) return NULL;
  if (y->type != KInteger) return NULL;
  YacasNativeNumber *result = (YacasNativeNumber*)x->Copy();
  result->value.i = x->value.i ^ y->value.i;
  return result;
}



