/* Math using the standard library, if the precision is less than 13 */
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "yacasbase.h"
#include "lispobject.h"
#include "lispatom.h"
#include "lispenvironment.h"
#include "numbers.h"
#include "platmath.h"


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

NativeNumber::NativeNumber(LispCharPtr aString,LispInt aPrecision,LispInt aBase)
{
  if (strchr(aString,'.') || strchr(aString,'e') || strchr(aString,'E'))
  {
    type  = KDouble;
    value.d = GetDouble(aString);
  }
  else
  {
    type  = KInteger;
    value.i = atoi(aString);
  }
}
void NativeNumber::ToString(LispString& aResult, LispInt aBase) const 
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
const LispCharPtr NativeNumber::NumericLibraryName() const
{
  return "Native types";
}

//          result->value.i assign ((NativeNumber)aX).value.i op ((NativeNumber)aY).value.i; 


#define TWO_FUNC_WITH(result,assign,op) \
{ \
const NativeNumber* x = ((const NativeNumber*)(&aX));\
const NativeNumber* y = ((const NativeNumber*)(&aY));\
  switch (x->type) \
  { \
    case KInteger: \
      switch (y->type) \
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
      switch (y->type) \
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
}

#define TWO_FUNC(assign,op) \
{ \
  LISPASSERT(!StrCompare(aX.NumericLibraryName(), aY.NumericLibraryName())); \
  LISPASSERT(!StrCompare(aX.NumericLibraryName(), NumericLibraryName())); \
  TWO_FUNC_WITH(this,assign,op) \
}


void NativeNumber::Multiply(const NumberBase& aX, const NumberBase& aY, LispInt aPrecision) 
TWO_FUNC(=,*)

void NativeNumber::MultiplyAdd(NumberBase& aResult,
              const NumberBase& aX, 
              const NumberBase& aY, 
              LispInt aPrecision) 
TWO_FUNC_WITH(((NativeNumber*)(&aResult)),+=,*)

void NativeNumber::Add(const NumberBase& aX, const NumberBase& aY, LispInt aPrecision) 
TWO_FUNC(=,+)

void NativeNumber::Negate(const NumberBase& aX) 
{
  switch (((const NativeNumber*)(&aX))->type)
  {
    case KInteger: value.i = -((NativeNumber*)(&aX))->value.i; break;
    case KDouble:  value.d = -((NativeNumber*)(&aX))->value.d; break;
  }
}
void NativeNumber::Divide(const NumberBase& aX, const NumberBase& aY, LispInt aPrecision) 
TWO_FUNC(=,/)

void NativeNumber::ShiftLeft( const NumberBase& aX, LispInt aNrToShift)
{
  LISPASSERT(!StrCompare(aX.NumericLibraryName(), NumericLibraryName())); 
  LISPASSERT(x.type == KInteger);
  value.i = ((NativeNumber*)(&aX))->value.i << aNrToShift;
}
void NativeNumber::ShiftRight( const NumberBase& aX, LispInt aNrToShift)
{
  LISPASSERT(!StrCompare(aX.NumericLibraryName(), NumericLibraryName())); 
  LISPASSERT(x.type == KInteger);
  value.i = ((NativeNumber*)(&aX))->value.i >> aNrToShift;
}
void NativeNumber::BitAnd(const NumberBase& aX, const NumberBase& aY)
{
  LISPASSERT(!StrCompare(aX.NumericLibraryName(), aY.NumericLibraryName())); 
  LISPASSERT(!StrCompare(aX.NumericLibraryName(), NumericLibraryName())); 
  LISPASSERT(x->type != KInteger);
  LISPASSERT(y->type != KInteger);
  value.i = (((NativeNumber*)(&aX))->value.i) & (((NativeNumber*)(&aY))->value.i);
}
void NativeNumber::BitOr(const NumberBase& aX, const NumberBase& aY)
{
  LISPASSERT(!StrCompare(aX.NumericLibraryName(), aY.NumericLibraryName())); 
  LISPASSERT(!StrCompare(aX.NumericLibraryName(), NumericLibraryName())); 
  LISPASSERT(x->type != KInteger);
  LISPASSERT(y->type != KInteger);
  value.i = (((NativeNumber*)(&aX))->value.i) | (((NativeNumber*)(&aY))->value.i);
}
void NativeNumber::BitXor(const NumberBase& aX, const NumberBase& aY)
{
  LISPASSERT(!StrCompare(aX.NumericLibraryName(), aY.NumericLibraryName())); 
  LISPASSERT(!StrCompare(aX.NumericLibraryName(), NumericLibraryName())); 
  LISPASSERT(x->type != KInteger);
  LISPASSERT(y->type != KInteger);
  value.i = (((NativeNumber*)(&aX))->value.i) ^ (((NativeNumber*)(&aY))->value.i);
}


double NativeNumber::Double() const
{
  LISPASSERT(!StrCompare(aX.NumericLibraryName(), NumericLibraryName())); 
  if (type == KInteger)
    return double(value.i);
  else//if (type == KDouble)
    return value.d;
}

LispInt NativeNumber::BitCount() const
{
  LispInt result = 0;
  if (type == KInteger)
  {
  // prepare the absolute value of the integer for shifting
    long temp = (value.i < 0) ? -value.i : value.i;
    while (temp>0)
    {
      temp >>= 1;
      ++result;
    }
  }
  else	// KDouble, return the binary exponent
  {
  // prepare the absolute value of the number
    double temp = (value.d < 0) ? -value.d : value.d;
    if (temp<1)
    {
      while (temp<1)
      {
        temp *= 2;
	++result;
      }
    }
    else if (temp>1)
    {
      while (temp>1)
      {
        temp /= 2;
	++result;
      }
    }
  }
  return result;
}

LispInt NativeNumber::Sign() const
{
  if (type == KInteger)
  {
    return ( (value.i > 0) ? 1 : ((value.i < 0) ? -1 : 0) );
  }
  else
  {
    return ( (value.d > 0) ? 1 : ((value.d < 0) ? -1 : 0) );
  }
}


void NativeNumber::SetTo(const NumberBase& aX)
{
  LISPASSERT(!StrCompare(aX.NumericLibraryName(), NumericLibraryName())); 
  type = ((NativeNumber*)(&aX))->type;
  value = ((NativeNumber*)(&aX))->value;
}

