
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


class YacasNativeNumber;
YacasBigNumber* MakeNativeNumber(LispCharPtr aString,LispInt aPrecision,LispInt aBase=10);


#endif

