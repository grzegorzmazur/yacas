#ifndef YACAS_PLATMATH_H
#define YACAS_PLATMATH_H

#include "lispenvironment.h"

// Beware the use of these functions! They cannot be guaranteed to be
// supported on any platform.
double GetDouble(LispObject* aInteger);
LispObject* Double(LispEnvironment& aEnvironment,double aValue);

LispObject* PlatArcSin(LispEnvironment& aEnvironment,LispObject* int1, int aPrecision);
LispObject* PlatLn(LispEnvironment& aEnvironment,LispObject* int1, int aPrecision);
LispObject* PlatPower(LispEnvironment& aEnvironment,LispObject* int1, LispObject* int2,
                        int aPrecision);

LispObject* PlatDiv(LispEnvironment& aEnvironment,LispObject* int1, LispObject* int2,int aPrecision);

LispObject* PlatIsPrime(LispEnvironment& aEnvironment,LispObject* int1, int aPrecision);

// table lookup for small prime numbers
unsigned primes_table_check(unsigned long p);
unsigned primes_table_range();

#endif

