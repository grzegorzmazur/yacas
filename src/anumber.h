
#ifndef __anumber_h__
#define __anumber_h__

#include "grower.h"
#include "yacasbase.h"
#include "lispassert.h"
#include "lispstring.h"



/* Quantities derived from the platform-dependent types for doing
 * arithmetic.
 */

#define WordBits  (8*sizeof(PlatWord))
#define WordBase  (1L<<WordBits)
#define WordMask  (WordBase-1)

/* Class ANumber represents an arbitrary precision number. it is
 * basically an array of PlatWord objects, with the first element
 * being the least significant. iExp <= 0 for integers.
 */
class ANumber : public CArrayGrower<PlatWord>
{
public:
    ANumber(const LispCharPtr aString,LispInt aPrecision=10,LispInt aBase=10);
    ANumber(LispInt aPrecision=10);
    ANumber(PlatWord *aArray, LispInt aSize, LispInt aPrecision=10);
    inline ANumber(ANumber& aOther) {CopyFrom(aOther);}
    ~ANumber();
    void CopyFrom(const ANumber& aOther);
    void SetTo(const LispCharPtr aString,LispInt aBase=10);
    inline void SetPrecision(LispInt aPrecision) {iPrecision = aPrecision;}
    void ChangePrecision(LispInt aPrecision);
public:
    LispInt iExp;
    LispInt iNegative;
    LispInt iPrecision;
    LispInt iTensExp;
};

inline LispBoolean IsPositive(ANumber& a) { return !a.iNegative; }
inline LispBoolean IsNegative(ANumber& a) { return a.iNegative;  }
inline LispBoolean IsEven(ANumber& a) { return ((a[0]&1) == 0); }
inline LispBoolean IsOdd(ANumber& a)  { return ((a[0]&1) == 1); }
inline LispInt     Precision(ANumber& a) { return !a.iPrecision; }

LispBoolean BaseLessThan(ANumber& a1, ANumber& a2);
void BaseDivide(ANumber& aQuotient, ANumber& aRemainder, ANumber& a1, ANumber& a2);

void IntegerDivide(ANumber& aQuotient, ANumber& aRemainder, ANumber& a1, ANumber& a2);

LispBoolean Significant(ANumber& a);

LispInt WordDigits(LispInt aPrecision, LispInt aBase);

// Operations on ANumber.
void Negate(ANumber& aNumber);
void  ANumberToString(LispString& aResult, ANumber& aNumber, LispInt aBase);
void Add(ANumber& aResult, ANumber& a1, ANumber& a2);
void Subtract(ANumber& aResult, ANumber& a1, ANumber& a2);
void Multiply(ANumber& aResult, ANumber& a1, ANumber& a2);
void Divide(ANumber& aQuotient, ANumber& aRemainder, ANumber& a1, ANumber& a2);
LispBoolean GreaterThan(ANumber& a1, ANumber& a2);
LispBoolean LessThan(ANumber& a1, ANumber& a2);
void BaseShiftRight(ANumber& a, LispInt aNrBits);
void BaseShiftLeft(ANumber& a, LispInt aNrBits);
void BaseGcd(ANumber& aResult, ANumber& a1, ANumber& a2);
void Sqrt(ANumber& aResult, ANumber& N);


#include "anumber.inl"


#endif

