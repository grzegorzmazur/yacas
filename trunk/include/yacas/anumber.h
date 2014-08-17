#ifndef YACAS_ANUMBER_H
#define YACAS_ANUMBER_H

#include "yacasbase.h"
#include "lispstring.h"

#include <cassert>
#include <vector>

/* Quantities derived from the platform-dependent types for doing
 * arithmetic.
 */

#define WordBits  (8*sizeof(PlatWord))
#define WordBase  (((PlatDoubleWord)1)<<WordBits)
#define WordMask  (WordBase-1)

/* Class ANumber represents an arbitrary precision number. it is
 * basically an array of PlatWord objects, with the first element
 * being the least significant. iExp <= 0 for integers.
 */
class ANumber : public std::vector<PlatWord>
{
public:
    ANumber(const LispChar * aString,LispInt aPrecision,LispInt aBase=10);
    ANumber(LispInt aPrecision);
    //TODO the properties of this object are set in the member initialization list, but then immediately overwritten by the CopyFrom. We can make this slightly cleaner by only initializing once.
    inline ANumber(const ANumber& aOther) : iExp(0),iNegative(false),iPrecision(0),iTensExp(0)
    {
      CopyFrom(aOther);
    }
    ~ANumber();
    void CopyFrom(const ANumber& aOther);
    bool ExactlyEqual(const ANumber& aOther);
    void SetTo(const LispChar * aString,LispInt aBase=10);
    inline void SetPrecision(LispInt aPrecision) {iPrecision = aPrecision;}
    void ChangePrecision(LispInt aPrecision);
    void RoundBits(void);
    void DropTrailZeroes();
    void Expand();

public:
    LispInt iExp;
    bool iNegative;
    LispInt iPrecision;
    LispInt iTensExp;
};

inline bool IsPositive(ANumber& a) { return !a.iNegative; }
inline bool IsNegative(ANumber& a) { return a.iNegative;  }
inline bool IsEven(ANumber& a) { return ((a[0]&1) == 0); }
inline bool IsOdd(ANumber& a)  { return ((a[0]&1) == 1); }
inline LispInt     Precision(ANumber& a) { return !a.iPrecision; }

bool BaseLessThan(ANumber& a1, ANumber& a2);
void BaseDivide(ANumber& aQuotient, ANumber& aRemainder, ANumber& a1, ANumber& a2);

void IntegerDivide(ANumber& aQuotient, ANumber& aRemainder, ANumber& a1, ANumber& a2);

bool Significant(ANumber& a);

LispInt WordDigits(LispInt aPrecision, LispInt aBase);

// Operations on ANumber.
void Negate(ANumber& aNumber);
void  ANumberToString(LispString& aResult, ANumber& aNumber, LispInt aBase, bool aForceFloat=0);
void Add(ANumber& aResult, ANumber& a1, ANumber& a2);
void Subtract(ANumber& aResult, ANumber& a1, ANumber& a2);
void Multiply(ANumber& aResult, ANumber& a1, ANumber& a2);
void Divide(ANumber& aQuotient, ANumber& aRemainder, ANumber& a1, ANumber& a2);
bool GreaterThan(ANumber& a1, ANumber& a2);
bool LessThan(ANumber& a1, ANumber& a2);
void BaseShiftRight(ANumber& a, LispInt aNrBits);
void BaseShiftLeft(ANumber& a, LispInt aNrBits);
void BaseGcd(ANumber& aResult, ANumber& a1, ANumber& a2);
void Sqrt(ANumber& aResult, ANumber& N);

void PrintNumber(const char* prefix,ANumber& aNumber);

#define CORRECT_DIVISION
void NormalizeFloat(ANumber& a2, LispInt digitsNeeded);


#include "anumber.inl"


#endif

