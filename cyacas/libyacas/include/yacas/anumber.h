#ifndef YACAS_ANUMBER_H
#define YACAS_ANUMBER_H

#include <cassert>
#include <cctype>
#include <string>
#include <vector>

// These define the internal types for the arbitrary precision
// number module. The larger they are the better. PlatDoubleWord
// should be at least twice as big as PlatWord, to prevent overflowing
// during multiplication.

typedef std::uint32_t PlatWord;
typedef std::uint64_t PlatDoubleWord;
typedef std::int64_t PlatSignedDoubleWord;

/* Quantities derived from the platform-dependent types for doing
 * arithmetic.
 */

#define WordBits  (8*sizeof(PlatWord))
#define WordBase  (((PlatDoubleWord)1)<<WordBits)

/* Class ANumber represents an arbitrary precision number. it is
 * basically an array of PlatWord objects, with the first element
 * being the least significant. iExp <= 0 for integers.
 */
class ANumber : public std::vector<PlatWord>
{
public:
    ANumber(const std::string& aString,int aPrecision,int aBase=10);
    explicit ANumber(int aPrecision);
    //TODO the properties of this object are set in the member initialization list, but then immediately overwritten by the CopyFrom. We can make this slightly cleaner by only initializing once.
    inline ANumber(const ANumber& aOther) : iExp(0),iNegative(false),iPrecision(0),iTensExp(0)
    {
      CopyFrom(aOther);
    }
    void CopyFrom(const ANumber& aOther);
    bool ExactlyEqual(const ANumber& aOther);
    void SetTo(const std::string& aString,int aBase=10);
    int Precision() const;
    void SetPrecision(int aPrecision) {iPrecision = aPrecision;}
    void ChangePrecision(int aPrecision);
    void RoundBits();
    void DropTrailZeroes();
    void Expand();

    void Negate();

    bool IsZero() const;
    bool IsNegative() const;
    bool IsEven() const;

    void Print(std::ostream&, const std::string& prefix) const;

public:
    int iExp;
    bool iNegative;
    int iPrecision;
    int iTensExp;
};

inline
int ANumber::Precision() const
{
    return iPrecision;
}

bool BaseLessThan(const ANumber& a1, const ANumber& a2);
bool BaseGreaterThan(const ANumber& a1, const ANumber& a2);

void BaseDivide(ANumber& aQuotient, ANumber& aRemainder, ANumber& a1, ANumber& a2);

void IntegerDivide(ANumber& aQuotient, ANumber& aRemainder, ANumber& a1, ANumber& a2);

bool Significant(ANumber& a);

int WordDigits(int aPrecision, int aBase);

// Operations on ANumber.
void Negate(ANumber& aNumber);
void ANumberToString(std::string& aResult, ANumber& aNumber, int aBase, bool aForceFloat=false);
void Add(ANumber& aResult, ANumber& a1, ANumber& a2);
void Subtract(ANumber& aResult, ANumber& a1, ANumber& a2);
void Multiply(ANumber& aResult, ANumber& a1, ANumber& a2);
void Divide(ANumber& aQuotient, ANumber& aRemainder, ANumber& a1, ANumber& a2);
bool GreaterThan(ANumber& a1, ANumber& a2);
bool LessThan(ANumber& a1, ANumber& a2);
void BaseShiftRight(ANumber& a, int aNrBits);
void BaseShiftLeft(ANumber& a, int aNrBits);
void BaseGcd(ANumber& aResult, ANumber& a1, ANumber& a2);
void Sqrt(ANumber& aResult, ANumber& N);

void NormalizeFloat(ANumber& a2, int digitsNeeded);

inline
void ANumber::Negate()
{
    iNegative = !iNegative;

    // FIXME: do we need negative zero?
    if (IsZero())
        iNegative = false;
}


inline
bool ANumber::IsZero() const
{
    for (const PlatWord& a: *this)
        if (a != 0)
            return false;

    return true;
}

inline
bool ANumber::IsNegative() const
{
    return iNegative;
}

inline
bool ANumber::IsEven() const
{
    return (front() & 1) == 0;
}

#include "anumber.inl"


#endif

