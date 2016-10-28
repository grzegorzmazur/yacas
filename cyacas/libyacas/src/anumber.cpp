


/* Arbitrary precision arithmetic classes. These are NOT designed
 * to be bleeding fast, just reaonably fast, but very clean code.
 *
 */

#include "yacas/anumber.h"

#include <algorithm>
#include <cstring>
#include <iostream>

/* The Base... functions perform actions on the mantissa part of the
 * number, that is, it treats them as unsigned integers.
 */
void BaseAddFull(ANumber& aResult, ANumber& a1, ANumber& a2);
void BaseSubtract(ANumber& aResult, ANumber& a1, ANumber& a2);
void BaseMultiplyFull(ANumber& aResult, ANumber& a1, ANumber& a2);
void BaseSqrt(ANumber& aResult, const ANumber& N);

void ANumber::Print(std::ostream& os, const std::string& prefix) const
{
    os << prefix << "\n";
    os << size() << " words, " << iExp
       << " after point (x10^" << iTensExp << "), 10-prec "
       << iPrecision << "\n";

    for (int i = size() - 1; i >= 0; --i)
    {
        if (iExp == i+1)
            os << ".\n";

        PlatWord w = at(i);
        PlatWord bit = (WordBase)>>1;

        int k=0;
        while (bit) {
            if ((k&3)==0)
                os << " ";
            k++;
            if (w & bit)
                os << "1";
            else
                os << "0";
            bit>>=1;
        }
        os << "\n";
    }
}

static int DigitIndex(int c)
{
    if (std::isdigit(c))
        return c-'0';
    if (std::islower(c))
        return c-'a'+10;
    if (std::isupper(c))
        return c-'A'+10;

    // TODO error!!!
    return 0;
}

static int Digit(int c)
{
    if (c == '.')
        return '.';
    if (c == '-')
        return '-';
    if (c<=9)
        return '0'+c;
    return 'a'+c-10;
}

ANumber::ANumber(int aPrecision) : iExp(0),iNegative(false),iPrecision(aPrecision),iTensExp(0)
{
  assert(sizeof(PlatDoubleWord) >= 2*sizeof(PlatWord));
  push_back(0);
}


/* ANumber: Constructor for an arbitrary precision number. */
ANumber::ANumber(const char* aString,int aPrecision,int aBase): iExp(0),iNegative(false),iPrecision(aPrecision),iTensExp(0)
{
    SetPrecision(aPrecision);
    SetTo(aString,aBase);
}

std::string IntToBaseString(PlatDoubleWord aInt, int aBase)
{
    std::string s;

    while (aInt!=0) {
        s.push_back(aInt%aBase);
        aInt/=aBase;
    }

    return s;
}

int WordDigits(int aPrecision, int aBase)
{
    if (aPrecision == 0) return 0;
    int bitsPerBase=0;

    while (aBase!=0)
    {
        aBase>>=1;
        bitsPerBase++;
    }
    // I changed this to add two extra words at the end in stead of one, when
    // we moved over to scientific notation. An example of what went wrong was
    // typing -6.23, which for sufficiently low precision got read as 6.229999
    // The original thought was that one word should be enough. This  will have
    // to be examined more closely.
    return (aPrecision*bitsPerBase+2*WordBits)/WordBits;
    //return (aPrecision*bitsPerBase+WordBits)/WordBits;
}


void ANumber::SetTo(const char* aString,int aBase)
{
    clear();

    assert(sizeof(PlatDoubleWord) >= 2*sizeof(PlatWord));
    assert(aBase<=36);
    iNegative=false;
    iExp = 0;
    iTensExp = 0;

    const char* endptr = aString;

    // Parse minus sign
    if (*endptr == '-')
    {
        iNegative=true;
        endptr++;
    }

    int endIntIndex=-1;
    int endFloatIndex=0;
    int endNumberIndex=0;
    while(aString[endNumberIndex] != '\0')
    {
        if (aString[endNumberIndex]=='.')
            endIntIndex = endNumberIndex;
        if ((aBase < 14 && aString[endNumberIndex]=='e') || aString[endNumberIndex]=='E')
            endFloatIndex = endNumberIndex;
        endNumberIndex++;
    }
    if (endFloatIndex == 0)
        endFloatIndex = endNumberIndex;

    if (endIntIndex == -1)
        endIntIndex = endFloatIndex;
    if (endFloatIndex == endIntIndex+1)
        endFloatIndex = endIntIndex;

    if (endFloatIndex-endIntIndex-1 > iPrecision)
      iPrecision = endFloatIndex-endIntIndex-1;

    // Go to least significant digit first
    const char* ptr = aString + endIntIndex-1;

    // Now parse the integer part of the number.
    ANumber factor2(iPrecision);
    factor2[0] = 1;
    while (ptr >= endptr)
    {
        ANumber term(iPrecision);
        term.CopyFrom(factor2);
        WordBaseTimesInt(term, DigitIndex(*ptr));
        WordBaseAdd(*this,term);
        WordBaseTimesInt(factor2, aBase);
        ptr--;
    }

    //Parse the fraction
    if (endFloatIndex > endIntIndex)
    {
        std::string fraction(aString + endIntIndex + 1);

        // Map to a char base number
        const int nr = endFloatIndex - endIntIndex-1;
        LispString::value_type * fractionPtr = &fraction[0];


        std::reverse(fractionPtr, fractionPtr + nr);
        std::transform(fractionPtr, fractionPtr + nr, fractionPtr, &DigitIndex);

        const std::string base = IntToBaseString(WordBase,aBase);

        const int nrDigits = WordDigits(iPrecision,aBase);

        for (int i = 0; i < nrDigits; ++i)
        {
            PlatWord word=0;
            std::string copied = fraction;

            BaseMultiply(fraction, copied, base, aBase);

            const int nrc=fraction.size();
            PlatDoubleWord factor=1;
            for (int j = nr; j < nrc; ++j)
            {
                word += fraction[j]*factor;
                factor = factor*aBase;
            }

            fraction.resize(nr);
            insert(begin(),word);
            iExp++;
        }
    }

    // Parse the E<num> part at the end
    if (endNumberIndex > endFloatIndex+1)
    {
      if (aString[endFloatIndex] == '.')
          endFloatIndex++;

      if (aString[endFloatIndex+1] == '+')
          endFloatIndex++;

      iTensExp = std::atoi(aString + endFloatIndex + 1);
    }

    DropTrailZeroes();
}

void ANumber::CopyFrom(const ANumber& aOther)
{
    iExp       = aOther.iExp;
    iTensExp   = aOther.iTensExp;
    iNegative  = aOther.iNegative;
    iPrecision = aOther.iPrecision;
    resize(aOther.size());

    const int nr = aOther.size();
    if (nr) {
        std::memcpy(&((*this)[0]), &(aOther[0]), nr*sizeof(ANumber::value_type));
    } else {
        resize(1);
        (*this)[0] = 0;
    }
}



bool ANumber::ExactlyEqual(const ANumber& aOther)
{
  if (iExp       != aOther.iExp) return false;
  if (iTensExp   != aOther.iTensExp) return false;
  if (iNegative  != aOther.iNegative) return false;
//  if (iPrecision != aOther.iPrecision) return false;
  if (size()     != aOther.size()) return false;

  //TODO there HAS to be a faster way to copy...
  int nr = size();
  if (nr)
  {
    const ANumber::value_type * sptr = &( aOther[0]);
    ANumber::value_type * tptr = &((*this)[0]);
    while (nr--)
    {
      if (*tptr++ != *sptr++) return false;
    }
  }
  return true;
}

void Multiply(ANumber& aResult, ANumber& a1, ANumber& a2)
{
    // Truncate zeroes (the multiplication is heavy enough as it is...)
    a1.DropTrailZeroes();
    a2.DropTrailZeroes();

    if (a1.iExp || a1.iTensExp)
        NormalizeFloat(a1,WordDigits(a1.iPrecision, 10));
    if (a2.iExp || a2.iTensExp)
        NormalizeFloat(a2,WordDigits(a2.iPrecision, 10));

    // this does some additional removing, as for the multiplication we don't need
    // any trailing zeroes at all, regardless of the value of iExp
    std::size_t end;

    end=a1.size();
    while (end>1 && a1[end-1]==0)
        end--;

    a1.resize(end);

    end=a2.size();
    while (end>1 && a2[end-1]==0)
        end--;

    a2.resize(end);

    // Multiply
    BaseMultiplyFull(aResult,a1,a2);

//PrintNumber("Mult",aResult);

    // Adjust the sign
    aResult.iNegative = a1.IsNegative() != a2.IsNegative();

    // Adjust the exponent.
    aResult.iExp = a1.iExp+a2.iExp;
    aResult.iTensExp = a1.iTensExp+a2.iTensExp;

    a1.Expand();
    a2.Expand();

    aResult.DropTrailZeroes();
    if (aResult.iExp || aResult.iTensExp)
        NormalizeFloat(aResult,WordDigits(aResult.iPrecision, 10));
}

static void BalanceFractions(ANumber& a1, ANumber& a2)
{
    PlatWord word=0;

//ANumber a3("10e2");
//PrintNumber("a3 enter ",a3);
//a3.iTensExp = 0;
//PrintNumber("a3 enter ",a3);

    int nr;

    nr = a2.iExp - a1.iExp;
    // a2 has more decimal digits...
    if (nr>0)
    {
        a1.insert(a1.begin(), nr, word);
        a1.iExp+=nr;
    }
    nr = a1.iExp - a2.iExp;
    // a1 has more decimal digits...
    if (nr>0)
    {
        a2.insert(a2.begin(), nr, word);
        a2.iExp+=nr;
    }


    //TODO this is not the fastest way to multiply by 10^exp
    if (a1.iTensExp < a2.iTensExp)
    {
      int diff = a2.iTensExp - a1.iTensExp;
      a2.iTensExp = a1.iTensExp;
      while (diff > 0)
      {
        WordBaseTimesInt(a2,10);
        diff--;
      }
    }
    else if (a2.iTensExp < a1.iTensExp)
    {
      int diff = a1.iTensExp - a2.iTensExp;
      a1.iTensExp = a2.iTensExp;
      while (diff > 0)
      {
        WordBaseTimesInt(a1,10);
        diff--;
      }
    }
}

void Add(ANumber& aResult, ANumber& a1, ANumber& a2)
{

    // if the numbers are float, make sure they are normalized
    if (a1.iExp || a1.iTensExp)
        NormalizeFloat(a1,WordDigits(a1.iPrecision, 10));
    if (a2.iExp || a2.iTensExp)
        NormalizeFloat(a2,WordDigits(a2.iPrecision, 10));

    //Two positive numbers
    BalanceFractions(a1, a2);

    if (!(a1.IsNegative() || a2.IsNegative()))
    {
        BaseAddFull(aResult, a1, a2);
        aResult.iNegative=false;
    }
    //Two negative numbers
    else if (a1.IsNegative() && a2.IsNegative())
    {
        BaseAddFull(aResult, a1, a2);
        aResult.iNegative=true;
    }
    //Negative plus positive
    else if (a1.IsNegative() && !a2.IsNegative())
    {
        //if |a1|<|a2| then BaseSubtract(a2,a1)
        if (BaseLessThan(a1,a2))
        {
            BaseSubtract(aResult,a2,a1);
            aResult.iNegative = false;
        }
        else if (BaseGreaterThan(a1,a2))
        {// else if (|a1| > |a2| Negate(BaseSubtract(a1,a2))
            BaseSubtract(aResult,a1,a2);
            aResult.iNegative = true;
        }
        else
        {
            ANumber zero(/*???"0",*/aResult.iPrecision);
            aResult.CopyFrom(zero);
        }
    }
    //Positive plus Negative
    else
    {
        assert(!a1.IsNegative() && a2.IsNegative());
        //if |a1|>|a2| then BaseSubtract(a2,a1)
        if (BaseGreaterThan(a1,a2))
        {
            BaseSubtract(aResult,a1,a2);
            aResult.iNegative = false;
        }
        else if (BaseLessThan(a1,a2))
        {// else if (|a1| > |a2| Negate(BaseSubtract(a1,a2))
            BaseSubtract(aResult,a2,a1);
            aResult.iNegative = true;
        }
        else
        {
            ANumber zero(/*???"0",*/aResult.iPrecision);
            aResult.CopyFrom(zero);
        }
    }
    aResult.DropTrailZeroes();

    if (aResult.iExp || aResult.iTensExp)
    {
      if (aResult.iPrecision < a2.iPrecision)
        aResult.iPrecision = a2.iPrecision;
      if (aResult.iPrecision < a1.iPrecision)
        aResult.iPrecision = a1.iPrecision;

      NormalizeFloat(aResult,WordDigits(aResult.iPrecision, 10));
    }
}



void Subtract(ANumber& aResult, ANumber& a1, ANumber& a2)
{
    BalanceFractions(a1, a2);
    if (!a1.IsNegative() && a2.IsNegative())
    {
        BaseAddFull(aResult, a1, a2);
        aResult.iNegative=false;
    }
    else if (a1.IsNegative() && !a2.IsNegative())
    {
        BaseAddFull(aResult, a1, a2);
        aResult.iNegative=true;
    }
    else if (a1.IsNegative() && a2.IsNegative())
    {
        //if |a1|<|a2| then BaseSubtract(a2,a1)
        if (BaseLessThan(a1,a2))
        {
            BaseSubtract(aResult,a2,a1);
            aResult.iNegative = false;
        }
        else if (BaseGreaterThan(a1,a2))
        {// else if (|a1| > |a2| Negate(BaseSubtract(a1,a2))
            BaseSubtract(aResult,a1,a2);
            aResult.iNegative = true;
        }
        else
        {
            ANumber zero(/*???"0",*/aResult.iPrecision);
            aResult.CopyFrom(zero);
        }
    }
    //Positive plus Negative
    else
    {
        assert(!(a1.IsNegative() || a2.IsNegative()));
        //if |a1|>|a2| then BaseSubtract(a2,a1)
        if (BaseGreaterThan(a1,a2))
        {
            BaseSubtract(aResult,a1,a2);
            aResult.iNegative = false;
        }
        else if (BaseLessThan(a1,a2))
        {// else if (|a1| > |a2| Negate(BaseSubtract(a1,a2))
            BaseSubtract(aResult,a2,a1);
            aResult.iNegative = true;
        }
        else
        {
            ANumber zero(/*???"0",*/aResult.iPrecision);
            aResult.CopyFrom(zero);
        }
    }
    aResult.DropTrailZeroes();
}




bool GreaterThan(ANumber& a1, ANumber& a2)
{
    BalanceFractions(a1, a2);
    if (a1.IsNegative() && !a2.IsNegative())
        return false;
    if (!a1.IsNegative() && a2.IsNegative())
        return true;
    if (!a1.IsNegative() && !a2.IsNegative())
        return BaseGreaterThan(a1,a2);
    return BaseLessThan(a1,a2);
}

bool LessThan(ANumber& a1, ANumber& a2)
{

    // if the numbers are float, make sure they are normalized
    if (a1.iExp || a1.iTensExp)
        NormalizeFloat(a1,WordDigits(a1.iPrecision, 10));
    if (a2.iExp || a2.iTensExp)
        NormalizeFloat(a2,WordDigits(a2.iPrecision, 10));

    BalanceFractions(a1, a2);
    if (a1.IsNegative() && !a2.IsNegative())
        return true;
    if (!a1.IsNegative() && a2.IsNegative())
        return false;
    if (!a1.IsNegative() && !a2.IsNegative() )
        return BaseLessThan(a1,a2);
    return BaseGreaterThan(a1,a2);
}


void  ANumberToString(LispString& aResult, ANumber& aNumber, int aBase, bool aForceFloat)
{
    while (aNumber.size() > 1 && aNumber.back() == 0)
        aNumber.pop_back();

    std::size_t nr = aNumber.size();

    //Formatting small numbers can be done faster.
    if (aNumber.iExp == 0 && nr == 1)
    {
        BaseIntNumber(aResult, aNumber[0], aBase);
        std::reverse(aResult.begin(), aResult.end());
        std::transform(aResult.begin(), aResult.end(), aResult.begin(), &Digit);

        if (aForceFloat && !(aResult.size()==1 && aResult[0] == '0'))
            aResult.push_back('.');

        if (aNumber.iNegative && (aResult.size()>1 || aResult[0] != '0'))
            aResult.insert(aResult.begin(),'-');

        goto TENSEXP;
    }
    {
        ANumber number(aNumber.iPrecision);
        number.CopyFrom(aNumber);

        assert(aBase<=36);
        // Reset number
        aResult.clear();
        aResult.push_back(0);

        // Create the number
        std::string factor2;
        BaseIntNumber(factor2, 1, aBase);

        std::string factor3;
        BaseIntNumber(factor3, WordBase, aBase);

        assert(number.iExp >= 0);

        const std::size_t ns = number.size();
        for (std::size_t i=number.iExp; i < ns; ++i)
        {
            //aResult = aResult + number[i] * factor2
            std::string term;
            BaseIntNumber(term, number[i], aBase);
            BaseAddMultiply(aResult, term, factor2, aBase);

            //factor2 = factor2*factor3
            term.swap(factor2);

            BaseMultiply(factor2, term, factor3, aBase);
        }

        //Remove trailing zeroes (most significant side)
        while (aResult.size() > 1 && aResult.back() == 0)
            aResult.pop_back();
        nr = aResult.size();

        std::reverse(aResult.begin(), aResult.end());

        // Get the fraction
        number.resize(number.iExp, 0);
        if (aForceFloat || (number.iExp > 0 && !number.IsZero()))
        {
            int digitPos = aResult.size();

            int i;
            // Build the fraction
            for (i=0;i<number.iPrecision+1;i++)
            {
                WordBaseTimesInt(number, aBase);
                if (int(number.size()) > number.iExp)
                {
                    aResult.push_back(number[number.iExp]);
                    number.resize(number.iExp);
                }
                else
                {
                    aResult.push_back(0);
                }
            }

            // Round off
            if (aResult[aResult.size()-1] >= (aBase>>1))
            {
                //TODO code bloat!
                int carry=1;
                for (i=aResult.size()-1;i>=0;i--)
                {
                    const int word = aResult[i]+carry;
                    aResult[i] = word%aBase;
                    carry = word / aBase;
                }
                if (carry)
                {
                    aResult.insert(aResult.begin(),carry);
                    digitPos++;
                }
            }
            aResult.resize(aResult.size()-1);
            // Insert dot
            aResult.insert(aResult.begin() + digitPos, '.');

            //Remove trailing zeros
            int nr = aResult.size();
            while (nr>1 && aResult[nr-1] == 0)
                nr--;

            if (aResult[nr-1] == '.' && nr == 2 && aResult[0] == 0)
                nr--;
            aResult.resize(nr);
        }

        // Map to ascii
        {
            LispString::value_type * rptr = &aResult[0];
            const std::size_t nr = aResult.size();
            for (std::size_t i = 0; i < nr; ++i)
            {
                *rptr = Digit(*rptr);
                rptr++;
            }
        }

        // If signed, insert a minus sign
        if (number.iNegative)
            if (aResult.size()>1 || aResult[0] != '0')
            {
                char c='-';
                aResult.insert(aResult.begin(),c);
            }
    }

    //Handle tens exp
TENSEXP:
    if (aNumber.iTensExp != 0 &&
        !(aResult[0] == '0' && aResult.size() == 1))
    {
        aResult.push_back('e');
        aResult.append(std::to_string(aNumber.iTensExp));
    }
}

void BaseAddFull(ANumber& aResult, ANumber& a1, ANumber& a2)
{
    // Initialize result
    aResult.CopyFrom(a1);
    WordBaseAdd(aResult,a2);
}



void BaseSubtract(ANumber& aResult, ANumber& a1, ANumber& a2)
{
    aResult.CopyFrom(a1);
    BaseSubtract(aResult, a2,0);
}

void BaseMultiplyFull(ANumber& aResult, ANumber& a1, ANumber& a2)
{
    // Initialize result
    WordBaseMultiply(aResult,a1,a2);
}


bool BaseGreaterThan(const ANumber& a1, const ANumber& a2)
{
    const int nr1 = a1.size();
    const int nr2 = a2.size();

    // Nr is the number of words the two numbers share.
    const int nr  = std::min(nr1, nr2);

    // Comparison of shared words.
    bool highSame;
    {
        int i = nr-1;
        while (i>0 && a1[i] == a2[i]) i--;
        highSame = (a1[i] > a2[i]);
    }

    // Two numbers with same numbers of words: compare these words.
    if (nr1 == nr2)
        return highSame;

    // a1 has more words.
    if (nr1 > nr2)
    {
        // If any of a1's higher words is non-zero, it will be bigger.
        int i;
        for (i=nr2;i<nr1;i++)
            if (a1[i] != 0)
                return true;
        // Otherwise compare the shared highest word.
        return highSame;
    }

    // If any of a2's higher words is non-zero, it will be bigger.
    int i;
    for (i=nr1;i<nr2;i++)
        if (a2[i] != 0)
            return false;
    // Otherwise compare the shared highest word.
    return highSame;
}

bool BaseLessThan(const ANumber& a1, const ANumber& a2)
{
    return BaseGreaterThan(a2,a1);
}


void BaseShiftRight(ANumber& a, int aNrBits)
{
    // Number of words a word jumps
    int wordsShifted = aNrBits/WordBits;

    // Residue: bits shifted out.
    int residue = aNrBits % WordBits;

    // Bit mask: bits that are going to be shifted out of each word.
    PlatDoubleWord bitMask = (PlatDoubleWord)((((PlatDoubleWord)1)<<residue)-1);

    // Nr of bits to move to the other side.
    int otherSideBits = WordBits-residue;

    int i;

    int nr = a.size();

    ANumber::value_type * ptr = &a[0];
    ANumber::value_type * ptrshifted = &a[wordsShifted];
    ANumber::value_type * endp = ptr +nr - wordsShifted;
    if (ptr<endp)
    {
      *ptr = ((*ptrshifted)>>residue);
      ptr++;
      ptrshifted++;
      while (ptr<endp)
  //    for (i=1;i<nr-wordsShifted;i++)
      {
        PlatDoubleWord newCarry = (((PlatDoubleWord)*ptrshifted) & bitMask)<<otherSideBits;
        *ptr = ((*ptrshifted)>>residue);
        ptr[-1] |= newCarry;
        ptr++;
        ptrshifted++;
      }
    }

    int start=nr-wordsShifted;
    if (start<0)
        start=0;
    for (i=start;i<nr;i++)
    {
        a[i] = 0;
    }
}

void BaseShiftLeft(ANumber& a, int aNrBits)
{
    // Number of words a word jumps
    int wordsShifted = aNrBits/WordBits;

    // Residue: bits shifted out.
    int residue = aNrBits % WordBits;

    // Nr of bits to move to the other side.
    int otherSideBits = WordBits-residue;

    // Bit mask: bits that are going to be shifted out of each word.
    int bitMask = ((1L<<residue)-1)<<otherSideBits;

    int i;
    int nr = a.size();

    for (i=0;i<=wordsShifted;i++)
    {
        a.push_back(0);
    }

    ANumber::value_type * ptr = &a[0];

    for (i=nr+wordsShifted;i>=wordsShifted;i--)
    {
        PlatDoubleWord newCarry =
            (((PlatDoubleWord)ptr[i-wordsShifted]) & bitMask)>>otherSideBits;
        ptr[i] = (ptr[i-wordsShifted]<<residue);

        if (i < nr+wordsShifted)
            ptr[i+1] |= newCarry;
    }
    for (i=wordsShifted-1;i>=0;i--)
    {
        ptr[i] = 0;
    }
}



/* Binary Greatest common divisor algorithm. */
void BaseGcd(ANumber& aResult, ANumber& a1, ANumber& a2)
{
    ANumber zero(aResult.Precision());
    ANumber u(aResult.Precision());
    ANumber v(aResult.Precision());
    u.CopyFrom(a1);
    v.CopyFrom(a2);
    u.iNegative = v.iNegative = false;

     int k=0;


    {
      int i=0;
      PlatWord bit=1;
      while (u[i] == 0 && v[i]==0) i++;
      k+=WordBits*i;
      while ((u[i]&bit) == 0 && (v[i]&bit)==0)
      {
        bit<<=1;
        k++;
      }
      BaseShiftRight(u,k);
      BaseShiftRight(v,k);
    }
    ANumber t(/*???"0",*/10);

    if (!u.IsEven())
    {
        t.CopyFrom(v);
        t.Negate();
    }
    else
        t.CopyFrom(u);

    while (!IsZero(t))
    {

        {
          int k=0;
          int i=0;
          PlatWord bit=1;
          while (t[i] == 0) i++;
          k+=WordBits*i;
          while ((t[i]&bit) == 0)
          {
            bit<<=1;
            k++;
          }
          BaseShiftRight(t,k);
        }
        if (GreaterThan(t,zero))
        {
            u.CopyFrom(t);
        }
        else
        {
            v.CopyFrom(t);
            v.Negate();
        }
        Subtract(t,u,v);
     }
    aResult.CopyFrom(u);
    aResult.iNegative=false;
    BaseShiftLeft(aResult,k);
}







void BaseDivide(ANumber& aQuotient, ANumber& aRemainder, ANumber& a1, ANumber& a2)
{
    // Find the values n and m as described in Knuth II:
    int n,m;
    n=a2.size();
    assert(n>0);
    assert(a2[n-1] != 0);

    //a1.size() = m+n => m = a1.size()-n
    m = a1.size()-n;
    assert(m>=0);

    aQuotient.resize(m+1);

    //D1:
    PlatDoubleWord d = WordBase/(a2[n-1]+1);
    WordBaseTimesInt(a1, d);
    WordBaseTimesInt(a2, d);
    a1.push_back(0);
    a2.push_back(0);

    //D2:
    int j = m;

    while (j>=0)
    {
        //D3:
        PlatDoubleWord q = (a1[j+n]*WordBase+a1[j+n-1])/a2[n-1];
        PlatDoubleWord r = (a1[j+n]*WordBase+a1[j+n-1])%a2[n-1];

    REDO:
        if (q == WordBase || q*a2[n-2] > WordBase*r+a1[j+n-2])
        {
            q = q - 1;
            r = r + a2[n-1];
            if (r < WordBase)
                goto REDO;
        }

        //D4:
        ANumber sub(aQuotient.Precision());
        sub.CopyFrom(a2);
        WordBaseTimesInt(sub, q);
        sub.push_back(0);

        PlatSignedDoubleWord carry;
        int digit;
        {//Subtract the two
            //TODO this can be generalized!!!!
            //
            // Beware though: this is not a normal subtraction. Only a
            // certain set of digits ends up being subtracted.

            // First check if qv isn't too big...
            carry = 0;
            for (digit=0;digit<=n;digit++)
            {
                PlatSignedDoubleWord word;
                word = ((PlatSignedDoubleWord)a1[digit+j]) -
                    ((PlatSignedDoubleWord)sub[digit]) +
                    (PlatSignedDoubleWord)carry;
                carry=0;
                while (word<0)
                {
                    word+=WordBase;
                    carry--;
                }
            }
            if (carry)
            {
                q--;
                sub.CopyFrom(a2);
                WordBaseTimesInt(sub, q);
                sub.push_back(0);
            }

            carry = 0;
            for (digit=0;digit<=n;digit++)
            {
                PlatSignedDoubleWord word;
                word = ((PlatSignedDoubleWord)a1[digit+j]) -
                    ((PlatSignedDoubleWord)sub[digit]) +
                    (PlatSignedDoubleWord)carry;
                carry=0;
                while (word<0)
                {
                    word+=WordBase;
                    carry--;
                }
                a1[digit+j] = ((PlatWord)(word));
            }
        }
        assert(carry == 0);

        //D5:
        aQuotient[j] = (PlatWord)q;
        //D7:
        j--;

    }

    //D8:
    a1.resize(n);
    PlatDoubleWord carry;
    BaseDivideInt(a1, d, WordBase,carry);
    aRemainder.CopyFrom(a1);
}


void IntegerDivide(ANumber& aQuotient, ANumber& aRemainder, ANumber& a1, ANumber& a2)
{
    assert(!a2.IsZero());

    int n=a2.size();

    while (a2[n-1] == 0) n--;
    a2.resize(n);

    if (n==1)
    {
        PlatDoubleWord carry;
        aQuotient.CopyFrom(a1);
        aQuotient.iExp = a1.iExp-a2.iExp;
        aQuotient.iTensExp = a1.iTensExp-a2.iTensExp;

        BaseDivideInt(aQuotient,a2[0], WordBase, carry);
        aRemainder.resize(1);
        aRemainder[0] = (PlatWord)carry;
    }
    // if |a1| < |a2| then result is zero.
    else if (BaseLessThan(a1,a2))
    {
        aQuotient.iExp = 0;
        aQuotient.iTensExp = 0;
        aQuotient.resize(1);
        aQuotient[0] = 0;
        aRemainder.CopyFrom(a1);
    }
    else
    {
        aQuotient.iExp = a1.iExp-a2.iExp;
        aQuotient.iTensExp = a1.iTensExp-a2.iTensExp;
        // Divide the mantissas
        WordBaseDivide(aQuotient, aRemainder, a1, a2);
    }

    // Correct for signs
    if (a1.IsNegative() == a2.IsNegative())
    {
        aQuotient.iNegative = false;
        aRemainder.iNegative = false;
    }
    else
    {
        aQuotient.iNegative = true;
        aRemainder.iNegative = true;
    }
}

void NormalizeFloat(ANumber& a2, int digitsNeeded)
{
  if (a2.iExp - digitsNeeded > 0)
  {
    a2.erase(a2.begin(), a2.begin() + a2.iExp - digitsNeeded);
    a2.iExp -= (a2.iExp-digitsNeeded);
  }

  const std::size_t min = std::max(1+digitsNeeded, a2.iExp+1);
  
  std::size_t n = a2.size();
  while (n > min ||
          (n == min && a2.back() > 10))
  {
    PlatDoubleWord carry = 0;
    BaseDivideInt(a2, 10, WordBase,carry);
    if (a2.back() == 0)
        a2.pop_back();
    a2.iTensExp++;
    n = a2.size();
  }
}


void Divide(ANumber& aQuotient, ANumber& aRemainder, ANumber& a1, ANumber& a2)
{

    // Now add some digits to the front, to end up with the correct
    // precision: the resulting precision will be a1.iExp-a2.iExp.
    // This value should at least be WordDigits, so grow a1
    // by WordDigits-(a1.iExp-a2.iExp) = WordDigits+a2.iExp-a1.iExp
    int digitsNeeded = WordDigits(aQuotient.iPrecision, 10);

    NormalizeFloat(a2,digitsNeeded);

    const int toadd = a2.iExp-a1.iExp;
    PlatWord zero=0;
    if (toadd > 0) {
        a1.insert(a1.begin(), toadd, zero);
        a1.iExp += toadd;
    }

    if (!a1.IsZero()) {
        const std::size_t n = a2.size();
        while (a1.size()< n + digitsNeeded || a1.back() < a2.back()) {
            WordBaseTimesInt(a1, 10);
            a1.iTensExp--;
        }
    }

    IntegerDivide(aQuotient,aRemainder,a1,a2);

    NormalizeFloat(aQuotient,digitsNeeded);
}


void BaseSqrt(ANumber& aResult, const ANumber& N)
{

    int l2;
    ANumber u  (aResult.Precision());
    ANumber v  (aResult.Precision());
    ANumber u2 (aResult.Precision());
    ANumber v2 (aResult.Precision());
    ANumber uv2(aResult.Precision());
    ANumber n  (aResult.Precision());
    ANumber two("2",10);

    //sqrt(1) = 1, sqrt(0) = 0
    if( BaseGreaterThan(two, N))
    {
        aResult.CopyFrom(N);
        return;
    }

    // Find highest set bit, l2
    u.CopyFrom(N);
    l2 = 0;
    while (!u.IsZero())
    {
        BaseShiftRight(u,1);
        l2++;
    }
    l2--;

    // 1<<(l2/2) now would be a good under estimate for the square root.
    // 1<<(l2/2) is definitely set in the result. Also it is the highest
    // set bit.
    l2 >>= 1;

    // initialize u and u2 (u2==u^2).
    u.SetTo("1");
    BaseShiftLeft(u,l2);
    u2.CopyFrom(u);
    BaseShiftLeft(u2,l2);

    // Now for each lower bit:
    while( l2-- )
    {
        // Get that bit in v, and v2 == v^2.
        v.SetTo("1");
        BaseShiftLeft(v,l2);
        v2.CopyFrom(v);
        BaseShiftLeft(v2,l2);

        // uv2 == 2*u*v
        uv2.CopyFrom(u);
        BaseShiftLeft(uv2,(l2 + 1));

        // n = (u+v)^2  =  u^2 + 2*u*v + v^2 = u2+uv2+v2
        n.CopyFrom(u2);
        WordBaseAdd(n,uv2);
        WordBaseAdd(n,v2);
        // if n (possible new best estimate for sqrt(N)^2 is smaller than
        // N, then the bit l2 is set in the result, and add v to u.
        if( !BaseGreaterThan(n , N) )
        {
            WordBaseAdd(u,v); // u <- u+v
            u2.CopyFrom(n);  // u^2 <- u^2 + 2*u*v + v^2
        }
    }
    aResult.CopyFrom(u);
}

void Sqrt(ANumber& aResult, ANumber& N)
{
    int digs = WordDigits(N.iPrecision, 10);
    PlatWord zero=0;
    if ((N.iTensExp&1) != 0)
    {
      WordBaseTimesInt(N,10);
      N.iTensExp--;
    }
    while(N.iExp<2*digs || (N.iExp&1))
    {
        N.insert(N.begin(),zero);
        N.iExp++;
    }

    const int resultDigits = N.iExp/2;
    const int resultTensExp = N.iTensExp/2;

    BaseSqrt(aResult, N);

    aResult.iExp=resultDigits;
    aResult.iTensExp = resultTensExp;
}


/*** Significant : return whether this number is not zero, up to
 * the number of digits specified behind the dot (as per aPrecision).
 */
bool Significant(ANumber& a)
{
    int significantDigits = WordDigits(a.iPrecision, 10);
    NormalizeFloat(a,significantDigits);
    //hier
    int nrExt = (a.size()-a.iExp)*((WordBits)/3);
    if ((-a.iTensExp) > a.iPrecision+2+nrExt)
    {
      return false;
    }
    return true;
}


void ANumber::RoundBits()
{
    PlatWord* ptr = data();
    if (front() >= (WordBase / 2)) {
        PlatDoubleWord carry = 1;
        for (int i = 1, nr = size(); i < nr; i++) {
            const PlatDoubleWord dword = ptr[i] + carry;
            ptr[i] = dword;
            carry = dword >> WordBits;
        }

        if (carry)
            push_back(carry);
    }

    front() = 0;
}

void ANumber::ChangePrecision(int aPrecision)
{
  //First, round.
/*FIXME TODO not working correctly yet */
  //TODO code bloat! Deserves its own routine!
  if (aPrecision == 0 && iExp>1)
  {
    RoundBits();
  }
//  return;

  //FIXME the following line is there to assure there are enough words. Somehow this got truncated?
  //FIXME numerics.yts fails
  Expand();

  int oldExp = iExp;

  iPrecision = aPrecision;
  int newExp = WordDigits(iPrecision,10);
  if (newExp < oldExp)
  {
    iExp = newExp;
    erase(begin(),begin()+oldExp-iExp);
  }
  else if (newExp > oldExp)
  {
    iExp = newExp;
    PlatWord zero = 0;
    insert(begin(), newExp-oldExp, zero);
  }
}



void ANumber::DropTrailZeroes()
{
  Expand();

  {
    int nr=size();
    while (nr>iExp+1 && (*this)[nr-1] == 0) nr--;
    resize(nr);
  }
  {
    int low=0;
    while (low<iExp && (*this)[low] == 0) low++;
    if (low)
    {
      erase(begin(), begin() + low);
      iExp-=low;
    }
  }
}

