


/* Arbitrary precision arithmetic classes. These are NOT designed
 * to be bleeding fast, just reaonably fast, but very clean code.
 *
 */


#include "anumber.h"
#include "mathutil.h"

/* The Base... functions perform actions on the mantissa part of the
 * number, that is, it treats them as unsigned integers.
 */
void BaseAddFull(ANumber& aResult, ANumber& a1, ANumber& a2);
void BaseSubtract(ANumber& aResult, ANumber& a1, ANumber& a2);
//void BaseSubtract(ANumber& aResult, ANumber& a2, LispInt offset);
void BaseMultiplyFull(ANumber& aResult, ANumber& a1, ANumber& a2);
void BaseDivide(ANumber& aQuotient, ANumber& aRemainder, ANumber& a1, ANumber& a2);
LispBoolean BaseGreaterThan(ANumber& a1, ANumber& a2);
LispBoolean BaseLessThan(ANumber& a1, ANumber& a2);
void BaseSqrt(ANumber& aResult, ANumber& N);



static LispInt DigitIndex(LispInt c)
{
    if (c>='0' && c<='9')
        return c-'0';
    if (c>='a' && c<='z')
        return c-'a'+10;
    if (c>='A' && c<='Z')
        return c-'A'+10;

    // TODO error!!!
    return 0;
}

static LispInt Digit(LispInt c)
{
    if (c == '.')
        return '.';
    if (c == '-')
        return '-';
    if (c<=9)
        return '0'+c;
    return 'a'+c-10;
}

ANumber::~ANumber()
{
}

ANumber::ANumber(LispInt aPrecision)
{
    LISPASSERT(sizeof(PlatDoubleWord) >= 2*sizeof(PlatWord));
    iPrecision = aPrecision;
    iNegative=LispFalse;
    iExp = 0;
    iTensExp = 0;
    Append(0);
}

/* Allow use of external arrays */
ANumber::ANumber(PlatWord *aArray, LispInt aSize, LispInt aPrecision)
{
    LISPASSERT(sizeof(PlatDoubleWord) >= 2*sizeof(PlatWord));
    iPrecision = aPrecision;
    iNegative=LispFalse;
    iExp = 0;
    iTensExp = 0;
    SetExternalArray(aArray, aSize);
}

/* ANumber: Constructor for an arbitrary precision number. */
ANumber::ANumber(LispCharPtr aString,LispInt aPrecision,LispInt aBase)
{
    SetPrecision(aPrecision);
    SetTo(aString,aBase);
}

void IntToBaseString(LispString& aString,PlatDoubleWord aInt, LispInt aBase)
{
    // Build the string
    aString.SetNrItems(0);
    LispInt i=0;
    while (aInt!=0)
    {
        aString.GrowTo(i+1);
        aString[i] = aInt%aBase;
        aInt/=aBase;
        i++;
    }
}


void IntToAscii(LispString& aString,PlatDoubleWord aInt, LispInt aBase)
{
    //    LispBoolean negative = (aInt<0);
    LispBoolean negative = false;
    
    // Sign will be handled separately
    if (negative)
        aInt = -aInt;

    IntToBaseString(aString,aInt,aBase);
    LispInt i;
    LispInt nr = aString.NrItems();
    for (i=0;i<nr>>1;i++)
    {
        LispChar c = aString[i];
        aString[i] = Digit(aString[nr-i-1]);
        aString[nr-i-1] = Digit(c);
    }
    if (nr&1)
    {
        aString[(nr>>1)] = Digit(aString[(nr>>1)]);
    }
    if (negative)
    {
        LispChar c = '-';
        aString.Insert(0,c);
    }
    aString.Append('\0');
}

LispInt WordDigits(LispInt aPrecision, LispInt aBase)
{
    LispInt bitsPerBase=0;

    while (aBase!=0)
    {
        aBase>>=1;
        bitsPerBase++;
    }
    return (aPrecision*bitsPerBase+WordBits)/WordBits;
}


void ANumber::SetTo(LispCharPtr aString,LispInt aBase)
{
    SetNrItems(0);
    
    LISPASSERT(sizeof(PlatDoubleWord) >= 2*sizeof(PlatWord));
    LISPASSERT(aBase<=36);
    iNegative=LispFalse;
    iExp = 0;
    iTensExp = 0;
    
    LispCharPtr endptr = aString;

    // Parse minus sign
    if (*endptr == '-')
    {
        iNegative=LispTrue;
        endptr++;
    }

    LispInt endIntIndex=-1;
    LispInt endFloatIndex=0;
    LispInt endNumberIndex=0;
    while(aString[endNumberIndex] != '\0')
    {
        if (aString[endNumberIndex]=='.')
            endIntIndex = endNumberIndex;
        if (aString[endNumberIndex]=='e' || aString[endNumberIndex]=='E')
            endFloatIndex = endNumberIndex;
        endNumberIndex++;
    }
    if (endFloatIndex == 0)
        endFloatIndex = endNumberIndex;

    if (endIntIndex == -1)
        endIntIndex = endFloatIndex;
    if (endFloatIndex == endIntIndex+1)
        endFloatIndex = endIntIndex;
        
    // Go to least significant digit first
    LispCharPtr ptr = aString + endIntIndex-1; 
    
    // Now parse the integer part of the number.
    ANumber factor2(iPrecision);
    factor2[0] = 1;
    while (ptr >= endptr)
    {
        ANumber term(iPrecision);
        term.CopyFrom(factor2);
        BaseTimesInt(term, DigitIndex(*ptr), WordBase);
        BaseAdd(*this,term,WordBase);
        /*
        ANumber current(iPrecision);
        current.CopyFrom(*this);
        BaseAddFull(*this,current,term);
        */
        BaseTimesInt(factor2, aBase, WordBase);
        ptr--;
    }

    //Parse the fraction
    if (endFloatIndex > endIntIndex)
    {
        LispString fraction(&aString[endIntIndex+1]);
        LispInt i;

        // Map to a char base number
        LispInt nr = fraction.NrItems()-1; //Excluding the zero terminator

        for (i=0;i<nr>>1;i++)
        {
            LispChar c = fraction[i];
            fraction[i] = DigitIndex(fraction[nr-i-1]);
            fraction[nr-i-1] = DigitIndex(c);
        }
        if (nr&1)
            fraction[nr>>1] = DigitIndex(fraction[nr>>1]);
            
        LispString base;
        IntToBaseString(base,WordBase,aBase);

        LispInt nrDigits = WordDigits(iPrecision,aBase);
        for (i=0;i<nrDigits;i++)
        {
            PlatWord word=0;
            LispString copied;
            LispInt j;

            //TODO!!! This is probably not the good way to copy!
            {
                LispInt nrc=fraction.NrItems();
                copied.GrowTo(nrc);
                copied.SetNrItems(nrc);
                for (j=0;j<nrc;j++)
                    copied[j]=fraction[j];
            }
            BaseMultiply(fraction, copied, base, aBase);

            {
                LispInt nrc=fraction.NrItems();
                PlatDoubleWord factor=1;
                for (j=nr;j<nrc;j++)
                {
                    word = word + fraction[j]*factor;
                    factor = factor*aBase;
                }
            }
            fraction.SetNrItems(nr);
            Insert(0,word);
            iExp++;
        }
    }

    // Parse the E<num> part at the end
    if (endNumberIndex > endFloatIndex+1)
    {
        iTensExp = PlatAsciiToInt(&aString[endFloatIndex+1]);
    }
}

void ANumber::CopyFrom(ANumber& aOther)
{
    iExp       = aOther.iExp;
    iTensExp   = aOther.iTensExp;
    iNegative  = aOther.iNegative;
    iPrecision = aOther.iPrecision;
    GrowTo(aOther.NrItems());
    SetNrItems(aOther.NrItems());

    //TODO there HAS to be a faster way to copy...
    LispInt nr = aOther.NrItems();
    ANumber::ElementTypePtr sptr = &aOther[0];
    ANumber::ElementTypePtr tptr = &Item(0);
    while (nr--)
    {
        *tptr++ = *sptr++;
    }
}


/* Negate negates a number. */
void Negate(ANumber& aNumber)
{
    aNumber.iNegative=!aNumber.iNegative;
    if (IsZero(aNumber))
        aNumber.iNegative = LispFalse;
        
}

void Multiply(ANumber& aResult, ANumber& a1, ANumber& a2)
{
    // Truncate zeroes (the multiplication is heavy enough as it is...)
    LispInt todel;

    todel=0;
    while (todel<a1.iExp && a1[todel] == 0)
    {
        todel++;
    }
    if (todel)
    {
        a1.iExp-=todel;
        a1.Delete(0,todel);
    }

    todel=0;
    while (todel<a2.iExp && a2[todel] == 0)
    {
        todel++;
    }
    if (todel)
    {
        a2.iExp-=todel;
        a2.Delete(0,todel);
    }

    LispInt end;

    end=a1.NrItems();
    while (end>1 && a1[end-1]==0)
    {
        end--;
    }
    a1.SetNrItems(end);

    end=a2.NrItems();
    while (end>1 && a2[end-1]==0)
    {
        end--;
    }
    a2.SetNrItems(end);
    
    // Multiply
    BaseMultiplyFull(aResult,a1,a2);

    // Adjust the sign
    if (IsPositive(a1) && IsPositive(a2))
        aResult.iNegative = LispFalse;
    else if (IsNegative(a1) && IsNegative(a2))
        aResult.iNegative = LispFalse;
    else
        aResult.iNegative = LispTrue;

    // Adjust the exponent.
    aResult.iExp = a1.iExp+a2.iExp;
    aResult.iTensExp = a1.iTensExp+a2.iTensExp;

    while(a1.NrItems()<a1.iExp+1)
        a1.Append(0);
    while(a2.NrItems()<a2.iExp+1)
        a2.Append(0);
    while(aResult.NrItems()<aResult.iExp+1)
        aResult.Append(0);
}

static void BalanceFractions(ANumber& a1, ANumber& a2)
{
    PlatWord word=0;

    LispInt nr;

    nr = a2.iExp - a1.iExp;
    // a2 has more decimal digits...
    if (nr>0)
    {
        a1.Insert(0,word,nr);
        a1.iExp+=nr;
    }
    nr = a1.iExp - a2.iExp;
    // a1 has more decimal digits...
    if (nr>0)
    {
        a2.Insert(0,word,nr);
        a2.iExp+=nr;
    }
}

void Add(ANumber& aResult, ANumber& a1, ANumber& a2)
{
    //Two positive numbers
    BalanceFractions(a1, a2);

    if (IsPositive(a1) && IsPositive(a2))
    {
        BaseAddFull(aResult, a1, a2);
        aResult.iNegative=LispFalse;
    }
    //Two negative numbers
    else if (IsNegative(a1) && IsNegative(a2))
    {
        BaseAddFull(aResult, a1, a2);
        aResult.iNegative=LispTrue;
    }
    //Negative plus positive
    else if (IsNegative(a1) && IsPositive(a2))
    {
        //if |a1|<|a2| then BaseSubtract(a2,a1)
        if (BaseLessThan(a1,a2))
        {
            BaseSubtract(aResult,a2,a1);
            aResult.iNegative = LispFalse;
        }
        else if (BaseGreaterThan(a1,a2))
        {// else if (|a1| > |a2| Negate(BaseSubtract(a1,a2))
            BaseSubtract(aResult,a1,a2);
            aResult.iNegative = LispTrue;
        }
        else
        {
            ANumber zero("0",aResult.iPrecision);
            aResult.CopyFrom(zero);
        }
    }
    //Positive plus Negative
    else 
    {
        LISPASSERT(IsPositive(a1) && IsNegative(a2));
        //if |a1|>|a2| then BaseSubtract(a2,a1)
        if (BaseGreaterThan(a1,a2))
        {
            BaseSubtract(aResult,a1,a2);
            aResult.iNegative = LispFalse;
        }
        else if (BaseLessThan(a1,a2))
        {// else if (|a1| > |a2| Negate(BaseSubtract(a1,a2))
            BaseSubtract(aResult,a2,a1);
            aResult.iNegative = LispTrue;
        }
        else
        {
            ANumber zero("0",aResult.iPrecision);
            aResult.CopyFrom(zero);
        }
    }
}



void Subtract(ANumber& aResult, ANumber& a1, ANumber& a2)
{
    BalanceFractions(a1, a2);
    if (IsPositive(a1) && IsNegative(a2))
    {
        BaseAddFull(aResult, a1, a2);
        aResult.iNegative=LispFalse;
    }
    else if (IsNegative(a1) && IsPositive(a2))
    {
        BaseAddFull(aResult, a1, a2);
        aResult.iNegative=LispTrue;
    }
    else if (IsNegative(a1) && IsNegative(a2))
    {
        //if |a1|<|a2| then BaseSubtract(a2,a1)
        if (BaseLessThan(a1,a2))
        {
            BaseSubtract(aResult,a2,a1);
            aResult.iNegative = LispFalse;
        }
        else if (BaseGreaterThan(a1,a2))
        {// else if (|a1| > |a2| Negate(BaseSubtract(a1,a2))
            BaseSubtract(aResult,a1,a2);
            aResult.iNegative = LispTrue;
        }
        else
        {
            ANumber zero("0",aResult.iPrecision);
            aResult.CopyFrom(zero);
        }
    }
    //Positive plus Negative
    else 
    {
        LISPASSERT(IsPositive(a1) && IsPositive(a2));
        //if |a1|>|a2| then BaseSubtract(a2,a1)
        if (BaseGreaterThan(a1,a2))
        {
            BaseSubtract(aResult,a1,a2);
            aResult.iNegative = LispFalse;
        }
        else if (BaseLessThan(a1,a2))
        {// else if (|a1| > |a2| Negate(BaseSubtract(a1,a2))
            BaseSubtract(aResult,a2,a1);
            aResult.iNegative = LispTrue;
        }
        else
        {
            ANumber zero("0",aResult.iPrecision);
            aResult.CopyFrom(zero);
        }
    }
}




LispBoolean GreaterThan(ANumber& a1, ANumber& a2)
{
    BalanceFractions(a1, a2);
    if (IsNegative(a1) && IsPositive(a2))
        return LispFalse;
    if (IsPositive(a1) && IsNegative(a2))
        return LispTrue;
    if (IsPositive(a1) && IsPositive(a2))
        return BaseGreaterThan(a1,a2);
    return BaseLessThan(a1,a2);
}

LispBoolean LessThan(ANumber& a1, ANumber& a2)
{
    BalanceFractions(a1, a2);
    if (IsNegative(a1) && IsPositive(a2))
        return LispTrue;
    if (IsPositive(a1) && IsNegative(a2))
        return LispFalse;
    if (IsPositive(a1) && IsPositive(a2))
        return BaseLessThan(a1,a2);
    return BaseGreaterThan(a1,a2);
}


void  ANumberToString(LispString& aResult, ANumber& aNumber, LispInt aBase)
{
    LispInt nr = aNumber.NrItems();
    while (nr>1 && aNumber[nr-1] == 0) nr--;
    aNumber.SetNrItems(nr);

    //Formatting small numbers can be done faster.
    if (aNumber.iExp == 0 && nr == 1)
    {
        BaseIntNumber(aResult, aNumber[0], aBase);
        nr=aResult.NrItems();
        // swap order of the digits, and map to ascii
        {
            LispInt i;
            LispString::ElementTypePtr rptr = &aResult[0];
            for (i=0;i<nr>>1;i++)
            {
                LispString::ElementType c=rptr[i];
                rptr[i] = Digit(rptr[nr-i-1]);
                rptr[nr-i-1] = Digit(c);
            }
            if (nr&1)
                aResult[nr>>1] = Digit(aResult[nr>>1]);
        }

        if (aNumber.iNegative)
        {
            if (aResult.NrItems()>1 || aResult[0] != '0')
            {
                LispChar c='-';
                aResult.Insert(0,c);
            }
        }
        goto TENSEXP;
    }
    {
        ANumber number(aNumber.iPrecision);
        number.CopyFrom(aNumber);

        LISPASSERT(aBase<=36);
        // Reset number
        aResult.SetNrItems(0);
        aResult.Append(0);

        // Create the number
        LispString factor2;
        BaseIntNumber(factor2, 1, aBase);

        LispString factor3;
        BaseIntNumber(factor3, WordBase, aBase);

        LISPASSERT(number.iExp >= 0);

        LispInt i;
        for (i=number.iExp;i<number.NrItems();i++)
        {
            //aResult = aResult + number[i] * factor2
            LispString term;
            BaseIntNumber(term, number[i], aBase);
            BaseAddMultiply(aResult, term, factor2, aBase);

            //TODO this one doesn't have to be done the last iteration
            //factor2 = factor2*factor3

            {
                LispInt nr = factor2.NrItems();
                term.GrowTo(nr);
                LispInt j;
                LispString::ElementTypePtr fptr = &factor2[0];
                LispString::ElementTypePtr tptr = &term[0];
                for (j=0;j<nr;j++)
                {
                    *tptr++ = *fptr++;
                }
            }
            BaseMultiply(factor2, term, factor3, aBase);
        }

        //Remove trailing zeroes (most significant side)
        nr = aResult.NrItems();
        while (nr>1 && aResult[nr-1] == 0) nr--;
        aResult.SetNrItems(nr);

        // swap order of the digits, and map to ascii
        {
            LispString::ElementTypePtr rptr = &aResult[0];
            for (i=0;i<nr>>1;i++)
            {
                LispString::ElementType c=rptr[i];
                rptr[i] = rptr[nr-i-1];
                rptr[nr-i-1] = c;
            }
        }

        // Get the fraction
        while(number.NrItems()<number.iExp)
            number.Append(0);
        number.SetNrItems(number.iExp);
        if (number.iExp > 0 && !IsZero(number))
        {
            LispInt digitPos = aResult.NrItems();

            LispInt i;
            // Build the fraction
            for (i=0;i<number.iPrecision+1;i++)
            {
                BaseTimesInt(number, aBase, WordBase);
                if (number.NrItems() > number.iExp)
                {
                    aResult.Append(number[number.iExp]);
                    number.SetNrItems(number.iExp);
                }
                else
                {
                    aResult.Append(0);
                }
            }

            // Round off
            if (aResult[aResult.NrItems()-1] >= (aBase>>1))
            {
                //TODO code bloat!
                LispInt carry=1;
                for (i=aResult.NrItems()-1;i>=0;i--)
                {
                    LispInt word = aResult[i]+carry;
                    aResult[i] = word%aBase;
                    carry = word / aBase;
                }
                if (carry)
                {
                    LispChar c = carry;
                    aResult.Insert(0,c);
                    digitPos++;
                }
            }
            aResult.SetNrItems(aResult.NrItems()-1);

            // Insert dot
            LispChar c='.';
            aResult.Insert(digitPos,c);

            //Remove trailing zeros
            LispInt nr = aResult.NrItems();
            while (nr>1 && aResult[nr-1] == 0)
            {
                nr--;
            }
            if (aResult[nr-1] == '.')
                nr--;
            aResult.SetNrItems(nr);
        }

        // Map to ascii
        {
            LispString::ElementTypePtr rptr = &aResult[0];
            LispInt nr = aResult.NrItems();
            for (i=0;i<nr;i++)
            {
                *rptr = Digit(*rptr);
                rptr++;
            }
        }

        // If signed, insert a minus sign
        if (number.iNegative)
            if (aResult.NrItems()>1 || aResult[0] != '0')
            {
                LispChar c='-';
                aResult.Insert(0,c);
            }
    }

    //Handle tens exp
TENSEXP:
    if (aNumber.iTensExp != 0)
    {
        aResult.Append('E');
        LispString tens;
        //hier
        IntToAscii(tens,aNumber.iTensExp, 10);
        LispInt i,nr;
        nr=PlatStrLen(&tens[0]);
        for (i=0;i<nr;i++)
            aResult.Append(tens[i]);
    }

    // Zero-terminate the resulting string
    aResult.Append('\0');
    return;
}

void BaseAddFull(ANumber& aResult, ANumber& a1, ANumber& a2)
{
    // Initialize result
    aResult.CopyFrom(a1);
    BaseAdd(aResult,a2,WordBase);
}



void BaseSubtract(ANumber& aResult, ANumber& a1, ANumber& a2)
{
    aResult.CopyFrom(a1);
    BaseSubtract(aResult, a2,0);
}
/*
// Added the offset param for the division algo.
void BaseSubtract(ANumber& aResult, ANumber& a2, LispInt offset)
{
    if (IsZero(a2))
        return;
    LISPASSERT(!IsZero(a2));
    // Initialize result
    LispInt nr = a2.NrItems();
    while (a2[nr-1] == 0)
        nr--;

    // Subtract on a per-digit basis
    PlatSignedDoubleWord carry=0;
    LispInt digit;
    for (digit=0;digit<nr;digit++)
    {
        PlatSignedDoubleWord word;
        word = ((PlatSignedDoubleWord)aResult[digit+offset]) -
            ((PlatSignedDoubleWord)a2[digit]) +
            (PlatSignedDoubleWord)carry;
        carry=0;
        while (word<0)
        {
            word+=WordBase;
            carry--;
        }
        aResult[digit+offset] = ((PlatWord)(word%WordBase));
    }

    while (carry != 0)
    {
        LISPASSERT(nr+offset<aResult.NrItems());

        LispInt newCarry = 0;
        PlatSignedDoubleWord ww = aResult[nr+offset]+carry;
        while (ww<0)
        {
            ww = ww + WordBase;
            newCarry = newCarry - 1;
        }
        aResult[nr+offset]=ww;
        carry = newCarry;
        offset++;
    }
}*/


void BaseMultiplyFull(ANumber& aResult, ANumber& a1, ANumber& a2)
{
    // Initialize result
    BaseMultiply(aResult,a1,a2,WordBase);
}


LispBoolean BaseGreaterThan(ANumber& a1, ANumber& a2)
{
    LispInt nr1 = a1.NrItems();
    LispInt nr2 = a2.NrItems();

    // Nr is the number of words the two numbers share.
    LispInt nr  = nr1;
    if (nr2<nr1)
        nr=nr2;

    // Comparison of shared words.
    LispBoolean highSame;
    {
        LispInt i = nr-1;
        while (i>0 && a1[i] == a2[i]) i--;
        highSame = (a1[i] > a2[i]);
    }
    
    // Two numbers with same numbers of words: compare these words.
    if (nr1 == nr2)
    {
        return highSame;
    }

    // a1 has more words.
    if (nr1 > nr2)
    {
        // If any of a1's higher words is non-zero, it will be bigger.
        LispInt i;
        for (i=nr2;i<nr1;i++)
            if (a1[i] != 0)
                return LispTrue;
        // Otherwise compare the shared highest word.
        return highSame;
    }
    else
    {
        // If any of a2's higher words is non-zero, it will be bigger.
        LispInt i;
        for (i=nr1;i<nr2;i++)
            if (a2[i] != 0)
                return LispFalse;
        // Otherwise compare the shared highest word.
        return highSame;
    }
    LISPASSERT(0); //This should never happen.
    return LispFalse;
}
LispBoolean BaseLessThan(ANumber& a1, ANumber& a2)
{
    return BaseGreaterThan(a2,a1);
}


void BaseShiftRight(ANumber& a, LispInt aNrBits)
{
    // Number of words a word jumps
    LispInt wordsShifted = aNrBits/WordBits;

    // Residue: bits shifted out.
    LispInt residue = aNrBits % WordBits;

    // Bit mask: bits that are going to be shifted out of each word.
    LispInt bitMask = (1L<<residue)-1;

    // Nr of bits to move to the other side.
    LispInt otherSideBits = WordBits-residue;
    
    LispInt i;

    LispInt nr = a.NrItems();

    for (i=0;i<nr-wordsShifted;i++)
    {
        PlatDoubleWord newCarry =
            (((PlatDoubleWord)a[i+wordsShifted]) & bitMask)<<otherSideBits;

        a[i] = (a[i+wordsShifted]>>residue);

        if (i > 0)
            a[i-1] |= newCarry;
    }

    int start=nr-wordsShifted;
    if (start<0)
        start=0;
    for (i=start;i<nr;i++)
    {
        a[i] = 0;
    }
}











void BaseShiftLeft(ANumber& a, LispInt aNrBits)
{
    // Number of words a word jumps
    LispInt wordsShifted = aNrBits/WordBits;

    // Residue: bits shifted out.
    LispInt residue = aNrBits % WordBits;

    // Nr of bits to move to the other side.
    LispInt otherSideBits = WordBits-residue;

    // Bit mask: bits that are going to be shifted out of each word.
    LispInt bitMask = ((1L<<residue)-1)<<otherSideBits;

    LispInt i;
    LispInt nr = a.NrItems();

    for (i=0;i<=wordsShifted;i++)
    {
        a.Append(0);
    }
    
    for (i=nr+wordsShifted;i>=wordsShifted;i--)
    {
        PlatDoubleWord newCarry =
            (((PlatDoubleWord)a[i-wordsShifted]) & bitMask)>>otherSideBits;
        a[i] = (a[i-wordsShifted]<<residue);

        if (i < nr+wordsShifted)
            a[i+1] |= newCarry;
    }
    for (i=wordsShifted-1;i>=0;i--)
    {
        a[i] = 0;
    }
}



/* Binary Greatest common divisor algorithm. */
void BaseGcd(ANumber& aResult, ANumber& a1, ANumber& a2)
{
    a1.iNegative = a2.iNegative = LispFalse;
    ANumber zero("0",Precision(aResult),10);
    ANumber u("0",Precision(aResult));
    ANumber v("0",Precision(aResult));
    u.CopyFrom(a1);
    v.CopyFrom(a2);

     LispInt k=0;
    
    while (IsEven(u) && IsEven(v))
    {
        BaseShiftRight(u,1);
        BaseShiftRight(v,1);
        k++;
    }

    ANumber t("0",10);

    if (IsOdd(u))
    {
        t.CopyFrom(v);
        Negate(t);
    }
    else
        t.CopyFrom(u);

    while (!IsZero(t))
    {
        while (IsEven(t))
        {
            BaseShiftRight(t,1);
        }

        if (GreaterThan(t,zero))
        {
            u.CopyFrom(t);
        }
        else
        {
            v.CopyFrom(t);
            Negate(v);
        }
        Subtract(t,u,v);
     }
    aResult.CopyFrom(u);
    BaseShiftLeft(aResult,k);
}







void BaseDivide(ANumber& aQuotient, ANumber& aRemainder, ANumber& a1, ANumber& a2)
{
    // Find the values n and m as described in Knuth II:
    LispInt n,m;
    n=a2.NrItems();
    LISPASSERT(n>0);
    LISPASSERT(a2[n-1] != 0);
    
    //a1.NrItems() = m+n => m = a1.NrItems()-n
    m = a1.NrItems()-n;
    LISPASSERT(m>=0);

    aQuotient.GrowTo(m+1);
    
    //D1:
    PlatDoubleWord d = WordBase/(a2[n-1]+1);
    BaseTimesInt(a1, d, WordBase);
    BaseTimesInt(a2, d, WordBase);
    a1.Append(0);
    a2.Append(0);
    
    //D2:
    LispInt j = m;

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
        ANumber sub(Precision(aQuotient));
        sub.CopyFrom(a2);
        BaseTimesInt(sub, q, WordBase);
        sub.Append(0);
        
        PlatSignedDoubleWord carry;
        LispInt digit;
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
                BaseTimesInt(sub, q, WordBase);
                sub.Append(0);
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
                a1[digit+j] = ((PlatWord)(word%WordBase));
            }
        }
        LISPASSERT(carry == 0);
            
        //D5:
        aQuotient[j] = q;
        //D7:
        j--;
        
    }

    //D8:
    a1.SetNrItems(n);
    PlatDoubleWord carry;
    BaseDivideInt(a1, d, WordBase,carry);
    aRemainder.CopyFrom(a1);
}


void IntegerDivide(ANumber& aQuotient, ANumber& aRemainder, ANumber& a1, ANumber& a2)
{
    LISPASSERT(!IsZero(a2));

    LispInt n=a2.NrItems();

//hier
//    printf("1: n=%d\n",n);

    while (a2[n-1] == 0) n--;
    a2.SetNrItems(n);

//    printf("1: n=%d\n",n);

    if (n==1)
    {
//        printf("DivideInt: %d\n",a2[0]);

        PlatDoubleWord carry;
        aQuotient.CopyFrom(a1);
        aQuotient.iExp = a1.iExp-a2.iExp;
        aQuotient.iTensExp = a1.iTensExp-a2.iTensExp;

//        printf("aQuotient.iExp = %d\n",aQuotient.iExp);

        BaseDivideInt(aQuotient,a2[0], WordBase, carry);
        aRemainder.SetNrItems(1);
        aRemainder[0] = carry;

        /*
         {
            int i;
            for (i=0;i<aQuotient.NrItems();i++)
            {
                printf("%d ",aQuotient[i]);
            }
            printf("\n");
            }
            */
        
//        printf("carry = %d\n",carry);
        
    }
    // if |a1| < |a2| then result is zero.
    else if (BaseLessThan(a1,a2))
    {
        aQuotient.iExp = 0;
        aQuotient.iTensExp = 0;
        aQuotient.SetNrItems(1);
        aQuotient[0] = 0;
        aRemainder.CopyFrom(a1);
    }
    else
    {
        aQuotient.iExp = a1.iExp-a2.iExp;
        aQuotient.iTensExp = a1.iTensExp-a2.iTensExp;
        // Divide the mantissas
        BaseDivide(aQuotient, aRemainder, a1, a2,WordBase);
    }

    // Correct for signs
    if ( (IsPositive(a1) && IsPositive(a2)) ||
         (IsNegative(a1) && IsNegative(a2)) )
    {
        aQuotient.iNegative = LispFalse;
        aRemainder.iNegative = LispFalse;
    }
    else
    {
        aQuotient.iNegative = LispTrue;
        aRemainder.iNegative = LispTrue;
    }
}


void Divide(ANumber& aQuotient, ANumber& aRemainder, ANumber& a1, ANumber& a2)
{

    // Now add some digits to the front, to end up with the correct
    // precision: the resulting precision will be a1.iExp-a2.iExp.
    // This value should at least be WordDigits, so grow a1
    // by WordDigits-(a1.iExp-a2.iExp) = WordDigits+a2.iExp-a1.iExp
    {
        LispInt toadd = WordDigits(aQuotient.iPrecision, 10)
            +a2.iExp-a1.iExp; 
        LispInt i;
        PlatWord zero=0;
        for (i=0;i<toadd;i++)
        {
            a1.Insert(0,zero);
            a1.iExp++;
        }
    }

    IntegerDivide(aQuotient,aRemainder,a1,a2);
}


void BaseSqrt(ANumber& aResult, ANumber& N)
{

    LispInt l2;
    ANumber u  (Precision(aResult));
    ANumber v  (Precision(aResult));
    ANumber u2 (Precision(aResult));
    ANumber v2 (Precision(aResult));
    ANumber uv2(Precision(aResult));
    ANumber n  (Precision(aResult));
    ANumber two("2");

    //sqrt(1) = 1, sqrt(0) = 0
    if( BaseGreaterThan(two, N))
    {
        aResult.CopyFrom(N);
        return;
    }

    // Find highest set bit, l2
    u.CopyFrom(N);
    l2 = 0;
    while( !IsZero(u) )
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
        BaseAdd(n,uv2,WordBase);
        BaseAdd(n,v2,WordBase);
        // if n (possible new best estimate for sqrt(N)^2 is smaller than
        // N, then the bit l2 is set in the result, and add v to u.
        if( !BaseGreaterThan(n , N) )
        {
            BaseAdd(u,v,WordBase); // u <- u+v
            u2.CopyFrom(n);  // u^2 <- u^2 + 2*u*v + v^2
        }
    }
    aResult.CopyFrom(u);
}

void Sqrt(ANumber& aResult, ANumber& N)
{
    LispInt digs = WordDigits(aResult.iPrecision, 10);
    PlatWord zero=0;
    while(N.iExp<2*digs)
    {
        N.Insert(0,zero);
        N.iExp++;
    }
    
    BaseSqrt(aResult, N);
    aResult.iExp=digs;

    //TODO!!!@@@###$$$ iTensExp???
}


/*** Significant : return whether this number is not zero, up to
 * the number of digits specified behind the dot (as per aPrecision).
 */
LispBoolean Significant(ANumber& a)
{
    // Calculate number of significant digits
    LispInt significantDigits = WordDigits(a.iPrecision, 10);

    // Calculate from where to check
    LispInt from = a.iExp-significantDigits;
    if (from<0)
        from = 0;

    // Check for non-zeroness from the significant digits
    LispInt i;
    LispInt nr = a.NrItems();
    for (i=from;i<nr;i++)
    {
        if (a[i] != 0)
            return LispTrue;
    }
    return LispFalse;
}


