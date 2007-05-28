


/* Arbitrary precision arithmetic classes. These are NOT designed
 * to be bleeding fast, just reaonably fast, but very clean code.
 *
 */

//#include <stdio.h>

#include "yacasprivate.h"
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


#ifdef HAVE_STDIO_H
#include <stdio.h> // Safe, only included if HAVE_STDIO_H is defined
#endif
void PrintNumber(char* prefix,ANumber& aNumber)
{
#ifdef HAVE_STDIO_H
  printf("%s\n",prefix);
  printf("%d words, %d after point (x10^%d), 10-prec. %d\n",
        aNumber.Size(),aNumber.iExp, aNumber.iTensExp,aNumber.iPrecision);
  int i;
  for (i=aNumber.Size()-1;i>=0;i--)
  {
    if (aNumber.iExp == i+1) printf(".\n");
    PlatWord w = (aNumber)[i];
    PlatWord bit = (WordBase)>>1;
    int k=0;
    while (bit)
    {
      if ((k&3)==0) printf(" ");
      k++;
      if (w&bit) printf("1");
      else printf("0");
      bit>>=1;
    }
    printf("\n");
  }
#endif
}

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

ANumber::ANumber(LispInt aPrecision) : ASuper(),iExp(0),iNegative(LispFalse),iPrecision(aPrecision),iTensExp(0)
{
  LISPASSERT(sizeof(PlatDoubleWord) >= 2*sizeof(PlatWord));
  Append(0);
}

/* Allow use of external arrays */
ANumber::ANumber(PlatWord *aArray, LispInt aSize, LispInt aPrecision): ASuper(),iExp(0),iNegative(LispFalse),iPrecision(aPrecision),iTensExp(0)
{
  LISPASSERT(sizeof(PlatDoubleWord) >= 2*sizeof(PlatWord));
  SetExternalArray(aArray, aSize);
}

/* ANumber: Constructor for an arbitrary precision number. */
ANumber::ANumber(const LispChar * aString,LispInt aPrecision,LispInt aBase): ASuper(),iExp(0),iNegative(LispFalse),iPrecision(aPrecision),iTensExp(0)
{
    SetPrecision(aPrecision);
    SetTo(aString,aBase);
}

void IntToBaseString(LispString& aString,PlatDoubleWord aInt, LispInt aBase)
{
    // Build the string
    aString.ResizeTo(0);
    LispInt i=0;
    while (aInt!=0)
    {
    aString.Append((LispChar)(aInt%aBase));
    aInt/=aBase;
        i++;
    }
}


void IntToAscii(LispString& aString,PlatDoubleWord aInt, LispInt aBase)
{

/*TODO handle negative integers also?
    LispBoolean negative = false;
 
    // Sign will be handled separately
    if (negative)
        aInt = -aInt;
*/

    IntToBaseString(aString,aInt,aBase);
    LispInt i;
    LispInt nr = aString.Size();
    for (i=0;i<(nr>>1);i++)
    {
        LispChar c = aString[i];
        aString[i] = Digit(aString[nr-i-1]);
        aString[nr-i-1] = Digit(c);
    }
    if (nr&1)
    {
        aString[(nr>>1)] = Digit(aString[(nr>>1)]);
    }
/*TODO handle negative integers also?
    if (negative)
    {
        LispChar c = '-';
        aString.Insert(0,c);
    }
*/
    aString.Append('\0');
}

LispInt WordDigits(LispInt aPrecision, LispInt aBase)
{
    if (aPrecision == 0) return 0;
    LispInt bitsPerBase=0;

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
//    return (aPrecision*bitsPerBase+2*WordBits)/WordBits;
    return (aPrecision*bitsPerBase+WordBits)/WordBits;
}


void ANumber::SetTo(const LispChar * aString,LispInt aBase)
{
    ResizeTo(0);
 
    LISPASSERT(sizeof(PlatDoubleWord) >= 2*sizeof(PlatWord));
    LISPASSERT(aBase<=36);
    iNegative=LispFalse;
    iExp = 0;
    iTensExp = 0;
 
    const LispChar * endptr = aString;

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
        if (aBase < 14 && aString[endNumberIndex]=='e' || aString[endNumberIndex]=='E')
            endFloatIndex = endNumberIndex;
        endNumberIndex++;
    }
    if (endFloatIndex == 0)
        endFloatIndex = endNumberIndex;

    if (endIntIndex == -1)
        endIntIndex = endFloatIndex;
    if (endFloatIndex == endIntIndex+1)
        endFloatIndex = endIntIndex;
//printf("%s\n",aString);
//printf("\tendint = %d, endfloat = %d, endnumber = %d\n",endIntIndex,endFloatIndex,endNumberIndex);

//printf("%d digits\n",endFloatIndex-endIntIndex-1);

    if (endFloatIndex-endIntIndex-1 > iPrecision)
    {
      iPrecision = endFloatIndex-endIntIndex-1;
    }
 
    // Go to least significant digit first
    const LispChar * ptr = aString + endIntIndex-1;
 
    // Now parse the integer part of the number.
    ANumber factor2(iPrecision);
    factor2[0] = 1;
    while (ptr >= endptr)
    {
        ANumber term(iPrecision);
        term.CopyFrom(factor2);
        WordBaseTimesInt(term, DigitIndex(*ptr));
        WordBaseAdd(*this,term);
        /*
        ANumber current(iPrecision);
        current.CopyFrom(*this);
        BaseAddFull(*this,current,term);
        */
        WordBaseTimesInt(factor2, aBase);
        ptr--;
    }

    //Parse the fraction
    if (endFloatIndex > endIntIndex)
    {
        LispString fraction((LispChar *)&aString[endIntIndex+1]);
        LispInt i;

        // Map to a char base number
        LispInt nr;// = fraction.Size()-1; //Excluding the zero terminator
        nr = endFloatIndex - endIntIndex-1;
        LispString::ElementType * fractionPtr = &fraction[0];

        for (i=0;i<(nr>>1);i++)
        {
            LispChar c = fractionPtr[i];
            fractionPtr[i] = DigitIndex(fractionPtr[nr-i-1]);
            fractionPtr[nr-i-1] = DigitIndex(c);
        }
        if (nr&1)
            fractionPtr[nr>>1] = DigitIndex(fractionPtr[nr>>1]);
 
        LispString base;
        IntToBaseString(base,WordBase,aBase);

        LispInt nrDigits;
        nrDigits = WordDigits(iPrecision,aBase)/*+1*/;

        for (i=0;i<nrDigits;i++)
        {
            PlatWord word=0;
            LispString copied;
            LispInt j;

            //TODO!!! This is probably not the good way to copy!
            {
                LispInt nrc=fraction.Size();
                copied.ResizeTo(nrc);
              //copied.ResizeTo(nrc);  // not needed -- ResizeTo does this
                PlatMemCopy(&copied[0],  &fraction[0], nrc*sizeof(LispString::ElementType));
            }
            BaseMultiply(fraction, copied, base, aBase);

            {
                LispInt nrc=fraction.Size();
                LispString::ElementType * fractionPtr = &fraction[0];
                PlatDoubleWord factor=1;
                for (j=nr;j<nrc;j++)
                {
                    word = word + (PlatWord)(fractionPtr[j]*factor);
                    factor = factor*aBase;
                }
            }
            fraction.ResizeTo(nr);
            Insert(0,word);
            iExp++;
        }
//        Delete(0);
//        iExp--;
    }

    // Parse the E<num> part at the end
    if (endNumberIndex > endFloatIndex+1)
    {
      if (aString[endFloatIndex] == '.') endFloatIndex++;

      if (aString[endFloatIndex+1] == '+') endFloatIndex++;

        iTensExp = PlatAsciiToInt((LispChar *)&aString[endFloatIndex+1]);
//printf("%s mapped to %d\n",&aString[endFloatIndex+1],iTensExp);
    }

    DropTrailZeroes();
//PrintNumber("      ",*this);
}

void ANumber::CopyFrom(const ANumber& aOther)
{
    iExp       = aOther.iExp;
    iTensExp   = aOther.iTensExp;
    iNegative  = aOther.iNegative;
    iPrecision = aOther.iPrecision;
    ResizeTo(aOther.Size());
    //ResizeTo(aOther.Size());  // not needed -- ResizeTo does this

    //TODO there HAS to be a faster way to copy...
    LispInt nr = aOther.Size();
    if (nr)
    {
//this is actually slower!      PlatMemCopy(&((*this)[0]),&( aOther[0]),nr*sizeof(ANumber::ElementType));

      ANumber::ElementType * sptr = &( aOther[0]);
      ANumber::ElementType * tptr = &((*this)[0]);
      while (nr--)
      {
          *tptr++ = *sptr++;
      }
    }
    else
    {
      ResizeTo(1);
      //ResizeTo(1);
      (*this)[0] = 0;
    }
}



LispBoolean ANumber::ExactlyEqual(const ANumber& aOther)
{
  if (iExp       != aOther.iExp) return LispFalse;
  if (iTensExp   != aOther.iTensExp) return LispFalse;
  if (iNegative  != aOther.iNegative) return LispFalse;
//  if (iPrecision != aOther.iPrecision) return LispFalse;
  if (Size()     != aOther.Size()) return LispFalse;

  //TODO there HAS to be a faster way to copy...
  LispInt nr = Size();
  if (nr)
  {
    ANumber::ElementType * sptr = &( aOther[0]);
    ANumber::ElementType * tptr = &((*this)[0]);
    while (nr--)
    {
      if (*tptr++ != *sptr++) return LispFalse;
    }
  }
  return LispTrue;
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
    a1.DropTrailZeroes();
    a2.DropTrailZeroes();

#ifdef CORRECT_DIVISION
    if (a1.iExp || a1.iTensExp) NormalizeFloat(a1,WordDigits(a1.iPrecision, 10));
    if (a2.iExp || a2.iTensExp) NormalizeFloat(a1,WordDigits(a2.iPrecision, 10));
#endif // CORRECT_DIVISION


    // this does some additional removing, as for the multiplication we don't need
    // any trailing zeroes at all, regardless of the value of iExp
    LispInt end;

    end=a1.Size();
    while (end>1 && a1[end-1]==0)
    {
        end--;
    }
    a1.ResizeTo(end);

    end=a2.Size();
    while (end>1 && a2[end-1]==0)
    {
        end--;
    }
    a2.ResizeTo(end);

    // Multiply
    BaseMultiplyFull(aResult,a1,a2);

//PrintNumber("Mult",aResult);


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

    while(a1.Size()<a1.iExp+1)
        a1.Append(0);
    while(a2.Size()<a2.iExp+1)
        a2.Append(0);
    while(aResult.Size()<aResult.iExp+1)
        aResult.Append(0);


    aResult.DropTrailZeroes();
#ifdef CORRECT_DIVISION
    if (aResult.iExp || aResult.iTensExp) NormalizeFloat(aResult,WordDigits(aResult.iPrecision, 10));
#endif // CORRECT_DIVISION
}

static void BalanceFractions(ANumber& a1, ANumber& a2)
{
    PlatWord word=0;

//ANumber a3("10e2");
//PrintNumber("a3 enter ",a3);
//a3.iTensExp = 0;
//PrintNumber("a3 enter ",a3);

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


    //TODO this is not the fastest way to multiply by 10^exp
    if (a1.iTensExp < a2.iTensExp)
    {
      LispInt diff = a2.iTensExp - a1.iTensExp;
      a2.iTensExp = a1.iTensExp;
      while (diff > 0)
      {
        WordBaseTimesInt(a2,10);
        diff--;
      }
    }
    else if (a2.iTensExp < a1.iTensExp)
    {
      LispInt diff = a1.iTensExp - a2.iTensExp;
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

#ifdef CORRECT_DIVISION
    // if the numbers are float, make sure they are normalized
    if (a1.iExp || a1.iTensExp) NormalizeFloat(a1,WordDigits(a1.iPrecision, 10));
    if (a2.iExp || a2.iTensExp) NormalizeFloat(a1,WordDigits(a2.iPrecision, 10));
#endif // CORRECT_DIVISION

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
            ANumber zero(/*???"0",*/aResult.iPrecision);
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
            ANumber zero(/*???"0",*/aResult.iPrecision);
            aResult.CopyFrom(zero);
        }
    }
    aResult.DropTrailZeroes();

#ifdef CORRECT_DIVISION
    if (aResult.iExp || aResult.iTensExp)
    {
      if (aResult.iPrecision < a2.iPrecision)
        aResult.iPrecision = a2.iPrecision;
      if (aResult.iPrecision < a1.iPrecision)
        aResult.iPrecision = a1.iPrecision;

      NormalizeFloat(aResult,WordDigits(aResult.iPrecision, 10));
    }
#endif // CORRECT_DIVISION

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
            ANumber zero(/*???"0",*/aResult.iPrecision);
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
            ANumber zero(/*???"0",*/aResult.iPrecision);
            aResult.CopyFrom(zero);
        }
    }
    aResult.DropTrailZeroes();
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

#ifdef CORRECT_DIVISION
    // if the numbers are float, make sure they are normalized
    if (a1.iExp || a1.iTensExp) NormalizeFloat(a1,WordDigits(a1.iPrecision, 10));
    if (a2.iExp || a2.iTensExp) NormalizeFloat(a1,WordDigits(a2.iPrecision, 10));
#endif // CORRECT_DIVISION

    BalanceFractions(a1, a2);
    if (IsNegative(a1) && IsPositive(a2))
        return LispTrue;
    if (IsPositive(a1) && IsNegative(a2))
        return LispFalse;
    if (IsPositive(a1) && IsPositive(a2))
        return BaseLessThan(a1,a2);
    return BaseGreaterThan(a1,a2);
}


void  ANumberToString(LispString& aResult, ANumber& aNumber, LispInt aBase, LispBoolean aForceFloat)
{
    LispInt nr = aNumber.Size();
    while (nr>1 && aNumber[nr-1] == 0) nr--;
    aNumber.ResizeTo(nr);

    LispInt tensExp = aNumber.iTensExp;

    //Formatting small numbers can be done faster.
    if (aNumber.iExp == 0 && nr == 1)
    {
        BaseIntNumber(aResult, aNumber[0], aBase);
        nr=aResult.Size();
        // swap order of the digits, and map to ascii
        {
            LispInt i;
            LispString::ElementType * rptr = &aResult[0];
            for (i=0;i<(nr>>1);i++)
            {
                LispString::ElementType c=rptr[i];
                rptr[i] = Digit(rptr[nr-i-1]);
                rptr[nr-i-1] = Digit(c);
            }
            if (nr&1)
                rptr[nr>>1] = Digit(rptr[nr>>1]);
        }

        if (aForceFloat)
        {
          if (!(aResult.Size()==1 && aResult[0] == '0'))
            aResult.Append('.');
        }
        if (aNumber.iNegative)
        {
            if (aResult.Size()>1 || aResult[0] != '0')
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
        aResult.ResizeTo(0);
        aResult.Append(0);

        // Create the number
        LispString factor2;
        BaseIntNumber(factor2, 1, aBase);

        LispString factor3;
        BaseIntNumber(factor3, WordBase, aBase);

        LISPASSERT(number.iExp >= 0);

        LispInt i;
        for (i=number.iExp;i<number.Size();i++)
        {
            //aResult = aResult + number[i] * factor2
            LispString term;
            BaseIntNumber(term, number[i], aBase);
            BaseAddMultiply(aResult, term, factor2, aBase);

            //TODO this one doesn't have to be done the last iteration
            //factor2 = factor2*factor3

            {
                LispInt nr = factor2.Size();
                term.ResizeTo(nr);
                LispInt j;
                LispString::ElementType * fptr = &factor2[0];
                LispString::ElementType * tptr = &term[0];
                for (j=0;j<nr;j++)
                {
                    *tptr++ = *fptr++;
                }
            }
            BaseMultiply(factor2, term, factor3, aBase);
        }

        //Remove trailing zeroes (most significant side)
        nr = aResult.Size();
        while (nr>1 && aResult[nr-1] == 0) nr--;
        aResult.ResizeTo(nr);

        // swap order of the digits, and map to ascii
        {
            LispString::ElementType * rptr = &aResult[0];
            for (i=0;i<(nr>>1);i++)
            {
                LispString::ElementType c=rptr[i];
                rptr[i] = rptr[nr-i-1];
                rptr[nr-i-1] = c;
            }
        }

        // Get the fraction
        while(number.Size()<number.iExp)
            number.Append(0);
        number.ResizeTo(number.iExp);
        if (aForceFloat || (number.iExp > 0 && !IsZero(number)))
        {
            LispInt digitPos = aResult.Size();

            LispInt i;
            // Build the fraction
            for (i=0;i<number.iPrecision+1;i++)
            {
                WordBaseTimesInt(number, aBase);
                if (number.Size() > number.iExp)
                {
                    aResult.Append((LispChar)(number[number.iExp]));
                    number.ResizeTo(number.iExp);
                }
                else
                {
                    aResult.Append(0);
                }
            }

            // Round off
            if (aResult[aResult.Size()-1] >= (aBase>>1))
            {
                //TODO code bloat!
                LispInt carry=1;
                for (i=aResult.Size()-1;i>=0;i--)
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
            aResult.ResizeTo(aResult.Size()-1);
            // Insert dot
            LispChar c='.';
            aResult.Insert(digitPos,c);

            //Remove trailing zeros
            LispInt nr = aResult.Size();
            while (nr>1 && aResult[nr-1] == 0)
            {
                nr--;
            }
            if (aResult[nr-1] == '.' && nr == 2 && aResult[0] == 0)
                nr--;
            aResult.ResizeTo(nr);
        }

        // Map to ascii
        {
            LispString::ElementType * rptr = &aResult[0];
            LispInt nr = aResult.Size();
            for (i=0;i<nr;i++)
            {
                *rptr = Digit(*rptr);
                rptr++;
            }
        }

        // If signed, insert a minus sign
        if (number.iNegative)
            if (aResult.Size()>1 || aResult[0] != '0')
            {
                LispChar c='-';
                aResult.Insert(0,c);
            }
    }

    //Handle tens exp
TENSEXP:
    if (tensExp != 0 &&
        !(aResult[0] == '0' && aResult.Size() == 1))
    {
        aResult.Append('e');
        LispString tens;
        //hier
        LispInt tenex = tensExp;
        if (tenex<0)
        {
          aResult.Append('-');
          tenex = -tenex;
        }
        IntToAscii(tens,tenex, 10);
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
    WordBaseAdd(aResult,a2);
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
    LispInt nr = a2.Size();
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
        LISPASSERT(nr+offset<aResult.Size());

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
    WordBaseMultiply(aResult,a1,a2);
}


LispBoolean BaseGreaterThan(ANumber& a1, ANumber& a2)
{
    LispInt nr1 = a1.Size();
    LispInt nr2 = a2.Size();

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
    PlatDoubleWord bitMask = (PlatDoubleWord)((((PlatDoubleWord)1)<<residue)-1);

    // Nr of bits to move to the other side.
    LispInt otherSideBits = WordBits-residue;
 
    LispInt i;

    LispInt nr = a.Size();

    ANumber::ElementType * ptr = &a[0];
    ANumber::ElementType * ptrshifted = &a[wordsShifted];
    ANumber::ElementType * endp = ptr +nr - wordsShifted;
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
    LispInt nr = a.Size();

    for (i=0;i<=wordsShifted;i++)
    {
        a.Append(0);
    }

    ANumber::ElementType * ptr = &a[0];
 
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
    ANumber zero(/*???"0",*/Precision(aResult)/*???,10*/);
    ANumber u(/*???"0",*/Precision(aResult));
    ANumber v(/*???"0",*/Precision(aResult));
    u.CopyFrom(a1);
    v.CopyFrom(a2);
    u.iNegative = v.iNegative = LispFalse;

     LispInt k=0;


    {
      LispInt i=0;
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

    if (IsOdd(u))
    {
        t.CopyFrom(v);
        Negate(t);
    }
    else
        t.CopyFrom(u);

    while (!IsZero(t))
    {

        {
          LispInt k=0;
          LispInt i=0;
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
            Negate(v);
        }
        Subtract(t,u,v);
     }
    aResult.CopyFrom(u);
    aResult.iNegative=LispFalse;
    BaseShiftLeft(aResult,k);
}







void BaseDivide(ANumber& aQuotient, ANumber& aRemainder, ANumber& a1, ANumber& a2)
{
    // Find the values n and m as described in Knuth II:
    LispInt n,m;
    n=a2.Size();
    LISPASSERT(n>0);
    LISPASSERT(a2[n-1] != 0);
 
    //a1.Size() = m+n => m = a1.Size()-n
    m = a1.Size()-n;
    LISPASSERT(m>=0);

    aQuotient.ResizeTo(m+1);
 
    //D1:
    PlatDoubleWord d = WordBase/(a2[n-1]+1);
    WordBaseTimesInt(a1, d);
    WordBaseTimesInt(a2, d);
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
        WordBaseTimesInt(sub, q);
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
                WordBaseTimesInt(sub, q);
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
                a1[digit+j] = ((PlatWord)(word));
            }
        }
        LISPASSERT(carry == 0);
 
        //D5:
        aQuotient[j] = (PlatWord)q;
        //D7:
        j--;
 
    }

    //D8:
    a1.ResizeTo(n);
    PlatDoubleWord carry;
    BaseDivideInt(a1, d, WordBase,carry);
    aRemainder.CopyFrom(a1);
}


void IntegerDivide(ANumber& aQuotient, ANumber& aRemainder, ANumber& a1, ANumber& a2)
{
    LISPASSERT(!IsZero(a2));

    LispInt n=a2.Size();

//hier
//    printf("1: n=%d\n",n);

    while (a2[n-1] == 0) n--;
    a2.ResizeTo(n);

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
        aRemainder.ResizeTo(1);
        aRemainder[0] = (PlatWord)carry;

        /*
         {
            int i;
            for (i=0;i<aQuotient.Size();i++)
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
        aQuotient.ResizeTo(1);
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

void NormalizeFloat(ANumber& a2, LispInt digitsNeeded)
{
  if (a2.iExp - digitsNeeded > 0)
  {
    a2.Delete(0,a2.iExp-digitsNeeded);
    a2.iExp -= (a2.iExp-digitsNeeded);
  }

  LispInt min = 1+digitsNeeded;
  if (a2.iExp+1>min)
    min = a2.iExp+1;
  while (a2.Size()>min ||
          (a2.Size()==min && a2[a2.Size()-1]>10))
  {
    PlatDoubleWord carry = 0;
    BaseDivideInt(a2, 10, WordBase,carry);
    if (a2[a2.Size()-1] == 0)
      a2.ResizeTo(a2.Size()-1);
    a2.iTensExp++;
  }
}


void Divide(ANumber& aQuotient, ANumber& aRemainder, ANumber& a1, ANumber& a2)
{

    // Now add some digits to the front, to end up with the correct
    // precision: the resulting precision will be a1.iExp-a2.iExp.
    // This value should at least be WordDigits, so grow a1
    // by WordDigits-(a1.iExp-a2.iExp) = WordDigits+a2.iExp-a1.iExp
    LispInt digitsNeeded = WordDigits(aQuotient.iPrecision, 10);
    {
#ifdef CORRECT_DIVISION

        NormalizeFloat(a2,digitsNeeded);

        LispInt toadd = a2.iExp-a1.iExp;
        {
          LispInt i;
          PlatWord zero=0;
          for (i=0;i<toadd;i++)
          {
              a1.Insert(0,zero);
              a1.iExp++;
          }
        }
 
        if (!IsZero(a1))
        {
          while (a1.Size()<a2.Size()+digitsNeeded || a1[a1.Size()-1]<a2[a2.Size()-1])
          {
            WordBaseTimesInt(a1, 10);
            a1.iTensExp--;
          }
        }


#else // CORRECT_DIVISION

        LispInt toadd = digitsNeeded+a2.iExp-a1.iExp;
        LispInt i;
        PlatWord zero=0;
        for (i=0;i<toadd;i++)
        {
            a1.Insert(0,zero);
            a1.iExp++;
        }
#endif // CORRECT_DIVISION
    }

    IntegerDivide(aQuotient,aRemainder,a1,a2);

#ifdef CORRECT_DIVISION

        NormalizeFloat(aQuotient,digitsNeeded);

#endif // CORRECT_DIVISION

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
    LispInt digs = WordDigits(N.iPrecision, 10);
    PlatWord zero=0;
    if ((N.iTensExp&1) != 0)
    {
      WordBaseTimesInt(N,10);
      N.iTensExp--;
    }
    while(N.iExp<2*digs || (N.iExp&1))
    {
        N.Insert(0,zero);
        N.iExp++;
    }
/*hier
    while (N.iTensExp<0)
    {
      PlatDoubleWord carry = 0;
      BaseDivideInt(N,10,WordBase,carry);
      N.iTensExp++;
    }
*/
    LispInt resultDigits = N.iExp/2;
    LispInt resultTensExp = N.iTensExp/2;
 
    BaseSqrt(aResult, N);
    aResult.iExp=resultDigits;
    aResult.iTensExp = resultTensExp;
}


/*** Significant : return whether this number is not zero, up to
 * the number of digits specified behind the dot (as per aPrecision).
 */
LispBoolean Significant(ANumber& a)
{

/*This is what I was working on */
#ifdef CORRECT_DIVISION
    LispInt significantDigits = WordDigits(a.iPrecision, 10);
    NormalizeFloat(a,significantDigits);
    //hier
    LispInt nrExt = (a.Size()-a.iExp)*((WordBits)/3);
    if ((-a.iTensExp) > a.iPrecision+2+nrExt)
    {
      return LispFalse;
    }
    return LispTrue;
#else
/* */
    // Calculate number of significant digits
    LispInt significantDigits = WordDigits(a.iPrecision, 10);

    // Calculate from where to check
    LispInt from = a.iExp-significantDigits;
    if (from<0)
        from = 0;

    // Check for non-zeroness from the significant digits
    LispInt i;
    LispInt nr = a.Size();
    for (i=from;i<nr;i++)
    {
        if (a[i] != 0)
            return LispTrue;
    }
    return LispFalse;
#endif // CORRECT_DIVISION
}


void ANumber::RoundBits(void)
{
  PlatWord* ptr = elements();
  if (*ptr < (WordBase/2))
  {
    *ptr = 0;
  }
  else
  {
    *ptr = 0;
    PlatDoubleWord carry = 1;
    for (LispInt i = 1, nr = Size(); i < nr; i++)
    {
        PlatDoubleWord dword = ptr[i]+carry;
        ptr[i] = (PlatWord)dword;
        carry = dword >> WordBits;
    }
    if (carry)
    {
    Append((ANumber::ElementType)(carry));  // PDG - cast to avoid compile-time warning
    }
  }
}

void ANumber::ChangePrecision(LispInt aPrecision)
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
  while (iExp+1>Size()) Append(0);

  LispInt oldExp = iExp;
 
  iPrecision = aPrecision;
  LispInt newExp = WordDigits(iPrecision,10);
  if (newExp < oldExp)
  {
    iExp = newExp;
    Delete(0,oldExp-iExp);
  }
  else if (newExp > oldExp)
  {
    iExp = newExp;
    PlatWord zero = 0;
    Insert(0,zero,newExp-oldExp);
  }
}



void ANumber::DropTrailZeroes()
{
  while (iExp+1>Size()) Append(0);
 
  {
    LispInt nr=Size();
    while (nr>iExp+1 && (*this)[nr-1] == 0) nr--;
    ResizeTo(nr);
  }
  {
    LispInt low=0;
    while (low<iExp && (*this)[low] == 0) low++;
    if (low)
    {
      Delete(0,low);
      iExp-=low;
    }
  }
}

