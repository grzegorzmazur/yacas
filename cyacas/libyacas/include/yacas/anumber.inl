
#include "anumber.h"


/* BaseTimesInt : multiply a with one digit in the range 0..(aBase-1)
 */
template<class T>
inline void BaseTimesInt(T& a,PlatDoubleWord aNumber, PlatDoubleWord aBase)
{
  PlatDoubleWord carry=0;
  const int nr=a.size();

  typename T::value_type * aptr = &a[0];
  for (int i=0;i<nr;i++)
  {
    const PlatDoubleWord word = ((PlatDoubleWord)(*aptr))*aNumber+carry;
    *aptr++ = word % aBase;
    carry= word / aBase;
  }
  if (carry)
    a.push_back(carry);
}

template<class T>
inline void WordBaseTimesInt(T& a,PlatDoubleWord aNumber)
{
  PlatDoubleWord carry=0;
  const int nr=a.size();

  typename T::value_type * aptr = &a[0];
  for (int i=0;i<nr;i++)
  {
    const PlatDoubleWord word = ((PlatDoubleWord)(*aptr))*aNumber+carry;
    *aptr++ = word;
    carry= word >> WordBits;
  }
  if (carry)
    a.push_back(carry);
}



template<class T>
inline void BaseDivideInt(T& a,PlatDoubleWord aNumber, PlatDoubleWord aBase, PlatDoubleWord& aCarry)
{
    const int nr=a.size();

    PlatDoubleWord carry=0;

    typename T::value_type * aptr = &a[0];
    for (int i=nr-1;i>=0;i--) {
        const PlatDoubleWord word = (carry*aBase)+((PlatDoubleWord)(aptr[i]));
        aptr[i] = word / aNumber;
        carry= word % aNumber;
    }

    //carry now is the remainder
    aCarry = carry;
}


/* GrowDigits : add digits to a until it has aDigits digits
 */
template<class T>
inline void GrowDigits(T& a, std::size_t aDigits)
{
    if (aDigits <= a.size())
        return;

    a.resize(aDigits, 0);
}

/* BaseAdd : destructively add aSource to aTarget, in base aBase.
 */
template<class T>
inline void BaseAdd(T& aTarget, const T& aSource, PlatDoubleWord aBase)
{
    // Initialize result

    GrowDigits(aTarget,aSource.size());
    aTarget.push_back(0);

    int nr = std::min(aTarget.size(), aSource.size());

    PlatDoubleWord carry=0;

   const typename T::value_type * sourcePtr = &aSource[0];
   typename T::value_type * targetPtr = &aTarget[0];
   for (int digit=0;digit<nr;digit++)
    {
        PlatDoubleWord word;
        word = (PlatDoubleWord)targetPtr[digit] +
            (PlatDoubleWord)sourcePtr[digit] + carry;
         PlatDoubleWord newDigit = (word%aBase);
         PlatDoubleWord newCarry = (word/aBase);
         targetPtr[digit] = (typename T::value_type)newDigit;
         carry          = newCarry;
    }
    while (carry != 0)
    {
        PlatSignedDoubleWord ww = targetPtr[nr];
        ww+=carry;
        targetPtr[nr] = (typename T::value_type)(ww%aBase);  // PDG - cast to avoid compile-time warning
        carry = ww/aBase;
        nr++;
    }
}


template<class T>
inline void WordBaseAdd(T& aTarget, const T& aSource)
{
    // Initialize result

    GrowDigits(aTarget,aSource.size());
    aTarget.push_back(0);

    int nr = std::min(aTarget.size(), aSource.size());

    PlatDoubleWord carry=0;

   const typename T::value_type * sourcePtr = &aSource[0];
   typename T::value_type * targetPtr = &aTarget[0];
   for (int digit=0;digit<nr;digit++)
    {
        PlatDoubleWord word;
        word = (PlatDoubleWord)targetPtr[digit] +
            (PlatDoubleWord)sourcePtr[digit] + carry;
         PlatWord newDigit = (PlatWord)(word);
         PlatWord newCarry = (PlatWord)(word >> WordBits);
         targetPtr[digit] = (typename T::value_type)newDigit;
         carry          = newCarry;
    }
    while (carry != 0)
    {
        PlatSignedDoubleWord ww = targetPtr[nr];
        ww+=carry;
        targetPtr[nr] = (typename T::value_type)ww;  // PDG - cast to avoid compile-time warning
        carry = ww >> WordBits;
        nr++;
    }
}





template<class T>
inline void BaseSubtract(T& aResult, T& a2, int offset)
{
    if (a2.IsZero())
        return;

    // Initialize result
    int nr = a2.size();

    typename T::value_type * resultPtr = &aResult[0];
    typename T::value_type * a2ptr = &a2[0];

    while (a2ptr[nr-1] == 0)
        nr--;

    // Subtract on a per-digit basis
    PlatSignedDoubleWord carry=0;
    int digit;

    for (digit=0;digit<nr;digit++)
    {
        PlatSignedDoubleWord word;
        word = ((PlatSignedDoubleWord)resultPtr[digit+offset]) -
            ((PlatSignedDoubleWord)a2ptr[digit]) +
            (PlatSignedDoubleWord)carry;
        carry=0;
        while (word<0)
        {
            word+=WordBase;
            carry--;
        }
        resultPtr[digit+offset] = ((PlatWord)(word));
    }

    while (carry != 0)
    {
        assert(nr+offset<aResult.size());

        int newCarry = 0;
        PlatSignedDoubleWord ww = resultPtr[nr+offset]+carry;
        while (ww<0)
        {
            ww = ww + WordBase;
            newCarry = newCarry - 1;
        }
        resultPtr[nr+offset]=(typename T::value_type)ww;
        carry = newCarry;
        offset++;
    }
}

/* BaseIntNumber : convert a number into a different base,
 */
inline void BaseIntNumber(std::string& aTarget, PlatSignedDoubleWord aNumber, PlatWord aBase)
{
  // Assume aBase is an integer > 0.
  // Assume aNumber is an integer > 0.
  // Assume PlatDoubleWord is an integer type.
  // Will maximum digit (i.e., aBase-1) convert to T::value_type right?
    //LISPASSERT( (typename T::value_type)(aBase) == (aBase) );  // use aBase instead, to help CTCE
    aTarget.clear();
    while (aNumber != 0)
    {
        aTarget.push_back(aNumber%aBase);
        aNumber/=aBase;
    }
    if (aTarget.empty())
        aTarget.push_back(0);
}

// BaseAddMultiply : multiply x and y, and add result to aTarget
//

inline
void BaseAddMultiply(std::string& aTarget, const std::string& x, const std::string& y, PlatDoubleWord aBase) {
    const unsigned nrx = x.size();
    const unsigned nry = y.size();
    GrowDigits(aTarget, nrx + nry + 1);

    std::string::value_type *targetPtr = &aTarget[0];
    const std::string::value_type *xPtr = &x[0];
    const std::string::value_type *yPtr = &y[0];

    for (unsigned ix = 0; ix < nrx; ix++) {
        PlatDoubleWord carry = 0;
        for (unsigned iy = 0; iy < nry; iy++) {
            const PlatDoubleWord word =
                    static_cast<PlatDoubleWord> (targetPtr[ix + iy]) +
                    static_cast<PlatDoubleWord> (xPtr[ix]) *
                    static_cast<PlatDoubleWord> (yPtr[iy]) + carry;


            targetPtr[ix + iy] = word % aBase;
            carry = word / aBase;
        }
        targetPtr[ix + nry] += carry;
    }
}

template<class T>
inline void WordBaseAddMultiply(T& aTarget, const T& x, const T& y)
{
    const unsigned nrx=x.size();
    const unsigned nry=y.size();
    GrowDigits(aTarget,nrx+nry+1);

    typename T::value_type *targetPtr = &aTarget[0];
    const typename T::value_type *xPtr = &x[0];
    const typename T::value_type *yPtr = &y[0];
    for (unsigned ix=0;ix<nrx;ix++)
    {
        PlatDoubleWord carry = 0;
        for (unsigned iy=0;iy<nry;iy++)
        {
            PlatDoubleWord word =
                static_cast<PlatDoubleWord>(targetPtr[ix+iy])+
                static_cast<PlatDoubleWord>(xPtr[ix])*
                static_cast<PlatDoubleWord>(yPtr[iy])+carry;

            targetPtr[ix+iy] = word;
            carry            = word >> WordBits;
        }

        const PlatDoubleWord word =
            static_cast<PlatDoubleWord>(targetPtr[ix+nry])+carry;
        targetPtr[ix+nry] = word;

        assert((word >> WordBits) == 0);
    }
}




/* BaseMultiply : multiply x and y, and put result in aTarget
 */


template<class T>
inline void BaseMultiply(T& aTarget, const T& x, const T& y, PlatDoubleWord aBase)
{
    aTarget.resize(1);
    aTarget[0] = 0;
    BaseAddMultiply(aTarget, x, y, aBase);
}

template<class T>
inline void WordBaseMultiply(T& aTarget, const T& x, const T& y)
{
    aTarget.resize(1);
    aTarget[0] = 0;
    WordBaseAddMultiply(aTarget, x, y);
}

template<class T>
inline
bool IsZero(const T& a)
{
    const typename T::value_type *ptr = &a[0];
    const typename T::value_type *endptr = ptr + a.size();

    while (ptr != endptr)
        if (*ptr++ != 0)
            return false;

    return true;
}



template<class T>
inline void WordBaseDivide(T& aQuotient, T& aRemainder, T& a1, T& a2)
{
    // Find the values n and m as described in Knuth II:
    int n=a2.size();
    assert(n>0);
    assert(a2[n-1] != 0);

    //a1.size() = m+n => m = a1.size()-n
    int m = a1.size()-n;
    assert(m>=0);

    aQuotient.resize(m+1);

    //D1:
    //this calculates d = base/(a2[n-1]+1);
    PlatDoubleWord d = WordBase/(static_cast<PlatDoubleWord>(a2[n-1])+1);


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
        aQuotient[j] = (typename T::value_type)q;
        //D7:
        j--;

    }

    //D8:
    a1.resize(n);
    PlatDoubleWord carry;
    BaseDivideInt(a1, d, WordBase,carry);
    aRemainder.CopyFrom(a1);
}

inline
void ANumber::Expand()
{
    if (iExp+1>int(size()))
        insert(end(), iExp+1-int(size()), 0);
}
