



#define WordBaseTimesInt(a,n) BaseTimesInt(a,n)
#define WordBaseAdd(a,b)      BaseAdd(a,b)

/* BaseTimesInt : multiply a with one digit in the range 0..(aBase-1)
 */
template<class T>
inline void BaseTimesInt(T& a,PlatDoubleWord aNumber, PlatDoubleWord aBase)
{
  PlatDoubleWord carry=0;
  LispInt i;
  LispInt nr=a.Size();

  typename T::ElementType * aptr = &a[0];
  for (i=0;i<nr;i++)
  {
    PlatDoubleWord word = ((PlatDoubleWord)(*aptr))*aNumber+carry;
    PlatWord digit = (PlatWord)(word % aBase);
    PlatWord newCarry= (PlatWord)(word / aBase);
    *aptr = digit;
    aptr++;
    carry= newCarry;
  }
  if (carry)
  {
    a.Append((typename T::ElementType)carry);
    carry = 0;
  }
  LISPASSERT(carry == 0);
}

template<class T>
inline void BaseTimesInt(T& a,PlatDoubleWord aNumber)
{
  PlatDoubleWord carry=0;
  LispInt i;
  LispInt nr=a.Size();

  typename T::ElementType * aptr = &a[0];
  for (i=0;i<nr;i++)
  {
    PlatDoubleWord word = ((PlatDoubleWord)(*aptr))*aNumber+carry;
    PlatWord digit = (PlatWord)(word);
    PlatWord newCarry= (PlatWord)(word >> WordBits);
    *aptr = digit;
    aptr++;
    carry= newCarry;
  }
  if (carry)
  {
    a.Append((typename T::ElementType)carry);
    carry = 0;
  }
  LISPASSERT(carry == 0);
}



template<class T>
inline void BaseDivideInt(T& a,PlatDoubleWord aNumber, PlatDoubleWord aBase, PlatDoubleWord& aCarry)
{
//    if (a[a.Size()-1] != 0)

    PlatDoubleWord carry=0;
    LispInt i;
    LispInt nr=a.Size();

    typename T::ElementType * aptr = &a[0];
    for (i=nr-1;i>=0;i--)
    {
        PlatDoubleWord word = ((carry*aBase)+((PlatDoubleWord)(aptr[i])));
        PlatWord digit = (PlatWord)(word / aNumber);
        PlatWord newCarry= (PlatWord)(word % aNumber);
        aptr[i] = digit;
        carry= newCarry;
    }
    //carry now is the remainder
    aCarry = carry;
}


/* GrowDigits : add digits to a until it has aDigits digits
 */
template<class T>
inline void GrowDigits(T& a,LispInt aDigits)
{
    LispInt i;

    if (aDigits <= a.Size())
        return;
 
    /*
     LispInt nrToAdd = aDigits-a.Size();

    for (i=0;i<nrToAdd;i++)
        a.Append(0);
*/
    LispInt origSize = a.Size();
    a.ResizeTo(aDigits);
    //a.ResizeTo(aDigits);
    if (aDigits<=origSize)
        return;
    typename T::ElementType* ptr = &a[origSize];
    for (i=origSize;i<aDigits;i++)
        *ptr++ = 0;
}

/* BaseAdd : destructively add aSource to aTarget, in base aBase.
 */
template<class T>
inline void BaseAdd(T& aTarget, const T& aSource, PlatDoubleWord aBase)
{
    // Initialize result

    GrowDigits(aTarget,aSource.Size());
    aTarget.Append(0);
 
    LispInt nr1 = aTarget.Size();
    LispInt nr2 = aSource.Size();
    LispInt nr;

    // nr represents min(nr1,nr2), the number of digits to add
    if (nr1>nr2)
        nr=nr2;
    else
        nr=nr1;
 
    PlatDoubleWord carry=0;
    LispInt digit;

   typename T::ElementType * sourcePtr = &aSource[0];
   typename T::ElementType * targetPtr = &aTarget[0];
   for (digit=0;digit<nr;digit++)
    {
        PlatDoubleWord word;
        word = (PlatDoubleWord)targetPtr[digit] +
            (PlatDoubleWord)sourcePtr[digit] + carry;
         PlatDoubleWord newDigit = (word%aBase);
         PlatDoubleWord newCarry = (word/aBase);
         targetPtr[digit] = (typename T::ElementType)newDigit;
         carry          = newCarry;
    }
    while (carry != 0)
    {
        PlatSignedDoubleWord ww = targetPtr[nr];
        ww+=carry;
        targetPtr[nr] = (typename T::ElementType)(ww%aBase);  // PDG - cast to avoid compile-time warning
        carry = ww/aBase;
        nr++;
    }
}


template<class T>
inline void BaseAdd(T& aTarget, const T& aSource)
{
    // Initialize result

    GrowDigits(aTarget,aSource.Size());
    aTarget.Append(0);
 
    LispInt nr1 = aTarget.Size();
    LispInt nr2 = aSource.Size();
    LispInt nr;

    // nr represents min(nr1,nr2), the number of digits to add
    if (nr1>nr2)
        nr=nr2;
    else
        nr=nr1;
 
    PlatDoubleWord carry=0;
    LispInt digit;

   typename T::ElementType * sourcePtr = &aSource[0];
   typename T::ElementType * targetPtr = &aTarget[0];
   for (digit=0;digit<nr;digit++)
    {
        PlatDoubleWord word;
        word = (PlatDoubleWord)targetPtr[digit] +
            (PlatDoubleWord)sourcePtr[digit] + carry;
         PlatWord newDigit = (PlatWord)(word);
         PlatWord newCarry = (PlatWord)(word >> WordBits);
         targetPtr[digit] = (typename T::ElementType)newDigit;
         carry          = newCarry;
    }
    while (carry != 0)
    {
        PlatSignedDoubleWord ww = targetPtr[nr];
        ww+=carry;
        targetPtr[nr] = (typename T::ElementType)ww;  // PDG - cast to avoid compile-time warning
        carry = ww >> WordBits;
        nr++;
    }
}





template<class T>
inline void BaseSubtract(T& aResult, T& a2, LispInt offset)
{
    if (IsZero(a2))
        return;
    LISPASSERT(!IsZero(a2));
    // Initialize result
    LispInt nr = a2.Size();

    typename T::ElementType * resultPtr = &aResult[0];
    typename T::ElementType * a2ptr = &a2[0];

    while (a2ptr[nr-1] == 0)
        nr--;

    // Subtract on a per-digit basis
    PlatSignedDoubleWord carry=0;
    LispInt digit;

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
        LISPASSERT(nr+offset<aResult.Size());

        LispInt newCarry = 0;
        PlatSignedDoubleWord ww = resultPtr[nr+offset]+carry;
        while (ww<0)
        {
            ww = ww + WordBase;
            newCarry = newCarry - 1;
        }
        resultPtr[nr+offset]=(typename T::ElementType)ww;
        carry = newCarry;
        offset++;
    }
}

/* BaseIntNumber : convert a number into a different base,
 * using growing arrays.
 */
template<class T>
inline void BaseIntNumber(T& aTarget, PlatSignedDoubleWord aNumber, PlatWord aBase)
{
  // Assume aBase is an integer > 0.
  // Assume aNumber is an integer > 0.
  // Assume PlatDoubleWord is an integer type.
  // Will maximum digit (i.e., aBase-1) convert to T::ElementType right?
    //LISPASSERT( (typename T::ElementType)(aBase) == (aBase) );  // use aBase instead, to help CTCE
    aTarget.ResizeTo(0);
    while (aNumber != 0)
    {
        PlatDoubleWord digit = aNumber%aBase;
    aTarget.Append((typename T::ElementType)(digit));  // PDG - cast to avoid compile-time warning
        aNumber/=aBase;
    }
    if (aTarget.Size() == 0)
        aTarget.Append(0);
}

// BaseAddMultiply : multiply x and y, and add result to aTarget
//
template<class T>
inline void BaseAddMultiply(T& aTarget, T& x, T& y, PlatDoubleWord aBase)
{
    LispInt nrx=x.Size();
    LispInt nry=y.Size();
    GrowDigits(aTarget,nrx+nry+1);
    LispInt ix,iy;

    typename T::ElementType *targetPtr = &aTarget[0];
    typename T::ElementType *xPtr = &x[0];
    typename T::ElementType *yPtr = &y[0];
    for (ix=0;ix<nrx;ix++)
    {
        PlatDoubleWord carry = 0;
        for (iy=0;iy<nry;iy++)
        {
            PlatDoubleWord word =
                static_cast<PlatDoubleWord>(targetPtr[ix+iy])+
                static_cast<PlatDoubleWord>(xPtr[ix])*
                static_cast<PlatDoubleWord>(yPtr[iy])+carry;
            // This calculates aTarget[ix+iy]+x[ix]*y[iy]+carry;


            targetPtr[ix+iy] = (typename T::ElementType)(word % aBase);
            carry          = word / aBase;
        }
        targetPtr[ix+nry] += (typename T::ElementType)(carry);
    }
}

template<class T>
inline void WordBaseAddMultiply(T& aTarget, T& x, T& y)
{
    LispInt nrx=x.Size();
    LispInt nry=y.Size();
    GrowDigits(aTarget,nrx+nry+1);
    LispInt ix,iy;

//printf("nrx=%d,nry=%d\n",nrx,nry);

    typename T::ElementType *targetPtr = &aTarget[0];
    typename T::ElementType *xPtr = &x[0];
    typename T::ElementType *yPtr = &y[0];
    for (ix=0;ix<nrx;ix++)
    {
        PlatDoubleWord carry = 0;
        for (iy=0;iy<nry;iy++)
        {
            PlatDoubleWord word =
                static_cast<PlatDoubleWord>(targetPtr[ix+iy])+
                static_cast<PlatDoubleWord>(xPtr[ix])*
                static_cast<PlatDoubleWord>(yPtr[iy])+carry;
            // This calculates aTarget[ix+iy]+x[ix]*y[iy]+carry;


            targetPtr[ix+iy] = (typename T::ElementType)(word);
            carry          = word >> WordBits;
        }
        {

            PlatDoubleWord word = static_cast<PlatDoubleWord>(targetPtr[ix+nry])+carry;
            targetPtr[ix+nry] = (typename T::ElementType)(word);
            carry          = word >> WordBits;
            LISPASSERT(carry == 0);

//          targetPtr[ix+nry] += (typename T::ElementType)(carry);
        }
    }

//printf("TARGET: %d words, %d decimal\n",aTarget.Size(),aTarget.iExp);
}




/* BaseMultiply : multiply x and y, and put result in aTarget
 */


template<class T>
inline void BaseMultiply(T& aTarget, T& x, T& y, PlatDoubleWord aBase)
{
    aTarget.ResizeTo(1);
    aTarget[0] = 0;
    BaseAddMultiply(aTarget, x, y, aBase);
}

template<class T>
inline void WordBaseMultiply(T& aTarget, T& x, T& y)
{
    aTarget.ResizeTo(1);
    aTarget[0] = 0;
    WordBaseAddMultiply(aTarget, x, y);
}



template<class T>
inline LispBoolean IsZero(T& a)
{
  register typename T::ElementType *ptr = &a[0];
  register typename T::ElementType *endptr = ptr+a.Size();
  while (ptr != endptr)
  {
    if (*ptr++ != 0)
      return LispFalse;
  }
  return LispTrue;
}



template<class T>
inline void WordBaseDivide(T& aQuotient, T& aRemainder, T& a1, T& a2)
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
    //this calculates d = base/(a2[n-1]+1);
    PlatDoubleWord d = WordBase/(static_cast<PlatDoubleWord>(a2[n-1])+1);


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
        aQuotient[j] = (typename T::ElementType)q;
        //D7:
        j--;
 
    }

    //D8:
    a1.ResizeTo(n);
    PlatDoubleWord carry;
    BaseDivideInt(a1, d, WordBase,carry);
    aRemainder.CopyFrom(a1);
}


