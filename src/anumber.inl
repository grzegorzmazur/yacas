

/*
#include <stdio.h>
template<class T>
inline void Thunk(T& a)
{
  int i;
  for (i=0;i<a.NrItems();i++)
    printf("%d ",a[i]);
  printf("\n");  
}
*/

#if 0
#  define WordBaseTimesInt(a,n) BaseTimesInt(a,n,WordBase) 
#  define WordBaseAdd(a,b)      BaseAdd(a,b,WordBase)
#else
#  define WordBaseTimesInt(a,n) BaseTimesInt(a,n) 
#  define WordBaseAdd(a,b)      BaseAdd(a,b)
#endif

/* BaseTimesInt : multiply a with one digit in the range 0..(aBase-1)
 */
template<class T>
inline void BaseTimesInt(T& a,PlatDoubleWord aNumber, PlatDoubleWord aBase)
{
  PlatDoubleWord carry=0;
  LispInt i;
  LispInt nr=a.NrItems();

  typename T::ElementTypePtr aptr = &a[0];
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
  LispInt nr=a.NrItems();

  typename T::ElementTypePtr aptr = &a[0];
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
//    if (a[a.NrItems()-1] != 0)

    PlatDoubleWord carry=0;
    LispInt i;
    LispInt nr=a.NrItems();

    typename T::ElementTypePtr aptr = &a[0];
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

    if (aDigits <= a.NrItems())
        return;
    
    /*
     LispInt nrToAdd = aDigits-a.NrItems();

    for (i=0;i<nrToAdd;i++)
        a.Append(0);
*/
    LispInt origSize = a.NrItems();
    a.GrowTo(aDigits);
    a.SetNrItems(aDigits);
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

    GrowDigits(aTarget,aSource.NrItems());
    aTarget.Append(0);
    
    LispInt nr1 = aTarget.NrItems();
    LispInt nr2 = aSource.NrItems();
    LispInt nr;

    // nr represents min(nr1,nr2), the number of digits to add
    if (nr1>nr2)
        nr=nr2;
    else
        nr=nr1;
    
    PlatDoubleWord carry=0;
    LispInt digit;

   typename T::ElementTypePtr sourcePtr = &aSource[0];
   typename T::ElementTypePtr targetPtr = &aTarget[0];
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
        targetPtr[nr] = ww%aBase;
        carry = ww/aBase;
        nr++;
    }
}


template<class T>
inline void BaseAdd(T& aTarget, const T& aSource)
{
    // Initialize result

    GrowDigits(aTarget,aSource.NrItems());
    aTarget.Append(0);
    
    LispInt nr1 = aTarget.NrItems();
    LispInt nr2 = aSource.NrItems();
    LispInt nr;

    // nr represents min(nr1,nr2), the number of digits to add
    if (nr1>nr2)
        nr=nr2;
    else
        nr=nr1;
    
    PlatDoubleWord carry=0;
    LispInt digit;

   typename T::ElementTypePtr sourcePtr = &aSource[0];
   typename T::ElementTypePtr targetPtr = &aTarget[0];
   for (digit=0;digit<nr;digit++)
    {
        PlatDoubleWord word;
        word = (PlatDoubleWord)targetPtr[digit] +
            (PlatDoubleWord)sourcePtr[digit] + carry;
         PlatWord newDigit = (PlatWord)(word);
         PlatWord newCarry = (word >> WordBits);
         targetPtr[digit] = (typename T::ElementType)newDigit;
         carry          = newCarry;
    }
    while (carry != 0)
    {
        PlatSignedDoubleWord ww = targetPtr[nr];
        ww+=carry;
        targetPtr[nr] = ww;
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
    LispInt nr = a2.NrItems();

    typename T::ElementTypePtr resultPtr = &aResult[0];
    typename T::ElementTypePtr a2ptr = &a2[0];

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
        LISPASSERT(nr+offset<aResult.NrItems());

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
    aTarget.SetNrItems(0);
    while (aNumber != 0)
    {
        PlatDoubleWord digit = aNumber%aBase;
        aTarget.Append(digit);
        aNumber/=aBase;
    }
    if (aTarget.NrItems() == 0)
        aTarget.Append(0);
}

// BaseAddMultiply : multiply x and y, and add result to aTarget
//
template<class T>
inline void BaseAddMultiply(T& aTarget, T& x, T& y, PlatDoubleWord aBase)
{
    LispInt nrx=x.NrItems();
    LispInt nry=y.NrItems();
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

//#include <stdio.h>
template<class T>
inline void WordBaseAddMultiply(T& aTarget, T& x, T& y)
{
    LispInt nrx=x.NrItems();
    LispInt nry=y.NrItems();
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

//printf("TARGET: %d words, %d decimal\n",aTarget.NrItems(),aTarget.iExp);
}



#ifdef USE_KARATSUBA

const int cKaratsubaCutoff = 8;

/*
#include <stdio.h>

inline void BaseKaratsubaMultiply(ANumber& aTarget, ANumber& x, ANumber& y, LispInt aSize, LispInt aBase)
{
    LispInt halfSize = aSize/2;

    ANumber F0;
    ANumber F1;
    ANumber G0;
    ANumber G1;

    GrowDigits(F0,halfSize);
    GrowDigits(F1,halfSize);
    GrowDigits(G0,halfSize);
    GrowDigits(G1,halfSize);
    LispInt i;
    for (i=0;i<halfSize;i++)
    {
        F0[i] = x[i];
        F1[i] = x[halfSize+i];
        G0[i] = y[i];
        G1[i] = y[halfSize+i];
    }
    
    ANumber F0G0;
    ANumber F1G1;
    ANumber Fsum,Gsum;
    ANumber combined;

    Multiply(F0G0, F0, G0);
    Multiply(F1G1, F1, G1);
    Add(Fsum,F0,F1);
    Add(Gsum,G0,G1);
    Multiply(combined, Fsum, Gsum);
    ANumber s1;
    Subtract(s1,combined,F0G0);
    ANumber s2;
    Subtract(s2,s1,F1G1);

    BaseShiftLeft(F1G1, aSize   *(8*sizeof(PlatWord)));
    BaseShiftLeft(s2  , halfSize*(8*sizeof(PlatWord)));

    ANumber s3;
    Add(s3,F1G1,s2);
    Add(aTarget,s3,F0G0);
}

inline void BaseAddMultiplyK(ANumber& aTarget, ANumber& x, ANumber& y, LispInt aBase)
{
    LispInt nrx=x.NrItems();
    LispInt nry=y.NrItems();
    LispInt i, i2;

    // Too small?  Use O(n^2) multiplication
    if (nrx+nry+1 <= cKaratsubaCutoff)
    {
        BaseAddMultiply(aTarget, x, y, aBase);
        return;
    }

    LispInt max = (nrx > nry) ? nrx : nry;
    LispInt maxtwos;
    for (maxtwos = 1; maxtwos < max; maxtwos *= 2);

    GrowDigits(aTarget,maxtwos);
    GrowDigits(x,maxtwos);
    GrowDigits(y,maxtwos);
    BaseKaratsubaMultiply(aTarget, x, y, maxtwos, aBase);
}
*/


/* BaseAddMultiplyK : multiply x and y, and add result to aTarget
 *					  using Karatsuba multiplication.  This function
 *					  just makes the numbers passed into even ones,
 *					  by adding zeros to the front, then multiplies.
 *
 *					  Needs enough memory to hold 4*(smallest power
 *					  of 2 bigger than operands) digits.
 *
 *					  Code based on
 *
 *			Here's a crude diagram of how the numbers are stored:
 *			(scratch space is where the x & y sums are stored)
 *
 *				 ___________ ___________ _______________________
 *				|           |           |  x & y scratch space  |
 *				|     x     |     y     |  starts on <-- side   |
 *				|___________|___________|____ and works --> ____| 
 *
 */


template<class T>
inline void BaseAddMultiplyK(T& aTarget, T& x, T& y, PlatDoubleWord aBase)
{
	LispInt nrx=x.NrItems();
	LispInt nry=y.NrItems();
	LispInt i, i2;
	typename T::ElementType *iArray, *iXArray, *iYArray, *iSumArray;

	// Too small?  Use O(n^2) multiplication
	if (nrx+nry+1 <= cKaratsubaCutoff) {
		BaseAddMultiply(aTarget, x, y, aBase);
		return;
	}

    i = (nrx > nry) ? nrx : nry;
    for (i2 = 1; i2 < i; i2 *= 2);

	// Allocate a array of the smallest power of 2 larger than both
	// numbers * 4 elements, clear it to zero, then copy x into the 
	// into the start, y above it, and use the high half for recursion.
	// ToDo: Is this portable ?!?
	iArray = (typename T::ElementType *) PlatAlloc(i2*4*sizeof(T::ElementType));
	PlatMemSet((LispChar *)iArray, 0, i2*4*sizeof(T::ElementType));

	// Split our array into x and y halves
	iXArray = &iArray[0];
	x.CopyToExternalArray(iXArray, true);

	iYArray = &iArray[i2];
	y.CopyToExternalArray(iYArray, true);

	iSumArray = &iArray[i2*2];

	BaseKaratsubaMultiply(aTarget, iXArray, iYArray, iSumArray, i2, aBase);

	PlatFree((LispChar *) iArray);
}


template<class T>
void BaseKaratsubaMultiply(T& aTarget, 
                           typename T::ElementType* x,
                           typename T::ElementType* y,
                           typename T::ElementType* aScratchSpace,
                           LispInt aSize,
                           PlatDoubleWord aBase)
{
    LispInt iHalfSize = aSize/2;
    LispInt i;

    typename T::ElementType *xlow= &x[0];
    typename T::ElementType *xsum= &aScratchSpace[0];
    typename T::ElementType *xhigh=&x[iHalfSize];

    typename T::ElementType *ylow= &y[0];
    typename T::ElementType *ysum= &aScratchSpace[iHalfSize];
    typename T::ElementType *yhigh=&y[iHalfSize];
    T p1, p2, p3;

    // Too small?  Use O(n^2) multiplication
    if (aSize+1 <= cKaratsubaCutoff)
    {
        T iX(x, iHalfSize), iY(y, iHalfSize);
        BaseAddMultiply(aTarget, iX, iY, aBase);
        return;
    }

    // Compute xsum & ysum and put into the low half of the
    // scratch space, the high half is used for recursion

#if 1
    typename T::ElementType carryx=0;
    typename T::ElementType carryy=0;
#endif
    for (i = 0; i < iHalfSize; i++)
    {
#if 1
        typename T::ElementType word;
        typename T::ElementType newDigit;
        typename T::ElementType newCarry;

        word = (typename T::ElementType)xlow[i] +
               (typename T::ElementType)xhigh[i] + carryx;
        newDigit= (word%aBase);
        newCarry = (word/aBase);
        xsum[i] = newDigit;
        carryx  = newCarry;

        word = (typename T::ElementType)ylow[i] +
               (typename T::ElementType)yhigh[i] + carryy;
        newDigit= (word%aBase);
        newCarry = (word/aBase);
        ysum[i] = newDigit;
        carryy  = newCarry;
#endif

        //xsum[i] = xlow[i] + xhigh[i];
        //ysum[i] = ylow[i] + yhigh[i];
    }
#if 1
    xsum[iHalfSize]+=carryx;
    ysum[iHalfSize]+=carryy;
#endif
    
    // ToDo: make array bigger, so we don't have to zero the
    // scratch space after the first two calls?
    //hier
    BaseKaratsubaMultiply(p1, xlow, ylow, &aScratchSpace[aSize],
                          iHalfSize, aBase);
    PlatMemSet((LispChar *)&aScratchSpace[aSize], 0, aSize*sizeof(typename T::ElementType));
    BaseKaratsubaMultiply(p2, xsum, ysum, &aScratchSpace[aSize],
                          iHalfSize, aBase);
    PlatMemSet((LispChar *)&aScratchSpace[aSize], 0, aSize*sizeof(typename T::ElementType));
    BaseKaratsubaMultiply(p3, xhigh, yhigh, &aScratchSpace[aSize],
                          iHalfSize, aBase);

    // Have a feeling these functions are slowing down the
    // multipication

    BaseSubtract(p2, p1, 0);
    BaseSubtract(p2, p3, 0);
    BaseAdd(aTarget, p1, aBase);
    BaseAdd(aTarget, p3, aBase);
    BaseAdd(aTarget, p2, aBase);
}

#endif

/* BaseMultiply : multiply x and y, and put result in aTarget
 */

/*
 #ifdef USE_KARATSUBA
inline void BaseMultiply(ANumber& aTarget, ANumber& x, ANumber& y, LispInt aBase)
{
    aTarget.SetNrItems(1);
    aTarget[0] = 0;
    BaseAddMultiplyK(aTarget, x, y, aBase);
}
#endif
*/

template<class T>
inline void BaseMultiply(T& aTarget, T& x, T& y, PlatDoubleWord aBase)
{
    aTarget.SetNrItems(1);
    aTarget[0] = 0;
#ifdef USE_KARATSUBA
    BaseAddMultiplyK(aTarget, x, y, aBase);
#else
    BaseAddMultiply(aTarget, x, y, aBase);
#endif
}

template<class T>
inline void WordBaseMultiply(T& aTarget, T& x, T& y)
{
    aTarget.SetNrItems(1);
    aTarget[0] = 0;
#ifdef USE_KARATSUBA
    BaseAddMultiplyK(aTarget, x, y, WordBase);
#else
    WordBaseAddMultiply(aTarget, x, y);
#endif
}



template<class T>
inline LispBoolean IsZero(T& a)
{
  register typename T::ElementType *ptr = &a[0];
  register typename T::ElementType *endptr = ptr+a.NrItems();
  while (ptr != endptr)
  {
    if (*ptr++ != 0)
      return LispFalse;
  }
  return LispTrue;
/*
  LispInt i;
  for (i=0;i<a.NrItems();i++)
      if (a[i] != 0)
          return LispFalse;
  return LispTrue;
*/
}



template<class T>
inline void WordBaseDivide(T& aQuotient, T& aRemainder, T& a1, T& a2)
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
    a1.SetNrItems(n);
    PlatDoubleWord carry;
    BaseDivideInt(a1, d, WordBase,carry);
    aRemainder.CopyFrom(a1);
}


