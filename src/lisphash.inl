 
//#include <stdio.h> //TODO!


/** LAssoc is a helper class for LispAssociatedHash
 */
template<class T>
class LAssoc : public YacasBase
{
public:
    inline LAssoc(LispStringPtr aString,const T& aData);
    inline ~LAssoc();
    LispStringSmartPtr iString;
    T iData;
private: //Functions no one is allowed to use
    inline LAssoc(const LAssoc& aOther);
    inline LAssoc& operator=(const LAssoc& aOther);
};
template<class T>
inline LAssoc<T>::LAssoc(LispStringPtr aString,const T& aData)
: iData(aData)
{
    LISPASSERT(aString != NULL);
    iString.Set(aString);
//TODO remove!    aString->IncreaseRefCount();
}

/*
template<class T>
inline LAssoc<T>::LAssoc(const LAssoc<T>& aOther)
: iData(aOther.iData)
{
//    printf("LAssoc copyconstruct\n");
    iString.Set(aOther.aString);
//TODO remove!    iString->IncreaseRefCount();
}

template<class T>
inline LAssoc<T>::operator=(const LAssoc<T>& aOther)
{
//    printf("LAssoc assignment\n");
//TODO remove!    iString->DecreaseRefCount();
    iString.Set(aOther.iString);
//TODO remove!    iString->IncreaseRefCount();
    iData=aOther.iData;
}
*/

template<class T>
inline LAssoc<T>::~LAssoc()
{
   iString.Set(NULL);
//printf(" %d ",iString->ReferenceCount());
//TODO remove!    iString->DecreaseRefCount();
//printf("lassoc2\n");
}


#define LH  LispHashPtr(aString) // LispHash(aString->String())

template<class T>
inline T* LispAssociatedHash<T>::LookUp(LispStringPtr aString)
{
    LispInt bin = LH;

    LispInt i=0;
    // Find existing version of string
    for (i = iHashTable[bin].NrItems()-1 ; i >= 0 ; i --)
    {
        LispStringSmartPtr &ptr = (((LAssoc<T>*)iHashTable[bin][i])->iString);
        if (ptr() == aString)
//TODO remove?        if (((LAssoc<T>*)iHashTable[bin][i])->iString() == aString)
        {
            return &((LAssoc<T>*)iHashTable[bin][i])->iData;
        }
    }
    return NULL;
}

template<class T>
inline void LispAssociatedHash<T>::SetAssociation(const T& aData, LispStringPtr aString)
{
    LispInt bin = LH;
    LispInt i;
    
    // Find existing version of string
    for (i=0;i<iHashTable[bin].NrItems();i++)
    {
        LispStringSmartPtr &ptr = (((LAssoc<T>*)iHashTable[bin][i])->iString);
        if (ptr() == aString)
//TODO remove?        if (((LAssoc<T>*)iHashTable[bin][i])->iString() == aString)
        {
            //change existing version of association
            ((LAssoc<T>*)iHashTable[bin][i])->iData = aData;
            return;
        }
    }

    // Append a NEW string association
    iHashTable[bin].Append(NEW LAssoc<T>(aString,aData));
    return;
}

template<class T>
inline void LispAssociatedHash<T>::Release(LispStringPtr aString)
{
    LispInt bin = LH;
    LispInt i;
    
    // Find existing version of string
    for (i=0;i<iHashTable[bin].NrItems();i++)
    {
        LispStringSmartPtr &ptr = (((LAssoc<T>*)iHashTable[bin][i])->iString);
        if (ptr() == aString)
//TODO remove?        if (((LAssoc<T>*)iHashTable[bin][i])->iString() == aString)
        {
            //change existing version of association
            delete ((LAssoc<T>*)iHashTable[bin][i]);
            iHashTable[bin][i] = NULL;
            iHashTable[bin].Delete(i);
            return;
        }
    }
}


template<class T>
inline LispAssociatedHash<T>::~LispAssociatedHash()
{
    LispInt bin;
    for (bin=0;bin<KSymTableSize;bin++)
    {
        LispInt j;
        LispInt nritems = iHashTable[bin].NrItems();
        for (j=0;j<nritems;j++)
        {
            delete ((LAssoc<T>*)iHashTable[bin][j]);
        }
    }
}


inline void HashByte(LispUnsLong& h, LispChar c)
{
    h=(h<<4)+c;
    LispUnsLong g = h&0xf0000000L;
    if (g)
    {
        h=h^(g>>24);
        h=h^g;
    }
}
#define HASHBIN(_h)    (LispInt)((_h)%KSymTableSize)


