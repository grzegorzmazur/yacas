
#include "lisperror.h"
#include "lisphash.h"


LispHashTable::~LispHashTable()
{
}



LispInt  LispHash( char *s )
//
// Simple hash function
//
{
    LispChar *p;
    LispUnsLong h=0;

    for (p=s;*p!='\0';p++)
    {
        HashByte( h, *p);
    }
    return HASHBIN(h);
}

LispInt  LispHashCounted( char *s,LispInt length )
//
// Simple hash function
//
{
    LispInt i;
    LispUnsLong h=0;

    for (i=0;i<length;i++)
    {
        HashByte( h, s[i]);
    }
    return HASHBIN(h);
}

LispInt  LispHashStringify( char *s )
//
// Simple hash function
//
{
    LispChar *p;
    LispUnsLong h=0;

    HashByte( h, '\"');
    for (p=s;*p!='\0';p++)
    {
        HashByte( h, *p);
    }
    HashByte( h, '\"');
    return HASHBIN(h);
}

LispInt  LispHashUnStringify( char *s )
//
// Simple hash function
//
{
    LispChar *p;
    LispUnsLong h=0;

    for (p=s+1;p[1]!='\0';p++)
    {
        HashByte( h, *p);
    }
    return HASHBIN(h);
}



LispInt LispHashPtr(LispStringPtr aString)
{
    LispCharPtr p = (LispCharPtr)aString;
    LispUnsLong h=0;

    switch (sizeof(LispStringPtr))
    {
    case 8: HashByte( h, *p++);
    case 7: HashByte( h, *p++);
    case 6: HashByte( h, *p++);
    case 5: HashByte( h, *p++);
    case 4: HashByte( h, *p++);
    case 3: HashByte( h, *p++);
    case 2: HashByte( h, *p++);
    case 1: HashByte( h, *p++);
    break;
    default:
        LISPASSERT(0); //Extend it then...
        
    }
    return (HASHBIN(h));
}


#ifdef YACAS_DEBUG
long theNrTokens=0;
#endif

// If string not yet in table, insert. Afterwards return the string.
LispStringPtr LispHashTable::LookUp(LispCharPtr aString,
                                    LispBoolean aStringOwnedExternally)
{
    LispInt bin = LispHash(aString);
    LispInt i;

    // Find existing version of string
    LispInt nrc=iHashTable[bin].NrItems();
    for (i=0;i<nrc;i++)
    {
        if (StrEqual((iHashTable[bin][i])->String(), aString))
        {
            return iHashTable[bin][i];
        }
    }

    // Append a new string
#ifdef YACAS_DEBUG
    theNrTokens++;
#endif
    iHashTable[bin].Append(new LispString(aString,aStringOwnedExternally));
    return iHashTable[bin][iHashTable[bin].NrItems()-1];
}


// If string not yet in table, insert. Afterwards return the string.
LispStringPtr LispHashTable::LookUp(LispStringPtr aString)
{
    LispInt bin = LispHash(aString->String());
    LispInt i;

    // Find existing version of string
    LispInt nrc=iHashTable[bin].NrItems();
    for (i=0;i<nrc;i++)
    {
        if (StrEqual((iHashTable[bin][i])->String(), aString->String()))
        {
            delete aString;
            return iHashTable[bin][i];
        }
    }

    // Append a new string
#ifdef YACAS_DEBUG
    theNrTokens++;
#endif
    iHashTable[bin].Append(aString);
    return iHashTable[bin][iHashTable[bin].NrItems()-1];
}


LispInt StrEqualCounted(LispCharPtr ptr1, LispCharPtr ptr2,LispInt length)
{
    LispInt i;
    for (i=0;i<length;i++)
    {
        if (ptr1[i] != ptr2[i])
            return 0;
    }
    if (ptr1[length] != '\0')
        return 0;
    return 1;
}

LispStringPtr LispHashTable::LookUpCounted(LispCharPtr aString,
                                           LispInt aLength)
{
    LispInt bin = LispHashCounted(aString,aLength);
    LispInt i;

    // Find existing version of string
    LispInt nrc=iHashTable[bin].NrItems();
    for (i=0;i<nrc;i++)
    {
        if (StrEqualCounted((iHashTable[bin][i])->String(), aString,aLength))
        {
            return iHashTable[bin][i];
        }
    }

    // Append a new string
#ifdef YACAS_DEBUG
    theNrTokens++;
#endif
    LispStringPtr str = new LispString();
    str->SetStringCounted(aString,aLength);

    iHashTable[bin].Append(str);
    return iHashTable[bin][iHashTable[bin].NrItems()-1];
}

LispInt StrEqualStringified(LispCharPtr ptr1, LispCharPtr ptr2)
{
    if (*ptr1 != '\"')
        return 0;
    ptr1++;
    while (ptr1[1] != 0 && *ptr2 != 0)
    {
        if (*ptr1 != *ptr2++)
            return 0;
        ptr1++;
    }
    if (*ptr1 != '\"')
        return 0;
    ptr1++;
    if (*ptr1 != *ptr2)
        return 0;
    return 1;
}

LispInt StrEqualUnStringified(LispCharPtr ptr1, LispCharPtr ptr2)
{
    if (*ptr2 != '\"')
        return 0;
    ptr2++;
    while (*ptr1 != 0 && ptr2[1] != 0)
    {
        if (*ptr1++ != *ptr2)
            return 0;
        ptr2++;
    }
    if (*ptr2 != '\"')
        return 0;
    ptr2++;
    if (*ptr1 != *ptr2)
        return 0;
    return 1;
}



// If string not yet in table, insert. Afterwards return the string.
LispStringPtr LispHashTable::LookUpStringify(LispCharPtr aString,
                              LispBoolean aStringOwnedExternally)
{
    LispInt bin = LispHashStringify(aString);
    LispInt i;

    // Find existing version of string
    LispInt nrc=iHashTable[bin].NrItems();
    for (i=0;i<nrc;i++)
    {
        if (StrEqualStringified((iHashTable[bin][i])->String(), aString))
        {
            return iHashTable[bin][i];
        }
    }

    // Append a new string
#ifdef YACAS_DEBUG
    theNrTokens++;
#endif
    LispStringPtr str = new LispString();
    str->SetStringStringified(aString);

    iHashTable[bin].Append(str);
    return iHashTable[bin][iHashTable[bin].NrItems()-1];
}
// If string not yet in table, insert. Afterwards return the string.
LispStringPtr LispHashTable::LookUpUnStringify(LispCharPtr aString,
                              LispBoolean aStringOwnedExternally)
{
    Check(aString[0] == '\"',KLispErrInvalidArg);
    LispInt bin = LispHashUnStringify(aString);
    LispInt i;

    // Find existing version of string
    LispInt nrc=iHashTable[bin].NrItems();
    for (i=0;i<nrc;i++)
    {
        if (StrEqualUnStringified((iHashTable[bin][i])->String(), aString))
        {
            return iHashTable[bin][i];
        }
    }

    // Append a new string
#ifdef YACAS_DEBUG
    theNrTokens++;
#endif
    LispStringPtr str = new LispString();
    str->SetStringUnStringified(aString);
    iHashTable[bin].Append(str);
    return iHashTable[bin][iHashTable[bin].NrItems()-1];
}



// GarbageCollect
void LispHashTable::GarbageCollect()
{
    LispInt bin;
    for (bin=0;bin<KSymTableSize;bin++)
    {
        LispInt j;
        LispInt nritems = iHashTable[bin].NrItems();
        for (j=0;j<nritems;j++)
        {
            if (iHashTable[bin][j]->ReferenceCount() == 0)
            {
//printf("deleting [%s]\n",iHashTable[bin][j]->String());
                delete iHashTable[bin][j];
                iHashTable[bin].Delete(j);
                j--;
                nritems--;
            }
        }
    }
}

