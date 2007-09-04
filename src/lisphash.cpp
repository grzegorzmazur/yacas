#include "yacasprivate.h"
#include "yacasbase.h"
#include "lisperror.h"
#include "lisphash.h"

#ifdef YACAS_DEBUG
#include <stdio.h> // Safe, only included if YACAS_DEBUG is defined
#endif

#ifdef YACAS_DEBUG
#define DBG_(xxx) xxx
#else
#define DBG_(xxx) /*xxx*/
#endif

typedef CArrayGrower<LispStringSmartPtr, ArrOpsCustomObj<LispStringSmartPtr> > LispStringSmartPtrArray;

LispHashTable::~LispHashTable()
{
  LispInt bin,i,n;
#ifdef YACAS_DEBUG
  LispBoolean fault = LispFalse;
  for (bin = 0; bin < KSymTableSize; bin++)
  {
    LispStringSmartPtrArray & aBin = iHashTable[bin];
    for (i = 0, n = aBin.Size(); i < n; i++)
    {
      if (aBin[i]->iReferenceCount != 1)
      {
        if (!fault) printf("ERROR: string objects with invalid reference counts during destruction of the hashtable!\n");
        fault = LispTrue;
        printf("refcount = %d, string = \"%s\"\n", (int)(aBin[i]->iReferenceCount), aBin[i]->c_str());
      }
    }
  }
  LISPASSERT(!fault);
#endif // YACAS_DEBUG
  for (bin = 0; bin < KSymTableSize; bin++)
  {
    LispStringSmartPtrArray & aBin = iHashTable[bin];
    for (i = 0, n = aBin.Size(); i < n; i++)
    {
      // Check that this LispHashTable is a unique pool!?
      aBin[i] = (NULL);
    }
  }
  LISPASSERT(!fault);
}

LispInt  LispHash( const char *s )
//
// Simple hash function
//
{
    const LispChar *p;
    LispUnsLong h=0;

    for (p=s;*p!='\0';p++)
    {
        HashByte( h, *p);
    }
    return HASHBIN(h);
}

LispInt  LispHashCounted( const char *s,LispInt length )
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

LispInt  LispHashStringify( const char *s )
//
// Simple hash function
//
{
    const LispChar *p;
    LispUnsLong h=0;

    HashByte( h, '\"');
    for (p=s;*p!='\0';p++)
    {
        HashByte( h, *p);
    }
    HashByte( h, '\"');
    return HASHBIN(h);
}

LispInt  LispHashUnStringify( const char *s )
//
// Simple hash function
//
{
    const LispChar *p;
    LispUnsLong h=0;

    for (p=s+1;p[1]!='\0';p++)
    {
        HashByte( h, *p);
    }
    return HASHBIN(h);
}

LispInt LispHashPtr(const LispString * aString)
{
    LispChar * p = (LispChar *)(&aString);
    LispUnsLong h=0;

    switch (sizeof(LispString *))
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

DBG_( long theNrTokens=0; )

// If string not yet in table, insert. Afterwards return the string.
LispString * LispHashTable::LookUp(const LispChar * aString, LispBoolean aStringOwnedExternally)
{
    LispInt bin = LispHash(aString);

    // Find existing version of string
  LispStringSmartPtrArray & aBin = iHashTable[bin];
  for (LispInt i = 0, n = aBin.Size(); i < n; i++)
    {
    // expr ... typeof(expr)
    // aBin ... LispStringSmartPtrArray&
    // aBin[i] ... LispStringSmartPtr&
    // aBin[i].operator->() ... LispString*
    // aBin[i].operator->()->c_str() ... LispChar*
        if (StrEqual(aBin[i]->c_str(), aString))
    {
            return aBin[i];
        }
    }

    // Append a new string
  DBG_( theNrTokens++; )
  // The const_cast is tacky, but nearly correct.  'ownedexternally' bites.
    LispString * result = NEW LispString(const_cast<LispChar*>(aString),aStringOwnedExternally);
    AppendString(bin,result);
    return result;
}

void LispHashTable::AppendString(LispInt bin,LispString * result)
{
  LispStringSmartPtr smartptr;
  int index = iHashTable[bin].Size();
  iHashTable[bin].ResizeTo(index+1);  // change to GrowBy sometime
  iHashTable[bin][index] = result;
}

// If string not yet in table, insert. Afterwards return the string.
LispString * LispHashTable::LookUp(LispString * aString)
{
    LispInt bin = LispHash(aString->c_str());

    // Find existing version of string
  LispStringSmartPtrArray & aBin = iHashTable[bin];
  for (LispInt i = 0, n = aBin.Size(); i < n; i++)
    {
        if (StrEqual(aBin[i]->c_str(), aString->c_str()))
    {
            //TODO we shouldn't be doing refcounting here???
            if (!aString->iReferenceCount)
            {
                delete aString;
            }
            return aBin[i];
    }
    }

    // Append a new string
  DBG_( theNrTokens++; )
    AppendString(bin,aString);
    return aString;
}

LispInt StrEqualCounted(const LispChar * ptr1, const LispChar * ptr2, LispInt length)
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

LispString * LispHashTable::LookUpCounted(LispChar * aString, LispInt aLength)
{
    LispInt bin = LispHashCounted(aString,aLength);

    // Find existing version of string
  LispStringSmartPtrArray & aBin = iHashTable[bin];
  for (LispInt i = 0, n = aBin.Size(); i < n; i++)
    {
        if (StrEqualCounted(aBin[i]->c_str(), aString, aLength))
        {
            return aBin[i];
        }
    }

    // Append a new string
  DBG_( theNrTokens++; )
    LispString * str = NEW LispString();
    str->SetStringCounted(aString,aLength);

    AppendString(bin,str);
    return str;
}

LispInt StrEqualStringified(const LispChar * ptr1, const LispChar * ptr2)
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

LispInt StrEqualUnStringified(const LispChar * ptr1, const LispChar * ptr2)
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
LispString * LispHashTable::LookUpStringify(LispChar * aString,
                              LispBoolean aStringOwnedExternally)
{
    LispInt bin = LispHashStringify(aString);

    // Find existing version of string
  LispStringSmartPtrArray & aBin = iHashTable[bin];
  for (LispInt i = 0, n = aBin.Size(); i < n; i++)
    {
        if (StrEqualStringified(aBin[i]->c_str(), aString))
        {
            return aBin[i];
        }
    }

    // Append a new string
  DBG_( theNrTokens++; )
    LispString * str = NEW LispString();
    str->SetStringStringified(aString);

    AppendString(bin,str);
    return str;
}

// If string not yet in table, insert. Afterwards return the string.
LispString * LispHashTable::LookUpUnStringify(LispChar * aString,
                              LispBoolean aStringOwnedExternally)
{
    Check(aString[0] == '\"',KLispErrInvalidArg);
    LispInt bin = LispHashUnStringify(aString);

    // Find existing version of string
  LispStringSmartPtrArray & aBin = iHashTable[bin];
  for (LispInt i = 0, n = aBin.Size(); i < n; i++)
    {
        if (StrEqualUnStringified(aBin[i]->c_str(), aString))
        {
            return aBin[i];
        }
  }

    // Append a new string
  DBG_( theNrTokens++; )
    LispString * str = NEW LispString();
    str->SetStringUnStringified(aString);
    AppendString(bin,str);
    return str;
}

// GarbageCollect
void LispHashTable::GarbageCollect()
{
  for (LispInt bin = 0; bin < KSymTableSize; bin++)
  {
    LispStringSmartPtrArray & aBin = iHashTable[bin];
    for (LispInt i = 0, n = aBin.Size(); i < n; i++)
    {
      if (aBin[i]->iReferenceCount != 1)
        continue;
      //printf("deleting [%s]\n",aBin[i]->String());
      // this should be cheaper than 'aBin[i]=NULL;aBin.Delete(i)'
      aBin[i] = aBin[n-1];
      aBin[n-1] = (NULL);
      aBin.ResizeTo(n-1);
      i--;
      n--;
    }
  }
}
