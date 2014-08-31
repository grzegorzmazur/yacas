#include "yacas/yacasprivate.h"
#include "yacas/yacasbase.h"
#include "yacas/lisperror.h"
#include "yacas/lisphash.h"

#include <cassert>
#include <cstring>

#ifdef YACAS_DEBUG
#include <stdio.h> // Safe, only included if YACAS_DEBUG is defined
#endif

#ifdef YACAS_DEBUG
#define DBG_(xxx) xxx
#else
#define DBG_(xxx) /*xxx*/
#endif

typedef std::vector<LispStringSmartPtr> LispStringSmartPtrArray;

LispHashTable::~LispHashTable()
{
  LispInt bin,i,n;
#ifdef YACAS_DEBUG
  bool fault = false;
  for (bin = 0; bin < KSymTableSize; bin++)
  {
    LispStringSmartPtrArray & aBin = iHashTable[bin];
    for (i = 0, n = aBin.Size(); i < n; i++)
    {
      if (aBin[i]->iReferenceCount != 1)
      {
        if (!fault) printf("ERROR: string objects with invalid reference counts during destruction of the hashtable!\n");
        fault = true;
        printf("refcount = %d, string = \"%s\"\n", (int)(aBin[i]->iReferenceCount), aBin[i]->c_str());
      }
    }
  }
  assert(!fault);
#endif // YACAS_DEBUG
  for (bin = 0; bin < KSymTableSize; bin++)
  {
    LispStringSmartPtrArray & aBin = iHashTable[bin];
    for (i = 0, n = aBin.size(); i < n; i++)
    {
      // Check that this LispHashTable is a unique pool!?
      aBin[i] = (nullptr);
    }
  }
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
        assert(0); //Extend it then...

    }
    return (HASHBIN(h));
}

DBG_( long theNrTokens=0; )


void LispHashTable::AppendString(LispInt bin, LispString* result)
{
    iHashTable[bin].push_back(result);
}

LispString* LispHashTable::LookUpCounted(const LispChar* aString, LispInt aLength)
{
    LispInt bin = LispHashCounted(aString, aLength);

    // Find existing version of string
    const LispStringSmartPtrArray& aBin = iHashTable[bin];
    const std::size_t n = aBin.size();
    for (std::size_t i = 0; i < n; ++i) {
        const char* const p = aBin[i]->c_str();
        if (!std::strncmp(p, aString, aLength) && p[aLength] == '\0')
            return aBin[i];
    }

    // Append a new string
    DBG_( theNrTokens++; )
    LispString * str = NEW LispString();
    str->assign(aString,aLength);

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

// If string not yet in table, insert. Afterwards return the string.
LispString * LispHashTable::LookUpStringify(const LispChar * aString)
{
    LispInt bin = LispHashStringify(aString);

    // Find existing version of string
  LispStringSmartPtrArray & aBin = iHashTable[bin];
  for (LispInt i = 0, n = aBin.size(); i < n; i++)
    {
        if (StrEqualStringified(aBin[i]->c_str(), aString))
        {
            return aBin[i];
        }
    }

    // Append a new string
  DBG_( theNrTokens++; )
    LispString * str = NEW LispString();

    str->assign("\"");
    str->append(aString);
    str->append("\"");

    AppendString(bin,str);
    return str;
}

// GarbageCollect
void LispHashTable::GarbageCollect()
{
  for (LispInt bin = 0; bin < KSymTableSize; bin++)
  {
    LispStringSmartPtrArray & aBin = iHashTable[bin];
    for (LispInt i = 0, n = aBin.size(); i < n; i++)
    {
      if (aBin[i]->iReferenceCount != 1)
        continue;
      //printf("deleting [%s]\n",aBin[i]->String());
      // this should be cheaper than 'aBin[i]=nullptr;aBin.Delete(i)'
      aBin[i] = aBin[n-1];
      aBin[n-1] = (nullptr);
      aBin.resize(n-1);
      i--;
      n--;
    }
  }
}
