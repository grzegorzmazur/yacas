#include "yacas/yacasprivate.h"
#include "yacas/yacasbase.h"
#include "yacas/lisperror.h"
#include "yacas/lisphash.h"

#include <cassert>

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
#ifdef YACAS_DEBUG
  LispInt bin,i,n;
  bool fault = false;
  for (bin = 0; bin < KSymTableSize; bin++)
  {
    LispStringSmartPtrArray & aBin = _rep[bin];
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
}

DBG_( long theNrTokens=0; )

const LispString* LispHashTable::LookUp(const std::string& s)
{
    const std::size_t bin = _hash(s) % KSymTableSize;

    for (const LispStringSmartPtr p: _rep[bin])
        if (*p == s)
            return p;

    // Append a new string
    DBG_( theNrTokens++; )

    LispString* str = NEW LispString(s);

    _rep[bin].push_back(str);

    return str;
}

// GarbageCollect
void LispHashTable::GarbageCollect()
{
  for (LispInt bin = 0; bin < KSymTableSize; bin++)
  {
    LispStringSmartPtrArray & aBin = _rep[bin];
    for (LispInt i = 0, n = aBin.size(); i < n; i++)
    {
      if (aBin[i]->iReferenceCount != 1)
        continue;
      //printf("deleting [%s]\n",aBin[i]->String());
      // this should be cheaper than 'aBin[i]=nullptr;aBin.Delete(i)'
      aBin[i] = aBin[n-1];
      aBin[n-1] = nullptr;
      aBin.resize(n-1);
      i--;
      n--;
    }
  }
}
