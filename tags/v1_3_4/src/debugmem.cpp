
#include <stdlib.h>
#include <stdio.h>
#include "yacasprivate.h"
#include "debugmem.h"
#include "stubs.h"
#include "lispassert.h"

#ifdef YACAS_DEBUG

#define DBG_(xxx) xxx

#ifdef NO_GLOBALS
#error "Memory heap checking only possible with global variables!"
#endif

// Bit-mask of selected 'intense' checks
#define INTENSE ( 0 )

typedef struct YacasMemBlock
{
    char      iMagicPrefix[4];
    const char *  iFile;
    int             iLine;
    unsigned long  iSize;
    unsigned char*  iData;
    char *      iMagicPostfix;
    //
    YacasMemBlock*      iPREV;
    YacasMemBlock*      iNEXT;
} YacasMemBlock;

YacasMemBlock*  iFirst  = NULL;
YacasMemBlock*  iLast  = NULL;

//------------------------------------------------------------------------------
// Three low-level check routines: CheckPred, CheckPtr, and CheckAllPtrs.

void CheckPred(int pred, const char* file, int line)
{
    if (!pred)
    {
        printf("%s(%d) : dereferencing invalid pointer\n",file,line);
    }
}

void CheckPtr( void * anAllocatedPtr, const char* file, int line )
{
    if (anAllocatedPtr == NULL)
        return;

  unsigned char * ptr = (unsigned char*)anAllocatedPtr;
    YacasMemBlock * t = ((YacasMemBlock*)anAllocatedPtr)-1;

    CheckPred ( t->iMagicPrefix[0] == 'x',file,line );
    LISPASSERT( t->iMagicPrefix[0] == 'x' );
    CheckPred ( t->iMagicPrefix[1] == 'y',file,line );
    LISPASSERT( t->iMagicPrefix[1] == 'y' );
    CheckPred ( t->iMagicPrefix[2] == 'z',file,line );
    LISPASSERT( t->iMagicPrefix[2] == 'z' );
    CheckPred ( t->iMagicPrefix[3] == 0,file,line );
    LISPASSERT( t->iMagicPrefix[3] == 0 );

    CheckPred ( t->iData    == ptr,file,line );
    LISPASSERT( t->iData    == ptr );
    CheckPred ( (unsigned char*)t->iMagicPostfix  == ptr+t->iSize,file,line );
    LISPASSERT( (unsigned char*)t->iMagicPostfix  == ptr+t->iSize );

    CheckPred ( t->iMagicPostfix[0] == 'x',file,line );
    LISPASSERT( t->iMagicPostfix[0] == 'x' );
    CheckPred ( t->iMagicPostfix[1] == 'y',file,line );
    LISPASSERT( t->iMagicPostfix[1] == 'y' );
    CheckPred ( t->iMagicPostfix[2] == 'z',file,line );
    LISPASSERT( t->iMagicPostfix[2] == 'z' );
    CheckPred ( t->iMagicPostfix[3] == 0,file,line );
    LISPASSERT( t->iMagicPostfix[3] == 0 );
}

void CheckAllPtrs(int final /*=0*/)
{
    if (final && iFirst!= NULL)
        printf("\n\n********** Memory leaks detected!!! ***********\n\n");

    for (YacasMemBlock * p = iFirst; p; p = p->iNEXT)
    {
        if (final) printf("%s(%d) : error C6666: memory leak! (0x%p)\n",p->iFile,p->iLine,(void*)(p+1));
        CheckPtr(p + 1, __FILE__, __LINE__);
    }
    if (final && iFirst!= NULL)
        printf("\n\n***********************************************\n\n");
}

//------------------------------------------------------------------------------
// YacasMallocPrivate, YacasReAllocPrivate, and YacasFreePrivate.

void* YacasMallocPrivate(unsigned long aSize, const char* aFile, int aLine)
{
  if (aSize<=0) return NULL;

  YacasMemBlock* t = (YacasMemBlock*)PlatObAlloc(aSize+sizeof(YacasMemBlock)+4);
  unsigned char* ptr = (unsigned char*)(t+1);

  t->iMagicPrefix[0]= 'x';
  t->iMagicPrefix[1]= 'y';
  t->iMagicPrefix[2]= 'z';
  t->iMagicPrefix[3]= 0;

  t->iFile  = aFile;
  t->iLine  = aLine;
  t->iSize  = aSize;
  t->iData  = (ptr);
  t->iMagicPostfix  = (char *)(ptr + aSize);
  t->iMagicPostfix[0] = 'x';
  t->iMagicPostfix[1] = 'y';
  t->iMagicPostfix[2] = 'z';
  t->iMagicPostfix[3] = 0  ;

  // maintain list of claimed elements!
  t->iNEXT = NULL;
  t->iPREV = NULL;
  if ( iLast==NULL )
  {
    iFirst  = t;
    iLast  = t;
  }
  else
  {
    iLast->iNEXT = t;
    t->iPREV = iLast;
    iLast = t;
  }

#if INTENSE & (1<<0)
  CheckAllPtrs();
#endif
#if INTENSE & (1<<1)
  CheckPtr( ptr, aFile, aLine );
#endif

  LISPASSERT(ptr);
  return ptr;
}

void* YacasReAllocPrivate(void* orig, unsigned long size, const char* aFile, int aLine)
{
  void* result = YacasMallocPrivate(size, aFile, aLine);
  if (orig)
  {
    unsigned long max = (((YacasMemBlock*)orig)-1)->iSize;
    if (max > size) max = size;
    memcpy(result,orig,max);
    YacasFreePrivate(orig);
  }
#if INTENSE & (1<<2)
  CheckPtr( result, aFile, aLine );
#endif
  return result;
}

void YacasFreePrivate(void* aOrig)
{
  if (aOrig)
  {{

    YacasMemBlock* t = ((YacasMemBlock*)aOrig)-1;

#if INTENSE & (1<<3)
  CheckPtr( aOrig, t->iFile, t->iLine );
#endif

    t->iMagicPrefix[0] = 'F';
    t->iMagicPrefix[1] = 'r';
    t->iMagicPrefix[2] = 'e';

    // maintain list of claimed elements!
    if ( t == iFirst ) // this is the first!
    {
        if (t->iNEXT) // there is a next
        {
            // it becomes the first
            iFirst = t->iNEXT;
            // that first does not have a previous!
            iFirst->iPREV=NULL;
        }
        else // this was the first AND the last
        {
            // list is now empty!
            iFirst=iLast=NULL;
        }
    }
    else // not the first!
    {
        LISPASSERT( t->iPREV!=NULL );

        if ( t->iNEXT )
        {
            t->iPREV->iNEXT = t->iNEXT;
            t->iNEXT->iPREV = t->iPREV;
        }
        else // this was the last one!
        {
            iLast = t->iPREV;
            iLast->iNEXT = NULL;
        }
    }
#if INTENSE & (1<<4)
    CheckAllPtrs();
#endif
    PlatObFree( t );
  }}
}

//------------------------------------------------------------------------------

namespace {
  class Warn
  {
    int warnings, wlimit; const char *msg1;
  public:
    Warn(int _wlimit, const char * _msg1) : warnings(0), wlimit(_wlimit), msg1(_msg1) {}
    void warn();
  };
  void Warn::warn()
  {
    if (warnings >= wlimit) return;
    printf(msg1, (++warnings >= wlimit) ? " (being quiet now)" : "");
  }
}

void* operator new(size_t size) NEW_THROWER
{
  static Warn w(5, "WARNING! Global new called%s\n"); w.warn();
  DBG_(while(0)) { int* ptr = NULL; *ptr = 1; }
    return PlatAlloc(size);
}

void* operator new[](size_t size) NEW_THROWER
{
  static Warn w(5, "WARNING! Global new[] called%s\n"); w.warn();
  DBG_(while(0)) { int* ptr = NULL; *ptr = 1; }
    return PlatAlloc(size);
}

void operator delete(void* object) DELETE_THROWER
{
  static Warn w(5, "WARNING! Global delete called%s\n"); w.warn();
  DBG_(while(0)) { int* ptr = NULL; *ptr = 1; }
    PlatFree(object);
}

void operator delete[](void* object) DELETE_THROWER
{
  static Warn w(5, "WARNING! Global delete[] called%s\n"); w.warn();
  DBG_(while(0)) { int* ptr = NULL; *ptr = 1; }
    PlatFree(object);
}

#endif  // YACAS_DEBUG
