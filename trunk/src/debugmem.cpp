
#include <stdlib.h>
#include <stdio.h>
#include "debugmem.h"
#include "lispassert.h"

#ifdef DEBUG_MODE

#ifdef NO_GLOBALS
#error "Memory heap checking only possible with global variables!"
#endif

typedef struct YacasMemBlock
{
    char			iMagicPrefix[4];
    const char *	iFile;
    int           iLine;
    unsigned long			  iSize;
    unsigned char*			iData;
    char *			iMagicPostfix;
    //
    YacasMemBlock*			iPREV;
    YacasMemBlock*			iNEXT;
} YacasMemBlock;
#define Header(p) ((YacasMemBlock*)(((unsigned char*)p)-sizeof(YacasMemBlock)))


static int totalBytesAllocated = 0;
static int totalNrAllocated = 0;
static int currentBytesAllocated = 0;
static int currentNrAllocated = 0;
static int totalSmall = 0;
static int maxBytesAllocated = 0;
static int maxPlusOverhead = 0;


void Increment(void* data)
{
  if (data)
  {
    int size = Header(data)->iSize;

    totalBytesAllocated += size;
    totalNrAllocated ++;
    currentBytesAllocated += size;
    currentNrAllocated ++;
    if (size<=256)totalSmall ++;
    if (maxBytesAllocated < currentBytesAllocated) 
    {
      maxBytesAllocated = currentBytesAllocated;
      maxPlusOverhead = currentBytesAllocated + currentNrAllocated*(sizeof(YacasMemBlock)+4);
    }
  }
}
void Decrement(void* data)
{
  if (data)
  {
    int size = Header(data)->iSize;
    currentBytesAllocated -= size;
    currentNrAllocated --;
    LISPASSERT(currentNrAllocated>=0);
  }
}



YacasMemBlock*	iFirst	= NULL;
YacasMemBlock*	iLast	= NULL;
void CheckPtr( void * anAllocatedPtr );
void CheckAllPtrs()
{
  YacasMemBlock*	p = iFirst;
  while (p!=NULL)
  {
    unsigned char* ptr = (unsigned char*)p;
    ptr += sizeof(YacasMemBlock);
    CheckPtr( ptr );
    p = p->iNEXT;
  }	
}


void CheckPtr( void * anAllocatedPtr )
{
  unsigned char* ptr = (unsigned char*)anAllocatedPtr;
  ptr -= sizeof(YacasMemBlock);
  YacasMemBlock* t = (YacasMemBlock*)ptr;

  if ( t->iMagicPrefix[0] != 'x' )
  {
     LISPASSERT( t->iMagicPrefix[0] == 'x' );
  }
  LISPASSERT( t->iMagicPrefix[1] == 'y' );
  LISPASSERT( t->iMagicPrefix[2] == 'z' );
  LISPASSERT( t->iMagicPrefix[3] == 0 );

  LISPASSERT( t->iData		== ptr+sizeof(YacasMemBlock) );
  LISPASSERT( (unsigned char*)t->iMagicPostfix	== ptr+sizeof(YacasMemBlock)+t->iSize );

  LISPASSERT( t->iMagicPostfix[0] == 'x' );
  LISPASSERT( t->iMagicPostfix[1] == 'y' );
  LISPASSERT( t->iMagicPostfix[2] == 'z' );
  LISPASSERT( t->iMagicPostfix[3] == 0 );
}



void* YacasMallocPrivate(unsigned long aSize, char* aFile, int aLine)
{
  if (aSize<=0) return NULL;
  unsigned char* ptr = (unsigned char*)malloc(aSize+sizeof(YacasMemBlock)+4);
  YacasMemBlock* t = (YacasMemBlock*)ptr;
  t->iMagicPrefix[0]= 'x';
  t->iMagicPrefix[1]= 'y';
  t->iMagicPrefix[2]= 'z';
  t->iMagicPrefix[3]= 0;  

  t->iFile  = aFile;
  t->iLine  = aLine;
  t->iSize	= aSize;
  t->iData	= (ptr + sizeof(YacasMemBlock));
  t->iMagicPostfix	= (char *)(ptr + sizeof(YacasMemBlock) + aSize);
  t->iMagicPostfix[0] = 'x';
  t->iMagicPostfix[1] = 'y';
  t->iMagicPostfix[2] = 'z';
  t->iMagicPostfix[3] = 0  ;

  // maintain list of claimed elements!
  t->iNEXT = NULL;
  t->iPREV = NULL;
  if ( iLast==NULL )
  {
    iFirst	= t;
    iLast	= t;
  }
  else
  {
    iLast->iNEXT = t;
    t->iPREV = iLast;
    iLast = t;
  }

  //CheckAllPtrs();

  ptr += sizeof(YacasMemBlock);
  //CheckPtr( ptr );

  LISPASSERT(ptr != NULL);
  Increment(ptr);
  return ptr;
}
void* YacasReAllocPrivate(void* orig, unsigned long size, char* aFile, int aLine)
{
  void* result = YacasMallocPrivate(size, aFile, aLine);
  if (orig)
  {
    unsigned long max = Header(orig)->iSize;
    if (max > size) max = size;
    memcpy(result,orig,max);
    YacasFreePrivate(orig);
  }
//  CheckPtr( result );

  return result;
}
void YacasFreePrivate(void* aOrig)
{
  if (aOrig != NULL)
  {
    Decrement(aOrig);
  }
  
  if (aOrig)
  {
//    CheckPtr( aOrig );

    unsigned char* ptr = (unsigned char*)aOrig;
    ptr -= sizeof(YacasMemBlock);
    YacasMemBlock* t = (YacasMemBlock*)ptr;

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
    //CheckAllPtrs();
    //
    free( t );
  }
}


void YacasCheckMemory()
{
    if (iFirst!= NULL && iLast!= NULL)
    {
        printf("\n\n********** Memory leaks detected!!! ***********\n\n");
        YacasMemBlock*	p = iFirst;
        while (p!=NULL)
        {
            printf("%s(%d) : error C6666: memory leak!\n",p->iFile,p->iLine);
            unsigned char* ptr = (unsigned char*)p;
            ptr += sizeof(YacasMemBlock);
            CheckPtr( ptr );
            p = p->iNEXT;
        }
        printf("\n\n***********************************************\n\n");
        CheckAllPtrs();
    }
}

#endif
