
#include <stdlib.h>
#include <stdio.h>
#include "debugmem.h"
#include "stubs.h"
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



YacasMemBlock*	iFirst	= NULL;
YacasMemBlock*	iLast	= NULL;
void CheckPtr( void * anAllocatedPtr,char* file, int line );
void CheckAllPtrs()
{
  YacasMemBlock*	p = iFirst;
  while (p!=NULL)
  {
    unsigned char* ptr = (unsigned char*)p;
    ptr += sizeof(YacasMemBlock);
    CheckPtr( ptr, __FILE__, __LINE__ );
    p = p->iNEXT;
  }	
}

void CheckPred(int pred, char* file, int line)
{
    if (!pred)
    {
        printf("%s(%d) : dereferencing invalid pointer\n",file,line);
    }
}

void CheckPtr( void * anAllocatedPtr, char* file, int line )
{
    if (anAllocatedPtr == NULL)
        return;
    unsigned char* ptr = (unsigned char*)anAllocatedPtr;
    ptr -= sizeof(YacasMemBlock);
    YacasMemBlock* t = (YacasMemBlock*)ptr;

    CheckPred ( t->iMagicPrefix[0] == 'x',file,line );
    LISPASSERT( t->iMagicPrefix[0] == 'x' );
    CheckPred ( t->iMagicPrefix[1] == 'y',file,line );
    LISPASSERT( t->iMagicPrefix[1] == 'y' );
    CheckPred ( t->iMagicPrefix[2] == 'z',file,line );
    LISPASSERT( t->iMagicPrefix[2] == 'z' );
    CheckPred ( t->iMagicPrefix[3] == 0,file,line );
    LISPASSERT( t->iMagicPrefix[3] == 0 );

    CheckPred ( t->iData		== ptr+sizeof(YacasMemBlock),file,line );
    LISPASSERT( t->iData		== ptr+sizeof(YacasMemBlock) );
    CheckPred ( (unsigned char*)t->iMagicPostfix	== ptr+sizeof(YacasMemBlock)+t->iSize,file,line );
    LISPASSERT( (unsigned char*)t->iMagicPostfix	== ptr+sizeof(YacasMemBlock)+t->iSize );

    CheckPred ( t->iMagicPostfix[0] == 'x',file,line );
    LISPASSERT( t->iMagicPostfix[0] == 'x' );
    CheckPred ( t->iMagicPostfix[1] == 'y',file,line );
    LISPASSERT( t->iMagicPostfix[1] == 'y' );
    CheckPred ( t->iMagicPostfix[2] == 'z',file,line );
    LISPASSERT( t->iMagicPostfix[2] == 'z' );
    CheckPred ( t->iMagicPostfix[3] == 0,file,line );
    LISPASSERT( t->iMagicPostfix[3] == 0 );

}



void* YacasMallocPrivate(unsigned long aSize, char* aFile, int aLine)
{
  if (aSize<=0) return NULL;
  unsigned char* ptr = (unsigned char*)PlatObAlloc(aSize+sizeof(YacasMemBlock)+4);
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
    PlatObFree( t );
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
            CheckPtr( ptr, __FILE__, __LINE__ );
            p = p->iNEXT;
        }
        printf("\n\n***********************************************\n\n");
        CheckAllPtrs();
    }
}

#endif
