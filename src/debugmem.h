

#ifndef __debugmem_h__
#define __debugmem_h__


#ifdef YACAS_DEBUG

#ifdef NO_GLOBALS
#error "Memory heap checking only possible with global variables!"
#endif

  void* YacasMallocPrivate(unsigned long size, char* aFile, int aLine);
  void* YacasReAllocPrivate(void* orig, unsigned long size, char* aFile, int aLine);
  void YacasFreePrivate(void* aOrig);
  void YacasCheckMemory();
  void CheckPtr( void * anAllocatedPtr, char* file, int line );
  void CheckAllPtrs();
#endif

#endif



