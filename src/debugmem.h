

#ifndef __debugmem_h__
#define __debugmem_h__


#ifdef DEBUG_MODE

#ifdef NO_GLOBALS
#error "Memory heap checking only possible with global variables!"
#endif

  void* YacasMallocPrivate(unsigned long size, char* aFile, int aLine);
  void* YacasReAllocPrivate(void* orig, unsigned long size, char* aFile, int aLine);
  void YacasFreePrivate(void* aOrig);
  void YacasCheckMemory();
  void CheckAllPtrs();
#endif

#endif



