#ifndef __yacasbase_h__
#define __yacasbase_h__
#include "yacasprivate.h"

/* /class YacasBase. All other objects should derive from YacasBase so that they use the correct
 * operators new and delete. This ensures a Yacas-specific memory manager is used.
 */

class YacasBase
{
public:
  inline YacasBase() {};
#ifdef YACAS_DEBUG
  // New operators with additional debug arguments need to be handled differently
  static inline void* operator new(size_t size, char* aFile, int aLine)
    { return YacasMallocPrivate(size,aFile,aLine); }
  static inline void* operator new[](size_t size, char* aFile, int aLine)
    { return YacasMallocPrivate(size,aFile,aLine); }
#else
  // Normal versions of operator new
  static inline void* operator new(size_t size)
    { return PlatAlloc(size); }
  static inline void* operator new[](size_t size)
    { return PlatAlloc(size); }
#endif
  /* Operators delete are shared, they behave the same whether in debug or release (as
     PlatFree is mapped to the debug version of freeing memory any way)
   */
  static inline void operator delete(void* object)
    { PlatFree(object); }
  static inline void operator delete[](void* object)
    { PlatFree(object); }
	// Placement form of new and delete.
  static inline void* operator new(size_t, void* where) { return where; }
  static inline void operator delete(void*, void*) {}
protected: 
  /** Since other objects are derived from this one and I don't want to force vtables, 
   *  one is not allowed to call the destructor of this class directly. Classes will be
   *  derived from this class, and they will have destructors that can be called, and
   *  as a consequence this destructor is called. The method is put here so that it can
   *  be explicitly made protected.
   */
  inline ~YacasBase() {};
};

#endif
