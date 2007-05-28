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
 
  /** If we don't want to force vtables, we can make the destructor inline and protected.
   *  The following might cause a little bit of overhead but has the advantage that even
   *  the compiler can see that this is safe (thus no unnecessary warnings). Preferrably
   *  though this class has a protected inline destructor.
   */
protected:
  inline ~YacasBase() {};
};

#endif