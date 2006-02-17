#ifndef __yacasbase_h__
#define __yacasbase_h__
#include "yacasprivate.h"

#define MY_ALLOCS

#define STA static

class YacasBase
{
public:
    inline YacasBase() {};
    inline ~YacasBase() {};
#ifdef MY_ALLOCS
#ifdef YACAS_DEBUG
    STA inline void* operator new(size_t size, char* aFile, int aLine)
		{ return YacasMallocPrivate(size,aFile,aLine); }
    STA inline void* operator new[](size_t size, char* aFile, int aLine)
		{ return YacasMallocPrivate(size,aFile,aLine); }
#if NEED_FUNNY_operator_delete || defined(_MSC_VER)	// TODO: woof woof woof
    STA inline void operator delete(void* object, char* aFile, int aLine)
		{ YacasFreePrivate(object); }
    STA inline void operator delete[](void* object, char* aFile, int aLine)
		{ YacasFreePrivate(object); }
#endif
#else
    STA inline void* operator new(size_t size)
		{ return PlatAlloc(size); }
    STA inline void* operator new[](size_t size)
		{ return PlatAlloc(size); }
#endif	// endif -- here, or two lines down? -- TODO: woof
    STA inline void operator delete(void* object)
		{ PlatFree(object); }
    STA inline void operator delete[](void* object)
		{ PlatFree(object); }
#endif
	// Placement form of new and delete.
	STA inline void* operator new(size_t, void* where) { return where; }
	STA inline void operator delete(void*, void*) {}
};

#endif
