#ifndef __yacasbase_h__
#define __yacasbase_h__
#include "yacasprivate.h"

#define MY_ALLOCS

class YacasBase
{
public:
    inline YacasBase() {};
    inline ~YacasBase() {};
#ifdef MY_ALLOCS

#ifdef DEBUG_MODE
    inline void* operator new(size_t size, char* aFile, int aLine);
    inline void* operator new[](size_t size, char* aFile, int aLine);
#else
    inline void* operator new(size_t size);
    inline void* operator new[](size_t size);
#endif
    inline void operator delete(void* object);
    inline void operator delete[](void* object);
#endif
};

#ifdef MY_ALLOCS

#ifdef DEBUG_MODE
inline void* YacasBase::operator new(size_t size, char* aFile, int aLine)
{
    return YacasMallocPrivate(size,aFile,aLine);
}
inline void* YacasBase::operator new[](size_t size, char* aFile, int aLine)
{
    return YacasMallocPrivate(size,aFile,aLine);
}
#else
inline void* YacasBase::operator new(size_t size)
{
    return PlatAlloc(size);
}
inline void* YacasBase::operator new[](size_t size)
{
    return PlatAlloc(size);
}
#endif

inline void YacasBase::operator delete(void* object)
{
    PlatFree((LispCharPtr)object);
}
inline void YacasBase::operator delete[](void* object)
{
    PlatFree((LispCharPtr)object);
}
#endif

#endif

