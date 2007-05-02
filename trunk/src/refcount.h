#ifndef __refcount_h__
#define __refcount_h__

#include "lispassert.h"
#include "yacasbase.h"

//------------------------------------------------------------------------------
// RefPtr - Smart pointer for (intrusive) reference counting.
// Simply, an object's reference count is the number of RefPtrs refering to it.
// The RefPtr will delete the referenced object when the count reaches zero.
// Note that the pointee needn't use RefCntMixin (below), it merely needs to
// behave like it does (i.e., RefPtr<T>::setPtr(T* ptr) compiles).
// TODO: We could someday use boost::shared_pointer, but SO much machinery....

class ReferenceCount
{
public:
   ReferenceCount() : m_count(0) {}
   ~ReferenceCount() { LISPASSERT(m_count == 0); }
   // Note that this *ignores* the thing that's being copied
   ReferenceCount(const ReferenceCount &) : m_count(0) {}
   ReferenceCount &operator=(const ReferenceCount &) { return *this; }
   // pre-increment/decrement only
   ReferenceCount &operator++() { ++m_count; return *this; }
   ReferenceCount &operator--() { LISPASSERT(m_count > 0); --m_count; return *this; }
   // testing
   //BAD IDEA! operator bool() const { return !!m_count; }	// is non-zero
   bool operator!() const { return !m_count; }	// is zero
   operator int() const { return m_count; }
   //operator int() const  { return m_count; }
   //bool isShared() const { return m_count > 1; }
private:
   ReferenceType m_count;
};

//#define NOIL inline /*__declspec(noinline)*/

template<class T>
class RefPtr : public YacasBase	// derived, so we can 'NEW LispPtr[nnn]'
{
public:
	// Default constructor (not explicit, so it auto-initializes)
	RefPtr() : m_ptr(0) {}

	// Construct from pointer to T
    explicit RefPtr(T *ptr) : m_ptr(ptr)
	{ if (ptr) ++ptr->iReferenceCount; }	// should use m_ptr

	// Copy constructor
	RefPtr(const RefPtr &refPtr) : m_ptr(refPtr.ptr())
	{ if (m_ptr) ++m_ptr->iReferenceCount; }

	// Destructor
	~RefPtr()
	{ if (m_ptr && !--m_ptr->iReferenceCount) delete m_ptr; }

	// Assignment from pointer
    RefPtr &operator=(T *ptr)
	{ if (ptr) ++ptr->iReferenceCount; if (m_ptr && !--m_ptr->iReferenceCount) delete m_ptr; m_ptr = ptr; return *this; }

	// Assignment from another
	RefPtr &operator=(const RefPtr &refPtr)
    { return this->operator=(refPtr.ptr()); }

    operator T*()    const { return  m_ptr; }	// implicit conversion to pointer to T
    T &operator*()   const { return *m_ptr; }	// so (*refPtr) is a reference to T
    T *operator->()  const { return  m_ptr; }	// so (refPtr->member) accesses T's member
    T *ptr()         const { return  m_ptr; }	// so (refPtr.ptr()) returns the pointer to T (boost calls this method 'get')
    bool operator!() const { return !m_ptr; }	// is null pointer

private:
   T *m_ptr;
};


#endif

