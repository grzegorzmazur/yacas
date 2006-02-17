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

#if 0	// Apparently makes "extern RefPtr<LispObject> graph; if (graph) ..." ambiguous.
		// TODO: woof - Try adding an "operator bool()"?
   // prevent application of delete
   operator void*() const;
#endif

#if 0	// useful, but not (yet) needed
public:
	// Construct from pointer to Y
	template<class Y>
    explicit RefPtr(Y *ptr) : m_ptr(ptr)
	{ if (ptr) ++ptr->iReferenceCount; }	// should use m_ptr

	// Copy constructor from RefPtr<U>
    template<class U>
    explicit RefPtr(const RefPtr<U> &refPtr) : m_ptr(refPtr.ptr())
	{ if (m_ptr) ++m_ptr->iReferenceCount; }

	// Assignment from pointer to U
	template<class U>
    RefPtr &operator=(U *ptr)
	{ if (ptr) ++ptr->iReferenceCount; if (m_ptr && !--m_ptr->iReferenceCount) delete m_ptr; m_ptr = ptr; return *this; }

	// Assignment from a RefPtr<U>
    template<class U>
    RefPtr &operator=(const RefPtr<U> &refPtr)
    { return this->operator=(refPtr.ptr()); }
#endif
};

//------------------------------------------------------------------------------
// RefCntMixin - Mixin that implements a reference count for RefPtr.
// Thus, the reference counts are embedded in the pointee.
// For example, declare your class:
//		class MyClass : public RefCntMixin,PossibilySomeBaseClass {...};

#if 0
class RefCntMixin
{
public:
    typedef ReferenceType value_type;
    RefCntMixin() : m_count(0) {}
    void IncreaseRefCount() { LISPASSERT(m_count<ReferenceMax); m_count++; }
    value_type DecreaseRefCount() { return --m_count; }
    value_type ReferenceCount() const { return m_count; }
private:
    value_type m_count;
};
#endif

//------------------------------------------------------------------------------
// A reference-counted Yacas object.

#if 0
class RefCountedObject : /*public RefCntMixin,*/ public YacasBase
{
public:
	ReferenceCount iReferenceCount;
};
#endif

#endif

