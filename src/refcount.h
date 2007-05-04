#ifndef __refcount_h__
#define __refcount_h__

#include "lispassert.h"
#include "yacasbase.h"

//------------------------------------------------------------------------------
// RefPtr - Smart pointer for (intrusive) reference counting.
// Simply, an object's reference count is the number of RefPtrs refering to it.
// The RefPtr will delete the referenced object when the count reaches zero.

/*TODO: this might be improved a little by having RefPtr wrap the object being 
  pointed to so the user of RefPtr does not need to add ReferenceCount explicitly.
  One can use RefPtr on any arbitrary object from that moment on.
 */

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
   bool operator!() const { return !m_count; }	// is zero
   operator int() const { return m_count; }
private:
   ReferenceType m_count;
};

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

