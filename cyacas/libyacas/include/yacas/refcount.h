#ifndef YACAS_REFCOUNT_H
#define YACAS_REFCOUNT_H

#include <cassert>

#include "lisptype.h"

//------------------------------------------------------------------------------
// RefPtr - Smart pointer for (intrusive) reference counting.
// Simply, an object's reference count is the number of RefPtrs refering to it.
// The RefPtr will delete the referenced object when the count reaches zero.

/*TODO: this might be improved a little by having RefPtr wrap the object being
  pointed to so the user of RefPtr does not need to add ReferenceCount explicitly.
  One can use RefPtr on any arbitrary object from that moment on.
 */

template<class T>
class RefPtr {
public:
  // Default constructor (not explicit, so it auto-initializes)
  inline RefPtr() : iPtr(nullptr) {}
  // Construct from pointer to T
  /*explicit*/ RefPtr(T* ptr) : iPtr(ptr) { if (ptr) { ptr->_use_count++; } }
  // Copy constructor
  RefPtr(const RefPtr &refPtr) : iPtr(refPtr.ptr()) { if (iPtr) { iPtr->_use_count++; } }
  // Destructor
  ~RefPtr()
  {
      if (iPtr && !--iPtr->_use_count)
        delete iPtr;
  }
  // Assignment from pointer
  RefPtr &operator=(T *ptr)
  {
    if (ptr)
      ptr->_use_count++;

    if (iPtr && !--iPtr->_use_count)
      delete iPtr;

    iPtr = ptr;

    return *this;
  }
  // Assignment from another
  RefPtr &operator=(const RefPtr &refPtr) { return this->operator=(refPtr.ptr()); }

  operator T*()    const { return  iPtr; }  // implicit conversion to pointer to T
  T &operator*()   const { return *iPtr; }  // so (*refPtr) is a reference to T
  T *operator->()  const { return  iPtr; }  // so (refPtr->member) accesses T's member
  T *ptr()         const { return  iPtr; }  // so (refPtr.ptr()) returns the pointer to T (boost calls this method 'get')
  bool operator!() const { return !iPtr; }  // is null pointer

private:
   T *iPtr;
};

class RefCount {
public:
  RefCount(): _use_count(0) {}
  RefCount(const RefCount&): _use_count(0) {}

  virtual ~RefCount() = default;

  RefCount& operator = (const RefCount&) { return *this; }

  unsigned use_count() const { return _use_count; }

private:
  template <typename T> friend class RefPtr;

  mutable unsigned _use_count;
};


#endif

