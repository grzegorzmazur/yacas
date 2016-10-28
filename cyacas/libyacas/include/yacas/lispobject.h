/** \file lispobject.h
 *  Implementation of basic LispObject, which is the base class for
 *  anything that can be put in a linked list.
 *
 * class LispObject. This class implements one lisp object, which is a
 * abstract class containing reference counting and a next pointer.
 * derive from this to implement specific types of list elements.
 * The local class LispPtr implements automatic garbage collection
 * through reference counting.
 *
 */

#ifndef YACAS_LISPOBJECT_H
#define YACAS_LISPOBJECT_H

#include "refcount.h"
#include "lispstring.h"
#include "genericobject.h"
#include "noncopyable.h"
#include "stubs.h"

class LispObject;
class BigNumber;


/** class LispPtr. A LispPtr is a smart pointer to a LispObject.
 *  It does the reference counting, and consequent destruction if needed.
 *  LispPtr is used in LispObject as a pointer to the next object; and
 *  LispPtr::GoNext() advances one step in this LispObject::Nixed() chain.
 *  Diverse built-in functions use LispPtr to hold temporary values.
 */
typedef RefPtr<LispObject> LispPtr;

/** class LispObject is the base object class that can be put in
 *  linked lists. It either has a pointer to a string, obtained through
 *  String(), or it is a holder for a sublist, obtainable through SubList(),
 *  or it is a generic object, in which case Generic() returns non-nullptr.
 *  Only one of these three functions should return a non-nullptr value.
 *  It is a reference-counted object. LispPtr handles the reference counting.
 */
class LispObject
{
public:
  inline LispPtr& Nixed();

public: //Derivables
  virtual ~LispObject() = default;

  /** Return string representation, or nullptr if the object doesn't have one.
   *  the string representation is only relevant if the object is a
   *  simple atom. This method returns nullptr by default.
   */
  virtual const LispString* String()  { return nullptr; }
  /** If this object is a list, return a pointer to it.
   *  Default behaviour is to return nullptr.
   */
  virtual LispPtr* SubList()      { return nullptr; }
  virtual GenericClass* Generic() { return nullptr; }

  /** If this is a number, return a BigNumber representation
   */
  virtual BigNumber* Number(int aPrecision) { return nullptr; }

  virtual LispObject* Copy() const = 0;

public:
  int Equal(LispObject& aOther);
  inline int operator==(LispObject& aOther);
  inline int operator!=(LispObject& aOther);
protected:
  inline LispObject() :
   iNext(),iReferenceCount()
  {
  }
  inline LispObject(const LispObject& other) :
  iNext(),iReferenceCount()
  {
  }

  inline LispObject& operator=(const LispObject& other)
  {
    return *this;
  }


private:
  LispPtr   iNext;
public:
  unsigned iReferenceCount;
  
  static inline void* operator new(size_t size) { return PlatAlloc(size); }
  static inline void* operator new[](size_t size) { return PlatAlloc(size); }
  static inline void operator delete(void* object) { PlatFree(object); }
  static inline void operator delete[](void* object) { PlatFree(object); }
  // Placement form of new and delete.
  static inline void* operator new(size_t, void* where) { return where; }
  static inline void operator delete(void*, void*) {}

};

/**
 * class LispIterator works almost like LispPtr, but doesn't enforce
 * reference counting, so it should be faster.  Use LispIterator
 * (instead of LispPtr) to traverse a lisp expression non-destructively.
 */
class LispIterator {
public:
  // ala TEMPLATE CLASS iterator
  //typedef forward_iterator_tag iterator_category;
  typedef LispPtr    value_type;
  typedef int /*ptrdiff_t*/  difference_type;
  typedef LispPtr*  pointer;
  typedef LispPtr&  reference;
public:
  LispIterator() : _Ptr(0) {}  // construct with null node pointer
  LispIterator(pointer ptr) : _Ptr(ptr) {}  // construct with node pointer
  /*non-standard*/ LispIterator(reference ref) : _Ptr(&ref) {}  // construct with node reference
  reference operator*() const { return (*(_Ptr)); }  // return designated value
  pointer operator->() const { return (_Ptr); }  // return pointer to class object
  inline LispIterator& operator++()  // preincrement
  {
    //precondition: _Ptr != nullptr
    assert(_Ptr != nullptr);
    //expand: _Ptr = _Nextnode(_Ptr);
    LispObject * pObj = _Ptr->operator->();
    _Ptr = pObj ? &(pObj->Nixed()) : nullptr;
    return (*this);
  }
  LispIterator operator++(int) { LispIterator _Tmp = *this; ++*this; return (_Tmp); }  // postincrement
  bool operator==(const LispIterator& other) const { return (_Ptr == other._Ptr); }  // test for iterator equality
  bool operator!=(const LispIterator& other) const { return (!(*this == other)); }  // test for iterator inequality
  // The following operators are not used yet, and would need to be tested before used.
  //LispIterator& operator--() { _Ptr = _Prevnode(_Ptr); return (*this); }  // predecrement
  //LispIterator operator--(int) { LispIterator _Tmp = *this; --*this; return (_Tmp); }  // postdecrement
protected:
  pointer _Ptr;  // pointer to node
public:
  inline LispObject* getObj() const { return (*_Ptr).operator->(); }
};

class LispConstIterator {
public:
  // ala TEMPLATE CLASS iterator
  //typedef forward_iterator_tag iterator_category;
  typedef const LispPtr    value_type;
  typedef int /*ptrdiff_t*/  difference_type;
  typedef const LispPtr*  pointer;
  typedef const LispPtr&  reference;
public:
  LispConstIterator() : _Ptr(0) {}  // construct with null node pointer
  LispConstIterator(pointer ptr) : _Ptr(ptr) {}  // construct with node pointer
  /*non-standard*/ LispConstIterator(reference ref) : _Ptr(&ref) {}  // construct with node reference
  reference operator*() const { return (*(_Ptr)); }  // return designated value
  pointer operator->() const { return (_Ptr); }  // return pointer to class object
  inline LispConstIterator& operator++()  // preincrement
  {
    //precondition: _Ptr != nullptr
    assert(_Ptr != nullptr);
    //expand: _Ptr = _Nextnode(_Ptr);
    LispObject * pObj = _Ptr->operator->();
    _Ptr = pObj ? &(pObj->Nixed()) : nullptr;
    return (*this);
  }
  LispConstIterator operator++(int) { LispConstIterator _Tmp = *this; ++*this; return (_Tmp); }  // postincrement
  bool operator==(const LispConstIterator& other) const { return (_Ptr == other._Ptr); }  // test for iterator equality
  bool operator!=(const LispConstIterator& other) const { return (!(*this == other)); }  // test for iterator inequality
  // The following operators are not used yet, and would need to be tested before used.
  //LispConstIterator& operator--() { _Ptr = _Prevnode(_Ptr); return (*this); }  // predecrement
  //LispConstIterator operator--(int) { LispConstIterator _Tmp = *this; --*this; return (_Tmp); }  // postdecrement
protected:
  pointer _Ptr;  // pointer to node
public:
  inline const LispObject* getObj() const { return (*_Ptr).operator->(); }
};

inline LispPtr& LispObject::Nixed()
{
    return iNext;
}

inline int LispObject::operator==(LispObject& aOther)
{
    return Equal(aOther);
}

inline int LispObject::operator!=(LispObject& aOther)
{
    return !Equal(aOther);
}


#endif
