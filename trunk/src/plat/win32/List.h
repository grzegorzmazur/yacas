// List.h: interface for the List class.
//
// List is double-linked list class, which can also be used as a
//   single-linked list.
//
// ListIter is the iterator for the List class.
//
// (C) 1999 James Gilbertson   azurite@telusplanet.net
//////////////////////////////////////////////////////////////////////

#ifndef _List_h
#define _List_h 1

template <class T>
class List
{
public:
  // The link node struct
  struct LinkNode{
    T* data;
    LinkNode *next;
    LinkNode *prev;
  };

// The interface of the List class
public:
  List<T>();                // Empty constructor
  List<T>(T *d);              // Creates a list with 1 node

  ~List();                // Destroys the list

  bool addHead(T *d);            // Add an element to either end
  bool addTail(T *d);            // of the list

  void insertAfter (T *d, int where);    // Insert an element
  void insertBefore(T *d, int where);

  void moveElemAfter(int where, int to);  // Move an element around
  void moveElemBefore(int where, int to);

  void removeHead();            // Remove an element
  void removeTail();
  void removeAt(int where);

  int getCount();              // Get total number of elements
  LinkNode *getNode(int where);      // Get a node at the given index
 
  // Memory management functions
  // Overide by providing a specific type template function
  //  i.e. char *List<char>::allocElem(int size)

  T *allocElem(int size);
  T *copyElem(T *to, T *from, int size);
  void freeElem(T *elem);

private:
  // Creates all new nodes in the list.
  LinkNode *newNode(LinkNode *prev, LinkNode *next);
  // Useful function that allocates space for and copys data
  T* allocAndCopyElem(T *to, T *from);
  // Destroys a node in the list.
  void delNode(LinkNode *deletee);

  LinkNode *head;
  LinkNode *tail;

  int count;                // Number of elems in list
};

// The iterator class
template <class T>
class ListIter
{
public:
  ListIter<T>(List<T> *l);
  virtual ~ListIter();
 
  bool isNext();                  // True if more nodes ahead
  bool isPrev();                  // True if more nodes behind
  int getCount();                 // Returns number of nodes

  T* getCurrent();
  T* getNext();
  T* getPrev();
  T* getHead();
  T* getTail();
  T* getAt(int where);
 
  void setHead(T *d);
  void setTail(T *d);
  void setAt(T *d, int where);
 
  List<T> *list;
private:
  List<T>::LinkNode *current;
};

#include "List.inl"

