// Macro to check if where we want to move to is valid, and make it
//  valid if not.
#define AdjustWhere(i) if(i < 0) i = 0; if(i > count) i = count

//Default memory management functions
template <class T>
T *List<T>::allocElem(int size)
{
	T *elem = (T *) malloc(size);
	return elem;
}

template <class T>
T *List<T>::copyElem(T *to, T *from, int size)
{
	while(size--)
		*to++ = *from++;
	return to;
}

template <class T>
inline void List<T>::freeElem(T *elem)
{
	free(elem);
}

// constructors and destructors -- nothing fancy
template <class T>
List<T>::List() : 
head(0), tail(0), count(0) {}

template <class T>
List<T>::List(T *d) : 
tail(0), count(0)
{
	head = newNode(0, 0);
	head->data = allocAndCopyElem(head->data, d)
}

template <class T>
List<T>::~List()
{
	while(count)
		delNode(tail);
}

// newNode -- all nodes must be made from here, otherwise, be prepared to get cozy
//			  with the debugger.
// If links are provided, the new node will be linked appropriately.
template <class T>
List<T>::LinkNode *List<T>::newNode(LinkNode *next, LinkNode *prev)
{
	LinkNode *ln = new LinkNode;
	if(prev) {
		ln->prev = prev;
		prev->next = ln;
	} else
		ln->prev = 0;

	if(next) {
		ln->next = next;
		next->prev = ln;
	} else
		ln->next = 0;

	count++;
	return ln;
}

// delNode -- delete a node. 
// If it is a head or tail node, set things up, otherwise, unlink the node and nuke.
template <class T>
void List<T>::delNode(LinkNode *deletee)
{
	if(deletee == head) {
		if(head->next) {
			head = head->next;
			head->prev = 0;
		}
		goto end;
	}else if(deletee == tail) {
		if(tail->prev) {
			tail = tail->prev;
			tail->next = 0;
		}
		goto end;
	}

	deletee->prev->next = deletee->next;
	deletee->next->prev = deletee->prev;

end:
	freeElem(deletee->data);
	delete deletee;
	count--;
}

// allocAndCopyElem -- allocate and copy data to an element
// It determines the size of memory to allocate from sizeof(from) * sizeof(T)
//   with the result being assigned to 'to' =)
template <class T>
T *List<T>::allocAndCopyElem(T *to, T *from)
{
	to = allocElem(sizeof(from));
	copyElem(to, from, sizeof(from));
	return to;
}

// addHead -- insert a new head node
template <class T>
bool List<T>::addHead(T *d)
{
	LinkNode *ln = newNode(head, 0);
	ln->data = allocAndCopyElem(ln->data, d);

	head = ln;
	if(!tail)
		tail = head;
	return true;
}

// addTail -- insert a new tail node
template <class T>
bool List<T>::addTail(T *d)
{
	LinkNode *ln = newNode(0, tail);	
	ln->data = allocAndCopyElem(ln->data, d);

	tail = ln;
	if(!head)
		head = tail;
	return true;
}

//insert* -- insert an element into the list.
template <class T>
void List<T>::insertAfter(T *d, int where)
{
	if(where >= count)
		return addTail(elem);

	LinkNode *tmp = getNode(where);
	LinkNode *ln = newNode(tmp->next, tmp);
	ln->data = allocAndCopyElem(ln->data, d);
}

template <class T>
void List<T>::insertBefore(T *d, int where)
{
	if(where <= 0)
		return addHead(elem);

	LinkNode *tmp = getNode(where);
	LinkNode *ln = newNode(tmp, tmp->prev);
	ln->data = allocAndCopyElem(ln->data, d);
}

template <class T>
inline int List<T>::getCount()
{
	return count;
}

// getNode -- moves to given index, which is adjusted for bounds,
//			  and returns the node there. 
// It first checks to see if it is trying to grab the head or tail,
//   and if so, returns it.  Otherwise, it divides the number of nodes
//	 by 2, then moves inwards from the end closer to it.
template <class T>
List<T>::LinkNode *List<T>::getNode(int where)
{
	int i = where, countq = where/2;
	LinkNode *tmp;

	AdjustWhere(i);

	if(i == 0)
		return head;
	else if(i == count)
		return tail;

	if(i > countq){
		tmp = tail;
		while(i--){
			if(tmp->prev == 0)
				return head;
			tmp = tmp->next;
		}
	} else if(i <= countq){
		tmp = head;
		while(i--){
			if(tmp->prev == 0)
				return tail;
			tmp = tmp->next;
		}
	}

	return tmp;
}

//moveElem* -- move an element around in the list.
template <class T>
void List<T>::moveElemAfter(int where, int to)
{
	LinkNode *i = getNode(where);
	LinkNode *j = getNode(to);

	// Not moving anywhere
	if(where == to)
		return;

	if(i == tail) {
		tail = tail->prev;
		tail->next = 0;
	} else if(i == head) {
		head = head->next;
		head->prev = 0;
	}

	if(j == tail) {
		i->next = 0;
		i->prev = tail;
		i->prev->next = i;
	} else {
		i->next = j->next;
		i->prev = j;
		i->prev->next = i;
	}
}

template <class T>
void List<T>::moveElemBefore(int where, int to)
{
	LinkNode *i = getNode(where);
	LinkNode *j = getNode(to);

	// Not moving anywhere
	if(where == to)
		return;

	if(i == tail) {
		tail = tail->prev;
		tail->next = 0;
	} else if(i == head) {
		head = head->next;
		head->prev = 0;
	}

	if(j == head) {
		i->prev = 0;
		i->next = head;
		i->next->prev = i;
	} else {
		i->prev = j->prev;
		i->next = j;
		i->next->prev = i;
	}

}

// remove* -- unlink and free a node.
template <class T>
inline void List<T>::removeAt(int where)
{
	LinkNode *nukee = moveTo(where);
	delNode(nukee);
}

template <class T>
inline void List<T>::removeHead()
{
	delNode(head);
}

template <class T>
inline void List<T>::removeTail()
{
	delNode(tail);
}

/////////////////////////////////
// Iter class begins here //
/////////////////////////////////

// constructors and destructors -- nothing fancy
template <class T>
ListIter<T>::ListIter(List<T> *l) : 
current(l->getNode(0)), list(l) {}

template <class T>
ListIter<T>::~ListIter() {}

// is* -- returns true if there is a node linked to current in the
//   way you are going, else false
template <class T>
bool ListIter<T>::isNext()
{
	if(current->next != 0)
		return true;
	return false;
}

template <class T>
bool ListIter<T>::isPrev()
{
	if(current->prev != 0)
		return true;
	return false;
}

// getCount -- returns number of nodes in the list
template <class T>
inline int ListIter<T>::getCount()
{
	return list->getCount();
}

// getCurrent -- returns the data for current node

template <class T>
inline T *ListIter<T>::getCurrent()
{
	return current->data;
}

// getNext, getPrev -- returns the data for the next or previous 
// node.
// If the node cannot go any farther (is at the head), it will
//   return the data of the head or tail node.
template <class T>
T *ListIter<T>::getNext()
{
	if(isNext()){
		current = current->next;
		return current->data;
	}
	return current->data;
}

template <class T>
T *ListIter<T>::getPrev()
{
	if(isPrev()){
		current = current->prev;
		return current->data;
	}
	return current->data;
}

// getHead, getTail -- returns the data for the head or tail node
// getAt -- moves to index given, and returns the data

template <class T>
T *ListIter<T>::getHead()
{
	current = list->getNode(0);
	return current->data;
}

template <class T>
T *ListIter<T>::getTail()
{
	current = list->getNode(list->getCount());
	return current->data;
}

template <class T>
T *ListIter<T>::getAt(int where)
{
	current = list->getNode(where);
	return current->data;
}

// set* -- set the data of the current node
// The current node can be moved by some of the functions 
//	 (setHead, setTail, etc...) 
template <class T>
void ListIter<T>::setHead(T *d)
{
	current = list->getNode(0);
	list->freeElem(current->data);
	current->data = list->allocElem(sizeof(*d));
	list->copyElem(current->data, d, sizeof(*d));
}

template <class T>
void ListIter<T>::setTail(T *d)
{
	current = list->getNode(list->getCount());
	list->freeElem(current->data);
	current->data = list->allocElem(sizeof(*d));
	list->copyElem(current->data, d, sizeof(*d));
}

template <class T>
void ListIter<T>::setAt(T *d, int where)
{
	current = list->getNode(where);
	list->freeElem(current->data);
	current->data = list->allocElem(sizeof(*d));
	list->copyElem(current->data, d, sizeof(*d));
}