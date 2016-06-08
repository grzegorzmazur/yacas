
#include "yacas/yacasprivate.h"
#include "yacas/lispobject.h"

LispInt LispObject::Equal(LispObject& aOther)
{
    // next line handles the fact that either one is a string
    if (String() != aOther.String())
        return 0;  // return false

    //So, no strings.
    LispPtr *iter1 = SubList();
    LispPtr *iter2 = aOther.SubList();
    assert(!!iter1 && !!iter2);

    // check all elements in sublist
    while (!!(*iter1) && !!(*iter2))
    {
        if (! (*iter1)->Equal(*(*iter2) ))
            return 0;
      iter1 = &(*iter1)->Nixed();
      iter2 = &(*iter2)->Nixed();
    }
    //One list longer than the other?
    if (!(*iter1) && !(*iter2))
        return 1;
    return 0;
}


