
#include "yacasprivate.h"
#include "lispobject.h"
//#include "lispatom.h"	// LispAnnotatedObject
#include "codecomment.h"

#ifdef YACAS_DEBUG
long theNrCurrent=0;
long theNrConstructed=0;
long theNrDestructed=0;
void IncNrObjects()
{
    theNrConstructed++;
    theNrCurrent++;
}
void DecNrObjects()
{
    theNrDestructed++;
    theNrCurrent--;
}
#endif

LispObject::~LispObject()
{
   DecNrObjects_destructor();
}

LispInt LispObject::Equal(LispObject& aOther)
{
    // next line handles the fact that either one is a string
    if (String() != aOther.String())
        return 0;	// return false

    //So, no strings.
    LispPtr *iter1 = SubList();
    LispPtr *iter2 = aOther.SubList();
    LISPASSERT(!!iter1 && !!iter2);

    // check all elements in sublist
    while (!!(*iter1) && !!(*iter2))
    {
        if (! (*iter1)->Equal(*(*iter2) ))
            return 0;
    }
    //One list longer than the other?
    if (!(*iter1) && !(*iter2))
        return 1;
    return 0;
}

#if HAS_NEW_LispPtrArray == 0
LispPtrArray::LispPtrArray(LispInt aSize,LispObject* aInitialItem)
{
    iSize=aSize;
    iArray = NEW LispPtr[aSize];
	if (aInitialItem)
		for (LispInt i=0;i<aSize;i++)
		{
			iArray[i] = (aInitialItem);
		}
}

LispPtrArray::~LispPtrArray()
{
    delete [] iArray;
}
#endif

/*PLEASECHECK TODO AYAL: Why do we do this here? I can imagine having one function somewhere where the code-comment string gets
  constructed on the fly based on whether defines are enabled or not.
 */
#define STR(tokens) #tokens
#define SHOWSTR(ctce) #ctce " = " STR(ctce)
#if defined(YACAS_DEBUG)
namespace{CodeComment varnameA("YACAS_DEBUG");}
#endif
#if defined(USE_ASSERT)
namespace{CodeComment varnameB("USE_ASSERT");}
#endif
#if defined(NO_EXCEPTIONS)
namespace{CodeComment varnameC("NO_EXCEPTIONS");}
#endif
namespace{
	//CodeComment varname6("ObjectHelper and no param to LispObject::Copy()");
}
