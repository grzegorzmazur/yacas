

#include "lispasem.h"
#include "lispio.h"
#include "infixparser.h"

LispPtr stack[1000];
LispPtr *stackCurrent = stack;
LispNativeFunctions native;

/*
 const LispObjectAdder MakeAtom(LispEnvironment& aEnvironment, char* s)
{
    return LA(ATOML(s));
}
const LispObjectAdder MakeList(LispObject* obj)
{
    return LA(LIST(obj));
}
*/

LispObject* MakeAtom(LispEnvironment& aEnvironment, char* s)
{
    return ATOML(s);
}

LispObject* MakeAtomInt(LispEnvironment& aEnvironment, int i)
{
    LispChar s[30];
    InternalIntToAscii(s, i);
    return ATOML(s);
}

LispInt MakeGetInteger(LispPtr& aVal)
{
//TODO    CHK_ARG(TOP(0).Get() != NULL, 1);
//TODO    CHK_ARG(TOP(0).Get()->String() != NULL, 1);
    return InternalAsciiToInt(aVal.Get()->String()->String());
}

LispObject* MakeList(LispObject* obj)
{
    return LIST(obj);
}
LispObject* AtomAdd(LispObject* obj1, LispObject* obj2)
{
    obj1->Next().Set(obj2);
    return obj1;
}

void MakeLocal(LispEnvironment& aEnvironment, char* var)
{
    aEnvironment.NewLocal(aEnvironment.HashTable().LookUp(var),NULL);
}

void MakePop()
{
    stackCurrent--;
    stackCurrent->Set(NULL);
}
void MakeEval(LispEnvironment& aEnvironment,LispPtr& aTrg, LispPtr& aSrc)
{
    LispPtr r;
    aEnvironment.iEvaluator->Eval(aEnvironment,r,aSrc);
    aTrg.Set(r.Get());
}

LispInt MakePushArguments(LispEnvironment& aEnvironment, LispPtr& aArguments)
{
    LispInt nr=0;

    LispPtr* loop = &aArguments;
    loop = &loop->Get()->Next();
    
    while(loop->Get())
    {
        PUSH();
        nr++;
        SET(TOP(0) ,Argument(aArguments,nr));
        loop = &loop->Get()->Next();
    }
    return nr;
}

void MakeWrite(LispEnvironment& aEnvironment, LispPtr& aObject)
{
    aEnvironment.CurrentPrinter().Print(aObject,
                                        *aEnvironment.CurrentOutput(),
                                        aEnvironment);
}

void DebugShowStack(char* str, LispEnvironment& aEnvironment)
{
    printf("SHOWSTACK %s\n",str);
    LispPtr* ptr = stack;
    LispInt i=0;
    while (ptr<=stackCurrent)
    {
        printf("%d: ",i);
        if (ptr->Get() != NULL)
            MakeWrite(aEnvironment, *ptr);
        printf("\n");
        ptr++;
        i++;
    }
}

