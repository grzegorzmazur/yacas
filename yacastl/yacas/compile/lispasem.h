
#ifndef __lispasem_h__
#define __lispasem_h__


#include "standard.h"
#include "lispenvironment.h"
#include "lispeval.h"
#include "native.h"

extern LispPtr *stackCurrent;
extern LispNativeFunctions native;

//const LispObjectAdder MakeAtom(LispEnvironment& aEnvironment, char* s);
//const LispObjectAdder MakeList(LispObject* obj);

LispObject* MakeAtom(LispEnvironment& aEnvironment, char* s);
LispObject* MakeAtomInt(LispEnvironment& aEnvironment, int i);
LispInt MakeGetInteger(LispPtr& aVal);
LispObject* MakeList(LispObject* obj);
LispObject* AtomAdd(LispObject* obj1, LispObject* obj2);
void MakeLocal(LispEnvironment& aEnvironment, char* var);
void MakePop();
void MakeEval(LispEnvironment& aEnvironment,LispPtr& aTrg, LispPtr& aSrc);
LispInt MakePushArguments(LispEnvironment& aEnvironment, LispPtr& aArguments);

void MakeWrite(LispEnvironment& aEnvironment, LispPtr& aObject);
void DebugShowStack(char* str, LispEnvironment& aEnvironment);

#define MAKEATOM(_s) MakeAtom(aEnvironment, _s)
#define MAKEINT(_s) MakeAtomInt(aEnvironment, _s)
#define MAKELIST(_s) MakeList(_s)
    


// Enter/exit code for routines
#include <stdio.h>
#define ENTERFILE(_f) printf("file %s",_f)
inline void shst(char* name)
{
//    printf("%s = %d\n",name,(int)(stackCurrent-stack));
}
#define  PROLOG()   { shst("ENTER"); LispLocalFrame frame(aEnvironment,LispFalse); LispPtr* bottom=stackCurrent;
#define  EPILOG()   shst("LEAVE"); while (stackCurrent != bottom) {stackCurrent--; (*stackCurrent) = (NULL);} }

// Stack manipulation
#define  VAR(_n)    (*(bottom[_n]))
#define  TOP(_n)    (*(stackCurrent-1+(_n)))
#define  TOPPTR()   stackCurrent
#define  PUSH()     stackCurrent++;
#define  POP()      MakePop()
// means pop and then push, which sets top of stack to null pointer
#define  CLEAR(_n) (*bottom[_n]) = (NULL);


#define  EVALUATE(_trg,_src) MakeEval(aEnvironment,_trg, _src)
#define  B_ISTRUE(_e)  IsTrue(_e)

inline LispObject* Object(const LispPtr& aObj) {return aObj;}
inline LispObject* Object(const LispPtr* aObj) {return (*aObj);}
inline LispObject* Object(const LispObjectAdder& aObj) {return aObj.iPtr;}
inline LispObject* Object(const LispObjectAdder* aObj) {return aObj->iPtr;}

// Setting values
#define  SET(_trg,_src)      _trg = (Object(_src));
#define  COPY(_trg,_src)     _trg = (_src);
#define  SETTRUE(_trg)       InternalTrue(aEnvironment, _trg)
#define  SETFALSE(_trg)      InternalFalse(aEnvironment, _trg)

#define  SETVAR(_var,_src)   aEnvironment.SetVariable(_var->String(), _src)
#define  SETVARSTR(_var,_src)   aEnvironment.SetVariable(aEnvironment.HashTable().LookUp(_var), _src)
#define LOCAL(_s)   MakeLocal(aEnvironment, _s)


#endif // __lispasem_h__


