
/** \file lispglobals.h
 *  Storage of globals in a associated hash
 *
 */

#ifndef __lispglobals_h__
#define __lispglobals_h__

#include "yacasbase.h"
#include "lispobject.h"
#include "lisphash.h"



class LispGlobalVariable : public YacasBase
{
public:
    inline LispGlobalVariable(const LispGlobalVariable& aOther);
    LispGlobalVariable(LispPtr& aValue): iValue(aValue) {}
    inline LispGlobalVariable& operator=(const LispGlobalVariable& aOther);

    inline void SetEvalBeforeReturn(LispBoolean aEval);
    LispPtr iValue;
    LispBoolean iEvalBeforeReturn;
};

class LispGlobal : public LispAssociatedHash<LispGlobalVariable>
{
};


inline LispGlobalVariable::LispGlobalVariable(const LispGlobalVariable& aOther)
{
    iValue.Set(aOther.iValue.Get());
    iEvalBeforeReturn = LispFalse;
}

inline void LispGlobalVariable::SetEvalBeforeReturn(LispBoolean aEval)
{
    iEvalBeforeReturn = aEval;
}


inline LispGlobalVariable& LispGlobalVariable::operator=(const LispGlobalVariable& aOther)
{
    iValue.Set(aOther.iValue.Get());
    return *this;
}


#endif

