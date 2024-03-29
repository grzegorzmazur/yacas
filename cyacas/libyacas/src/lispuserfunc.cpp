
#include "yacas/lispuserfunc.h"
#include "yacas/standard.h"

LispUserFunction* LispMultiUserFunction::UserFunc(int aArity)
{
    // Find function body with the right arity
    for (LispArityUserFunction* p: iFunctions) {
        assert(p);
        if (p->IsArity(aArity))
            return p;
    }

    // if function not found, just unaccept!
    // User-defined function not found! Returning nullptr
    return nullptr;
}

void LispMultiUserFunction::DeleteBase(int aArity)
{
    // Find function body with the right arity
    const std::size_t nrc = iFunctions.size();
    for (std::size_t i = 0; i < nrc; ++i) {
        assert(iFunctions[i]);
        if (iFunctions[i]->IsArity(aArity)) {
            delete iFunctions[i];
            iFunctions.erase(iFunctions.begin() + i);
            return;
        }
    }
}

LispMultiUserFunction::~LispMultiUserFunction()
{
    for (LispArityUserFunction* p : iFunctions)
        delete p;
}

void LispMultiUserFunction::HoldArgument(const LispString* aVariable)
{
    const std::size_t n = iFunctions.size();
    for (std::size_t i = 0; i < n; ++i) {
        assert(iFunctions[i]);
        iFunctions[i]->HoldArgument(aVariable);
    }
}

void LispMultiUserFunction::DefineRuleBase(LispArityUserFunction* aNewFunction)
{
    // Find function body with the right arity
    const std::size_t nrc = iFunctions.size();
    for (std::size_t i = 0; i < nrc; ++i) {
        assert(iFunctions[i]);
        assert(aNewFunction);
        if (iFunctions[i]->IsArity(aNewFunction->Arity()) ||
            aNewFunction->IsArity(iFunctions[i]->Arity()))
            throw LispErrArityAlreadyDefined();
    }
    iFunctions.push_back(aNewFunction);
}
