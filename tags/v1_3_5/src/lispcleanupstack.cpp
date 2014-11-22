
#include "yacas/yacasprivate.h"
#include "yacas/lispcleanupstack.h"
#include "yacas/lisptype.h"

#include <cassert>

void LispCleanup::Push(LispBase& aObject)
{
}

void LispCleanup::Pop()
{
}

void LispCleanup::Delete()
{
    iObjects.clear();
}

void LispCleanup::CheckStackEmpty()
{
    assert(iObjects.size() == 0);
}

void DeletingLispCleanup::Push(LispBase& aObject)
{
    iObjects.push_back(&aObject);
}

void DeletingLispCleanup::Pop()
{
    iObjects.pop_back();
}

void DeletingLispCleanup::Delete()
{
    while (!iObjects.empty()) {
        iObjects.back()->Delete();
        iObjects.pop_back();
    }
}
