
#include "yacasprivate.h"
#include "lispcleanupstack.h"
#include "lispassert.h"
#include "lisptype.h"


LispCleanup::~LispCleanup()
{
}

DeletingLispCleanup::~DeletingLispCleanup()
{
}

void LispCleanup::Push(LispBase& aObject)
{
}

void LispCleanup::Pop()
{
}

void LispCleanup::Delete()
{
    iObjects.ResizeTo(0);
}

void LispCleanup::CheckStackEmpty()
{
    LISPASSERT(iObjects.Size() == 0);
}

void DeletingLispCleanup::Push(LispBase& aObject)
{
    iObjects.Append(&aObject);
}

void DeletingLispCleanup::Pop()
{
    iObjects.ResizeTo(iObjects.Size()-1);
}

void DeletingLispCleanup::Delete()
{
    LispInt i;
    for (i=iObjects.Size()-1;i>=0;i--)
    {
        iObjects[i]->Delete();
    }
    iObjects.ResizeTo(0);
}


