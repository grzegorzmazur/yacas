
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
    iObjects.SetNrItems(0);
}

void LispCleanup::CheckStackEmpty()
{
    LISPASSERT(iObjects.NrItems() == 0);
}

void DeletingLispCleanup::Push(LispBase& aObject)
{
    iObjects.Append(&aObject);
}

void DeletingLispCleanup::Pop()
{
    iObjects.SetNrItems(iObjects.NrItems()-1);
}

void DeletingLispCleanup::Delete()
{
    LispInt i;
    for (i=iObjects.NrItems()-1;i>=0;i--)
    {
        iObjects[i]->Delete();
    }
    iObjects.SetNrItems(0);
}


