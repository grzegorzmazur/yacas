#include "yacas/yacasprivate.h"
#include "yacas/lisphash.h"

const LispString* LispHashTable::LookUp(const std::string& s)
{
    std::unordered_map<std::string, LispStringSmartPtr>::const_iterator i = _rep.find(s);
    if (i != _rep.end())
        return i->second;

    LispString* ls = new LispString(s);
    ls->iReferenceCount = 1;

    return _rep.insert(std::make_pair(s, ls)).first->second;
}

void LispHashTable::GarbageCollect()
{
    for (auto i = _rep.begin(); i != _rep.end(); ++i)
        while (i != _rep.end() && i->second->iReferenceCount == 1)
            i = _rep.erase(i);
}
