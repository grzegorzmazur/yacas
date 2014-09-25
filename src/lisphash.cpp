#include "yacas/yacasprivate.h"
#include "yacas/lisphash.h"

const LispString* LispHashTable::LookUp(const std::string& s)
{
    std::unordered_map<std::string, LispStringSmartPtr>::const_iterator i = _rep.find(s);
    if (i != _rep.end())
        return i->second;

    return _rep.insert(std::make_pair(s, new LispString(s))).first->second;
}

void LispHashTable::GarbageCollect()
{
    typedef std::unordered_map<std::string, LispStringSmartPtr>::iterator iterator;

    for (iterator i = _rep.begin(); i != _rep.end(); ++i)
        while (i != _rep.end() && i->second->iReferenceCount == 1)
            i = _rep.erase(i);
}
