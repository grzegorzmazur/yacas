/*
 * This file is part of yacas.
 * Yacas is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesset General Public License as
 * published by the Free Software Foundation, either version 2.1
 *  of the License, or (at your option) any later version.
 *
 * Yacas is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with yacas.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

/* 
 * File:   associationclass.h
 * Author: mazur
 *
 * Created on September 29, 2015, 3:44 PM
 */

#ifndef ASSOCIATIONCLASS_H
#define ASSOCIATIONCLASS_H

#include "lispobject.h"
#include "genericobject.h"
#include "standard.h"

#include <map>

class AssociationClass : public GenericClass
{
public:
    AssociationClass(const LispEnvironment& env);
    const LispChar* TypeName() const override;

    std::size_t Size() const;
    bool Contains(LispObject* k) const;
    LispObject* GetElement(LispObject* k);
    void SetElement(LispObject* k,LispObject* v);
    bool DropElement(LispObject* k);

private:
    class Key {
    public:
        Key(const LispEnvironment& env, LispObject* p):
            env(env), p(p) {}
        
        bool operator == (const Key& rhs) const
        {
            return InternalEquals(env, p, rhs.p);
        }
        
        bool operator < (const Key& rhs) const
        {
            return InternalStrictTotalOrder(env, p, rhs.p);
        }
    private:
        const LispEnvironment& env;
        LispPtr p;
    };

    const LispEnvironment& _env;
    std::map<Key, LispPtr> _map;
};

inline
AssociationClass::AssociationClass(const LispEnvironment& env):
    _env(env)
{
}

inline
const LispChar* AssociationClass::TypeName() const
{
    return "\"Association\"";
}

inline
std::size_t AssociationClass::Size() const
{
    return _map.size();
}

inline
bool AssociationClass::Contains(LispObject* k) const
{
    return _map.find(Key(_env, k)) != _map.end();
}

inline
LispObject* AssociationClass::GetElement(LispObject* k)
{
    auto p = _map.find(Key(_env, k));
    if (p != _map.end())
        return p->second;
    return nullptr;
}

inline
void AssociationClass::SetElement(LispObject* k, LispObject* v)
{
    _map[Key(_env, LispPtr(k))] = v;
}

inline
bool AssociationClass::DropElement(LispObject* k)
{
    return _map.erase(Key(_env, LispPtr(k)));
}

#endif /* ASSOCIATIONCLASS_H */
