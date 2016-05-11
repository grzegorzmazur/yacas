/*
 * Copyright (C) 2016 Grzegorz Mazur.
 *
 * Yacas is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * Yacas is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301  USA
 */

#include "yacas/associationclass.h"

LispPtr AssociationClass::Keys() const
{
    LispPtr head(LispAtom::New(const_cast<LispEnvironment&>(_env), "List"));
    LispPtr p(head);
    for (std::map<Key, LispPtr>::const_reference e: _map) {
        p->Nixed() = e.first.value->Copy();
        p = p->Nixed();
    }
    return LispPtr(LispSubList::New(head));
}

LispPtr AssociationClass::ToList() const
{
    LispPtr head(LispAtom::New(const_cast<LispEnvironment&>(_env), "List"));
    LispPtr p(head);
    for (std::map<Key, LispPtr>::const_reference e: _map) {
        LispPtr q(LispAtom::New(const_cast<LispEnvironment&>(_env), "List"));
        p->Nixed() = LispSubList::New(q);
        p = p->Nixed();
        q->Nixed() = e.first.value->Copy();
        q = q->Nixed();
        q->Nixed() = e.second->Copy();
    }
    return LispPtr(LispSubList::New(head));
}

LispPtr AssociationClass::Head() const
{
    assert(_map.size());
    
    std::map<Key, LispPtr>::const_reference e = *_map.begin();
    LispPtr p(LispAtom::New(const_cast<LispEnvironment&>(_env), "List"));
    LispPtr q(p);
    q->Nixed() = e.first.value->Copy();
    q = q->Nixed();
    q->Nixed() = e.second->Copy();
    return LispPtr(LispSubList::New(p));
}
