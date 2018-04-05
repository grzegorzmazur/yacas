/*
 * This file is part of yacas.
 * Yacas is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesset General Public License as
 * published by the Free Software Foundation, either version 2.1
 * of the License, or (at your option) any later version.
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
 * File:   lispoperator.h
 * Author: mazur
 *
 * Created on February 12, 2016, 3:05 PM
 */

#ifndef LISPOPERATOR_H
#define LISPOPERATOR_H

#include <unordered_map>

#ifdef YACAS_NO_CONSTEXPR
const int KMaxPrecedence = 60000;
#else
constexpr int KMaxPrecedence = 60000;
#endif

class LispInFixOperator {
public:
    explicit
#ifndef YACAS_NO_CONSTEXPR
    constexpr
#endif
    LispInFixOperator(int aPrecedence = KMaxPrecedence):
        iPrecedence(aPrecedence),
        iLeftPrecedence(aPrecedence),
        iRightPrecedence(aPrecedence),
        iRightAssociative(false)
    {}

    void SetRightAssociative()
    {
        iRightAssociative = true;
    }

    void SetLeftPrecedence(int aPrecedence)
    {
        iLeftPrecedence = aPrecedence;
    }

    void SetRightPrecedence(int aPrecedence)
    {
        iRightPrecedence = aPrecedence;
    }

    int iPrecedence;
    int iLeftPrecedence;
    int iRightPrecedence;
    bool iRightAssociative;
};

typedef std::unordered_map<const LispStringSmartPtr, LispInFixOperator, std::hash<const LispString*> > LispOperators;

#endif

