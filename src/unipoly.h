
#ifndef __unipoly_h__
#define __unipoly_h__

#include "yacasbase.h"
#include "grower.h"

typedef  LispInt ZZ;

class ZZMod : public YacasBase
{
public:
    ZZMod(LispInt aMod);
    inline ZZ Add(ZZ x, ZZ y);
    inline ZZ Sub(ZZ x, ZZ y);
    inline ZZ Mul(ZZ x, ZZ y);
    inline ZZ Div(ZZ x, ZZ y);
    inline ZZ Mod(ZZ x);
private:
    LispInt iMod;
    CArrayGrower<ZZ> iInverses;
};

class ZZPoly : public CArrayGrower<ZZ>
{
public:
    void DropEndZeroes();
    inline ZZ Degree();
    inline void Multiply(const ZZ& x,ZZMod& aMod);
};

class ZZPolyList : public CDeletingArrayGrower<ZZPoly*>
{
};


inline ZZ ZZMod::Add(ZZ x, ZZ y)
{
    return Mod(x+y);
}
inline ZZ ZZMod::Sub(ZZ x, ZZ y)
{
    return Mod(x-y);
}
inline ZZ ZZMod::Mul(ZZ x, ZZ y)
{
    return Mod(x*y);
}
inline ZZ ZZMod::Div(ZZ x, ZZ y)
{
    return Mod(x*iInverses[Mod(y)]);
}
inline ZZ ZZMod::Mod(ZZ x)
{
    ZZ result = x%iMod;
    if (result<0) result+=iMod;
    return result;
}

inline ZZ ZZPoly::Degree()
{
    DropEndZeroes();
    return NrItems()-1;
}


void Berlekamp(ZZPolyList& aResult,ZZPoly& aPoly, ZZ modulo);


#endif
