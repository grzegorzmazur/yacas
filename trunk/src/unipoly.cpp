
// unipoly.cpp unipoly.h mkunipoly

/*TODO
 - ModGcd
 - result computation.
 */


#include "lisptype.h"
#include "unipoly.h"

#define TEST

#ifdef TEST
#include <stdio.h>
void PrintPoly(ZZPoly& p)
{
    int i;
    for (i=0;i<p.NrItems();i++)
    {
        printf("%d ",(int)(p[i]));
    }
    printf("(%d)\n",p.NrItems());
}
void PrintPolyList(ZZPolyList& p)
{
    int i;
    for (i=0;i<p.NrItems();i++)
        PrintPoly(*(p[i]));
    printf("\n");
}
#endif

ZZMod::ZZMod(LispInt aMod) : iMod(aMod)
{
    iInverses.GrowTo(aMod);
    ZZ i;
    for (i=1;i<aMod;i++)
    {
        // z = x/y mod p
        // z*y = x mod p
        ZZ z = 0;
        while ( Mod((z*i)) != 1) z++;
        iInverses[i] = z;
    }
}

void ZZPoly::DropEndZeroes()
{
    LispInt nr = NrItems();
    if (nr>1) while (nr>1 && Item(nr-1) == 0) nr--;
    SetNrItems(nr);
}
inline ZZ ZZPoly::Degree()
{
    DropEndZeroes();
    return NrItems()-1;
}
inline void ZZPoly::Multiply(const ZZ& x,ZZMod& aMod)
{
    ZZ i,nr;
    nr = NrItems();
    for (i=0;i<nr;i++)
    {
        Item(i) = aMod.Mul(Item(i),x);
    }
}


ZZPoly* ShowPat(ZZPoly& a)
{
  ZZPoly* result = new ZZPoly();
  ZZ i;
  for (i=a.NrItems()-2;i>=0;i--) result->Append(a[i]);
  return result;
}

void Pat(ZZPoly& a,ZZPoly& u,ZZ deg,ZZMod& p)
{
  ZZ t,i;
  t=a[0];
  for(i=0;i<(deg);i++)
  {
    a[i] = p.Mod(a[i+1]-t*u[(deg-1)-i]);
  }
}

void NullSpaceAlg(ZZPolyList& Q,ZZ deg,ZZPolyList& v,ZZMod& p)
{
    ZZ k,r;
    ZZPoly c;

    for (k=0;k<deg;k++) c.Append(-1);

//TODO    printf("c array\n");
//    PrintPoly(c);
    
    r=0;
    for (k=0;k<deg;k++)
    {
        ZZ j,jfound;

        for (jfound=0;jfound<deg;jfound++)
        {
            if ((*Q[k])[jfound] != 0 && c[jfound] < 0)
            {
//                printf("jfound = %d\n",jfound);
                break;
            }
        }
        if (jfound<deg)
        {
            ZZ ak,ki,ji;
            ak=(*Q[k])[jfound];

            ZZ oneover = p.Div(-1,ak); /* (-1/ak); */
            for (ki=0;ki<deg;ki++)
                (*Q[ki])[jfound] = p.Mod((*Q[ki])[jfound] * oneover);
            for(ji=0;ji<deg;ji++)
            {
                if(ji!=jfound)
                {
                    ZZ Qkji = (*Q[k])[ji];
                    for (ki=0;ki<deg;ki++)
                    {
                        (*Q[ki])[ji] =
                            p.Mod((*Q[ki])[ji]+Qkji*(*Q[ki])[jfound]);
                    }
                    //Q[i] = NListMod(Q[i] + Q[i][k]*Q[jfound],p);
                }
            }
            c[jfound] = k;
//            PrintPolyList(Q);
        }
        else
        {
            ZZPoly *vnew = new ZZPoly;
            ZZ j,s;

            r = r+1;
            for (j=0;j<deg;j++) vnew->Append(0);
            (*vnew)[k] = 1;
            for(s=0;s<deg;s++)
            {
                for (j=0;j<deg;j++)
                {
                    if(c[s] == j)
                    {
                        (*vnew)[j] = (*Q[k])[s];
                    }
                }
            }
            v.Append(vnew);
        }
    }
    
}


void Berlekamp(ZZPolyList& aResult,ZZPoly& aPoly, ZZ modulo)
{
    ZZMod p(modulo);
    ZZPoly a;
    ZZ i,j,k;
    ZZ deg = aPoly.Degree();
    
    for (i=0;i<=deg;i++) a.Append(0);
    a[deg-1] = 1;

#ifdef TEST
    {
        int i;
        for (i=1;i<13;i++)
        {
            printf("%d ",p.Mod(i*p.Div(1,i)));
        }
        printf("\n");
    }
#endif
    
#ifdef TEST
//    printf("a_0:\n");
//    PrintPoly(a);
#endif
    ZZPolyList Q;
    Q.Append(ShowPat(a));

    for (k=1;k<deg;k++)
    {
        for(j=1;j<=modulo;j++)
        {
            Pat(a,aPoly,deg,p);
        }
        Q.Append(ShowPat(a));
    }

    for (k=0;k<deg;k++)
    {
        (*Q[k])[k] = p.Sub((*Q[k])[k],1);
    }
#ifdef TEST
    printf("Q-I matrix:\n");
    PrintPolyList(Q);
#endif

    ZZPolyList v;
    NullSpaceAlg(Q,deg,v,p);
#ifdef TEST
    printf("v matrix:\n");
    PrintPolyList(v);
#endif
    /*
     */
}

#ifdef TEST

 int main(void)
{


    ZZPolyList result;
    ZZPoly poly;
    poly.Append( 8);   // 8*x^0
    poly.Append( 2);   // 2*x^1
    poly.Append( 8);   // 8*x^2
    poly.Append(10);   //10*x^3
    poly.Append(10);   //10*x^4
    poly.Append( 0);   // 0*x^5
    poly.Append( 1);   // 1*x^6
    poly.Append( 0);   // 0*x^7
    poly.Append( 1);   // 1*x^8
    printf("Degree = %d\n",poly.Degree());
    Berlekamp(result, poly, 13);
    return 0;
    }
#endif



