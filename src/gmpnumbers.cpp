

/* Implementation of the number classes (the functionality used
 * by yacas any way
 */

#include <gmp.h>
#include <stdio.h>
#include "lisptype.h"
#include "numbers.h"
#include "standard.h"
#include "platmath.h"

static LispStringPtr IntegerToString(mpz_t& aInt, LispHashTable& aHashTable);
static LispStringPtr FloatToString(mpf_t& aInt, LispHashTable& aHashTable
                                  , LispInt aBase = 10);
static void SetGMPPrecision(LispInt aPrecision);
static LispStringPtr MultiplyInteger(LispCharPtr int1, LispCharPtr int2,
                       LispHashTable& aHashTable);
static LispStringPtr AddInteger(LispCharPtr int1, LispCharPtr int2,
                       LispHashTable& aHashTable);
static LispStringPtr SubtractInteger(LispCharPtr int1, LispCharPtr int2,
                       LispHashTable& aHashTable);
static LispStringPtr DivideInteger(LispCharPtr int1, LispCharPtr int2,
                          LispHashTable& aHashTable, LispInt aPrecision);
static LispStringPtr PowerInteger(LispCharPtr int1, LispCharPtr int2,
                      LispHashTable& aHashTable);
static LispStringPtr NegateInteger(LispCharPtr int1, LispHashTable& aHashTable);
static LispBoolean IsInteger(LispCharPtr num);
static LispStringPtr EFloat( LispHashTable& aHashTable, LispInt aPrecision);

LispInt NumericSupportForMantissa()
{
    return LispTrue;
}

const LispCharPtr NumericLibraryName()
{
    return "Gmp";
}

void SetGMPPrecision(LispInt aPrecision)
{
// FIXME this is very primitive
    mpf_set_default_prec((unsigned long)aPrecision<<2);
}


LispStringPtr GcdInteger(LispCharPtr int1, LispCharPtr int2,
                         LispHashTable& aHashTable)
{
    mpz_t i1;
    mpz_t i2;
    mpz_init(i1);
    mpz_set_str(i1,int1,10);
    mpz_init(i2);
    mpz_set_str(i2,int2,10);

    mpz_t res;
    mpz_init(res);
    mpz_gcd(res,i1,i2);
    LispStringPtr result = IntegerToString(res, aHashTable);
    mpz_clear(i1);
    mpz_clear(i2);
    mpz_clear(res);
    return result;
}

LispStringPtr LispFactorial(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision)
{
    LispInt nr = InternalAsciiToInt(int1);
    Check(nr>=0,KLispErrInvalidArg);
    mpz_t res;
    mpz_init(res);
    mpz_fac_ui (res, nr);
    LispStringPtr result = IntegerToString(res, aHashTable);
    mpz_clear(res);
    return result;
}

LispStringPtr MultiplyInteger(LispCharPtr int1, LispCharPtr int2,
                       LispHashTable& aHashTable)
{
    mpz_t i1;
    mpz_t i2;
    mpz_init(i1);
    mpz_set_str(i1,int1,10);
    mpz_init(i2);
    mpz_set_str(i2,int2,10);

    mpz_t res;
    mpz_init(res);
    mpz_mul(res,i1,i2);
    LispStringPtr result = IntegerToString(res, aHashTable);
    mpz_clear(i1);
    mpz_clear(i2);
    mpz_clear(res);
    return result;
}

LispStringPtr AddInteger(LispCharPtr int1, LispCharPtr int2,
                       LispHashTable& aHashTable)
{
    mpz_t i1;
    mpz_t i2;
    mpz_init(i1);
    mpz_set_str(i1,int1,10);
    mpz_init(i2);
    mpz_set_str(i2,int2,10);

    mpz_t res;
    mpz_init(res);
    mpz_add(res,i1,i2);
    LispStringPtr result = IntegerToString(res, aHashTable);
    mpz_clear(i1);
    mpz_clear(i2);
    mpz_clear(res);
    return result;
}

LispStringPtr SubtractInteger(LispCharPtr int1, LispCharPtr int2,
                       LispHashTable& aHashTable)
{
    mpz_t i1;
    mpz_t i2;
    mpz_init(i1);
    mpz_set_str(i1,int1,10);
    mpz_init(i2);
    mpz_set_str(i2,int2,10);

    mpz_t res;
    mpz_init(res);
    mpz_sub(res,i1,i2);
    LispStringPtr result = IntegerToString(res, aHashTable);
    mpz_clear(i1);
    mpz_clear(i2);
    mpz_clear(res);
    return result;
}

LispStringPtr NegateInteger(LispCharPtr int1, LispHashTable& aHashTable)
{
    mpz_t i1;
    mpz_init(i1);
    mpz_set_str(i1,int1,10);

    mpz_t res;
    mpz_init(res);
    mpz_neg(res, i1);
    LispStringPtr result = IntegerToString(res, aHashTable);
    mpz_clear(i1);
    mpz_clear(res);
    return result;
}

LispStringPtr DivideInteger(LispCharPtr int1, LispCharPtr int2,
                          LispHashTable& aHashTable, LispInt aPrecision)
{
    LispStringPtr result;
    mpz_t i1;
    mpz_t i2;
    mpq_t r;
    mpz_init(i1);
    mpz_set_str(i1,int1,10);
    mpz_init(i2);
    mpz_set_str(i2,int2,10);
    mpq_init(r);
    mpq_set_num(r,i1);
    mpq_set_den(r,i2);
    mpq_canonicalize(r);

    if (!mpz_cmp_ui(mpq_denref(r),1)) {
      mpq_get_num(i1,r);
      result = IntegerToString(i1, aHashTable);
    } else {
      SetGMPPrecision(aPrecision);
      mpf_t res;
      mpf_init(res);
      mpf_set_q(res,r);
      result = FloatToString(res,aHashTable);
      mpf_clear(res);
    }
    mpz_clear(i1);
    mpz_clear(i2);
    mpq_clear(r);
    return result;    
}

LispStringPtr PowerInteger(LispCharPtr int1, LispCharPtr int2, 
                      LispHashTable& aHashTable)
{
    mpz_t i1;
    mpz_init(i1);
    mpz_set_str(i1,int1,10);

    mpz_t res;
    mpz_init(res);
//FIXME: handle case where int2 > MAX_LONG_INT
    mpz_pow_ui(res,i1,(unsigned long)atol(int2));
    LispStringPtr result = IntegerToString(res, aHashTable);
    mpz_clear(i1);
    mpz_clear(res);
    return result;
}

LispStringPtr MultiplyFloat(LispCharPtr int1, LispCharPtr int2,
                            LispHashTable& aHashTable,LispInt aPrecision)
{
    if (IsInteger(int1) && IsInteger(int2)) {
      return MultiplyInteger(int1, int2, aHashTable);
    } else {
      SetGMPPrecision(aPrecision);
      mpf_t i1;
      mpf_t i2;
      mpf_init(i1);
      mpf_set_str(i1,int1,10);
      mpf_init(i2);
      mpf_set_str(i2,int2,10);

      mpf_t res;
      mpf_init(res);
      mpf_mul(res,i1,i2);
      LispStringPtr result = FloatToString(res, aHashTable);
      mpf_clear(i1);
      mpf_clear(i2);
      mpf_clear(res);
      return result;
    }
}

LispStringPtr AddFloat(LispCharPtr int1, LispCharPtr int2,
                       LispHashTable& aHashTable,LispInt aPrecision)
{
    if (IsInteger(int1) && IsInteger(int2)) {
      return AddInteger(int1, int2, aHashTable);
    } else {
      SetGMPPrecision(aPrecision);
      mpf_t i1;
      mpf_t i2;
      mpf_init(i1);
      mpf_set_str(i1,int1,10);
      mpf_init(i2);
      mpf_set_str(i2,int2,10);

      mpf_t res;
      mpf_init(res);
      mpf_add(res,i1,i2);
      LispStringPtr result = FloatToString(res, aHashTable);
      mpf_clear(i1);
      mpf_clear(i2);
      mpf_clear(res);
      return result;
    }
}

LispStringPtr PlusFloat(LispCharPtr int1,LispHashTable& aHashTable
                       ,LispInt aPrecision)
{
// RVS 16 June 2001:
// TODO: is this routine ever really used?
    SetGMPPrecision(aPrecision);
    mpf_t i1;
    mpf_init(i1);
    mpf_set_str(i1,int1,10);

    mpf_t res;
    mpf_init(res);
    mpf_set(res, i1);
    LispStringPtr result = FloatToString(res, aHashTable);
    mpf_clear(i1);
    mpf_clear(res);
    return result;
}

LispStringPtr SubtractFloat(LispCharPtr int1, LispCharPtr int2,
                            LispHashTable& aHashTable,LispInt aPrecision)
{
    if (IsInteger(int1) && IsInteger(int2)) {
      return SubtractInteger(int1, int2, aHashTable);
    } else {
      SetGMPPrecision(aPrecision);
      mpf_t i1;
      mpf_t i2;
      mpf_init(i1);
      mpf_set_str(i1,int1,10);
      mpf_init(i2);
      mpf_set_str(i2,int2,10);
  
      mpf_t res;
      mpf_init(res);
      mpf_sub(res,i1,i2);
      LispStringPtr result = FloatToString(res, aHashTable);
      mpf_clear(i1);
      mpf_clear(i2);
      mpf_clear(res);
      return result;
    }
}

LispStringPtr NegateFloat(LispCharPtr int1, LispHashTable& aHashTable
                          ,LispInt aPrecision)
{
    if (IsInteger(int1)) {
      return NegateInteger(int1, aHashTable);
    } else {
      SetGMPPrecision(aPrecision);
      mpf_t i1;
      mpf_init(i1);
      mpf_set_str(i1,int1,10);
  
      mpf_t res;
      mpf_init(res);
      mpf_neg(res, i1);
      LispStringPtr result = FloatToString(res, aHashTable);
      mpf_clear(i1);
      mpf_clear(res);
      return result;
    }
}

LispStringPtr DivideFloat(LispCharPtr int1, LispCharPtr int2,
                          LispHashTable& aHashTable,LispInt aPrecision)
{
    if (IsInteger(int1) && IsInteger(int2)) {
      return DivideInteger(int1, int2, aHashTable, aPrecision);
    } else {
      SetGMPPrecision(aPrecision);
      mpf_t i1;
      mpf_t i2;
      mpf_init(i1);
      mpf_set_str(i1,int1,10);
      mpf_init(i2);
      mpf_set_str(i2,int2,10);
  
      mpf_t res;
      mpf_init(res);
      mpf_div(res,i1,i2);
      LispStringPtr result = FloatToString(res, aHashTable);
      mpf_clear(i1);
      mpf_clear(i2);
      mpf_clear(res);
      return result;
    }
}

LispStringPtr PowerFloat(LispCharPtr int1, LispCharPtr int2,
                         LispHashTable& aHashTable,LispInt aPrecision)
{
    if(IsInteger(int1)) {
      return PowerInteger(int1, int2, aHashTable);
    } else {
    //TODO
      return PlatPower(int1, int2, aHashTable, 0);

//  FOR THIS YOU NEED GMP >= 3.0  
/*  
      SetGMPPrecision(aPrecision);
      mpf_t i1;
      mpf_init(i1);
      mpf_set_str(i1,int1,10);

      mpf_t res;
      mpf_init(res);
      mpf_pow_ui(res,i1,(unsigned long)atol(int2));
      LispStringPtr result = FloatToString(res, aHashTable);
      mpf_clear(i1);
      mpf_clear(i2);
      mpf_clear(res);
      return result;*/
    }
}



LispStringPtr SinFloat(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision)
{
    //TODO
    return PlatSin(int1, aHashTable, 0);
}

LispStringPtr CosFloat(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision)
{
    //TODO
    return PlatCos(int1, aHashTable, 0);
}

LispStringPtr TanFloat(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision)
{
    //TODO
    return PlatTan(int1, aHashTable, 0);
}

LispStringPtr ArcSinFloat(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision)
{
    //TODO
    return PlatArcSin(int1,  aHashTable, 0);
}

LispStringPtr ArcCosFloat(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision)
{
    //TODO
    return PlatArcCos(int1,  aHashTable, 0);
}

LispStringPtr ArcTanFloat(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision)
{
    //TODO
    return PlatArcTan(int1,  aHashTable, 0);
}

LispStringPtr ExpFloat(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision)
{
    //TODO
    return PlatExp(  int1,  aHashTable, 0);
}

LispStringPtr LnFloat(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision)
{
    //TODO
    return PlatLn(   int1,  aHashTable, 0);
}


LispStringPtr SqrtFloat(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision)
{
//TODO handle integer case
    SetGMPPrecision(aPrecision);
    mpf_t i1;
    mpf_init(i1);
    mpf_set_str(i1,int1,10);

    mpf_t res;
    mpf_init(res);
    mpf_sqrt(res,i1);
    LispStringPtr result = FloatToString(res, aHashTable);
    mpf_clear(i1);
    mpf_clear(res);
    return result;
}

LispStringPtr AbsFloat( LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision)
{
//TODO handle integer case
    SetGMPPrecision(aPrecision);
    mpf_t i1;
    mpf_init(i1);
    mpf_set_str(i1,int1,10);

    mpf_t res;
    mpf_init(res);
    mpf_abs(res,i1);
    LispStringPtr result = FloatToString(res, aHashTable);
    mpf_clear(i1);
    mpf_clear(res);
    return result;
}



LispBoolean LessThan(LispCharPtr int1, LispCharPtr int2,
                       LispHashTable& aHashTable,LispInt aPrecision)
{
//TODO handle integer case
    SetGMPPrecision(aPrecision);
    mpf_t i1;
    mpf_t i2;
    mpf_init(i1);
    mpf_set_str(i1,int1,10);
    mpf_init(i2);
    mpf_set_str(i2,int2,10);

    LispBoolean result = mpf_cmp(i1,i2)<0;
    mpf_clear(i1);
    mpf_clear(i2);
    return result;
}

LispBoolean GreaterThan(LispCharPtr int1, LispCharPtr int2,
                       LispHashTable& aHashTable,LispInt aPrecision)
{
//TODO handle integer case
    SetGMPPrecision(aPrecision);
    mpf_t i1;
    mpf_t i2;
    mpf_init(i1);
    mpf_set_str(i1,int1,10);
    mpf_init(i2);
    mpf_set_str(i2,int2,10);

    LispBoolean result = mpf_cmp(i1,i2)>0;
    mpf_clear(i1);
    mpf_clear(i2);
    return result;
}



LispStringPtr ShiftLeft( LispCharPtr int1, LispCharPtr int2, LispHashTable& aHashTable,LispInt aPrecision)
{
    mpz_t i1;
    mpz_init(i1);
    mpz_set_str(i1,int1,10);
    LispInt bits = InternalAsciiToInt(int2);
    mpz_t res;
    mpz_init(res);
    mpz_mul_2exp(res,i1,bits);
    LispStringPtr result = IntegerToString(res, aHashTable);
    mpz_clear(i1);
    mpz_clear(res);
    return result;
}


LispStringPtr ShiftRight( LispCharPtr int1, LispCharPtr int2, LispHashTable& aHashTable,LispInt aPrecision)
{
    mpz_t i1;
    mpz_init(i1);
    mpz_set_str(i1,int1,10);
    LispInt bits = InternalAsciiToInt(int2);
    mpz_t res;
    mpz_init(res);
    mpz_tdiv_q_2exp(res,i1,bits);
    LispStringPtr result = IntegerToString(res, aHashTable);
    mpz_clear(i1);
    mpz_clear(res);
    return result;
}


LispStringPtr FromBase( LispCharPtr int1, LispCharPtr int2, LispHashTable& aHashTable,
                        LispInt aPrecision)
{
//TODO handle integer case
    SetGMPPrecision(aPrecision);
    LispInt base = InternalAsciiToInt(int1);
    mpf_t i1;
    mpf_init(i1);
    mpf_set_str(i1,int2,base);
    LispStringPtr result = FloatToString(i1, aHashTable,10);
    mpf_clear(i1);
    return result;

}


LispStringPtr ToBase( LispCharPtr int1, LispCharPtr int2, LispHashTable& aHashTable,
                    LispInt aPrecision)
{
//TODO handle integer case
    SetGMPPrecision(aPrecision);
    LispInt base = InternalAsciiToInt(int1);
    mpf_t i1;
    mpf_init(i1);
    mpf_set_str(i1,int2,10);
    LispStringPtr result = FloatToString(i1, aHashTable,base);
    mpf_clear(i1);
    return result;
}


LispStringPtr FloorFloat( LispCharPtr int1, LispHashTable& aHashTable,
                        LispInt aPrecision)
{
//TODO
    return PlatFloor( int1, aHashTable, aPrecision);
}

LispStringPtr CeilFloat( LispCharPtr int1, LispHashTable& aHashTable,
                         LispInt aPrecision)
{
//TODO
    return PlatCeil( int1,aHashTable, aPrecision);
}

LispStringPtr ModFloat( LispCharPtr int1, LispCharPtr int2, LispHashTable& aHashTable,
                        LispInt aPrecision)
{
//TODO
    return PlatMod( int1, int2, aHashTable, aPrecision);
}

LispStringPtr DivFloat( LispCharPtr int1, LispCharPtr int2, LispHashTable& aHashTable,
                        LispInt aPrecision)
{
//TODO
    return PlatDiv( int1, int2, aHashTable, aPrecision);
}

LispStringPtr EFloat( LispHashTable& aHashTable,
                        LispInt aPrecision)
{
    SetGMPPrecision(aPrecision);
    mpf_t s;
    mpf_t s1;
    mpf_t t;
    mpf_init_set_ui(s,1);
    mpf_init_set_ui(t,1);
    mpf_init(s1);
    unsigned long i;
    for(i=2;;i++) {
       mpf_add(s1,s,t);
       if (!mpf_cmp(s,s1)) break;
       mpf_set(s,s1);
       mpf_div_ui(t,t,i);
    }
    LispStringPtr result = FloatToString(s, aHashTable);
    mpf_clear(s);
    mpf_clear(t);
    mpf_clear(s1);
    return result;
}

LispStringPtr PiFloat( LispHashTable& aHashTable,
                        LispInt aPrecision)
{
    SetGMPPrecision(aPrecision);
    mpf_t sum1;
    mpf_t s;
    mpf_t s1;
    mpf_t t;
    mpf_t t1;
    mpf_t negfourth;
    mpf_init_set_ui(s,0);
    mpf_init_set_d(t,0.5);
    mpf_init_set_d(t1,0.5);
    mpf_init_set_d(negfourth,-0.25);
    mpf_init(sum1);
    mpf_init(s1);
    unsigned long i;
    for(i=3;;i+=2) {
       mpf_add(s1,s,t);
       if (!mpf_cmp(s,s1)) break;
       mpf_set(s,s1);
       mpf_mul(t1,t1,negfourth);
       mpf_div_ui(t,t1,i);
    }
    mpf_set(sum1,s);

    mpq_t tmp;
    mpq_init(tmp);
    mpq_set_ui(tmp,1,3);
    mpf_set_ui(s,0);
    mpf_set_q(t,tmp);
    mpf_set_q(t1,tmp);
    mpq_set_si(tmp,-1,9);
    mpf_t g;
    mpf_init(g);
    mpf_set_q(g,tmp);
    mpq_clear(tmp);
    for(i=3;;i+=2) {
       mpf_add(s1,s,t);
       if (!mpf_cmp(s,s1)) break;
       mpf_set(s,s1);
       mpf_mul(t1,t1,g);
       mpf_div_ui(t,t1,i);
    }
    mpf_add(s,s,sum1);
    mpf_mul_ui(s,s,4);

    LispStringPtr result = FloatToString(s, aHashTable);
    mpf_clear(sum1);
    mpf_clear(s);
    mpf_clear(t);
    mpf_clear(s1);
    mpf_clear(t1);
    mpf_clear(negfourth);
    mpf_clear(g);
    return result;
}






static LispStringPtr IntegerToString(mpz_t& aInt,
                                     LispHashTable& aHashTable)
{
    char* result = mpz_get_str(NULL,10,aInt);
    LispStringPtr toreturn = aHashTable.LookUp(result);
    free(result);
    return toreturn;
}

static void RemoveTrailingZeroes(LispString& printable)
{
    LispInt i;
    // remove trailing zeros
    i=printable.NrItems()-2;
    while (printable[i] == '0')
    {
        printable[i] = '\0';
        i--;
    }
    if (printable[i] == '.')
        printable[i] = '\0';
}

static LispStringPtr FloatToString(mpf_t& aInt,
                            LispHashTable& aHashTable, LispInt aBase)
{
    mp_exp_t expo;
    char* result = mpf_get_str(NULL,&expo,aBase,0,aInt);

    /* Now build our own version of a float number. Trying to keep
     * it integer
     */
    LispString printable;
    printable.SetNrItems(0);
    char* ptr=result;
    LispInt i;
    
    // Allow for sign
    if (*ptr == '-')
    {
        printable.Append('-');
        ptr++;
    }
    LispInt nrdigits = strlen(ptr);

    //Number is smaller than 0.1
    if(expo<0)
    {
        char exc[20];
        sprintf(exc,"%ld",expo);
        printable.Append('0');
        printable.Append('.');
        for (i=0;i<nrdigits;i++)
            printable.Append(ptr[i]);
        printable.Append('E');
        nrdigits=strlen(exc);
        for (i=0;i<nrdigits;i++)
            printable.Append(exc[i]);
        printable.Append('\0');
        RemoveTrailingZeroes(printable);
    }
    // Number is something like 0.xxx
    else if (expo == 0)
    {
        printable.Append('0');
        printable.Append('.');
        for (i=0;i<nrdigits;i++)
            printable.Append(ptr[i]);
        printable.Append('\0');
        RemoveTrailingZeroes(printable);
    }
    // Number has its decimal floating somewhere in the middle of the digits
    else if (expo<nrdigits)
    {
        for (i=0;i<nrdigits;i++)
        {
            if (i == expo)
                printable.Append('.');
            printable.Append(ptr[i]);
        }
        printable.Append('\0');
        RemoveTrailingZeroes(printable);
    }
    // Number is actually an integer.
    else
    {
        for (i=0;i<nrdigits;i++)
        {
            printable.Append(ptr[i]);
        }
        for (i=nrdigits;i<expo;i++)
        {
            printable.Append('0');
        }
        printable.Append('\0');
    }
    
    free(result);

    return aHashTable.LookUp(printable.String());
}


LispStringPtr BitAnd(LispCharPtr int1, LispCharPtr int2,
                     LispHashTable& aHashTable,LispInt aPrecision)
{
    mpz_t i1;
    mpz_t i2;
    mpz_init(i1);
    mpz_set_str(i1,int1,10);
    mpz_init(i2);
    mpz_set_str(i2,int2,10);

    mpz_t res;
    mpz_init(res);
		mpz_and(res,i1,i2);
    LispStringPtr result = IntegerToString(res, aHashTable);
    mpz_clear(i1);
    mpz_clear(i2);
    mpz_clear(res);
    return result;
}

LispStringPtr BitOr(LispCharPtr int1, LispCharPtr int2,
                     LispHashTable& aHashTable,LispInt aPrecision)
{
    mpz_t i1;
    mpz_t i2;
    mpz_init(i1);
    mpz_set_str(i1,int1,10);
    mpz_init(i2);
    mpz_set_str(i2,int2,10);

    mpz_t res;
    mpz_init(res);
		mpz_ior(res,i1,i2);
    LispStringPtr result = IntegerToString(res, aHashTable);
    mpz_clear(i1);
    mpz_clear(i2);
    mpz_clear(res);
    return result;
}

LispStringPtr BitXor(LispCharPtr int1, LispCharPtr int2,
                     LispHashTable& aHashTable,LispInt aPrecision)
{
    mpz_t i1;
    mpz_t i2;
    mpz_init(i1);
    mpz_set_str(i1,int1,10);
    mpz_init(i2);
    mpz_set_str(i2,int2,10);

    mpz_t res;
    mpz_t tmp1;
    mpz_t tmp2;
    mpz_init(res);
    mpz_init(tmp1);
    mpz_init(tmp2);
    // this works because i1 ^ i2 <=> (i1 & i2) | (!i1 & !i2)
    // but it's clumsy
    mpz_and(tmp1, i1, i2);
    mpz_com(i1, i1);
    mpz_com(i2, i2);
    mpz_and(tmp2, i1, i2);
    mpz_ior(res, tmp1, tmp2);
    LispStringPtr result = IntegerToString(res, aHashTable);
    mpz_clear(i1);
    mpz_clear(i2);
    mpz_clear(tmp1);
    mpz_clear(tmp2);
    mpz_clear(res);
    return result;

//  FOR THIS YOU NEED GMP >= 3.0  
/*    mpz_t i1;
    mpz_t i2;
    mpz_init(i1);
    mpz_set_str(i1,int1,10);
    mpz_init(i2);
    mpz_set_str(i2,int2,10);

    mpz_t res;
    mpz_init(res);
    mpz_xor(res, i1, i2);
    LispStringPtr result = IntegerToString(res, aHashTable);
    mpz_clear(i1);
    mpz_clear(i2);
    mpz_clear(res);
    return result;*/
}

LispBoolean IsInteger(LispCharPtr num)
{
  if (strpbrk(num,".eE/")==NULL) {
    return LispTrue;
  } else {
    return LispFalse;
  }
}

