

/* Implementation of the number classes (the functionality used
 * by yacas any way
 * 24 June 2001: being rewritten by Robert Schipper, still working on it
 */

#include <gmp.h>
// do not use math.h without necessity
/*
#ifdef HAVE_MATH_H
  #include <math.h>
#endif
*/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "lisptype.h"
#include "numbers.h"
#include "standard.h"
#include "platmath.h"
#include "errors.h"

// we still need some of these functions but it will eventually all be removed
//#ifdef NO_USE_BIGFLOAT
#if 1

struct GMPNumber {
  mpz_t man;
  long exp;
};

static void initGMPNumber(GMPNumber& x, GMPNumber& y);
static void initGMPNumber(GMPNumber& x, long y=0);
static void initGMPNumber(GMPNumber& x, char* str);
static void initGMPNumber(GMPNumber& x, mpz_t mpz);
static void clearGMPNumber(GMPNumber& x);
static void ConvToGMPNumber(GMPNumber& r, mpf_t x, long prec=0);
static char* getstrGMPNumber(GMPNumber& x, long prec=0);
static LispStringPtr GMPNumberToString(GMPNumber& x, LispHashTable& h, 
                                       LispInt prec=0);
static void GMPNumberCopy(GMPNumber& x, GMPNumber& y);
static void GMPNumberZeroFill(GMPNumber& r, GMPNumber& x);
static void GMPNumberTrunc(GMPNumber& r, GMPNumber& x, long prec);
static void GMPNumberAdd(GMPNumber& r, GMPNumber& x, GMPNumber& y);
static void GMPNumberSubtract(GMPNumber& r, GMPNumber& x, GMPNumber& y); 
static void GMPNumberMultiply(GMPNumber& r, GMPNumber& x, 
                              GMPNumber& y, long prec);
static void GMPNumberDivide(GMPNumber& r, GMPNumber& x, 
                                 GMPNumber& y, long prec);
static void GMPNumberInvert(GMPNumber& r, GMPNumber& x, long prec);
static void GMPNumberPower(GMPNumber& r, GMPNumber& x, long e, long prec);
static void GMPNumberNeg(GMPNumber& r, GMPNumber& x);
static void GMPNumberAbs(GMPNumber& r, GMPNumber& x);
static void GMPNumberFloor(GMPNumber& r, GMPNumber& x);
static void GMPNumberCeil(GMPNumber& r, GMPNumber& x);
static void GMPNumberRound(GMPNumber& r, GMPNumber& x);
static void GMPNumberDiv(GMPNumber& r, GMPNumber& x, GMPNumber& y);
static void GMPNumberMod(GMPNumber& r, GMPNumber& x, GMPNumber& y);
static void GMPNumberSqrt(GMPNumber& r, GMPNumber& x, long prec);
static void GMPNumberExp(GMPNumber& r, GMPNumber& x, long prec);
static void GMPNumberLog(GMPNumber& r, GMPNumber& x, long prec);
static void GMPNumberPi(GMPNumber& r, long prec);
static void GMPNumberShiftLeft(GMPNumber& r, GMPNumber& x, unsigned long d);
static void GMPNumberShiftRight(GMPNumber& r, GMPNumber& x, unsigned long d);
static void SetMPFPrecision(LispInt aPrecision);

static LispStringPtr IntegerToString(mpz_t& aInt, LispHashTable& aHashTable);
static LispStringPtr FloatToString(mpf_t& aInt, LispHashTable& aHashTable, 
                                   LispInt aBase = 10);

LispInt NumericSupportForMantissa()
{
    return LispTrue;
}

const LispCharPtr NumericLibraryName()
{
    return "Gmp";
}

void initGMPNumber(GMPNumber& x, GMPNumber& y)
{
  mpz_t man;
  mpz_init_set(man,y.man);
  x.man = man;
  x.exp = y.exp;
}

void initGMPNumber(GMPNumber& x, long y)
{
  mpz_t man;
  mpz_init_set_ui(man,y);
  x.man = man;
  x.exp = 0;
}

void initGMPNumber(GMPNumber& x, mpz_t mpz)
{
  mpz_t man;
  mpz_init_set(man,mpz);
  x.man = man;
  x.exp = 0;
}

void initGMPNumber(GMPNumber& x, char* str)
{
  size_t length = strlen(str);
  char *man = (char*)malloc(length+1);
  char* s = str;
  char* m = man;
  long exp = 0;
  int neg = 0;
  int enot = 0;
  int tiny = 0;
  if (*s=='-') {neg=1;s++;}
  while(*s=='0')s++;
  if(*s=='.') {
    s++;
    while(*s=='0') {
      s++;
      exp--;
    }
  } else {
    while(*s) {
      if(*s=='.'){s++;break;}
      *m=*s;
      m++;
      s++;
      if(*s=='E'||*s=='e'){s++;enot=1;break;}
    }
  }
  if (!enot) {
    while(*s) {
      *m=*s;
      m++;
      s++;
      exp--;
      if(*s=='E'||*s=='e'){s++;enot=1;break;}
    }
  }  
  if (enot) exp+=atol(s);
  *m='\0';
  m--;
  while (*m=='0') {
    *m='\0';
    m--;
    exp++;
  }
  if (!*man) {*man='0';*(man+1)='\0';neg=0;exp=0;}
  mpz_t ma;
  mpz_init_set_str(ma,man,10);
  free(man);
  if(neg)mpz_neg(ma,ma);
  x.man=ma;
  x.exp=exp;
}

void clearGMPNumber(GMPNumber& x)
{
  mpz_clear(x.man);
}

static void ConvToGMPNumber(GMPNumber& r, mpf_t x, long prec) {
  mp_exp_t expt;
  char* str=mpf_get_str(NULL,&expt,10,prec,x);
  mpz_set_str(r.man,str,10);
  r.exp=expt-strlen(str)+(mpz_sgn(r.man)<0);
  free(str);
}

char* getstrGMPNumber(GMPNumber& x, long prec=0)
{
  long rawsize = mpz_sizeinbase(x.man,10);
  size_t size;
  if (rawsize+x.exp<=0) size=-x.exp+4;
  else if (x.exp>0) size=rawsize+x.exp+4;
  else size=rawsize+4;
  char *s, *str=(char*)malloc(size);
  mpz_get_str(str,10,x.man);
  char * st=str;
  size_t length = strlen(str);
  if(mpz_sgn(x.man)<0) {st++;length--;}
  if (!x.exp) return str;
  if (x.exp>0) {
    s = st+length-1;
    int exp=x.exp;
    while (s>st && *s == '0') {s--; exp++; length--;}
    *++s='\0';
    if (exp>prec) sprintf(s,"E%ld\0",exp);
    else {
      while(exp) {
        *s++='0';
        exp--;
      }
      *s='\0';
    }
    return str;
  }
  size_t exp = -x.exp;
  if (length<=exp) {
    s = st + length - 1;
    while (s>st && *s == '0') {s--; exp--; length--;}
    *++s='\0';
    s = st;
    if (prec && length>prec) {
      exp-=length-prec;
      length=prec;
      *(s+length)='\0';
    }
    if (exp-length<=prec) {
      memmove(s+exp-length+2,s,length+1);
      *s++='0';
      *s++='.';
      exp-=length;
      while (exp--) *s++='0';
    } else {
      memmove(s+2,s,length+1);
      *s++='0';
      *s++='.';
      s+=length;
      exp-=length;
      sprintf(s,"E-%ld\0",exp);
    }
  } else {
    if (prec && exp>prec) {
      length-=exp-prec;
      exp=prec;
      *(st+length)='\0';
    }
    size_t index = length-exp;
    memmove(st+index+1, st+index, exp+1);
    st[index]='.';
    s = st+length;
    while (*s=='0') s--;
    if (*s=='.') *s='\0';
    else *++s='\0';
  }
  return str;
}

static LispStringPtr GMPNumberToString(GMPNumber& x, LispHashTable& h, 
                                       LispInt prec=0)
{
  char* result = getstrGMPNumber(x, prec);
  LispStringPtr toreturn = h.LookUp(result);
  free(result);
  return toreturn;
}
 
void GMPNumberCopy(GMPNumber& x, GMPNumber& y)
{
  mpz_set(x.man,y.man);
  x.exp=y.exp;
}

static void GMPNumberZeroFill(GMPNumber& r, GMPNumber& x)
{
  if (x.exp>0) {
    mpz_t factor;
    mpz_init_set_ui(factor,10);
    mpz_pow_ui(factor,factor,x.exp);
    mpz_mul(r.man,x.man,factor);
    mpz_clear(factor);
    r.exp=0;
  } else GMPNumberCopy(r,x);
}

void GMPNumberAdd(GMPNumber& r, GMPNumber& x, GMPNumber& y)
{
  if (!mpz_sgn(x.man)) {GMPNumberCopy(r,y); return;}
  if (!mpz_sgn(y.man)) {GMPNumberCopy(r,x); return;}
  int diff = x.exp-y.exp;
  if (diff) {
    mpz_t factor;
    mpz_init_set_ui(factor,10);
    GMPNumber tmp;
    if (diff<0) {
      mpz_pow_ui(factor,factor,-diff);
      initGMPNumber(tmp,y);
      mpz_mul(tmp.man,tmp.man,factor);
      mpz_add(r.man,x.man,tmp.man);
      r.exp=x.exp;
    } else if (diff>0) {
      mpz_pow_ui(factor,factor,diff);
      initGMPNumber(tmp,x);
      mpz_mul(tmp.man,tmp.man,factor);
      mpz_add(r.man,tmp.man,y.man);
      r.exp=y.exp;
    }
    clearGMPNumber(tmp);
    mpz_clear(factor);
  } else {
    mpz_add(r.man,x.man,y.man);
    r.exp=x.exp;
  }
  if (!mpz_sgn(r.man)) r.exp = 0;
}

void GMPNumberSubtract(GMPNumber& r, GMPNumber& x, GMPNumber& y)
{
  if (!mpz_sgn(x.man)) {mpz_neg(r.man,y.man); r.exp=y.exp; return;}
  if (!mpz_sgn(y.man)) {GMPNumberCopy(r,x); return;}
  int diff = x.exp-y.exp;
  if (diff) {
    mpz_t factor;
    mpz_init_set_ui(factor,10);
    GMPNumber tmp;
    if (diff<0) {
      mpz_pow_ui(factor,factor,-diff);
      initGMPNumber(tmp,y);
      mpz_mul(tmp.man,tmp.man,factor);
      mpz_sub(r.man,x.man,tmp.man);
      r.exp=x.exp;
    } else if (diff>0) {
      mpz_pow_ui(factor,factor,diff);
      initGMPNumber(tmp,x);
      mpz_mul(tmp.man,tmp.man,factor);
      mpz_sub(r.man,tmp.man,y.man);
      r.exp=y.exp;
    }
    clearGMPNumber(tmp);
    mpz_clear(factor);
  } else {
    mpz_sub(r.man,x.man,y.man);
    r.exp=x.exp;
  }
  if (!mpz_sgn(r.man)) r.exp = 0;
}

static void GMPNumberTrunc(GMPNumber& r, GMPNumber& x, long prec)
{
  long e = -x.exp;
  if (e>prec) {
    e -= prec;
    mpz_t factor;
    mpz_init_set_ui(factor,10);
    mpz_pow_ui(factor,factor,e);
    mpz_tdiv_q(r.man,x.man,factor);
    mpz_clear(factor);
    r.exp = -prec;
  }
}

void GMPNumberMultiply(GMPNumber& r, GMPNumber& x, GMPNumber& y, long prec)
{
  int neg = mpz_sgn(x.man)*mpz_sgn(y.man);
  if (neg) {
    mpz_mul(r.man,x.man,y.man);
    r.exp = x.exp+y.exp;
  } else {
    mpz_set_ui(r.man,0);
    r.exp = 0;
  }
}

void GMPNumberDivide(GMPNumber& r, GMPNumber& x, GMPNumber& y, long prec)
{
  int neg = mpz_sgn(x.man)/mpz_sgn(y.man);
  if (neg) {
    long e = prec + x.exp - y.exp;
    if (e<prec) e=prec;
    size_t xlen = mpz_sizeinbase(x.man,10);
    size_t ylen = mpz_sizeinbase(y.man,10);
    if (ylen>xlen) e+=ylen-xlen+1;
//    if (ylen>xlen) e+=ylen<<1-xlen+1;
//    else e+=xlen;
    mpz_t factor;
    mpz_init_set_ui(factor,10);
    mpz_pow_ui(factor,factor,e);
    mpz_mul(r.man,x.man,factor);
    mpz_tdiv_q(r.man,r.man,y.man);
    r.exp = mpz_sgn(r.man) ? x.exp-y.exp-e : 0;
    mpz_clear(factor);
  } else {
    mpz_set_ui(r.man,0);
    r.exp = 0;
  }
}

static void GMPNumberSqrt(GMPNumber& r, GMPNumber& x, long prec)
{
  int neg = mpz_sgn(x.man);
  if (neg==0) {
    mpz_set_ui(r.man,0);
    r.exp = 0;
  } else if (neg>0) {
    GMPNumberZeroFill(x,x);
    long exp = -x.exp;
    long l = mpz_sizeinbase(x.man,10);
    l+=l&1;
    long adjust = exp&1;
    mpz_t factor;
    mpz_init_set_ui(factor,10);
    mpz_pow_ui(factor,factor,(prec+l)<<1+adjust);
    mpz_mul(r.man,x.man,factor);
    mpz_sqrt(r.man,r.man);
    mpz_clear(factor);
    r.exp=-((exp-adjust)>>1)-prec-l;
    GMPNumberTrunc(r,r,prec);
  } else {
  //TODO Error: taking sqrt of negative number
  }
}

static void GMPNumberDiv(GMPNumber& r, GMPNumber& x, GMPNumber& y)
{
  GMPNumberZeroFill(x,x);
  GMPNumberZeroFill(y,y);
  mpz_set_ui(r.man,0);
  r.exp=0;
  if (!(x.exp || y.exp)) {
    int neg = mpz_sgn(x.man)/mpz_sgn(y.man);
    if (neg) mpz_tdiv_q(r.man,x.man,y.man);
  }
}

static void GMPNumberMod(GMPNumber& r, GMPNumber& x, GMPNumber& y)
{
  GMPNumberZeroFill(x,x);
  GMPNumberZeroFill(y,y);
  mpz_set_ui(r.man,0);
  r.exp=0;
  if (!(x.exp || y.exp)) {
    int neg = mpz_sgn(x.man)/mpz_sgn(y.man);
    if (neg) mpz_tdiv_r(r.man,x.man,y.man);
  }
}

void GMPNumberInvert(GMPNumber& r, GMPNumber& x, long prec)
{
  GMPNumber one;
  initGMPNumber(one, 1);
  GMPNumberDivide(r,one,x,prec); 
  clearGMPNumber(one);
}

void GMPNumberPower(GMPNumber& r, GMPNumber& x, long e, long prec)
{
    int inv = 0;
    if (e<0) {inv=1; e=-e;}
    mpz_pow_ui(r.man,x.man,e);
    r.exp = e*x.exp;
    if (inv) GMPNumberInvert(r,r,prec);
}

static void GMPNumberExp(GMPNumber& r, GMPNumber& x, long prec)
{
  mpf_set_default_prec(prec<<2);
  mpf_t e;
  mpf_init(e);
  mpf_set_z(e,x.man);
  mpz_t factor;
  mpz_init_set_ui(factor,10);
  long exp;
  int neg=0;
  if (x.exp<0) {neg=1;exp=-x.exp;}
  else exp=x.exp;
  mpz_pow_ui(factor,factor,exp);
  mpf_t f;
  mpf_init(f);
  mpf_set_z(f,factor);
  neg ? mpf_div(e,e,f) : mpf_mul(e,e,f);
  mpz_clear(factor);
  mpf_clear(f);
  unsigned long i;
  mpf_t s;
  mpf_t s1;
  mpf_t t;
  mpf_init_set_ui(s,1);
  mpf_init_set(t,e);
  mpf_init(s1);
  for(i=2;;i++) {
    mpf_add(s1,s,t);
    if (!mpf_cmp(s,s1)) break;
    mpf_set(s,s1);
    mpf_mul(t,t,e);
    mpf_div_ui(t,t,i);
  }
  mpf_clear(t);
  mpf_clear(s1);
  mpf_clear(e);
  ConvToGMPNumber(r,s,prec);
  mpf_clear(s);
}

static void GMPNumberLog(GMPNumber& r, GMPNumber& x, long prec)
{
  mpf_set_default_prec(prec<<2);
  mpf_t y;
  mpf_init(y);
  mpf_set_z(y,x.man);
  mpz_t factor;
  mpz_init_set_ui(factor,10);
  long exp;
  int neg=0;
  if (x.exp<0) {neg=1;exp=-x.exp;}
  else exp=x.exp;
  mpz_pow_ui(factor,factor,exp);
  mpf_t f;
  mpf_init(f);
  mpf_set_z(f,factor);
  neg ? mpf_div(y,y,f) : mpf_mul(y,y,f);
  mpz_clear(factor);
  mpf_clear(f);
  unsigned long n=0;
  mpf_t d;
  mpf_init_set_d(d,1.01);
  while (mpf_cmp(y,d)>0) {
    mpf_sqrt(y,y);
    n++;
  }
  mpf_clear(d);
  mpf_ui_sub(y,1,y);
  unsigned long i;
  mpf_t s;
  mpf_t s1;
  mpf_t t;
  mpf_t t1;
  mpf_init_set_ui(s,0);
  mpf_init_set(t,y);
  mpf_init_set(t1,y);
  mpf_init(s1);
  for(i=2;;i++) {
    mpf_add(s1,s,t);
    if (!mpf_cmp(s,s1)) break;
    mpf_set(s,s1);
    mpf_mul(t1,t1,y);
    mpf_div_ui(t,t1,i);
  }
  mpf_clear(t);
  mpf_clear(t1);
  mpf_clear(s1);
  mpf_clear(y);
  mpf_neg(s,s);
  mpf_mul_2exp(s,s,n);
  ConvToGMPNumber(r,s,0);
  mpf_clear(s);
}

static void GMPNumberPi(GMPNumber& r, long prec)
{
  mpf_set_default_prec(prec<<2);
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
  mpf_clear(sum1);
  mpf_clear(t);
  mpf_clear(s1);
  mpf_clear(t1);
  mpf_clear(negfourth);
  mpf_clear(g);
  ConvToGMPNumber(r,s,prec+1);
  mpf_clear(s);
}

static void GMPNumberFloor(GMPNumber& r, GMPNumber& x)
{
  if (x.exp<0) {
    mpz_t factor;
    mpz_init_set_ui(factor,10);
    mpz_pow_ui(factor,factor,-x.exp);
    mpz_t rem;
    mpz_init(rem);
    mpz_fdiv_q(r.man,x.man,factor);
    mpz_clear(rem);
    mpz_clear(factor);
    r.exp=0;
  } else GMPNumberCopy(r,x);
}

static void GMPNumberCeil(GMPNumber& r, GMPNumber& x)
{
  if (x.exp<0) {
    mpz_t factor;
    mpz_init_set_ui(factor,10);
    mpz_pow_ui(factor,factor,-x.exp);
    mpz_cdiv_q(r.man,x.man,factor);
    mpz_clear(factor);
    r.exp=0;
  } else GMPNumberCopy(r,x);
}

static void GMPNumberRound(GMPNumber& r, GMPNumber& x)
{
  long exp = x.exp;
  if (exp<0) {
    exp = -exp-1;
    mpz_t factor;
    mpz_init_set_ui(factor,10);
    mpz_t half;
    mpz_init_set_ui(half,5);
    if (exp) {
      mpz_pow_ui(factor,factor,exp);
      mpz_mul(half,half,factor);
      mpz_mul_ui(factor,factor,10);
    }
    mpz_add(r.man,x.man,half);
    mpz_fdiv_q(r.man,r.man,factor);
    mpz_clear(factor);
    mpz_clear(half);
    r.exp=0;
  } else GMPNumberCopy(r,x);
}

void GMPNumberNeg(GMPNumber& r, GMPNumber& x)
{
  mpz_neg(r.man,x.man);
}

void GMPNumberAbs(GMPNumber& r, GMPNumber& x)
{
  mpz_abs(r.man,x.man);
}

static void GMPNumberShiftLeft(GMPNumber& r, GMPNumber& x, unsigned long d)
{
  GMPNumberZeroFill(r,x);
  mpz_mul_2exp(r.man,r.man,d);
}

static void GMPNumberShiftRight(GMPNumber& r, GMPNumber& x, unsigned long d)
{
  GMPNumberZeroFill(r,x);
  mpz_fdiv_q_2exp(r.man,r.man,d);
}

void SetMPFPrecision(LispInt aPrecision)
{
// FIXME this is still very primitive
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
    GMPNumber r;
    initGMPNumber(r,res);
    LispStringPtr result = GMPNumberToString(r, aHashTable);
    clearGMPNumber(r);
    mpz_clear(i1);
    mpz_clear(i2);
    mpz_clear(res);
    return result;
}

LispStringPtr LispFactorial(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision)
{
    LispInt nr = InternalAsciiToInt(int1);
    Check(nr>=0,KLispErrInvalidArg);
    GMPNumber r;
    initGMPNumber(r);
    mpz_fac_ui (r.man, nr);
    LispStringPtr result = GMPNumberToString(r, aHashTable);
    clearGMPNumber(r);
    return result;
}

LispStringPtr AddFloat(LispCharPtr int1, LispCharPtr int2,
                       LispHashTable& aHashTable,LispInt aPrecision)
{
  GMPNumber r,x,y;
  initGMPNumber(r);
  initGMPNumber(x,int1);
  initGMPNumber(y,int2);
  GMPNumberAdd(r,x,y);        
  LispStringPtr result = GMPNumberToString(r, aHashTable, aPrecision);
  clearGMPNumber(r);
  clearGMPNumber(x);
  clearGMPNumber(y);
  return result;
}

LispStringPtr PlusFloat(LispCharPtr int1,LispHashTable& aHashTable
                       ,LispInt aPrecision)
{
// RVS 16 June 2001:
// TODO: is this routine ever really used?
  GMPNumber x;
  initGMPNumber(x,int1);
  LispStringPtr result = GMPNumberToString(x, aHashTable, aPrecision);
  clearGMPNumber(x);
  return result;
}

LispStringPtr SubtractFloat(LispCharPtr int1, LispCharPtr int2,
                            LispHashTable& aHashTable,LispInt aPrecision)
{
  GMPNumber r,x,y;
  initGMPNumber(r);
  initGMPNumber(x,int1);
  initGMPNumber(y,int2);
  GMPNumberSubtract(r,x,y);        
  LispStringPtr result = GMPNumberToString(r, aHashTable, aPrecision);
  clearGMPNumber(r);
  clearGMPNumber(x);
  clearGMPNumber(y);
  return result;
}

LispStringPtr NegateFloat(LispCharPtr int1, LispHashTable& aHashTable
                          ,LispInt aPrecision)
{
  GMPNumber x;
  initGMPNumber(x,int1);
  GMPNumberNeg(x,x);
  LispStringPtr result = GMPNumberToString(x, aHashTable, aPrecision);
  clearGMPNumber(x);
  return result;
}

LispStringPtr DivideFloat(LispCharPtr int1, LispCharPtr int2,
                          LispHashTable& aHashTable,LispInt aPrecision)
{
  GMPNumber r,x,y;
  initGMPNumber(r);
  initGMPNumber(x,int1);
  initGMPNumber(y,int2);
  GMPNumberDivide(r,x,y,aPrecision);        
  LispStringPtr result = GMPNumberToString(r, aHashTable, aPrecision);
  clearGMPNumber(r);
  clearGMPNumber(x);
  clearGMPNumber(y);
  return result;
}

LispStringPtr MultiplyFloat(LispCharPtr int1, LispCharPtr int2,
                            LispHashTable& aHashTable,LispInt aPrecision)
{
  GMPNumber r,x,y;
  initGMPNumber(r);
  initGMPNumber(x,int1);
  initGMPNumber(y,int2);
  GMPNumberMultiply(r,x,y,aPrecision);        
  LispStringPtr result = GMPNumberToString(r, aHashTable, aPrecision);
  clearGMPNumber(r);
  clearGMPNumber(x);
  clearGMPNumber(y);
  return result;
}

LispStringPtr PowerFloat(LispCharPtr int1, LispCharPtr int2,
                         LispHashTable& aHashTable,LispInt aPrecision)
{
//TODO: handle case int2 != integer
  GMPNumber r,x;
  long y = atol(int2);
  initGMPNumber(r);
  initGMPNumber(x,int1);
  GMPNumberPower(r,x,y,aPrecision);        
  LispStringPtr result = GMPNumberToString(r, aHashTable, aPrecision);
  clearGMPNumber(r);
  clearGMPNumber(x);
  return result;
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
  GMPNumber x;
  initGMPNumber(x,int1);
  GMPNumberExp(x,x,aPrecision);
  LispStringPtr result = GMPNumberToString(x, aHashTable, aPrecision);
  clearGMPNumber(x);
  return result;
}

LispStringPtr LnFloat(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision)
{
  GMPNumber x;
  initGMPNumber(x,int1);
  GMPNumberLog(x,x,aPrecision);
  LispStringPtr result = GMPNumberToString(x, aHashTable, aPrecision);
  clearGMPNumber(x);
  return result;
}


LispStringPtr SqrtFloat(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision)
{
  GMPNumber x;
  initGMPNumber(x,int1);
  GMPNumberSqrt(x,x,aPrecision);
  LispStringPtr result = GMPNumberToString(x, aHashTable, aPrecision);
  clearGMPNumber(x);
  return result;
}

LispStringPtr AbsFloat(LispCharPtr int1, LispHashTable& aHashTable
                          ,LispInt aPrecision)
{
  GMPNumber x;
  initGMPNumber(x,int1);
  GMPNumberAbs(x,x);
  LispStringPtr result = GMPNumberToString(x, aHashTable, aPrecision);
  clearGMPNumber(x);
  return result;
}

LispBoolean LessThan(LispCharPtr int1, LispCharPtr int2,
                       LispHashTable& aHashTable,LispInt aPrecision)
{
//    SetMPFPrecision(aPrecision);
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
//    SetMPFPrecision(aPrecision);
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
  GMPNumber x;
  initGMPNumber(x,int1);
  unsigned long bits = atol(int2);
  GMPNumberShiftLeft(x,x,bits);
  LispStringPtr result = GMPNumberToString(x, aHashTable, aPrecision);
  clearGMPNumber(x);
  return result;
}


LispStringPtr ShiftRight( LispCharPtr int1, LispCharPtr int2, LispHashTable& aHashTable,LispInt aPrecision)
{
  GMPNumber x;
  initGMPNumber(x,int1);
  unsigned long bits = atol(int2);
  GMPNumberShiftRight(x,x,bits);
  LispStringPtr result = GMPNumberToString(x, aHashTable, aPrecision);
  clearGMPNumber(x);
  return result;
}


LispStringPtr FromBase( LispCharPtr int1, LispCharPtr int2, LispHashTable& aHashTable,
                        LispInt aPrecision)
{
//TODO handle integer case
    SetMPFPrecision(aPrecision);
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
    SetMPFPrecision(aPrecision);
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
  GMPNumber x;
  initGMPNumber(x,int1);
  GMPNumberFloor(x,x);
  LispStringPtr result = GMPNumberToString(x, aHashTable, aPrecision);
  clearGMPNumber(x);
  return result;
}

LispStringPtr CeilFloat( LispCharPtr int1, LispHashTable& aHashTable,
                         LispInt aPrecision)
{
  GMPNumber x;
  initGMPNumber(x,int1);
  GMPNumberCeil(x,x);
  LispStringPtr result = GMPNumberToString(x, aHashTable, aPrecision);
  clearGMPNumber(x);
  return result;
}

LispStringPtr ModFloat( LispCharPtr int1, LispCharPtr int2, 
                        LispHashTable& aHashTable, LispInt aPrecision)
{
  GMPNumber r,x,y;
  initGMPNumber(r);
  initGMPNumber(x,int1);
  initGMPNumber(y,int2);
  GMPNumberMod(r,x,y);        
  LispStringPtr result = GMPNumberToString(r, aHashTable, aPrecision);
  clearGMPNumber(r);
  clearGMPNumber(x);
  clearGMPNumber(y);
  return result;
}

LispStringPtr DivFloat( LispCharPtr int1, LispCharPtr int2, LispHashTable& aHashTable,
                        LispInt aPrecision)
{
  GMPNumber r,x,y;
  initGMPNumber(r);
  initGMPNumber(x,int1);
  initGMPNumber(y,int2);
  GMPNumberDiv(r,x,y);        
  LispStringPtr result = GMPNumberToString(r, aHashTable, aPrecision);
  clearGMPNumber(r);
  clearGMPNumber(x);
  clearGMPNumber(y);
  return result;
}

LispStringPtr PiFloat( LispHashTable& aHashTable,
                        LispInt aPrecision)
{
  GMPNumber x;
  initGMPNumber(x);
  GMPNumberPi(x,aPrecision);
  LispStringPtr result = GMPNumberToString(x, aHashTable, aPrecision);
  clearGMPNumber(x);
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

#else // if not NO_USE_BIGFLOAT
LispInt NumericSupportForMantissa()
{
    return LispTrue;
}

const LispCharPtr NumericLibraryName()
{
    return BigNumber::NumericLibraryName();
}


#endif // NO_USE_BIGFLOAT

#ifdef USE_NEW_BIGNUM

/// Implementation of BigFloat and BigInt through GMP.



//////////////////////////////////////////////////
///// Start of BigFloat implementation
//////////////////////////////////////////////////

// initialization, set precision
void BigFloat::init(LispInt aPrecision)
{
	mpf_init2(iFloat, aPrecision);
	iPrecision = aPrecision;
}

/// assign a float from given string, using exactly aPrecision *bits*
BigFloat::BigFloat(const LispCharPtr aString,LispInt aPrecision,LispInt aBase)
{
	init(aPrecision);
	SetTo(aString, aPrecision, aBase);
}

/// copy constructor
BigFloat::BigFloat(const BigFloat& aOther)
{
	init(aOther.GetPrecision());
	SetTo(aOther);
}


// no constructors from int or double to avoid automatic conversions
BigFloat::BigFloat(LispInt aPrecision)
{
	init(aPrecision);
}


BigFloat::~BigFloat()
{
	mpf_clear(iFloat);
}

/// set precision to a given # of bits, maybe reallocate number
void BigFloat::Precision(LispInt aPrecision)
{
	iPrecision = aPrecision;
	mpf_set_prec(iFloat, iPrecision);
}

// assign from another float number
void BigFloat::SetTo(const BigFloat& aOther)
{
	Precision(aOther.GetPrecision());
	mpf_set(iFloat, aOther.iFloat);
}


// assign from another integer number
void BigFloat::SetTo(const BigInt& aOther, LispInt aPrecision)
{
	Precision(aPrecision);
	mpf_set_z(iFloat, aOther.iInt);
}


// assign from string, using exactly aPrecision *bits*
void BigFloat::SetTo(const LispCharPtr aString,LispInt aPrecision,LispInt aBase)
{
	Precision(aPrecision);
	if (mpf_set_str(iFloat, aString, aBase)!=0)
	{
		RaiseError("BigFloat::SetTo: Error reading a float from string '%s'\n", aString);
	    SetTo(0.);
		iPrecision = 0;
	}

}


// assign from a platform type
void BigFloat::SetTo(double value)
{
	Precision(53);	// 53 bits in a double
	mpf_set_d(iFloat, value);
}


/// GetMantissaExp: return a string representation of the mantissa in aResult
/// to given precision (base digits), and the exponent in the same base into aExponent
void BigFloat::GetMantissaExp(LispCharPtr aBuffer, unsigned long aBufferSize, long* aExponent, LispInt aPrecision, LispInt aBase) const
{
	mpf_get_str(aBuffer, aExponent, aBase, aPrecision, iFloat);
}


/// Give approximate representation as a double number
double BigFloat::Double() const
{
	return mpf_get_d(iFloat);
}


/// Numeric library name
const LispCharPtr BigFloat::NumericLibraryName()
{
	return BigInt::NumericLibraryName();
}


/// Compare exactly as float numbers, bit-for-bit, no rounding
LispBoolean BigFloat::Equals(const BigFloat& aOther) const
{
	return (mpf_cmp(iFloat, aOther.iFloat)==0);
}


/// check that a float has exactly an integer value
LispBoolean BigFloat::IsIntValue() const
{
	return mpf_integer_p(iFloat);
}


/// check that a float is less than aOther, exactly as floats, no rounding
LispBoolean BigFloat::LessThan(const BigFloat& aOther) const
{
	return (mpf_cmp(iFloat, aOther.iFloat)<0);
}


/// Multiply two numbers and put result in *this, result should have at least aPrecision correct digits
void BigFloat::Multiply(const BigFloat& aX, const BigFloat& aY, LispInt aPrecision)
{
	Precision(aPrecision);
	mpf_mul(iFloat, aX.iFloat, aY.iFloat);
}


/** Multiply two numbers, and add to *this (this is useful and generally efficient to implement).
* This is most likely going to be used by internal functions only, using aResult as an accumulator.
*/
void BigFloat::MultiplyAdd(const BigFloat& aX, const BigFloat& aY, LispInt aPrecision)
{	// GMP does not have this function
	BigFloat temp;
	temp.Multiply(aX, aY, aPrecision);
	Add(*this, temp, aPrecision);
}


/// Add two numbers at given precision and return result in *this
void BigFloat::Add(const BigFloat& aX, const BigFloat& aY, LispInt aPrecision)
{
	Precision(aPrecision);
	mpf_add(iFloat, aX.iFloat, aY.iFloat);
}


/// Negate the given number, return result in *this
void BigFloat::Negate(const BigFloat& aX)
{
	Precision(aX.GetPrecision());
	mpf_neg(iFloat, aX.iFloat);
}


/// Divide two numbers and return result in *this. Note: if the two arguments are integer, it should return an integer result!
void BigFloat::Divide(const BigFloat& aX, const BigFloat& aY, LispInt aPrecision)
{
	Precision(aPrecision);
	mpf_div(iFloat, aX.iFloat, aY.iFloat);
}


/// return the integer part of the number (still as float value)
void BigFloat::Floor(const BigFloat& aX)
{
	Precision(aX.GetPrecision());
	mpf_floor(iFloat, aX.iFloat);
}


/// Multiplication by a power of 2, return result in *this.
void BigFloat::Multiply2exp(const BigFloat& aX, LispInt aNrToShift)
{
	Precision(aX.GetPrecision());
	if (aNrToShift>0)
		mpf_mul_2exp(iFloat, aX.iFloat, aNrToShift);
	else
		mpf_div_2exp(iFloat, aX.iFloat, - aNrToShift);
}


/// return the binary exponent (shortcut for binary logarithm)
long BigFloat::GetBinaryExp() const
{
	long result;
	(void) mpf_get_d_2exp(&result, iFloat);
	return result;
}


/// Give sign (-1, 0, 1)
LispInt BigFloat::Sign() const
{
	return mpf_sgn(iFloat);
}


  /// Import/export underlying objects.
void BigFloat::ImportData(const void* aData)
{// assuming that aData is a pointer to a valid, initialized GMP object of type mpf_t
	Precision(mpf_get_prec(*(mpf_t*)aData));
	mpf_set(iFloat, *(mpf_t*)aData);
}

const void* BigFloat::ExportData() const
{// export a pointer to iFloat
	return (const void*)&iFloat;
}

//////////////////////////////////////////////////
///// End of BigFloat implementation
//////////////////////////////////////////////////


//////////////////////////////////////////////////
///// Start of BigInt implementation
//////////////////////////////////////////////////

void BigInt::init()
{
	mpz_init(iInt);
}

/// assign an int from given string
BigInt::BigInt(const LispCharPtr aString, LispInt aBase)
{
	init();
	SetTo(aString);
}

/// copy constructor
BigInt::BigInt(const BigInt& aOther)
{
	init();
	SetTo(aOther);
}

// no constructors from int or double to avoid automatic conversions
BigInt::BigInt()
{
	init();
}

BigInt::~BigInt()
{
	mpz_clear(iInt);
}

// assign from another number
void BigInt::SetTo(const BigFloat& aOther)
{
	mpz_set_f(iInt, aOther.iFloat);
}

void BigInt::SetTo(const BigInt& aOther)
{
	mpz_set(iInt, aOther.iInt);
}

// assign from string
void BigInt::SetTo(const LispCharPtr aString, LispInt aBase)
{

  	if (mpz_set_str(iInt, aString, aBase)!=0)
	{
	    RaiseError("BigInt::SetTo: Error reading an integer from string '%s'\n", aString);
	}
}

// assign from a platform type
void BigInt::SetTo(long value)
{
	mpz_set_si(iInt, value);
}


/// ToString: return a string representation in the given aBuffer
void BigInt::ToString(LispCharPtr aBuffer, unsigned long aBufferSize, LispInt aBase) const
{
	mpz_get_str(aBuffer, aBase, iInt);
}


/// Give approximate representation as a double number
double BigInt::Double() const
{
	return mpz_get_d(iInt);
}


/// Numeric library name
const LispCharPtr BigInt::NumericLibraryName()
{
	static char buf[20];
	snprintf(buf, 18, "GMP %s", gmp_version); 
	return buf;
}

/// check if integers are equal
LispBoolean BigInt::Equals(const BigInt& aOther) const
{
	return (mpz_cmp(iInt, aOther.iInt)==0);
}


/// check if the integer fits into a platform long
LispBoolean BigInt::IsSmall() const
{
	return mpz_fits_slong_p(iInt);
}


/// check that an integer is smaller
LispBoolean BigInt::LessThan(const BigInt& aOther) const
{
	return (mpz_cmp(iInt, aOther.iInt)<0);
}


/// Multiply two integers and put result in *this
void BigInt::Multiply(const BigInt& aX, const BigInt& aY)
{
	mpz_mul(iInt, aX.iInt, aY.iInt);
}

/** Multiply two numbers, and add to *this (this is useful and generally efficient to implement).
* This is most likely going to be used by internal functions only, using aResult as an accumulator.
*/
void BigInt::MultiplyAdd(const BigInt& aX, const BigInt& aY)
{
	mpz_addmul(iInt, aX.iInt, aY.iInt);
}

/// Add two integers and return result in *this
void BigInt::Add(const BigInt& aX, const BigInt& aY)
{
	mpz_add(iInt, aX.iInt, aY.iInt);
}

/// Negate the given number, return result in *this
void BigInt::Negate(const BigInt& aX)
{
	mpz_neg(iInt, aX.iInt);
}

/// Divide two numbers and return result in *this. (This is the integer division!)
void BigInt::Div(const BigInt& aX, const BigInt& aY)
{
	mpz_tdiv_q(iInt, aX.iInt, aY.iInt);	// e.g. divide -5/3 = -1
}


/// integer operation: *this = y mod z
void BigInt::Mod(const BigInt& aX, const BigInt& aY)
{
	mpz_mod(iInt, aX.iInt, aY.iInt);
}

/// Bitwise operations, return result in *this.
void BigInt::ShiftLeft(const BigInt& aX, LispInt aNrToShift)
{
	mpz_mul_2exp(iInt, aX.iInt, aNrToShift);
}

void BigInt::ShiftRight(const BigInt& aX, LispInt aNrToShift)
{
	mpz_tdiv_q_2exp(iInt, aX.iInt, aNrToShift);
}

void BigInt::BitAnd(const BigInt& aX, const BigInt& aY)
{
	mpz_and(iInt, aX.iInt, aY.iInt);
}

void BigInt::BitOr(const BigInt& aX, const BigInt& aY)
{
	mpz_ior(iInt, aX.iInt, aY.iInt);
}

void BigInt::BitXor(const BigInt& aX, const BigInt& aY)
{
	mpz_xor(iInt, aX.iInt, aY.iInt);
}

void BigInt::BitNot(const BigInt& aX)
{
	mpz_com(iInt, aX.iInt);
}

/// Bit count operation: return the number of significant bits,
/// give bit count as a platform integer
long BigInt::BitCount() const
{
	long result;
	(void) mpz_get_d_2exp(&result, iInt);
	return result;
}

/// Give sign (-1, 0, 1)
LispInt BigInt::Sign() const
{
	return mpz_sgn(iInt);
}

  /// Import/export underlying objects.
void BigInt::ImportData(const void* aData)
{// assuming that aData is a pointer to the GMP object mpz_t
	mpz_set(iInt, *(mpz_t*)aData);
}

const void* BigInt::ExportData() const
{// export a pointer to iInt
	return (const void*)&iInt;
}

//////////////////////////////////////////////////
///// End of BigInt implementation
//////////////////////////////////////////////////

#else	// if not USE_NEW_BIGNUM

//////////////////////////////////////////////////
///// BigNumber implementation by wrapping GMP
///// (coded by Serge Winitzki)
//////////////////////////////////////////////////

/// The number class describes either integers or floats, depending on the type_ flag. However, both the float part (float_, exponent_) and the integer part (int_) are always initialized for the life of the BigNumber object.
/// Wrapping of the GMP library is done using its mpz_t / mpf_t pointer types.
/// A special mode is that of "exp-float" number. Then the type_ flag has the value KExpFloat. The number is equal to float_ * 2^exponent_ where exponent_ is a big integer and float_ is a normal GMP float value with a normal exponent. Otherwise the value of the exponent_ stays zero (actually it is ignored).
/// The exp-float mode is to be used only for really large or really small exponents. (not yet fully implemented)
/// A BigFloat object contains valid, initialized GMP objects at all times. It contains a GMP integer and a GMP floating-point number. Only one of them is used at any time, depending on the current type of the number.

// TO DO:
// no need to call _ui / _si functions, as gmp tells us not to worry.
// make sure that precision control works correctly.
// test precision on from/to string conversion.
// implement exp-float.


// debugging
#define RaiseError1 printf

BigNumber::BigNumber(LispInt aPrecision) { iPrecision = aPrecision; init(); }

void BigNumber::init()
{// this function should be called only from constructors because it is not recommended to re-initialize a non-cleared gmp object
	turn_int();	// by default all numbers are created integer
	mpz_init2(int_, 32);	// default precision
	mpf_init2(float_, 32);
	mpz_init2(exponent_, 32);
	iPrecision = 32;
}

BigNumber::~BigNumber()
{
	mpz_clear(int_);
	mpf_clear(float_);
	mpz_clear(exponent_);
}
// construct from string
BigNumber::BigNumber(const LispCharPtr aString,LispInt aPrecision,LispInt aBase)
{
//	iPrecision = aPrecision;	// iPrecision is going to be set in SetTo
	init();
	SetTo(aString, aPrecision, aBase);
}


/// copy constructor
BigNumber::BigNumber(const BigNumber& aOther)
{
//	iPrecision = aOther.GetPrecision();
	init();
	SetTo(aOther);
}


// assign from another number
void BigNumber::SetTo(const BigNumber& aOther)
{
  if (this == &aOther)
	return;
  else
  if (aOther.IsInt())
  {
	turn_int();
	mpz_set(int_, aOther.int_);
  }
  else
  {
	turn_float();
	iPrecision = aOther.GetPrecision();
	mpf_set_d(float_, 1.);	// otherwise gmp doesn't really set the precision? FIXME

	mpf_set_prec(float_, mpf_get_prec(aOther.float_));
	mpf_set(float_, aOther.float_);
	if (aOther.IsExpFloat())
	  mpz_set(exponent_, aOther.exponent_);
  }
  type_ = aOther.type_;
}

// assign from string, result is always a float type
void BigNumber::SetTo(const LispCharPtr aString,LispInt aPrecision,LispInt aBase)
{// FIXME: we use gmp to read into float_, so we cannot read expfloats, e.g. 1.3e123412341234123412341234, which should be possible.
  if (aBase<2 || aBase>32)
  {
  	RaiseError("BigNumber::SetTo(string): error: aBase should be between 2 and 32, not %d\n", aBase);
  	return;
  }
	// decide whether the string is an integer or a float
  if (strchr(aString, '.') || aBase<=10 && (strchr(aString, 'e') || strchr(aString,'E')) || strchr(aString, '@'))
  {	// converting to a float
	// estimate the number of bits we need to have
	  // find the first significant digit:
	  LispInt digit1 = strspn(aString, ".-0");	// initial zeros are not significant
	 // find the number of significant base digits (sig_digits)
	  LispInt sig_digits = strcspn(aString+digit1, (aBase<=10) ? "-eE@" : "-@"); // trailing zeros and . *are* significant, do not include them in the sets
	  if (sig_digits<=0)
	  {	// this is when we have "0." in various forms
		  sig_digits=1;
		  // precision of 0.: for now let's set it to 1 significant digits (in principle we might allow things like 0.00e10 to denote special floating zeros but it's probably not very useful, since our floating zeros have binary precision and not decimal)
	  }
	  else
	  {	// our number is nonzero
		  if (strchr(aString+digit1, '.'))
			  sig_digits--;	// this is when we have "1.000001" where "." is not a digit, so need to decrement
	  }
	  // ok, so we need to represent MAX(aPrecision,sig_digits) digits in base aBase
	  iPrecision=(LispInt) digits_to_bits(MAX(aPrecision,sig_digits), aBase);

	  turn_float();
//	mpf_set_d(float_, 1.);	// otherwise gmp doesn't really set the precision? FIXME
	  // GMP does NOT read all digits from the string unless the precision is preset to the right number of bits.
	  mpf_set_prec(float_, iPrecision+GUARD_BITS);
	
	if (mpf_set_str(float_, aString, aBase)!=0)
	{// FIXME: this clause is executed when there is an error in the string (GMP couldn't read it). Need to signal the error somehow.
	    RaiseError("BigNumber::SetTo: Error reading a float from string '%s'\n", aString);
	    SetTo(0.);
		iPrecision = 0;
	}
  }
  else
  {	// converting to an integer, aPrecision is ignored
    turn_int();
  	if (mpz_set_str(int_, aString, aBase)!=0)
	{
	    RaiseError("BigNumber::SetTo: Error reading an integer from string '%s'\n", aString);
	    SetTo(0);
	}
  }
}


// assign from a platform type
void BigNumber::SetTo(long value)
{
  turn_int();
  mpz_set_si(int_, value);
}


void BigNumber::SetTo(double value)
{
  turn_float();
  mpf_set_prec(float_, 53);
  mpf_set_d(float_, value);
  iPrecision = 53;	// standard double has 53 bits
}


// Convert back to other types
/// ToString : return string representation of the number at given precision (base digits) in aResult

void BigNumber::ToString(LispString& aResult, LispInt aPrecision, LispInt aBase) const
{
  if (aBase<2 || aBase>32)
  {
  	RaiseError("BigNumber::ToString: error: aBase should be between 2 and 32, not %d\n", aBase);
  	return;
  }
  if (IsInt())
  {	// find how many chars we need
	LispInt size=mpz_sizeinbase(int_, aBase);
	char* buffer=(char*)malloc(size+2);
	mpz_get_str(buffer, aBase, int_);
	// assign result
	aResult.SetStringCounted(buffer, strlen(buffer));
	free(buffer);
  }
  else
  {	// we have a floating-point number. Note that we cannot print exp-float numbers (except in base 2^n).
  // FIXME: maybe allow printing exp-float numbers in bases 2, 4, 8, 16?
    if (IsExpFloat())
    {
	    RaiseError("BigNumber::ToString: printing exp-floats is not implemented\n");
	    return;
    }
	// note: aPrecision means *base digits* here
	LISPASSERT(aPrecision>=0);
	if (aPrecision <=0)
		aPrecision = 1;	// refuse to print with 0 or fewer digits
    unsigned long size=(unsigned long)aPrecision;
    // how many base digits to print
    unsigned long print_prec = bits_to_digits((unsigned long)iPrecision, aBase);
	print_prec = MIN(print_prec, (unsigned long)aPrecision);
    // the size needed to print the exponent cannot be more than 200 chars since we refuse to print exp-floats
    size += 200;
    char* buffer=(char*)malloc(size);
    if (!buffer)
    {
	    RaiseError("BigNumber::ToString: out of memory, need %ld chars\n", size);
	    return;
    }
    char* offset = buffer;
    if (Sign()==0)
    {    // print zero - note that gmp does not print "0.", it prints nothing at all.
    // according to the latest revelations, the floating-point zero must be printed as "0."
	    strcpy(offset, "0."); // end of string is here too
    }
    else
    {
	// print a number using fixed point if the exponent is between -4 and +8
	const long lower_exp=-4, upper_exp=8;
//	gmp_snprintf(buffer, size-1, "%*g", float_, aPrecision);
// cannot use gmp_printf because we need to print in a given base
	long exp_small;
	// get the exponent in given base and print the string at the same time
	// string is printed starting at an offset to allow for leading zeros/decimal point
	offset += 2-lower_exp;
	(void) mpf_get_str(offset, &exp_small, aBase, print_prec, float_);
	if (lower_exp <= exp_small && exp_small <= upper_exp)
	{	// print in fixed point.
		if (exp_small>0)
		{	// Insert a point somewhere in the middle. Uses memmove() if needed
			// *offset contains something like "-123" and exp_small is 2, then we need to get "-12.3". Or perhaps we had "-123" and exp_small=5, then we need to get "-12300."
			// point position
			char* point_pos = offset + exp_small;
			if (Sign()<0) ++point_pos;
			// decide if the point is within the printed part of the string
			size=strlen(offset);
			if (point_pos-offset>=(signed)size) // ok here because point_pos-offset is always positive if exp_small>0
			{	// no moving required, but perhaps need to pad with zeros, and we need to add the point
				int i=size;
				for(; i<point_pos-offset; ++i)
				  offset[i] = '0';
				strcpy(offset+i, ".");	// end of string
			}
			else
			{	// need to insert the point and shift the rest of the string
				memmove(point_pos+1, point_pos, size-(point_pos-offset)+1);
				*point_pos = '.';
			}
		}
		else
		{// add leading zeros and decimal point, handle sign
		// we need to add (1-exp_small) zeros total
			if (Sign()<0)
			{	// *offset contains something like "-123..."
				// and instead we need -0.00123... (if small_exp=-2)
				// final offset:
				offset = offset-(2-exp_small);
				memcpy(offset, "-0.",3);	// 3 chars added
				// fill more zeros
				for (int i=3; i<3-exp_small; ++i)
				  offset[i] = '0';
				// done printing
			}
			else	// the number is surely nonzero.
			{	// *offset contains something like "123..."
				// and instead we need 0.00123... (if small_exp=-2)
				// final offset:
				offset = offset-(2-exp_small);
				memcpy(offset, "0.",2);	// 2 chars added
				// fill more zeros
				for (int i=2; i<2-exp_small; ++i)
				  offset[i] = '0';
				// done printing to buffer
			}
		}	// end of printing numbers < 1
	}	// end of printing in fixed point
	else
	{ // printing in floating point
		// if the number is negative, need to handle the - sign
		offset -= 2;	// final offset already known
		if (Sign()<0)
			memcpy(offset, "-0.", 3);
		else
			memcpy(offset, "0.", 2);
		
		// now print the exponent: either "e" or "@" and then the long integer
		size = strlen(offset);
		offset[size] = (aBase<=10) ? 'e' : '@';	// one character there, start exponent at offset size+1. Printing as per GMP instructions.
		// compute the total exponent
		BigNumber exp_total;
		exp_total.SetTo((LispInt)exp_small);
//		if (IsExpFloat()) mpz_add(exp_total.int_, exp_total.int_, exponent_);	// this won't work since exp_ is the binary exponent
		mpz_get_str(offset+size+1, aBase, exp_total.int_);
	}
	// assign result
     }	// finished printing a float
     aResult.SetStringCounted(offset, strlen(offset));
     free(buffer);
   } // finished printing a nonzero number
}

/// Give approximate representation as a double number
double BigNumber::Double() const
{
  if(IsInt())
	return mpz_get_d(int_);
  else if (IsExpFloat())
  {
	  RaiseError("BigNumber::Double: error: cannot convert an ExpFloat to double\n");
	  return 0;
  }
  else
	return mpf_get_d(float_);
}

/// get library name
const LispCharPtr BigNumber::NumericLibraryName()
{
	static char buf[20];
	snprintf(buf, 18, "GMP %s", gmp_version); 
	return buf;
}



//basic object manipulation
LispBoolean BigNumber::Equals(const BigNumber& aOther) const
{
// bit counts and signs must not be different
  long B_x = this->BitCount(), B_y = aOther.BitCount();
  if (B_x != B_y || this->Sign() * aOther.Sign() == -1)
  	return LispFalse;
  // any two zeros are equal
  if (this->Sign() == 0 && aOther.Sign() == 0)
  	return LispTrue;
  // now check for zero compared with nonzero
  if (
		  // *this is zero and aOther is nonzero
		  this->Sign() == 0 && B_y < - this->GetPrecision()
		  ||
		  // *this is zero and aOther is nonzero
		  aOther.Sign() == 0 && B_x < - aOther.GetPrecision()
	) return LispTrue;
  // now we may assume that both are nonzero and that the bit counts are equal
  if (this->IsInt())
    if (aOther.IsInt())
    	return mpz_cmp(int_, aOther.int_)==0;
    else	// comparing integers with floats: must convert both to floats
    {	// *this is integer, aOther is float
		// convert *this to a float and compare as floats
    	BigNumber x(*this);
		x.BecomeFloat(aOther.GetPrecision()+1);	// 1 guard digit here
		return x.Equals(aOther);
    }
  else
    if (aOther.IsInt())
    {	// *this is float, aOther is integer
	return aOther.Equals(*this);
    }
    else
    /* ExpFloats are on hold for now
	 // two floats are equal when the values are equal and both not ExpFloat, or if they are both ExpFloat and their exponents are also equal
    	return
		mpf_cmp(float_, aOther.float_)==0
		&& (!IsExpFloat() && !aOther.IsExpFloat() 
		  || IsExpFloat() && aOther.IsExpFloat() 
		    && mpz_cmp(exponent_, aOther.exponent_)==0);
    */
    // two floats x, y are equal if |x-y| < max(Delta x, Delta y), where Delta x is the absolute error of x
    {
    	BigNumber x_minus_y(*this);
	// compute min(m,n)
	long min_prec = MIN(this->GetPrecision(), aOther.GetPrecision());
	// compute x-y
	x_minus_y.Negate(x_minus_y);
	x_minus_y.Add(x_minus_y, aOther, min_prec);
	// compute Abs(x-y)
	if (x_minus_y.Sign()<0) x_minus_y.Negate(x_minus_y);
	long B_x_minus_y = x_minus_y.BitCount();
	// perform quick checks on signs and bit counts
	if (x_minus_y.Sign()==0)	// got "exact" float 0, bit count will be wrong
		return LispTrue;
	if (B_x_minus_y <= B_y-min_prec-1)
		return LispTrue;
	if (B_x_minus_y > B_y-min_prec+1)
		return LispFalse;
	// quick checks didn't work, compute Abs(x)*2^(-m) and Abs(y)*2^(-n)
	BigNumber abs_x(*this);
	if (abs_x.Sign()<0) abs_x.Negate(abs_x);
	abs_x.ShiftRight(abs_x, this->GetPrecision());
	BigNumber abs_y(aOther);
	if (abs_y.Sign()<0) abs_y.Negate(abs_y);
	abs_y.ShiftRight(abs_y, aOther.GetPrecision());
	// return true if Abs(x-y) < Abs(x)*2^(-m) or if Abs(x-y) < Abs(y)*2^(-n)
	return (mpf_cmp(x_minus_y.float_, abs_x.float_) < 0) || (mpf_cmp(x_minus_y.float_, abs_y.float_) < 0);
    }
}


LispBoolean BigNumber::IsInt() const
{
	return (type_ & KInt)!=0;
}


LispBoolean BigNumber::IsExpFloat() const
{
	return (type_ & KExpFloat)==KExpFloat;
}

LispBoolean BigNumber::IsIntValue() const
{
	// *this has integer value if it's an integer or if it does not have enough digits to be non-integer, or if it's exactly equal to an integer
  if (IsInt() || GetPrecision() < BitCount() || mpf_integer_p(float_))
	  return LispTrue;
  // check if the number is integer within its precision
  // compute y=x-Floor(x)
  BigNumber y;
  y.Floor(*this);
  y.Negate(y);
  y.Add(y,*this,GetPrecision()+1);	// 1 guard digit here
  // return true if B(y)<-n
  return y.BitCount() < - this->GetPrecision();
}


LispBoolean BigNumber::IsSmall() const
{
  if (IsInt())
  	return mpz_fits_slong_p(int_);
  else
  // a function to test smallness of a float is not present in GMP, need to code a workaround to determine whether a mpf_t fits into double.
  {
	long exp_small;
	(void) mpf_get_d_2exp(&exp_small, float_);
	return
	(
		!IsExpFloat() 
		&& iPrecision <= 53	// standard float is 53 bits
		&& fabs(exp_small)<1021
	);
	// standard range of double precision is about 53 bits of mantissa and binary exponent of about 1021
  }

}


void BigNumber::BecomeInt()
{
  if (!IsInt())
  {
	iPrecision = 0;	// arbitrary but nonnegative
	turn_int();
	mpz_set_f(int_, float_);
  }
}


/// Transform integer to float, setting a given bit precision.
/// Note that aPrecision=0 means automatic setting (just enough digits to represent the integer).
void BigNumber::BecomeFloat(LispInt aPrecision)
{
//	if (Sign()!=0 && aPrecision<0) RaiseError("BigNumber::BecomeFloat debug: %e, %d", Double(), aPrecision);
	if (Sign()!=0) LISPASSERT(aPrecision >= 0);
  if (IsInt())
  {	// the precision of the resulting float is at least the number of bits in the int_, i.e. its bit count
	iPrecision = MAX((LispInt)BitCount(), aPrecision)+GUARD_BITS;
	mpf_set_prec(float_, iPrecision);
	mpf_set_z(float_, int_);	// FIXME need to test that all digits are preserved after this
  	turn_float();
  }
}


LispBoolean BigNumber::LessThan(const BigNumber& aOther) const
{ // first check whether the two numbers are equal up to precision
  if (this->Equals(aOther))
  	return LispFalse;
  // not equal, now we can simply compare the values
  if (IsInt())
    if (aOther.IsInt())
  	return mpz_cmp(int_, aOther.int_)<0;
    else
    {	// need to temporarily convert us to a float
    	BigNumber temp(*this);
	temp.BecomeFloat();
    	return temp.LessThan(aOther);
    }
  else
    if (aOther.IsInt())
    {	// need to temporarily convert aOther to a float
    	BigNumber temp(aOther);
	temp.BecomeFloat();
    	return this->LessThan(temp);
    }
    else
  	return mpf_cmp(float_, aOther.float_)<0;
	// FIXME: this does not work for KExpFloat
}


//arithmetic
/// Multiply two numbers at given precision and put result in *this
void BigNumber::Multiply(const BigNumber& aX, const BigNumber& aY, LispInt aPrecision)
{
	LISPASSERT(aPrecision>=0);
  if (aX.IsInt())
    if (aY.IsInt())	// both integer
    {
      if (!IsInt()) turn_int(); // if we are float, then we are not aX or aY, can clear our values
      mpz_mul(int_, aX.int_, aY.int_);
    }
    else	// int + float, need to promote to float
    {
		if (aX.Sign()==0)
		{	// multiplying by integer 0, set result to integer 0
			SetTo(0);
		}
		else
		{	// multiplying by nonzero integer, precision is unmodified
    	  BigNumber temp(aX);
    	  temp.BecomeFloat(aY.GetPrecision()+GUARD_BITS);	// enough digits here
    	  Multiply(temp, aY, aPrecision);
		  iPrecision = MIN(aPrecision, aY.GetPrecision());
		}
    }
  else
    if (aY.IsInt())	// float + int, need to promote to float
    {
      Multiply(aY, aX, aPrecision);
    }
    else	// float + float
    {
      if (IsInt()) turn_float(); // if we are int, then we are not aX or aY, can clear our values
	  if (aX.Sign()*aY.Sign()==0)
	  {	// one or both are zero
		// use a modified definition of BitCount now
		long B_x = (aX.Sign()==0) ? 1-aX.GetPrecision() : aX.BitCount();
		long B_y = (aY.Sign()==0) ? 1-aY.GetPrecision() : aY.BitCount();
		long xy_prec = 2-B_x-B_y;
		SetTo(0.);
		iPrecision = xy_prec;
	  }
	  else
	  {
		// both aX and aY are nonzero
		long xy_prec = MIN(aX.GetPrecision(), aY.GetPrecision()) - DIST(aX.GetPrecision(), aY.GetPrecision());
		// set our new precision
		mpf_set_prec(float_, MIN((long)aPrecision, xy_prec)+GUARD_BITS);	// GMP does calculations in target precision
		mpf_mul(float_, aX.float_, aY.float_);
		iPrecision = xy_prec;
/*		if (iPrecision<=0)
			RaiseError("BigNumber::Multiply: loss of precision with arguments %e, %e", aX.Double(), aY.Double());
*/
	  }
      // FIXME: this does not work for KExpFloat
    }
}


/* Multiply two numbers, and add to *this (this is useful and generally efficient to implement).
*/
void BigNumber::MultiplyAdd(const BigNumber& aX, const BigNumber& aY, LispInt aPrecision)
{
  if (aX.IsInt() && aY.IsInt() && IsInt())
  {// all three are integers
	mpz_addmul(int_, aX.int_, aY.int_);
  }
  else
  if (!aX.IsInt() && !aY.IsInt() && !IsInt())
  {// all three are floats
  // there is no addmul for floats in GMP, so let's not reinvent the wheel.
	  BigNumber temp;
	  temp.Multiply(aX, aY, aPrecision);
	  this->Add(*this, temp, aPrecision);
  }
  else
  {	// some are integers and some are floats. Need to promote all to floats and then call MultiplyAdd again
    if (IsInt())
    {
    	this->BecomeFloat(aPrecision+GUARD_BITS);
	MultiplyAdd(aX, aY, aPrecision);
    }
    else
    if (aX.IsInt())
    {
    	BigNumber temp(aX);
	temp.BecomeFloat(aPrecision+GUARD_BITS);
	MultiplyAdd(temp, aY, aPrecision);
    }
    else
    if (aY.IsInt())
    {	// need to promote both aX and aY to floats
      	BigNumber temp(aY);
	temp.BecomeFloat(aPrecision+GUARD_BITS);
	MultiplyAdd(aX, temp, aPrecision);
    }
  }
}


/// Add two numbers at given precision and return result in *this
void BigNumber::Add(const BigNumber& aX, const BigNumber& aY, LispInt aPrecision)
{
  LISPASSERT(aPrecision>=0);
  if (aX.IsInt())
    if (aY.IsInt())	// both integer
    {
      if (!IsInt())
		  turn_int(); // if we are float, then we are not aX or aY
      mpz_add(int_, aX.int_, aY.int_);
    }
    else	// int + float, need to promote to float
    {
      BigNumber temp(aX);
      // avoid negative precision
      temp.BecomeFloat(MAX((long)0, 1 + aY.GetPrecision()+aX.BitCount()-aY.BitCount()) +4);	// 4 guard bits here
      Add(temp, aY, aPrecision);
    }
  else
    if (aY.IsInt())	// float + int, need to promote to float
    {
      Add(aY, aX, aPrecision);
    }
    else	// float + float
    {
      if (IsInt()) turn_float(); // if we are int, then we are not aX or aY, can clear our values
	  // bit counts of aX, aY, and of absolute errors DeltaX, DeltaY
	  long B_x = aX.BitCount();
	  long B_y = aY.BitCount();
	  long B_Dx = B_x-aX.GetPrecision();
	  long B_Dy = B_y-aY.GetPrecision();
	  // check for apriori underflow
	  if (B_x<=B_Dy-1)
	  {	// neglect x, assign z=y, set precision
		  if (this!= &aY)
		  {
			  mpf_set_prec(float_, mpf_get_prec(aY.float_));
			  mpf_set(float_, aY.float_);
		  }
		  iPrecision = MIN((long)aPrecision, aY.GetPrecision()-DIST(B_x, B_Dy-1));
/*
		  if (iPrecision<=0)
			RaiseError("BigNumber::Add: loss of precision with arguments %e, %e", aX.Double(), aY.Double());
*/
	  }
	  else
	  if (B_y<=B_Dx-1)
	  {	// neglect y, assign z=x, set precision
		  Add(aY, aX, aPrecision);
	  }
	  else	 // no apriori underflow
	  {
		  // precision needed for computing x+y
		  long xy_prec = 1+MAX(B_x, B_y)-MAX(B_Dx, B_Dy);
		  // precision with which we shall actually be performing the addition
		  long real_xy_prec = xy_prec+GUARD_BITS;
		  // need to introduce a temporary here, because otherwise we might wrongly truncate float_
		  mpf_t result;
    	  // GMP performs all calculations in target precision, need to set it here
		  mpf_init2(result, real_xy_prec);
    	  mpf_add(result, aX.float_, aY.float_);
		  long B_z = 0;
		  (void) mpf_get_d_2exp(&B_z, result);	// bit count of z=x+y
		  // compute the actual precision p of the result (z)
		  // not always optimal but a good first value:
		  long p = B_z-1 - MAX(B_Dx, B_Dy) - DIST(B_Dx, B_Dy);
		  if (B_Dx > B_Dy && B_x > B_y || B_Dx < B_Dy && B_x < B_y)
		  {	// optimization 1: the error of x dominates and the value of x also dominates, or ditto for y
			  p++;
		  }
		  // optimization 2: check for minimum roundoff (when both arguments are of the same sign)
		  if (aX.Sign()*aY.Sign()==1)
		  {
			  p = MAX(p, (long)MIN(aX.GetPrecision(), aY.GetPrecision()));
		  }
		  // do not make the result more precise than asked
		  p = MIN((long)aPrecision, p);
		  // check for underflow again
		  if (p < 0 && mpf_sgn(result)!=0)
		  {	// underflow, set result to zero and reset precision
			  p = p-B_z;
			  mpf_set_d(result, 0);
		  }
		  import_gmp(result);
		  mpf_clear(result);
		  iPrecision = p;
/*  
	 	  if (iPrecision<=0)
			RaiseError("BigNumber::Add: loss of precision with arguments %e, %e", aX.Double(), aY.Double());
*/
	  }	// handled no apriori underflow
      // FIXME: this does not work for KExpFloat
    }	// handled float + float
}


/// Negate the given number, return result in *this
void BigNumber::Negate(const BigNumber& aX)
{
  if (aX.IsInt())
  {
    if (!IsInt()) turn_int();
    mpz_neg(int_, aX.int_);
  }
  else
  {
    if (IsInt()) turn_float();	// we are not aX
    mpf_neg(float_, aX.float_);
  }
  iPrecision = aX.GetPrecision();
}


/// Divide two numbers and return result in *this. Note: if the two arguments are integer, it should return an integer result!
void BigNumber::Divide(const BigNumber& aX, const BigNumber& aY, LispInt aPrecision)
{
  if (aY.Sign()==0)
  {	// zero division, report and do nothing
  	RaiseError("BigNumber::Divide: zero division request ignored\n");
	return;
  }
  if (aX.IsInt())
  {
	// check for zero
	if (aX.Sign()==0)
	{	// divide 0 by something, set result to integer 0
		SetTo(0);
	}
	else if (aY.IsInt())
    {
    	if (!IsInt()) turn_int();
		mpz_tdiv_q(int_, aX.int_, aY.int_);	// e.g. divide -5/3 = -1
    }
    else
	{	// divide nonzero integer by nonzero float, precision is unmodified
   	  BigNumber temp(aX);
   	  temp.BecomeFloat(aY.GetPrecision()+GUARD_BITS);	// enough digits here
	  long p = MIN(aPrecision, aY.GetPrecision());
   	  Divide(temp, aY, p);
	  iPrecision = p;
    }
  }
  else	// aX is a float, aY is nonzero
  {
	// check for a floating zero
	if (aX.Sign()==0)
	{
		// result is 0. with precision m-B(y)+1
		long p = aX.GetPrecision()-aY.BitCount()+1;
		SetTo(0.);
		iPrecision = p;
	}
    else if (aY.IsInt())
    {	// aY is integer, must be promoted to float
      BigNumber temp(aY);
      temp.BecomeFloat(MIN(aPrecision, aX.GetPrecision())+GUARD_BITS);
      Divide(aX, temp, aPrecision);
    }
    else
    {	// both aX and aY are nonzero floats
		if (IsInt()) turn_float();	// we are not aX or aY
		long p = MIN(aX.GetPrecision(), aY.GetPrecision()) - DIST(aX.GetPrecision(), aY.GetPrecision());
		p = MIN((long)aPrecision, p);
		mpf_set_prec(float_, p+GUARD_BITS);
		mpf_div(float_, aX.float_, aY.float_);
		iPrecision = p;
/*		if (iPrecision<=0)
			RaiseError("BigNumber::Divide: loss of precision with arguments %e, %e", aX.Double(), aY.Double());
*/
    }
  }
	// FIXME: this does not work for KExpFloat
}

void BigNumber::DumpDebugInfo()
{//FIXME
}


/// integer operation: *this = y mod z
void BigNumber::Mod(const BigNumber& aY, const BigNumber& aZ)
{
  if (aY.IsInt() && aZ.IsInt())
  {
  	if (!IsInt()) turn_int();
  	mpz_mod(int_, aY.int_, aZ.int_);
  }
}

/* this can be done later in plugins or in library-dependent wrappers
/// integer operation: *this = x^y mod z
void BigNumber::PowerMod(const BigNumber& aX, const BigNumber& aY, const BigNumber& aZ)
{
  if (aX.IsInt() && aY.IsInt() && aZ.IsInt())
  {
  	mpz_powm(int_, aX.int_, aY.int_, aZ.int_);
  }
}
*/
void BigNumber::Floor(const BigNumber& aX)
{
// check that aX is a float and that it has enough digits to evaluate its integer part
  if (!aX.IsInt() && !aX.IsExpFloat())
  {
    if(aX.GetPrecision() >= aX.BitCount())
    {	// now aX is a float for which we can evaluate Floor()
      turn_float();	// just in case we are integer; we are not aX
      // we are float now
      mpf_floor(float_, aX.float_);
      BecomeInt();	// we are integer now
    }
    else
    {	// do not have enough precision, raise error
      RaiseError("BigNumber::Floor: error: not enough precision of argument %e (%ld bits)", aX.Double(), aX.GetPrecision());
      SetTo(aX);
    }
  }
  else if (this != &aX) // no change for integers or for exp floats, or if we don't have enough digits; just assign the value
  {
	SetTo(aX);
  }

  if (Sign()!=0) LISPASSERT(iPrecision >= 0);
}

// round to a given precision (in bits) and set target precision. Does not change the number if the current precision is lower, or if the number is an integer.
void BigNumber::Precision(LispInt aPrecision)
{
  if (!IsInt())
  {	// set precision flags
  // we allow negative precision only on floating zeros
	if (Sign()!=0) LISPASSERT(aPrecision >= 0);
	iPrecision = aPrecision;
	aPrecision = MAX(aPrecision + (LispInt)GUARD_BITS, 1);	// pretend that the requested precision is actually larger
  	mpf_set_prec(float_, aPrecision);
  	long int exp_small = 0, shift_amount = 0;
	// determine the binary exponent
	(void) mpf_get_d_2exp(&exp_small, float_);
	// determine the shift amount
	shift_amount = exp_small - aPrecision;
	// truncate at aPrecision bits
	if (shift_amount >= 0)
	{
		mpf_div_2exp(float_, float_, (unsigned) shift_amount);
		mpf_trunc(float_, float_);
		mpf_mul_2exp(float_, float_, (unsigned) shift_amount);
	}
	else if (shift_amount < 0)
	{
		mpf_mul_2exp(float_, float_, (unsigned) (-shift_amount));
		mpf_trunc(float_, float_);
		mpf_div_2exp(float_, float_, (unsigned) (-shift_amount));
	}
  }	// do nothing if integer
}



/// Bitwise operations, return result in *this.
void BigNumber::ShiftLeft(const BigNumber& aX, LispInt aNrToShift)
{
  if (aNrToShift>=0)
  {
    if (aX.IsInt() && aNrToShift >= 0)
    {
  	if (!IsInt()) turn_int();
	mpz_mul_2exp(int_, aX.int_, aNrToShift);
    }
    else
    {
  	if (IsInt()) turn_float();	// we are not aX
	mpf_mul_2exp(float_, aX.float_, aNrToShift);
    }
  }	// do nothing if the shift amount is negative
}


void BigNumber::ShiftRight(const BigNumber& aX, LispInt aNrToShift)
{
  if (aNrToShift>=0)
  {
    if (aX.IsInt() && aNrToShift >= 0)
    {
  	if (!IsInt()) turn_int();
	mpz_tdiv_q_2exp(int_, aX.int_, aNrToShift);
    }
    else
    {
  	if (IsInt()) turn_float();	// we are not aX
	mpf_div_2exp(float_, aX.float_, aNrToShift);
    }
  }	// do nothing if the shift amount is negative
}


void BigNumber::ShiftLeft(const BigNumber& aX, const BigNumber& aNrToShift)
{
// first, see if we can use short numbers
  if (aNrToShift.IsInt() && aNrToShift.Sign()>=0)
  {
    if (aNrToShift.IsSmall())
    {
  	long shift_amount=(long)aNrToShift.Double();
	ShiftLeft(aX, shift_amount);
    }
    else
    {// only floats can be shifted by a non-small number, so convert to float and use exponent_
    // FIXME: make this work for large shift amounts
    }
  }	// do nothing if shift amount is not integer or negative
}


void BigNumber::ShiftRight(const BigNumber& aX, const BigNumber& aNrToShift)
{
// first, see if we can use short numbers
  if (aNrToShift.IsInt() && aNrToShift.Sign()>=0)
  {
    if (aNrToShift.IsSmall())
    {
  	long shift_amount=(long)aNrToShift.Double();
	ShiftRight(aX, shift_amount);
    }
    else
    {// only floats can be shifted by a non-small number, so convert to float and use exponent_
    // FIXME: make this work for large shift amounts
    }
  }	// do nothing if shift amount is not integer or negative
}


void BigNumber::BitAnd(const BigNumber& aX, const BigNumber& aY)
{
  if (aX.IsInt() && aY.IsInt())
  {
  	if (!IsInt()) turn_int();
	mpz_and(int_, aX.int_, aY.int_);
  }	// do nothing if the arguments are not integer
}


void BigNumber::BitOr(const BigNumber& aX, const BigNumber& aY)
{
  if (aX.IsInt() && aY.IsInt())
  {
  	if (!IsInt()) turn_int();
	mpz_ior(int_, aX.int_, aY.int_);
  }
}


void BigNumber::BitXor(const BigNumber& aX, const BigNumber& aY)
{
  if (aX.IsInt() && aY.IsInt())
  {
  	if (!IsInt()) turn_int();
	mpz_xor(int_, aX.int_, aY.int_);
  }
}


void BigNumber::BitNot(const BigNumber& aX)
{
  if (aX.IsInt())
  {
  	if (!IsInt()) turn_int();
	mpz_com(int_, aX.int_);
  }
}


/// return true if the bit count fits into signed long
LispBoolean BigNumber::BitCountIsSmall() const
{
	return !IsExpFloat();
}

// give BitCount as platform integer; if it is an expfloat, give the bit count of the float part
signed long BigNumber::BitCount() const
{
//  LISPASSERT(0);
  long bit_count;
  if (Sign()==0)
	  return 1;	// BitCount(0)=1
  if (IsInt())
  {
  	(void) mpz_get_d_2exp(&bit_count, int_);	// find the # of digits in base 2
  }
  else
  {
  	(void) mpf_get_d_2exp(&bit_count, float_);
  }
  return bit_count;
}
/// Bit count operation: return the number of significant bits if integer, return the binary exponent if float (shortcut for binary logarithm)
void BigNumber::BitCount(const BigNumber& aX)
{
	SetTo(aX.BitCount());
	if (aX.IsExpFloat()) mpz_add(int_, int_, aX.exponent_);
}



/// Give sign (-1, 0, 1)
LispInt BigNumber::Sign() const
{
  if (IsInt())
  {
  	return mpz_sgn(int_);
  }
  else
  {
  	return mpf_sgn(float_);
  }
}

/// these functions do not change the number but merely prepare its type
void BigNumber::turn_float()
{
//  if (IsInt())
  {
    type_ = KFloat;
  }
}

void BigNumber::turn_int()
{
//  if (!IsInt())
  {
    type_ = KInt;
  }
}

// copy from gmp objects
void BigNumber::import_gmp(mpz_t gmp_int)
{
	turn_int();
	mpz_set(int_, gmp_int);
}
void BigNumber::import_gmp(mpf_t gmp_float)
{
	turn_float();
	mpf_set_prec(float_, iPrecision = mpf_get_prec(gmp_float));
	mpf_set(float_, gmp_float);
}
// copy to gmp objects
void BigNumber::export_gmp(mpz_t gmp_int) const
{
	mpz_set(gmp_int, int_);
}
void BigNumber::export_gmp(mpf_t gmp_float) const
{
	mpf_set_prec(gmp_float, mpf_get_prec(float_));
	mpf_set(gmp_float, float_);
}


//////////////////////////////////////////////////
///// End of BigNumber implementation
//////////////////////////////////////////////////

//////////////////////////////////////////////////
///// Hooks to some GMP functions for efficiency
//////////////////////////////////////////////////

/// These are examples of how we could add more hooks later.
/// Right now the functions export_gmp and import_gmp *copy* everything,
/// but in principle we could avoid this and pass pointers to valid gmp objects.
/// this would avoid copying and initializing/clearing.

/// Square root (return float for an integer result).
void math_sqrt(BigNumber& result, const BigNumber& x)
{
	if(x.IsInt())
	{
		mpz_t temp_int;
		mpz_init(temp_int);
		x.export_gmp(temp_int);
		mpf_t temp_float;
		mpf_init2(temp_float,32);
		mpf_set_z(temp_float, temp_int);
		mpf_sqrt(temp_float, temp_float);
		result.BecomeFloat();
		result.import_gmp(temp_float);
		mpz_clear(temp_int);
		mpf_clear(temp_float);
	}
	else
	{
		mpf_t temp_float;
		mpf_init2(temp_float,32);
		x.export_gmp(temp_float);
		mpf_sqrt(temp_float, temp_float);
		result.BecomeFloat();
		result.import_gmp(temp_float);
		mpf_clear(temp_float);
	}
}

/// Compute the GCD of two integers, result is an integer.
/// If arguments are floats, do nothing (FIXME: should signal error).
void math_gcd(BigNumber& result, const BigNumber& x, const BigNumber& y)
{
	if (x.IsInt() && y.IsInt())
	{
		mpz_t temp_int, temp1, temp2;
		mpz_init(temp_int);
		mpz_init(temp1);
		mpz_init(temp2);
		x.export_gmp(temp1);
		y.export_gmp(temp2);
		mpz_gcd(temp_int, temp1, temp2);
		result.BecomeInt();
		result.import_gmp(temp_int);
		mpz_clear(temp_int);
	}
	else
	{
/// If one of the arguments is a float, do nothing (FIXME: should signal error).
	}
}

/// Compute factorial of a number x, when x is integer and fits into unsigned long.
/// (Otherwise need to signal error.)
void math_factorial(BigNumber& result, const BigNumber& x)
{
	if (x.IsInt() && x.IsSmall())
	{
		mpz_t temp_int;
		mpz_init(temp_int);
		unsigned long arg = (unsigned long) x.Double();
		mpz_fac_ui(temp_int, arg);
		result.BecomeInt();
		result.import_gmp(temp_int);
		mpz_clear(temp_int);
	}
	else
	{
/// FIXME: should signal error
	}
}



#endif // USE_NEW_BIGNUM
