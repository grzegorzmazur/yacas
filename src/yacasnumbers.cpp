

/* Implementation of the number classes (the functionality used
 * by yacas any way
 */

#include "yacasbase.h"
#include "numbers.h"
#include "standard.h"
#include "anumber.h"
#include "platmath.h"
#include "lisperror.h"

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

static LispStringPtr FloatToString(ANumber& aInt, LispHashTable& aHashTable
                                  , LispInt aBase = 10);

LispInt NumericSupportForMantissa()
{
  return LispTrue;
    // TODO make it a mission in life to support mantissa!
//    return LispFalse;
}

const LispCharPtr NumericLibraryName()
{
    return "Internal";
}


/* Converting between internal formats and ascii format.
 * It is best done as little as possible. Usually, during calculations,
 * the ascii version of a number will not be required, so only the
 * internal version needs to be stored.
 */
void* AsciiToNumber(LispCharPtr aString,LispInt aPrecision)
{
    Check(IsNumber(aString,LispTrue),KLispErrInvalidArg);
    return NEW ANumber(aString,aPrecision);
}
LispStringPtr NumberToAscii(void* aNumber,LispHashTable& aHashTable,
                           LispInt aBase)
{
    return FloatToString(*((ANumber*)aNumber),aHashTable,aBase);
}

void* NumberCopy(void* aOriginal)
{
    ANumber* orig = (ANumber*)aOriginal;
    return NEW ANumber(*orig);
}

void NumberDestroy(void* aNumber)
{
    ANumber* orig = (ANumber*)aNumber;
    delete orig;
}


LispStringPtr GcdInteger(LispCharPtr int1, LispCharPtr int2,
                         LispHashTable& aHashTable)
{
    ANumber i1(int1);
    ANumber i2(int2);
    Check(i1.iExp == 0, KLispErrNotInteger);
    Check(i2.iExp == 0, KLispErrNotInteger);
    ANumber res;
    BaseGcd(res,i1,i2);
    LispStringPtr result = FloatToString(res, aHashTable);
    return result;
}

LispStringPtr MultiplyFloat(LispCharPtr int1, LispCharPtr int2,
                            LispHashTable& aHashTable,LispInt aPrecision)
{
    ANumber i1(int1,aPrecision);
    ANumber i2(int2,aPrecision);
    ANumber res(aPrecision);
    Multiply(res,i1,i2);
    LispStringPtr result = FloatToString(res, aHashTable);
    return result;
}

LispStringPtr AddFloat(LispCharPtr int1, LispCharPtr int2,
                       LispHashTable& aHashTable,LispInt aPrecision)
{
    ANumber i1(int1,aPrecision);
    ANumber i2(int2,aPrecision);
    ANumber res(aPrecision);
    Add(res,i1,i2);
    LispStringPtr result = FloatToString(res, aHashTable);
    return result;
}

LispStringPtr PlusFloat(LispCharPtr int1,LispHashTable& aHashTable
                       ,LispInt aPrecision)
{
    ANumber i1(int1,aPrecision);
    LispStringPtr result = FloatToString(i1, aHashTable);
    return result;
}


LispStringPtr SubtractFloat(LispCharPtr int1, LispCharPtr int2,
                            LispHashTable& aHashTable,LispInt aPrecision)
{
    ANumber i1(int1,aPrecision);
    ANumber i2(int2,aPrecision);
    ANumber res(aPrecision);
    Subtract(res,i1,i2);
    LispStringPtr result = FloatToString(res, aHashTable);
    return result;
}

LispStringPtr NegateFloat(LispCharPtr int1, LispHashTable& aHashTable
                          ,LispInt aPrecision)
{
    ANumber i1(int1,aPrecision);
    Negate(i1);
    LispStringPtr result = FloatToString(i1, aHashTable);
    return result;
}

LispStringPtr DivideFloat(LispCharPtr int1, LispCharPtr int2,
                          LispHashTable& aHashTable,LispInt aPrecision)
{
    ANumber i1(int1,aPrecision);
    ANumber i2(int2,aPrecision);

    Check(!IsZero(i2),KLispErrDivideByZero);

    if (IsZero(i1))
    {
        return aHashTable.LookUp("0");
    }
    ANumber res(aPrecision);
    ANumber remainder(aPrecision);
    Divide(res,remainder,i1,i2);
    LispStringPtr result = FloatToString(res, aHashTable);
    return result;
}


static void Trigonometry(ANumber& x,ANumber& i,ANumber& sum,ANumber& term)
{
    ANumber x2(sum.iPrecision);
    Multiply(x2,x,x);
    ANumber one("1",sum.iPrecision);
    ANumber dummy;

    LispInt requiredDigits = WordDigits(sum.iPrecision, 10)+
        x2.NrItems()-x2.iExp+1;
//    printf("WordDigits=%d\n",requiredDigits);
//    printf("[%d,%d]:",x.NrItems()-x.iExp,x.iExp);

    // While (term>epsilon)
    while (Significant(term))      
    {
        ANumber orig(sum.iPrecision);

        //   term <- term*x^2/((i+1)(i+2))
        //   i <= i+2

        // added this: truncate digits to speed up the calculation
        {
            LispInt toDunk = term.iExp - requiredDigits;
            if (toDunk > 0)
            {
                term.Delete(0,toDunk);
                term.iExp = requiredDigits;
            }
        }

        orig.CopyFrom(term);

        Multiply(term,orig,x2);
//
        BaseAdd(i, one, WordBase);
        orig.CopyFrom(term);
        Divide(term, dummy, orig, i);
//
        BaseAdd(i, one, WordBase);
        orig.CopyFrom(term);
        Divide(term, dummy, orig, i);

        //   negate term
        Negate(term);
        //   sum <- sum+term
        orig.CopyFrom(sum);
        Add(sum, orig, term);
    }
//    printf("[%d,%d]:",sum.NrItems()-sum.iExp,sum.iExp);
}

static void SinFloat(ANumber& aResult, ANumber& x)
{
    // i <- 1
    ANumber i("1",aResult.iPrecision);
    // sum <- x
    aResult.CopyFrom(x);
    // term <- x
    ANumber term(aResult.iPrecision);
    term.CopyFrom(x);
    Trigonometry(x,i,aResult,term);
}

static void SinFloat(ANumber& aResult, LispCharPtr int1)
{
    // Sin(x)=Sum(i=0 to Inf) (-1)^i x^(2i+1) /(2i+1)!
    // Which incrementally becomes the algorithm:
    //
    ANumber x(int1,aResult.iPrecision);
    SinFloat(aResult,x);
}

static void CosFloat(ANumber& aResult, ANumber& x)
{
    // i <- 0
    ANumber i("0",aResult.iPrecision);
    // sum <- 1
    aResult.SetTo("1");
    // term <- 1
    ANumber term("1",aResult.iPrecision);
    Trigonometry(x,i,aResult,term);
}

static void CosFloat(ANumber& aResult, LispCharPtr int1)
{
    // Cos(x)=Sum(i=0 to Inf) (-1)^i x^(2i) /(2i)!
    // Which incrementally becomes the algorithm:
    //
    ANumber x(int1,aResult.iPrecision);
    CosFloat(aResult,x);
}

LispStringPtr SinFloat(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision)
{
    ANumber sum(aPrecision);
    SinFloat(sum, int1);
    return FloatToString(sum, aHashTable);
}


LispStringPtr CosFloat(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision)
{
    ANumber sum(aPrecision);
    CosFloat(sum, int1);
    return FloatToString(sum, aHashTable);
}

LispStringPtr TanFloat(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision)
{
    // Tan(x) = Sin(x)/Cos(x)

    ANumber s(aPrecision);
    SinFloat(s, int1);

    ANumber c(aPrecision);
    CosFloat(c, int1);

    ANumber result(aPrecision);
    ANumber dummy(aPrecision);
    Divide(result,dummy,s,c);
    
    return FloatToString(result, aHashTable);

}

LispStringPtr ArcSinFloat(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision)
{
	// Use Newton's method to solve sin(x) = y by iteration:
    // x := x - (Sin(x) - y) / Cos(x)
	// this is similar to PiFloat()
	// we are using PlatArcSin() as the initial guess
	// maybe, for y very close to 1 or to -1 convergence will
	// suffer but seems okay in some tests
    LispStringPtr iResult = PlatArcSin(int1,  aHashTable, 0);
	ANumber result(iResult->String(), aPrecision);	// hack, hack, hack
	// how else do I get an ANumber from the result of PlatArcSin()?
    ANumber x(aPrecision);	// dummy variable
    ANumber q("10", aPrecision);	// initial value must be "significant"
    ANumber s(aPrecision);
    ANumber c(aPrecision);
	
	while (Significant(q))
    {
        x.CopyFrom(result);
        SinFloat(s, x);
		Negate(s);
        x.CopyFrom(s);
		ANumber y(int1, aPrecision);
		Add(s, x, y);
        // now s = y - Sin(x)
		x.CopyFrom(result);
        CosFloat(c, x);
        Divide(q,x,s,c);
		// now q = (y - Sin(x)) / Cos(x)

        // Calculate result:=result+q;
        x.CopyFrom(result);
        Add(result,x,q);
    }
    return FloatToString(result, aHashTable);
}

// ArcCosFloat should be defined in scripts through ArcSinFloat
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

static void ExpFloat(ANumber& aResult, ANumber& x)
{
    // Exp(x)=Sum(i=0 to Inf)  x^(i) /(i)!
    // Which incrementally becomes the algorithm:
    //
    ANumber one("1",aResult.iPrecision);
    // i <- 0
    ANumber i("0",aResult.iPrecision);     
    // sum <- 1
    aResult.SetTo("1");
    // term <- 1
    ANumber term("1",aResult.iPrecision);  
    ANumber dummy;

    LispInt requiredDigits = WordDigits(aResult.iPrecision, 10)+
        x.NrItems()-x.iExp+1;

    // While (term>epsilon)
    while (Significant(term))      
    {
        ANumber orig(aResult.iPrecision);

        {
            LispInt toDunk = term.iExp - requiredDigits;
            if (toDunk > 0)
            {
                term.Delete(0,toDunk);
                term.iExp = requiredDigits;
            }
        }

        
        //   i <- i+1
        BaseAdd(i, one, WordBase);     

        //   term <- term*x/(i)
        orig.CopyFrom(term);
        Multiply(term,orig,x);
        orig.CopyFrom(term);
        Divide(term, dummy, orig, i);

        //   sum <- sum+term
        orig.CopyFrom(aResult);
        Add(aResult, orig, term);
    }
}

LispStringPtr ExpFloat(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision)
{
    ANumber sum(aPrecision);
    ANumber x(int1,aPrecision);
    ExpFloat(sum, x);
    return FloatToString(sum, aHashTable);
}


static void LnFloat(ANumber& aResult, LispCharPtr int1)
{
    // Optimization for convergence: the following taylor
    // series converges faster when x is close to zero.
    // So a trick is to first take the square root a couple
    // of times, until x is sufficiently close to 1.
    

    // Ln(y) = Ln(1+x) = Sum(i=1 to inf) (-1)^(i+1) * x^(i) / i
    // thus y=1+x => x = y-1
    //

    LispInt shifts=0;
    LispBoolean smallenough=LispFalse;
    LispInt precision = 2*aResult.iPrecision;
    ANumber y(int1,precision);
    ANumber minusone("-1",precision);
    ANumber x(precision);

    ANumber delta("0.01",precision);
    while (!smallenough)
    {
        ANumber tosquare(precision);
        tosquare.CopyFrom(y);
        Sqrt(y,tosquare);
        shifts++;
        Add(x,y,minusone);
        if (BaseLessThan(x,delta))
            smallenough=LispTrue;
    }
    // i <- 0
    ANumber i("0",precision);
    // sum <- 1
    aResult.SetTo("0");
    // term <- 1
    ANumber term("-1",precision);
    ANumber dummy;

    ANumber one("1",precision);
    // While (term>epsilon)
    while (Significant(term))      
    {
        //   negate term
        Negate(term);

        ANumber orig(precision);

        orig.CopyFrom(term);
        Multiply(term,orig,x);
        //
        BaseAdd(i, one, WordBase);
        orig.CopyFrom(term);
        ANumber newTerm(precision);
        Divide(newTerm, dummy, orig, i);

        //   sum <- sum+term
        orig.CopyFrom(aResult);
        Add(aResult, orig, newTerm);
    }
    if (shifts)
        BaseShiftLeft(aResult,shifts);
}



LispStringPtr LnFloat(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision)
{
//TODO remove    return PlatLn(int1, aHashTable,aPrecision);
    ANumber sum(aPrecision);
    LnFloat(sum, int1);
    return FloatToString(sum, aHashTable);
}

LispStringPtr PowerFloat(LispCharPtr int1, LispCharPtr int2,
                         LispHashTable& aHashTable,LispInt aPrecision)
{
    // If is integer
    if (IsNumber(int2,LispFalse))
    {
        // Raising to the power of an integer can be done fastest by squaring
        // and bitshifting: x^(a+b) = x^a*x^b . Then, regarding each bit
        // in y (seen as a binary number) as added, the algorithm becomes:
        //
        ANumber x(int1,aPrecision);
        ANumber y(int2,aPrecision);
        LispBoolean neg = y.iNegative;
        y.iNegative=LispFalse;
        
        // result <- 1
        ANumber result("1",aPrecision);
        // base <- x
        ANumber base(aPrecision);
        base.CopyFrom(x);

        ANumber copy(aPrecision);

        // while (y!=0)
        while (!IsZero(y))
        {
            // if (y&1 != 0)
            if ( (y[0] & 1) != 0)
            {
                // result <- result*base
                copy.CopyFrom(result);
                Multiply(result,copy,base);
            }
            // base <- base*base
            copy.CopyFrom(base);
            Multiply(base,copy,copy);
            // y <- y>>1
            BaseShiftRight(y,1);
        }

        if (neg)
        {
            ANumber one("1",aPrecision);
            ANumber dummy;
            copy.CopyFrom(result);
            Divide(result,dummy,one,copy);
        }
        
        // result
        return FloatToString(result, aHashTable);
    }

    ANumber lnn(aPrecision);
    LnFloat(lnn, int1);

    ANumber exn(int2,aPrecision);

    ANumber x(aPrecision);
    Multiply(x,exn,lnn);
    ANumber result(aPrecision);
    ExpFloat(result, x);
    return FloatToString(result, aHashTable);
}



LispStringPtr SqrtFloat(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision)
{
    ANumber i1(int1,aPrecision);
    ANumber res(aPrecision);
    Sqrt(res,i1);
    LispStringPtr result = FloatToString(res, aHashTable);
    return result;
}

LispStringPtr AbsFloat( LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision)
{
    ANumber i1(int1,aPrecision);
    i1.iNegative = LispFalse;
    LispStringPtr result = FloatToString(i1, aHashTable);
    return result;
}



LispBoolean LessThan(LispCharPtr int1, LispCharPtr int2,
                       LispHashTable& aHashTable,LispInt aPrecision)
{
    ANumber i1(int1,aPrecision);
    ANumber i2(int2,aPrecision);
    LispBoolean result = LessThan(i1,i2);
    return result;
}

LispBoolean GreaterThan(LispCharPtr int1, LispCharPtr int2,
                       LispHashTable& aHashTable,LispInt aPrecision)
{
    ANumber i1(int1,aPrecision);
    ANumber i2(int2,aPrecision);
    LispBoolean result = GreaterThan(i1,i2);
    return result;
}



LispStringPtr ShiftLeft( LispCharPtr int1, LispCharPtr int2, LispHashTable& aHashTable,LispInt aPrecision)
{
    ANumber i1(int1,aPrecision);
    LISPASSERT(i1.iExp == 0);

    LispInt bits = InternalAsciiToInt(int2);
    BaseShiftLeft(i1,bits);
    LispStringPtr result = FloatToString(i1, aHashTable);
    return result;
}


LispStringPtr ShiftRight( LispCharPtr int1, LispCharPtr int2, LispHashTable& aHashTable,LispInt aPrecision)
{
    ANumber i1(int1,aPrecision);
    LISPASSERT(i1.iExp == 0);

    LispInt bits = InternalAsciiToInt(int2);
    BaseShiftRight(i1,bits);
    LispStringPtr result = FloatToString(i1, aHashTable);
    return result;
}


LispStringPtr FromBase( LispCharPtr int1, LispCharPtr int2, LispHashTable& aHashTable,
                        LispInt aPrecision)
{
    LispInt base = InternalAsciiToInt(int1);
    ANumber i2(int2,aPrecision,base);
    LispStringPtr result = FloatToString(i2, aHashTable,10);
    return result;
}


LispStringPtr ToBase( LispCharPtr int1, LispCharPtr int2, LispHashTable& aHashTable,
                    LispInt aPrecision)
{
    LispInt base = InternalAsciiToInt(int1);
    ANumber i2(int2,aPrecision,10);
    LispStringPtr result = FloatToString(i2, aHashTable,base);
    return result;
}

LispStringPtr FloorFloat( LispCharPtr int1, LispHashTable& aHashTable,
                        LispInt aPrecision)
{
    ANumber i1(int1,aPrecision);
    LispInt i=0;
    LispInt fraciszero=LispTrue;
    while (i<i1.iExp && fraciszero)
    {
        if (i1[i] != 0)
            fraciszero=LispFalse;
        i++;
    }
    i1.Delete(0,i1.iExp);
    i1.iExp=0;
    if (i1.iNegative && !fraciszero)
    {
        ANumber orig(aPrecision);
        orig.CopyFrom(i1);
        ANumber minone("-1");
        Add(i1,orig,minone);
    }
    return FloatToString(i1, aHashTable,10);
}

LispStringPtr CeilFloat( LispCharPtr int1, LispHashTable& aHashTable,
                         LispInt aPrecision)
{
    ANumber i1(int1,aPrecision);
    LispInt i=0;
    LispInt fraciszero=LispTrue;
    while (i<i1.iExp && fraciszero)
    {
        if (i1[i] != 0)
            fraciszero=LispFalse;
        i++;
    }
    i1.Delete(0,i1.iExp);
    i1.iExp=0;
    if (!i1.iNegative && !fraciszero)
    {
        ANumber orig(aPrecision);
        orig.CopyFrom(i1);
        ANumber one("1");
        Add(i1,orig,one);
    }
    return FloatToString(i1, aHashTable,10);
}

static void DivideInteger( ANumber& aQuotient, ANumber& aRemainder,
                        LispCharPtr int1, LispCharPtr int2, 
                        LispInt aPrecision)
{
    ANumber a1(int1,aPrecision);
    ANumber a2(int2,aPrecision);
    
    Check(a1.iExp == 0, KLispErrNotInteger);
    Check(a2.iExp == 0, KLispErrNotInteger);
    Check(!IsZero(a2),KLispErrInvalidArg);
    IntegerDivide(aQuotient, aRemainder, a1, a2);
}

LispStringPtr ModFloat( LispCharPtr int1, LispCharPtr int2, LispHashTable& aHashTable,
                        LispInt aPrecision)
{
    ANumber quotient(static_cast<LispInt>(0));
    ANumber remainder(static_cast<LispInt>(0));
    DivideInteger( quotient, remainder, int1, int2, aPrecision);
    return FloatToString(remainder, aHashTable,10);

}

LispStringPtr DivFloat( LispCharPtr int1, LispCharPtr int2, LispHashTable& aHashTable,
                        LispInt aPrecision)
{
    ANumber quotient(static_cast<LispInt>(0));
    ANumber remainder(static_cast<LispInt>(0));
    DivideInteger( quotient, remainder, int1, int2, aPrecision);
    return FloatToString(quotient, aHashTable,10);
}

LispStringPtr PiFloat( LispHashTable& aHashTable,
                        LispInt aPrecision)
{
    // Newton's method for finding pi:
    // x[0] := 3.1415926
    // x[n+1] := x[n] + Sin(x[n])

	LispInt initial_prec = aPrecision;	// target precision of first iteration, will be computed below
    LispInt cur_prec = 40;  // precision of the initial guess
    ANumber result("3.141592653589793238462643383279502884197169399",cur_prec + 3);    // initial guess is stored with 3 guard digits
    ANumber x(cur_prec);
	ANumber s(cur_prec);

	// optimize precision sequence
	while (initial_prec > cur_prec*3)
		initial_prec = int((initial_prec+2)/3);
	cur_prec = initial_prec;
    while (cur_prec <= aPrecision)
    {
 		// start of iteration code
		result.ChangePrecision(cur_prec);	// result has precision cur_prec now
        // Get Sin(result)
        x.CopyFrom(result);
		s.ChangePrecision(cur_prec);
        SinFloat(s, x);
        // Calculate new result: result := result + Sin(result);
        x.CopyFrom(result);	// precision cur_prec
        Add(result,x,s);
		// end of iteration code
		// decide whether we are at end of loop now
		if (cur_prec == aPrecision)	// if we are exactly at full precision, it's the last iteration
			cur_prec = aPrecision+1;	// terminate loop
		else {
			cur_prec *= 3;	// precision triples at each iteration
			// need to guard against overshooting precision
 			if (cur_prec > aPrecision)
				cur_prec = aPrecision;	// next will be the last iteration
		}
    }
	
//    return aHashTable.LookUp("3.14"); // Just kidding, Serge ;-)
    return FloatToString(result, aHashTable);
}



static LispStringPtr FloatToString(ANumber& aInt,
                            LispHashTable& aHashTable, LispInt aBase)
{
    LispString result;
    ANumberToString(result, aInt, aBase);
    return aHashTable.LookUp(result.String());
}



LispStringPtr BitAnd(LispCharPtr int1, LispCharPtr int2,
                     LispHashTable& aHashTable,LispInt aPrecision)
{
    ANumber i1(int1,aPrecision);
    ANumber i2(int2,aPrecision);
    Check(i1.iExp == 0, KLispErrNotInteger);
    Check(i2.iExp == 0, KLispErrNotInteger);

    ANumber res(aPrecision);
    LispInt len1=i1.NrItems(), len2=i2.NrItems();
    LispInt min=len1,max=len2;
    if (min>max)
    {
        LispInt swap=min;
        min=max;
        max=swap;
    }
    res.GrowTo(min);
    LispInt i;
    for (i=0;i<len1 && i<len2;i++)
    {
        res[i] = i1[i] & i2[i];
    }

    LispStringPtr result = FloatToString(res, aHashTable);
    return result;
}

LispStringPtr BitOr(LispCharPtr int1, LispCharPtr int2,
                     LispHashTable& aHashTable,LispInt aPrecision)
{
    ANumber i1(int1,aPrecision);
    ANumber i2(int2,aPrecision);
    Check(i1.iExp == 0, KLispErrNotInteger);
    Check(i2.iExp == 0, KLispErrNotInteger);
    ANumber res(aPrecision);
    LispInt len1=i1.NrItems(), len2=i2.NrItems();
    LispInt min=len1,max=len2;
    if (min>max)
    {
        LispInt swap=min;
        min=max;
        max=swap;
    }
    
    res.GrowTo(max);

    LispInt i;
    for (i=0;i<len1 && i<len2;i++)
    {
        res[i] = i1[i] | i2[i];
    }
    for (i=len1;i<len2;i++)
    {
        res[i] = i2[i];
    }
    for (i=len2;i<len1;i++)
    {
        res[i] = i1[i];
    }
    
    LispStringPtr result = FloatToString(res, aHashTable);
    return result;
}

LispStringPtr BitXor(LispCharPtr int1, LispCharPtr int2,
                     LispHashTable& aHashTable,LispInt aPrecision)
{
    ANumber i1(int1,aPrecision);
    ANumber i2(int2,aPrecision);
    Check(i1.iExp == 0, KLispErrNotInteger);
    Check(i2.iExp == 0, KLispErrNotInteger);
    ANumber res(aPrecision);
    LispInt len1=i1.NrItems(), len2=i2.NrItems();
    LispInt min=len1,max=len2;
    if (min>max)
    {
        LispInt swap=min;
        min=max;
        max=swap;
    }
    
    res.GrowTo(max);

    LispInt i;
    for (i=0;i<len1 && i<len2;i++)
    {
        res[i] = i1[i] ^ i2[i];
    }
    for (i=len1;i<len2;i++)
    {
        res[i] = i2[i];
    }
    for (i=len2;i<len1;i++)
    {
        res[i] = i1[i];
    }
    
    LispStringPtr result = FloatToString(res, aHashTable);
    return result;
}

LispStringPtr LispFactorial(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision)
{
    LispInt nr = InternalAsciiToInt(int1);
    Check(nr>=0,KLispErrInvalidArg);
    ANumber fac("1",aPrecision);
    LispInt i;
    for (i=2;i<=nr;i++)
    {
        BaseTimesInt(fac,i, WordBase);
    }
/*
    for (i=2;i<nr;i+=2)
    {
        BaseTimesInt(fac,i*(i+1), WordBase);
    }
    if (i==nr) BaseTimesInt(fac, i, WordBase);
*/
    return FloatToString(fac, aHashTable);
}

/* This code will compute factorials faster when multiplication becomes better than quadratic time

// return old result*product of all integers from iLeft to iRight
void tree_factorial(ANumber& result, LispInt iLeft, LispInt iRight, LispInt aPrecision)
{
	if (iRight == iLeft) BaseTimesInt(result, iLeft, WordBase);
	else if (iRight == iLeft + 1) BaseTimesInt(result, iLeft*iRight, WordBase);
	else if (iRight == iLeft + 2) BaseTimesInt(result, iLeft*iRight*(iLeft+1), WordBase);
	else
	{
	    ANumber fac1("1", aPrecision), fac2("1", aPrecision);
	    LispInt i = (iLeft+iRight)>>1;
		tree_factorial(fac1, iLeft, i, aPrecision);
		tree_factorial(fac2, i+1, iRight, aPrecision);
		Multiply(result, fac1, fac2);
	}
}

LispStringPtr LispFactorial(LispCharPtr int1, LispHashTable& aHashTable,LispInt aPrecision)
{
    LispInt nr = InternalAsciiToInt(int1);
    Check(nr>=0,KLispErrInvalidArg);
	ANumber fac("1",aPrecision);
	tree_factorial(fac, 1, nr, aPrecision);
    return FloatToString(fac, aHashTable);
}

*/






#ifndef USE_NATIVE


BigNumber::BigNumber(const LispCharPtr aString,LispInt aPrecision,LispInt aBase)
{
  iNumber = NEW ANumber(aString,aPrecision,aBase);
}
BigNumber::BigNumber(const BigNumber& aOther)
{
  iNumber = NEW ANumber(*aOther.iNumber);
}
BigNumber::BigNumber()
{
  iNumber = NEW ANumber();
}

BigNumber::~BigNumber()
{
  delete iNumber;
}

void BigNumber::SetTo(const BigNumber& aOther)
{
  iNumber->CopyFrom(*aOther.iNumber);
}
void BigNumber::ToString(LispString& aResult, LispInt aPrecision, LispInt aBase) const
{
  ANumberToString(aResult, *iNumber, aBase);
}
double BigNumber::Double() const
{
// There are platforms that don't have strtod
#ifdef HAVE_STRTOD
  LispString str;
  ANumberToString(str, *iNumber, 10);
  char* endptr;
  return strtod(str.String(),&endptr);
#else
  LISPASSERT(0);
  return 0.0;
#endif
}

const LispCharPtr BigNumber::NumericLibraryName()
{
  return "Internal Yacas numbers";
}

void BigNumber::Multiply(const BigNumber& aX, const BigNumber& aY, LispInt aPrecision)
{
  :: Multiply(*iNumber,*aX.iNumber,*aY.iNumber);
}
void BigNumber::MultiplyAdd(const BigNumber& aX, const BigNumber& aY, LispInt aPrecision)
{//FIXME
  LISPASSERT(0);
}
void BigNumber::Add(const BigNumber& aX, const BigNumber& aY, LispInt aPrecision)
{
	::Add(*iNumber, *aX.iNumber, *aY.iNumber);
}
void BigNumber::Negate(const BigNumber& aX)
{
  if (aX.iNumber != iNumber)
  {
    iNumber->CopyFrom(*aX.iNumber);
  }
  ::Negate(*iNumber);

}
void BigNumber::Divide(const BigNumber& aX, const BigNumber& aY, LispInt aPrecision)
{
    ANumber remainder(aPrecision);
    ::Divide(*iNumber,remainder,*aX.iNumber,*aY.iNumber);
}
void BigNumber::ShiftLeft(const BigNumber& aX, LispInt aNrToShift)
{
  if (aX.iNumber != iNumber)
  {
    iNumber->CopyFrom(*aX.iNumber);
  }
  ::BaseShiftLeft(*iNumber,aNrToShift);
}
void BigNumber::ShiftRight(const BigNumber& aX, LispInt aNrToShift)
{
  if (aX.iNumber != iNumber)
  {
    iNumber->CopyFrom(*aX.iNumber);
  }
  ::BaseShiftRight(*iNumber,aNrToShift);
}
void BigNumber::BitAnd(const BigNumber& aX, const BigNumber& aY)
{
  LispInt len1=aX.iNumber->NrItems(), len2=aY.iNumber->NrItems();
  LispInt min=len1,max=len2;
  if (min>max)
  {
    LispInt swap=min;
    min=max;
    max=swap;
  }
  iNumber->GrowTo(min);
  LispInt i;
  for (i=0;i<len1 && i<len2;i++)
  {
    (*iNumber)[i] = (*aX.iNumber)[i] & (*aY.iNumber)[i];
  }
}
void BigNumber::BitOr(const BigNumber& aX, const BigNumber& aY)
{
  LispInt len1=(*aX.iNumber).NrItems(), len2=(*aY.iNumber).NrItems();
  LispInt min=len1,max=len2;
  if (min>max)
  {
    LispInt swap=min;
    min=max;
    max=swap;
  }
  
  iNumber->GrowTo(max);

  LispInt i;
  for (i=0;i<len1 && i<len2;i++)
  {
    (*iNumber)[i] = (*aX.iNumber)[i] | (*aY.iNumber)[i];
  }
  for (i=len1;i<len2;i++)
  {
    (*iNumber)[i] = (*aY.iNumber)[i];
  }
  for (i=len2;i<len1;i++)
  {
    (*iNumber)[i] = (*aX.iNumber)[i];
  }
}
void BigNumber::BitXor(const BigNumber& aX, const BigNumber& aY)
{
  LispInt len1=(*aX.iNumber).NrItems(), len2=(*aY.iNumber).NrItems();
  LispInt min=len1,max=len2;
  if (min>max)
  {
    LispInt swap=min;
    min=max;
    max=swap;
  }
  
  iNumber->GrowTo(max);

  LispInt i;
  for (i=0;i<len1 && i<len2;i++)
  {
    (*iNumber)[i] = (*aX.iNumber)[i] ^ (*aY.iNumber)[i];
  }
  for (i=len1;i<len2;i++)
  {
    (*iNumber)[i] = (*aY.iNumber)[i];
  }
  for (i=len2;i<len1;i++)
  {
    (*iNumber)[i] = (*aX.iNumber)[i];
  }
}
void BigNumber::BitCount(const BigNumber& aX)
{//FIXME
  LISPASSERT(0);
}
LispInt BigNumber::Sign() const
{
  if (iNumber->iNegative) return -1;
  if (IsZero(*iNumber)) return 0;
  return 1;
}




/// integer operation: *this = y mod z
void BigNumber::Mod(const BigNumber& aY, const BigNumber& aZ)
{//FIXME
}

void BigNumber::Floor(const BigNumber& aX)
{//FIXME
}


void BigNumber::Precision(LispInt aPrecision)
{//FIXME
}


//basic object manipulation
bool BigNumber::Equals(const BigNumber& aOther) const
{//FIXME
  return false; // function has to return *some* result
}


bool BigNumber::IsInt() const
{//FIXME
  return false; // function has to return *some* result
}


bool BigNumber::IsIntValue() const
{//FIXME
  return false; // function has to return *some* result
}


bool BigNumber::IsSmall() const
{//FIXME
  return false; // function has to return *some* result
}


void BigNumber::BecomeInt()
{//FIXME
}


void BigNumber::BecomeFloat()
{//FIXME
}


bool BigNumber::LessThan(const BigNumber& aOther) const
{
	return ::LessThan(*(this->iNumber), *(aOther.iNumber));
}

// assign from a platform type
void BigNumber::SetTo(LispInt value)
{//FIXME
}


void BigNumber::SetTo(double value)
{//FIXME
}


// assign from string
void BigNumber::SetTo(const LispCharPtr aString,LispInt aPrecision,LispInt aBase)
{//FIXME
}


void BigNumber::ShiftLeft(const BigNumber& aX, const BigNumber& aNrToShift)
{//FIXME
}


void BigNumber::ShiftRight(const BigNumber& aX, const BigNumber& aNrToShift)
{//FIXME
}

#endif
