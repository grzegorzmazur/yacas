
#include "yacasbase.h"
#include "lispenvironment.h"
#include "standard.h"
#include "lispeval.h"
#include "lispatom.h"
#include "lispparser.h"
#include "stdfileio.h"
#include "stringio.h"
#include "lisperror.h"
#include "infixparser.h"
#include "lispuserfunc.h"
#include "mathuserfunc.h"
#include "platmath.h"
#include "numbers.h"
#include "anumber.h"
#include "arrayclass.h"
#include "patternclass.h"
#include "substitute.h"
#include "errors.h"
#include "patcher.h"
#include "platdll.h"
#include "unipoly.h"


/*TODO

 - Make a separate Number object, for fast calculations.

 - as an alternative, I *could* implement MathFac ;-)

 - Add Number and CopiedNumber methods to LispObject
 - implement Number and CopiedNumber in LispAtom
 - define a LispNumber that can cache a number. This would already
   speed up twofold.
 - implement at least addition and multiplication so they
   can use these.
 - IsNumber and IsInteger should Check ->Number() before doing ->String()

 

 The trick will obviously be to make sure no conversions are made to
 ascii until needed.

 
 */







#define InternalEval aEnvironment.iEvaluator->Eval

void LispArithmetic2(LispEnvironment& aEnvironment, LispPtr& aResult,
                     LispPtr& aArguments,
                     LispStringPtr (*func)(LispCharPtr f1, LispCharPtr f2,LispHashTable& aHashTable,LispInt aPrecision),
                    LispBoolean arbbase=LispFalse);

void LispArithmetic1(LispEnvironment& aEnvironment, LispPtr& aResult,
                     LispPtr& aArguments,
                     LispStringPtr (*func)(LispCharPtr f1, LispHashTable& aHashTable,LispInt aPrecision));






void LispArithmetic1(LispEnvironment& aEnvironment, LispPtr& aResult,
                     LispPtr& aArguments,
                     LispStringPtr (*func)(LispCharPtr f1, LispHashTable& aHashTable,LispInt aPrecision))
{
    TESTARGS(2);

    LispStringPtr str1;

    LispPtr result1;
    InternalEval(aEnvironment, result1, Argument(aArguments,1));

    str1 = result1.Get()->String();
    CHK_ARG(str1 != NULL,1);
    CHK_ARG(IsNumber(str1->String(),LispTrue),1);
    aResult.Set(LispAtom::New(func(str1->String(),
                                   aEnvironment.HashTable(),
                                   aEnvironment.Precision())));
}

void LispArithmetic2(LispEnvironment& aEnvironment, LispPtr& aResult,
                     LispPtr& aArguments,
                     LispStringPtr (*func)(LispCharPtr f1, LispCharPtr f2,LispHashTable& aHashTable,LispInt aPrecision),
                    LispBoolean arbbase)
{
    TESTARGS(3);

    LispStringPtr str1;
    LispStringPtr str2;

    LispPtr result1;
    LispPtr result2;
    InternalEval(aEnvironment, result1, Argument(aArguments,1));
    InternalEval(aEnvironment, result2, Argument(aArguments,2));

    str1 = result1.Get()->String();
    str2 = result2.Get()->String();
    CHK_ARG(str1 != NULL,1);
    CHK_ARG(str2 != NULL,2);
    if (!arbbase)
    {
        CHK_ARG(IsNumber(str1->String(),LispTrue) ,1);
        CHK_ARG(IsNumber(str2->String(),LispTrue) ,2);
    }

    aResult.Set(LispAtom::New(func(str1->String(),str2->String(),
                                   aEnvironment.HashTable(),
                                   aEnvironment.Precision())));
}



void LispMultiply(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispArithmetic2(aEnvironment, aResult, aArguments, MultiplyFloat);
}

void LispAdd(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispInt length = InternalListLength(aArguments);
    if (length == 2)
    {
        LispArithmetic1(aEnvironment, aResult, aArguments, PlusFloat);
    }
    else
    {
        LispArithmetic2(aEnvironment, aResult, aArguments, AddFloat);
    }
}

void LispSubtract(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispInt length = InternalListLength(aArguments);
    if (length == 2)
    {
        LispArithmetic1(aEnvironment, aResult, aArguments, NegateFloat);
    }
    else
    {
        LispArithmetic2(aEnvironment, aResult, aArguments, SubtractFloat);
    }
}


void LispDivide(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispArithmetic2(aEnvironment, aResult, aArguments, DivideFloat);
}


void LispSin(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispArithmetic1(aEnvironment, aResult, aArguments, SinFloat);
}

void LispCos(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispArithmetic1(aEnvironment, aResult, aArguments, CosFloat);
}

void LispTan(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispArithmetic1(aEnvironment, aResult, aArguments, TanFloat);
}

void LispArcSin(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispArithmetic1(aEnvironment, aResult, aArguments, ArcSinFloat);
}

void LispArcCos(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispArithmetic1(aEnvironment, aResult, aArguments, ArcCosFloat);
}

void LispArcTan(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispArithmetic1(aEnvironment, aResult, aArguments, ArcTanFloat);
}

void LispSqrt(LispEnvironment& aEnvironment, LispPtr& aResult,
              LispPtr& aArguments)
{
    LispArithmetic1(aEnvironment, aResult, aArguments, SqrtFloat);
}

void LispFloor(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispArithmetic1(aEnvironment, aResult, aArguments, FloorFloat);
}

void LispCeil(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispArithmetic1(aEnvironment, aResult, aArguments, CeilFloat);
}

void LispAbs(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispArithmetic1(aEnvironment, aResult, aArguments, AbsFloat);
}

void LispMod(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispArithmetic2(aEnvironment, aResult, aArguments, ModFloat);
}

void LispDiv(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispArithmetic2(aEnvironment, aResult, aArguments, DivFloat);
}

void LispLog(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispArithmetic1(aEnvironment, aResult, aArguments, LnFloat);
}

void LispExp(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispArithmetic1(aEnvironment, aResult, aArguments, ExpFloat);
}

void LispPower(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispArithmetic2(aEnvironment, aResult, aArguments, PowerFloat);
}



void LispFac(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispArithmetic1(aEnvironment, aResult, aArguments, LispFactorial);
}




void LispFastSin(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispArithmetic1(aEnvironment, aResult, aArguments, PlatSin);
}

void LispFastCos(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispArithmetic1(aEnvironment, aResult, aArguments, PlatCos);
}
void LispFastTan(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispArithmetic1(aEnvironment, aResult, aArguments, PlatTan);
}

void LispFastArcSin(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispArithmetic1(aEnvironment, aResult, aArguments, PlatArcSin);
}
void LispFastArcCos(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispArithmetic1(aEnvironment, aResult, aArguments, PlatArcCos);
}
void LispFastArcTan(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispArithmetic1(aEnvironment, aResult, aArguments, PlatArcTan);
}
void LispFastExp(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispArithmetic1(aEnvironment, aResult, aArguments, PlatExp);
}
void LispFastLog(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispArithmetic1(aEnvironment, aResult, aArguments, PlatLn);
}

void LispFastPower(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispArithmetic2(aEnvironment, aResult, aArguments, PlatPower);
}

void LispFastSqrt(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispArithmetic1(aEnvironment, aResult, aArguments, PlatSqrt);
}

void LispFastFloor(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispArithmetic1(aEnvironment, aResult, aArguments, PlatFloor);
}

void LispFastCeil(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispArithmetic1(aEnvironment, aResult, aArguments, PlatCeil);
}
void LispFastMod(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispArithmetic2(aEnvironment, aResult, aArguments, PlatMod);
}

void LispFastAbs(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispArithmetic1(aEnvironment, aResult, aArguments, PlatAbs);
}



void LispShiftLeft(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispArithmetic2(aEnvironment, aResult, aArguments, ShiftLeft);
}
void LispShiftRight(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispArithmetic2(aEnvironment, aResult, aArguments, ShiftRight);
}

void LispBitAnd(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispArithmetic2(aEnvironment, aResult, aArguments, BitAnd);
}
void LispBitOr(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispArithmetic2(aEnvironment, aResult, aArguments, BitOr);
}
void LispBitXor(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispArithmetic2(aEnvironment, aResult, aArguments, BitXor);
}



void LispFromBase(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispArithmetic2(aEnvironment, aResult, aArguments, FromBase,LispTrue);
}
void LispToBase(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispArithmetic2(aEnvironment, aResult, aArguments, ToBase);
}



void LispApplyPure(LispEnvironment& aEnvironment, LispPtr& aResult,
                   LispPtr& aArguments)
{
    TESTARGS(3);

    LispPtr oper;
    InternalEval(aEnvironment, oper, Argument(aArguments,1));
    LispPtr args;
    InternalEval(aEnvironment, args, Argument(aArguments,2));

    CHK_ARG(args.Get()->SubList() != NULL,2);
    CHK(args.Get()->SubList()->Get() != NULL,2);

    

    // Apply a pure string
    if (oper.Get()->String() != NULL)
    {
        InternalApplyString(aEnvironment, aResult,
                    oper.Get()->String(),
                    args.Get()->SubList()->Get()->Next());
    }
    else
    {   // Apply a pure function {args,body}.
        CHK_ARG(oper.Get()->SubList() != NULL,1);
        CHK_ARG(oper.Get()->SubList()->Get() != NULL,1);
        LispPtr oper2;
        oper2.Set(oper.Get()->SubList()->Get()->Next().Get());
        CHK_ARG(oper2.Get() != NULL,1);

        LispPtr body;
        body.Set(oper2.Get()->Next().Get());
        CHK_ARG(body.Get() != NULL,1);

        CHK_ARG(oper2.Get()->SubList() != NULL,1);
        CHK_ARG(oper2.Get()->SubList()->Get() != NULL,1);
        oper2.Set(oper2.Get()->SubList()->Get()->Next().Get());

        LispPtr args2;
        args2.Set(args.Get()->SubList()->Get()->Next().Get());

        LispLocalFrame frame(aEnvironment,LispFalse);

        while (oper2.Get() != NULL)
        {
            CHK_ARG(args2.Get() != NULL,2);

            LispStringPtr var = oper2.Get()->String();
            CHK_ARG(var != NULL,1);

            LispPtr newly;
            newly.Set(args2.Get()->Copy(LispFalse));
            aEnvironment.NewLocal(var,newly.Get());

            oper2.Set(oper2.Get()->Next().Get());
            args2.Set(args2.Get()->Next().Get());
        }
        CHK_ARG(args2.Get() == NULL,2);
        InternalEval(aEnvironment, aResult, body);
    }
}


void LispPrettyPrinter(LispEnvironment& aEnvironment, LispPtr& aResult,
                       LispPtr& aArguments)
{
    LispInt nrArguments = InternalListLength(aArguments);

    if (nrArguments == 1)
    {
        aEnvironment.SetPrettyPrinter(NULL);
    }
    else
    {
        CHK(nrArguments == 2,KLispErrWrongNumberOfArgs);
        LispPtr oper;
        InternalEval(aEnvironment, oper, Argument(aArguments,1));
        CHK_ISSTRING(oper,1);
        aEnvironment.SetPrettyPrinter(oper.Get()->String());
    }
    InternalTrue(aEnvironment,aResult);
}


void LispGarbageCollect(LispEnvironment& aEnvironment, LispPtr& aResult,
                        LispPtr& aArguments)
{
    TESTARGS(1);
    aEnvironment.HashTable().GarbageCollect();
    
    InternalTrue(aEnvironment,aResult);
}

void LispLazyGlobal(LispEnvironment& aEnvironment, LispPtr& aResult,
                    LispPtr& aArguments)
{
    TESTARGS(2);
    LispStringPtr string = Argument(aArguments,1).Get()->String();
    CHK_ARG(string != NULL, 1);
    aEnvironment.SetGlobalEvaluates(string);
    InternalTrue(aEnvironment,aResult);

}

void LispPatchLoad(LispEnvironment& aEnvironment, LispPtr& aResult,
                   LispPtr& aArguments)
{
    TESTARGS(2);

    LispPtr evaluated;
    InternalEval(aEnvironment, evaluated, Argument(aArguments,1));

    LispStringPtr string = evaluated.Get()->String();
    CHK_ARG(string != NULL, 1);

    LispString oper;
    InternalUnstringify(oper, string);
    LispStringPtr hashedname = aEnvironment.HashTable().LookUp(oper.String());

    InputStatus oldstatus = aEnvironment.iInputStatus;
    aEnvironment.iInputStatus.SetTo(hashedname->String());

    
    LispLocalFile localFP(aEnvironment, oper.String(),LispTrue,
                          aEnvironment.iInputDirectories);
    Check(localFP.iOpened != 0, KLispErrFileNotFound);
    FILEINPUT newInput(localFP,aEnvironment.iInputStatus);

    PatchLoad(newInput.StartPtr(),
              *aEnvironment.CurrentOutput(),
              aEnvironment);
    aEnvironment.iInputStatus.RestoreFrom(oldstatus);
    InternalTrue(aEnvironment,aResult);
}

void LispPatchString(LispEnvironment& aEnvironment, LispPtr& aResult,
                     LispPtr& aArguments)
{
    TESTARGS(2);

    LispPtr evaluated;
    InternalEval(aEnvironment, evaluated, Argument(aArguments,1));
    
    LispStringPtr string = evaluated.Get()->String();
    CHK_ARG(string != NULL, 1);
    LispString oper;
    InternalUnstringify(oper, string);

    LispString str;
    StringOutput newOutput(str);

    LispLocalOutput localOutput(aEnvironment, &newOutput);

    PatchLoad(&oper[0], newOutput, aEnvironment);

    aResult.Set(LispAtom::New(aEnvironment.HashTable().LookUpStringify(str.String())));
}

void LispDllLoad(LispEnvironment& aEnvironment, LispPtr& aResult,
                   LispPtr& aArguments)
{
    TESTARGS(2);

    LispPtr evaluated;
    InternalEval(aEnvironment, evaluated, Argument(aArguments,1));

    LispStringPtr string = evaluated.Get()->String();
    CHK_ARG(string != NULL, 1);

    LispString oper;
    InternalUnstringify(oper, string);
    LispDllBase *dll = new DLLCLASS;
    Check(dll != NULL,KLispErrNotEnoughMemory);
    LispInt opened;

//    printf("file is [%s]\n"&(oper[0]));
    
    opened = dll->Open(&oper[0]);
    if (!opened) delete dll;
    Check(opened,KLispErrLibraryNotFound);
    LispPluginBase* plugin = dll->GetPlugin();
    if (plugin == NULL)
    {
        delete dll;
        Check(plugin != NULL,KLispErrLibraryNotFound);
    }
    aEnvironment.iDlls.Append(dll);
    plugin->Add(aEnvironment);

    InternalTrue(aEnvironment,aResult);
}

void LispSetExtraInfo(LispEnvironment& aEnvironment, LispPtr& aResult,
                   LispPtr& aArguments)
{
    TESTARGS(3);

    LispPtr object;
    InternalEval(aEnvironment, object, Argument(aArguments,1));

    LispPtr info;
    InternalEval(aEnvironment, info, Argument(aArguments,2));

    aResult.Set( object.Get()->SetExtraInfo(info) );
}


void LispGetExtraInfo(LispEnvironment& aEnvironment, LispPtr& aResult,
                   LispPtr& aArguments)
{
    TESTARGS(2);

    LispPtr object;
    InternalEval(aEnvironment, object, Argument(aArguments,1));

    LispPtr* result = object.Get()->ExtraInfo();
    if (result == NULL)
    {
        InternalFalse(aEnvironment,aResult);
    }
    else if (result->Get() == NULL)
    {
        InternalFalse(aEnvironment,aResult);
    }
    else
    {
        aResult.Set(result->Get());
    }
}

void LispBerlekamp(LispEnvironment& aEnvironment, LispPtr& aResult,
                   LispPtr& aArguments)
{
    TESTARGS(3);
    LispPtr polyobj;
    InternalEval(aEnvironment, polyobj , Argument(aArguments,1));
    LispPtr primeobj;
    InternalEval(aEnvironment, primeobj, Argument(aArguments,2));

    LispInt prime = InternalAsciiToInt(primeobj.Get()->String()->String());

    ZZPoly poly;
    CHK_ARG(polyobj.Get()->SubList() != NULL, 1);
    LispPtr hold;
    hold.Set(polyobj.Get());
    polyobj.Set(polyobj.Get()->SubList()->Get());

    CHK_ARG(polyobj.Get() != NULL, 1);
    polyobj.Set(polyobj.Get()->Next().Get());

    while (polyobj.Get() != NULL)
    {
        ZZ fact;
        CHK_ARG(polyobj.Get()->String() != NULL, 1);
        fact = InternalAsciiToInt(polyobj.Get()->String()->String());
        poly.Append(fact);
        polyobj.Set(polyobj.Get()->Next().Get());
    }
    ZZPolyList result;
    Berlekamp(result, poly, prime);

    LispInt i;
    LispObject *res = NULL;
    for (i=result.NrItems()-1;i>=0;i--)
    {
        LispInt j;
        LispObject *sub = NULL;
        for (j=result[i]->Degree();j>=0;j--)
        {
            LispChar s[20];
            InternalIntToAscii(s,(*result[i])[j]);
            sub = LA(ATOML(s)) + LA(sub);
        }
        res = LIST(LA(ATOML("List")) + LA(sub)) + LA(res);
    }
    aResult.Set(LIST(LA(ATOML("List")) + LA(res)));
}

void LispDefaultTokenizer(LispEnvironment& aEnvironment, LispPtr& aResult,
                   LispPtr& aArguments)
{
    TESTARGS(1);
    aEnvironment.iCurrentTokenizer = &aEnvironment.iDefaultTokenizer;
    InternalTrue(aEnvironment,aResult);
}
void LispCTokenizer(LispEnvironment& aEnvironment, LispPtr& aResult,
                   LispPtr& aArguments)
{
    TESTARGS(1);
    aEnvironment.iCurrentTokenizer = &aEnvironment.iCTokenizer;
    InternalTrue(aEnvironment,aResult);
}

void LispXmlTokenizer(LispEnvironment& aEnvironment, LispPtr& aResult,
                      LispPtr& aArguments)
{
    TESTARGS(1);
    aEnvironment.iCurrentTokenizer = &aEnvironment.iXmlTokenizer;
    InternalTrue(aEnvironment,aResult);
}

void LispExplodeTag(LispEnvironment& aEnvironment, LispPtr& aResult,
                   LispPtr& aArguments)
{
    TESTARGS(2);
    LispPtr out;
    InternalEval(aEnvironment, out, Argument(aArguments,1));
    CHK_ISSTRING(out,1);

    LispCharPtr str = out.Get()->String()->String();
    str++;
    if (str[0] != '<')
    {
        aResult.Set(out.Get());
        return;
    }
    CHK_ARG(str[0] == '<',1);
    str++;
    LispCharPtr type = "\"Open\"";

    if (str[0] == '/')
    {
        type = "\"Close\"";
        str++;
    }
    LispString tag;
    tag.SetNrItems(0);
    
    tag.Append('\"');
    while (IsAlpha(*str))
    {
        LispChar c = *str++;
        if (c >= 'a' && c <= 'z')
            c = c + ('A'-'a');
        tag.Append(c);
    }
    tag.Append('\"');
    tag.Append('\0');

    LispObject* info = NULL;

    while (*str == ' ') str++;
    while (*str != '>')
    {
        LispString name;
        name.SetNrItems(0);
        name.Append('\"');
        
        LispCharPtr start = str;
        while (IsAlpha(*str))
        {
            LispChar c = *str++;
            if (c >= 'a' && c <= 'z')
                c = c + ('A'-'a');
            name.Append(c);
        }
        name.Append('\"');
        name.Append('\0');
//printf("Should be =, is %c\n",str[0]);
        CHK_ARG(str[0] == '=',1);
        str++;
//printf("Should be \", is %c\n",str[0]);
        CHK_ARG(str[0] == '\"',1);
        LispString value;
        value.SetNrItems(0);
        value.Append(*str++);
        while (*str != '\"')
        {
            value.Append(*str++);
        }
        value.Append(*str++);
        value.Append('\0');
//printf("[%s], [%s]\n",name.String(),value.String());
        info =  LIST(LA(ATOML("List")) + LA(ATOML(name.String())) + LA(ATOML(value.String()))) + LA(info);
        while (*str == ' ') str++;

//printf("End is %c\n",str[0]);
        if (*str == '/')
        {
            type = "\"OpenClose\"";
            str++;
            while (*str == ' ') str++;
        }
    }
    
    info = LIST(LA(ATOML("List")) + LA(info));
    aResult.Set(
                LIST(
                     LA(ATOML("XmlTag")) +
                     LA(ATOML(tag.String())) +
                     LA(info) +
                     LA(ATOML(type))
                    )
               );
}


void LispFastAssoc(LispEnvironment& aEnvironment, LispPtr& aResult,
                   LispPtr& aArguments)
{
    // Check that we have one argument.
    TESTARGS(3);

    // key to find
    LispPtr key;
    InternalEval(aEnvironment, key , Argument(aArguments,1));
    
    // assoc-list to find it in
    LispPtr list;
    InternalEval(aEnvironment, list , Argument(aArguments,2));

    LispObject* t;

    //Check that it is a compound object
    CHK_ARG(list.Get()->SubList() != NULL, 2);
    t = list.Get()->SubList()->Get();
    CHK_ARG(t != NULL, 2);
    t = t->Next().Get();

    while (t != NULL)
    {
        if (t->SubList())
        {
            LispObject* sub = t->SubList()->Get();
            if (sub)
            {
                sub = sub->Next().Get();
                LispPtr temp;
                temp.Set(sub);
                if(InternalEquals(aEnvironment,key,temp))
                {
                    aResult.Set(t);
                    return;
                }
                
            }
        }
        t = t->Next().Get();
    }

    
    aResult.Set(ATOML("Empty"));
}


void LispSetCRemarkReceiver(LispEnvironment& aEnvironment, LispPtr& aResult,
                            LispPtr& aArguments)
{
}


