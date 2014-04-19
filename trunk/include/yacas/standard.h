#ifndef YACAS_STANDARD_H
#define YACAS_STANDARD_H

#include "yacasbase.h"
#include "lispobject.h"
#include "lispenvironment.h"
#include "lisphash.h"
#include "lispatom.h"
#include "numbers.h"

// Prototypes
class LispHashTable;

bool InternalIsList(const LispPtr& aPtr);
bool InternalIsString(LispString * aOriginal);
void InternalUnstringify(LispString& aResult, LispString * aOriginal);
void InternalStringify(LispString& aResult, LispString * aOriginal);
void InternalIntToAscii(LispChar * aTrg,LispInt aInt);
LispInt InternalAsciiToInt(LispString * aString);
bool IsNumber(const LispChar * ptr,bool aAllowFloat);

void InternalNth(LispPtr& aResult, const LispPtr& aArg, LispInt n);
void InternalTail(LispPtr& aResult, const LispPtr& aArg);
void InternalAssociate(LispPtr& aResult, const LispPtr& aKey,
                      const LispPtr& aAssociationList);

void InternalReverseList(LispPtr& aResult, const LispPtr& aOriginal);
void InternalFlatCopy(LispPtr& aResult, LispPtr& aOriginal);
LispInt InternalListLength(LispPtr& aOriginal);

bool InternalEquals(LispEnvironment& aEnvironment,
                           LispPtr& aExpression1,
                           LispPtr& aExpression2);


inline LispPtr& Argument(LispPtr& cur, LispInt n);

inline void InternalTrue(LispEnvironment& aEnvironment, LispPtr& aResult);
inline void InternalFalse(LispEnvironment& aEnvironment, LispPtr& aResult);
inline void InternalBoolean(LispEnvironment& aEnvironment, LispPtr& aResult,
                            bool aValue);
inline bool IsTrue(LispEnvironment& aEnvironment, LispPtr& aExpression);
inline bool IsFalse(LispEnvironment& aEnvironment, LispPtr& aExpression);
inline void InternalNot(LispPtr& aResult, LispEnvironment& aEnvironment, LispPtr& aExpression);

void DoInternalLoad(LispEnvironment& aEnvironment,LispInput* aInput);
void InternalLoad(LispEnvironment& aEnvironment,LispString * aFileName);
void InternalUse(LispEnvironment& aEnvironment,LispString * aFileName);
void InternalApplyString(LispEnvironment& aEnvironment, LispPtr& aResult,
                         LispString * aOperator,LispPtr& aArgs);
void InternalApplyPure(LispPtr& oper,LispPtr& args2,LispPtr& aResult,LispEnvironment& aEnvironment);

void InternalEvalString(LispEnvironment& aEnvironment, LispPtr& aResult,
                        LispChar * aString);

#define ATOML(_s) LispAtom::New(aEnvironment,_s)
#define LIST(_c) LispSubList::New(_c)
class LispObjectAdder : public YacasBase
{
public:
    LispObjectAdder(LispObject* aPtr)
        : iPtr(aPtr) {};
   LispObject* iPtr;
};
#define LA(_o) LispObjectAdder(_o)

LispObject* operator+(const LispObjectAdder& left, const LispObjectAdder& right);

void ParseExpression(LispPtr& aResult, const LispChar* aString, LispEnvironment& aEnvironment);

void ReturnUnEvaluated(LispPtr& aResult,LispPtr& aArguments,
                       LispEnvironment& aEnvironment);

/** PrintExpression : print an expression into a string,
 limiting it to a maximum number of characters. If aMaxChars
 is less than zero, the result is not truncated.
 */
void PrintExpression(LispString& aResult, LispPtr& aExpression,
                     LispEnvironment& aEnvironment,
                     LispInt aMaxChars);

LispString* SymbolName(LispEnvironment& aEnvironment, const LispChar* aSymbol);




#include "standard.inl"


#endif
