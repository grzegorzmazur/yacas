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

bool InternalIsList(const LispEnvironment& env, const LispPtr& aPtr);
bool InternalIsString(const LispString* aOriginal);
std::string InternalUnstringify(const std::string& s);
LispInt InternalAsciiToInt(const LispString& aString);
bool IsNumber(const LispChar * ptr, bool aAllowFloat);

void InternalNth(LispPtr& aResult, const LispPtr& aArg, LispInt n);
void InternalTail(LispPtr& aResult, const LispPtr& aArg);
void InternalAssociate(LispPtr& aResult, const LispPtr& aKey,
                      const LispPtr& aAssociationList);

void InternalReverseList(LispPtr& aResult, const LispPtr& aOriginal);
void InternalFlatCopy(LispPtr& aResult, const LispPtr& aOriginal);
LispInt InternalListLength(const LispPtr& aOriginal);

bool InternalStrictTotalOrder(const LispEnvironment& env,
                  const LispPtr& e1,
                  const LispPtr& e2);

bool InternalEquals(const LispEnvironment& aEnvironment,
                    const LispPtr& aExpression1,
                    const LispPtr& aExpression2);


inline LispPtr& Argument(LispPtr& cur, LispInt n);

inline void InternalTrue(const LispEnvironment& aEnvironment, LispPtr& aResult);
inline void InternalFalse(const LispEnvironment& aEnvironment, LispPtr& aResult);
inline void InternalBoolean(LispEnvironment& aEnvironment, LispPtr& aResult,
                            bool aValue);
inline bool IsTrue(LispEnvironment& aEnvironment, const LispPtr& aExpression);
inline bool IsFalse(LispEnvironment& aEnvironment, const LispPtr& aExpression);
inline void InternalNot(LispPtr& aResult, LispEnvironment& aEnvironment, LispPtr& aExpression);

void DoInternalLoad(LispEnvironment& aEnvironment,LispInput* aInput);
void InternalLoad(LispEnvironment& aEnvironment, const std::string& aFileName);
void InternalUse(LispEnvironment& aEnvironment, const std::string& aFileName);
void InternalApplyString(LispEnvironment& aEnvironment, LispPtr& aResult,
                         const LispString* aOperator,LispPtr& aArgs);
void InternalApplyPure(LispPtr& oper,LispPtr& args2,LispPtr& aResult,LispEnvironment& aEnvironment);

void InternalEvalString(LispEnvironment& aEnvironment, LispPtr& aResult,
                        const LispChar* aString);

class LispObjectAdder : public YacasBase
{
public:
    LispObjectAdder(LispObject* aPtr)
        : iPtr(aPtr) {};
   LispObject* iPtr;
};

LispObject* operator+(const LispObjectAdder& left, const LispObjectAdder& right);

void ParseExpression(LispPtr& aResult, const LispChar* aString, LispEnvironment& aEnvironment);

void ReturnUnEvaluated(LispPtr& aResult,LispPtr& aArguments,
                       LispEnvironment& aEnvironment);

/** PrintExpression : print an expression into a string,
 limiting it to a maximum number of characters. If aMaxChars
 is less than zero, the result is not truncated.
 */
void PrintExpression(LispString& aResult,
                     LispPtr& aExpression,
                     LispEnvironment& aEnvironment,
                     std::size_t aMaxChars);

const LispString* SymbolName(LispEnvironment& aEnvironment, const std::string& aSymbol);




#include "standard.inl"


#endif
