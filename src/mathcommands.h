
#ifndef __mathcommands_h__
#define __mathcommands_h__

#include "lispenvironment.h"
#include "lispevalhash.h"
#include "lispobject.h"
#include "lispglobals.h"


//
// Evaluation direction.
//
void LispQuote(LispEnvironment& aEnvironment, LispPtr& aResult,
                        LispPtr& aArguments);
void LispEval(LispEnvironment& aEnvironment,LispPtr& aResult,
              LispPtr& aArguments);

//
// Input/output functions
//
void LispWrite(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispSpace(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispNewLine(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispWriteString(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispFullForm(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispDefaultDirectory(LispEnvironment& aEnvironment, LispPtr& aResult,
                          LispPtr& aArguments);
void LispFromFile(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispFromString(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);

void LispRead(LispEnvironment& aEnvironment, LispPtr& aResult,
              LispPtr& aArguments);
void LispReadToken(LispEnvironment& aEnvironment, LispPtr& aResult,
                   LispPtr& aArguments);
void LispToFile(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispToString(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispLoad(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);

//
// Variable setting/clearing
//
void LispSetVar(LispEnvironment& aEnvironment, LispPtr& aResult,
                LispPtr& aArguments);
void LispMacroSetVar(LispEnvironment& aEnvironment, LispPtr& aResult,
                LispPtr& aArguments);
void LispClearVar(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispMacroClearVar(LispEnvironment& aEnvironment,
                  LispPtr& aResult,LispPtr& aArguments);
void LispNewLocal(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispMacroNewLocal(LispEnvironment& aEnvironment, LispPtr& aResult,
                       LispPtr& aArguments);


//
// List and compound object manipulation
//
void LispHead(LispEnvironment& aEnvironment, LispPtr& aResult,
              LispPtr& aArguments);
void LispNth(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispTail(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispDestructiveReverse(LispEnvironment& aEnvironment, LispPtr& aResult,
                            LispPtr& aArguments);
void LispLength(LispEnvironment& aEnvironment, LispPtr& aResult,
                LispPtr& aArguments);
void LispList(LispEnvironment& aEnvironment, LispPtr& aResult,
              LispPtr& aArguments);
void LispUnList(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispListify(LispEnvironment& aEnvironment, LispPtr& aResult,
                 LispPtr& aArguments);
void LispConcatenate(LispEnvironment& aEnvironment, LispPtr& aResult,
              LispPtr& aArguments);
void LispConcatenateStrings(LispEnvironment& aEnvironment, LispPtr& aResult,
              LispPtr& aArguments);
void LispDelete(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispInsert(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispReplace(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispAtomize(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispStringify(LispEnvironment& aEnvironment, LispPtr& aResult,
                   LispPtr& aArguments);
void LispDestructiveDelete(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispDestructiveInsert(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispDestructiveReplace(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispFlatCopy(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);

//
// Program control flow
//
void LispProgBody(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispWhile(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispIf(LispEnvironment& aEnvironment, LispPtr& aResult,
            LispPtr& aArguments);
void LispCheck(LispEnvironment& aEnvironment,LispPtr& aResult,
               LispPtr& aArguments);


//
// User function definition
//
void LispPreFix(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispInFix(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispPostFix(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispBodied(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispRuleBase(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispMacroRuleBase(LispEnvironment& aEnvironment, LispPtr& aResult,
                       LispPtr& aArguments);
void LispHoldArg(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispNewRule(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispMacroNewRule(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispUnFence(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispTryRetract(LispEnvironment& aEnvironment, LispPtr& aResult,
                 LispPtr& aArguments);

//
// Predicates
//
void LispNot(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispLazyAnd(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispLazyOr(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispEquals(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispLessThan(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispGreaterThan(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispIsFunction(LispEnvironment& aEnvironment,LispPtr& aResult,
              LispPtr& aArguments);
void LispIsAtom(LispEnvironment& aEnvironment,LispPtr& aResult,
              LispPtr& aArguments);
void LispIsNumber(LispEnvironment& aEnvironment,LispPtr& aResult,
              LispPtr& aArguments);
void LispIsInteger(LispEnvironment& aEnvironment,LispPtr& aResult,
              LispPtr& aArguments);
void LispIsList(LispEnvironment& aEnvironment,LispPtr& aResult,
              LispPtr& aArguments);
void LispIsString(LispEnvironment& aEnvironment,LispPtr& aResult,
              LispPtr& aArguments);
void LispIsBound(LispEnvironment& aEnvironment,LispPtr& aResult,
              LispPtr& aArguments);


//
// Math functions (REQUIRING number inputs).
//
void LispMultiply(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispAdd(LispEnvironment& aEnvironment, LispPtr& aResult,
             LispPtr& aArguments);
void LispSubtract(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispDivide(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispSin(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispCos(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispTan(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispArcSin(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispArcCos(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispArcTan(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispPrecision(LispEnvironment& aEnvironment, LispPtr& aResult,
                   LispPtr& aArguments);
void LispSqrt(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispFloor(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispCeil(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispAbs(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispMod(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispDiv(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispLog(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispExp(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispPower(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispPi(LispEnvironment& aEnvironment, LispPtr& aResult,
            LispPtr& aArguments);
void LispGcd(LispEnvironment& aEnvironment, LispPtr& aResult,
             LispPtr& aArguments);

void LispSystemCall(LispEnvironment& aEnvironment,LispPtr& aResult,
               LispPtr& aArguments);


void LispFastSin(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispFastCos(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispFastTan(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispFastArcSin(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispFastArcCos(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispFastArcTan(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispFastExp(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispFastLog(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispFastPower(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);

void LispFastSqrt(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispFastPi(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispFastFloor(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispFastCeil(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispFastMod(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispFastAbs(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);

void LispShiftLeft(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispShiftRight(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispFromBase(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispToBase(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);


void LispMaxEvalDepth(LispEnvironment& aEnvironment, LispPtr& aResult,
                      LispPtr& aArguments);

void LispDefLoad(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);

void LispUse(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);

void LispRightAssociative(LispEnvironment& aEnvironment, LispPtr& aResult,
                          LispPtr& aArguments);
void LispLeftPrecedence(LispEnvironment& aEnvironment, LispPtr& aResult,
                          LispPtr& aArguments);
void LispRightPrecedence(LispEnvironment& aEnvironment, LispPtr& aResult,
                          LispPtr& aArguments);


void LispIsInFix(LispEnvironment& aEnvironment, LispPtr& aResult,
               LispPtr& aArguments);
void LispIsPreFix(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispIsPostFix(LispEnvironment& aEnvironment, LispPtr& aResult,
                   LispPtr& aArguments);
void LispGetPrecedence(LispEnvironment& aEnvironment, LispPtr& aResult,
                       LispPtr& aArguments);
void LispGetLeftPrecedence(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments);
void LispGetRightPrecedence(LispEnvironment& aEnvironment, LispPtr& aResult,
                            LispPtr& aArguments);
void LispGetPrecision(LispEnvironment& aEnvironment, LispPtr& aResult,
                      LispPtr& aArguments);


void LispBitAnd(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispBitOr(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);
void LispBitXor(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);

void LispSecure(LispEnvironment& aEnvironment,LispPtr& aResult,
              LispPtr& aArguments);


void LispFindFile(LispEnvironment& aEnvironment,LispPtr& aResult,
              LispPtr& aArguments);
void LispFindFunction(LispEnvironment& aEnvironment,LispPtr& aResult,
                      LispPtr& aArguments);

/* Generic objecs support */
void LispIsGeneric(LispEnvironment& aEnvironment,LispPtr& aResult,
                   LispPtr& aArguments);
void LispGenericTypeName(LispEnvironment& aEnvironment,LispPtr& aResult,
                         LispPtr& aArguments);
void GenArrayCreate(LispEnvironment& aEnvironment,LispPtr& aResult,
                    LispPtr& aArguments);
void GenArraySize(LispEnvironment& aEnvironment,LispPtr& aResult,
                  LispPtr& aArguments);
void GenArrayGet(LispEnvironment& aEnvironment,LispPtr& aResult,
                 LispPtr& aArguments);
void GenArraySet(LispEnvironment& aEnvironment,LispPtr& aResult,
                 LispPtr& aArguments);

void LispTrace(LispEnvironment& aEnvironment,LispPtr& aResult, LispPtr& aArguments);
void LispTraceRule(LispEnvironment& aEnvironment,LispPtr& aResult, LispPtr& aArguments);
void LispTraceStack(LispEnvironment& aEnvironment,LispPtr& aResult, LispPtr& aArguments);

void LispReadLisp(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);

void LispType(LispEnvironment& aEnvironment,LispPtr& aResult,
              LispPtr& aArguments);

void LispStringMid(LispEnvironment& aEnvironment,LispPtr& aResult, LispPtr& aArguments);
void LispSetStringMid(LispEnvironment& aEnvironment,LispPtr& aResult, LispPtr& aArguments);


/* Pattern matching */
void GenPatternCreate(LispEnvironment& aEnvironment,LispPtr& aResult,
                      LispPtr& aArguments);
void GenPatternMatches(LispEnvironment& aEnvironment,LispPtr& aResult,
                       LispPtr& aArguments);

void LispRuleBaseDefined(LispEnvironment& aEnvironment,LispPtr& aResult,
                         LispPtr& aArguments);
void LispRuleBaseArgList(LispEnvironment& aEnvironment,LispPtr& aResult,
                         LispPtr& aArguments);


void LispNewRulePattern(LispEnvironment& aEnvironment, LispPtr& aResult,
                        LispPtr& aArguments);
void LispMacroNewRulePattern(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);


void LispSubst(LispEnvironment& aEnvironment, LispPtr& aResult,
               LispPtr& aArguments);
void LispLocalSymbols(LispEnvironment& aEnvironment, LispPtr& aResult,
                      LispPtr& aArguments);


void LispFac(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments);

void LispApplyPure(LispEnvironment& aEnvironment, LispPtr& aResult,
                   LispPtr& aArguments);

void LispPrettyPrinter(LispEnvironment& aEnvironment, LispPtr& aResult,
                       LispPtr& aArguments);


void LispGarbageCollect(LispEnvironment& aEnvironment, LispPtr& aResult,
                        LispPtr& aArguments);

void LispLazyGlobal(LispEnvironment& aEnvironment, LispPtr& aResult,
                    LispPtr& aArguments);

void LispPatchLoad(LispEnvironment& aEnvironment, LispPtr& aResult,
                   LispPtr& aArguments);
void LispPatchString(LispEnvironment& aEnvironment, LispPtr& aResult,
                     LispPtr& aArguments);

void LispDllLoad(LispEnvironment& aEnvironment, LispPtr& aResult,
                 LispPtr& aArguments);

void LispSetExtraInfo(LispEnvironment& aEnvironment, LispPtr& aResult,
                   LispPtr& aArguments);
void LispGetExtraInfo(LispEnvironment& aEnvironment, LispPtr& aResult,
                   LispPtr& aArguments);


#endif



