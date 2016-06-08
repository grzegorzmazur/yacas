
#include "yacas/yacasprivate.h"
#include "yacas/lispparser.h"
#include "yacas/lispatom.h"
#include "yacas/lisperror.h"
#include "yacas/lispenvironment.h"

LispParser::LispParser(LispTokenizer& aTokenizer, LispInput& aInput,
                       LispEnvironment& aEnvironment):
    iTokenizer(aTokenizer),
    iInput(aInput),
    iEnvironment(aEnvironment),
    iListed(false)
{
}

void LispParser::Parse(LispPtr& aResult)
{
    aResult = nullptr;

    // Get token.
    const LispString* token = iTokenizer.NextToken(iInput, iEnvironment.HashTable());

    if (token->empty()) {
        aResult = iEnvironment.iEndOfFile->Copy();
        return;
    }
    ParseAtom(aResult, token);
}

void LispParser::ParseAtom(LispPtr& aResult, const LispString* aToken)
{
    // if token is empty string, return null pointer (no expression)
    if (aToken->empty())
        return;
    // else if token is "(" read in a whole array of objects until ")",
    //   and make a sublist
    if (aToken == iEnvironment.iBracketOpen->String()) {
        LispPtr subList;
        ParseList(subList);
        aResult = LispSubList::New(subList);
        return;
    }
    // else make a simple atom, and return it.
    aResult = LispAtom::New(iEnvironment, *aToken);
}

void LispParser::ParseList(LispPtr& aResult)
{
    LispPtr* iter = &aResult;
    if (iListed) {
        aResult = iEnvironment.iList->Copy();
        iter = &(aResult->Nixed());
    }
    for (;;) {
        //Get token.
        const LispString* token = iTokenizer.NextToken(iInput, iEnvironment.HashTable());

        // if token is empty string, error!
        if (token->empty())
            throw InvalidToken();

        // if token is ")" return result.
        if (token == iEnvironment.iBracketClose->String())
            return;

        // else parse simple atom with Parse, and append it to the
        // results list.
        ParseAtom(*iter, token);
        iter = &((*iter)->Nixed());
    }
}

void LispPrinter::Print(
                        const LispPtr& aExpression,
                        std::ostream& aOutput,
                        LispEnvironment& aEnvironment)
{
    PrintExpression(aExpression, aOutput, aEnvironment, 0);
}

void LispPrinter::Indent(std::ostream& aOutput, LispInt aDepth)
{
    aOutput.put('\n');
    LispInt i;
    for (i = aDepth; i > 0; i--) {
        aOutput.write("  ", 2);
    }
}

void LispPrinter::PrintExpression(const LispPtr& aExpression,
                                  std::ostream& aOutput,
                                  LispEnvironment& aEnvironment,
                                  LispInt aDepth)
{
    const LispPtr* iter = &aExpression;
    LispInt item = 0;
    while (!!(*iter)) {
        // if String not null pointer: print string
        const LispString* string = (*iter)->String();

        if (string) {
            aOutput << *string << ' ';
        }            // else print "(", print sublist, and print ")"
        else if ((*iter)->SubList()) {
            if (item != 0) {
                Indent(aOutput, aDepth + 1);
            }
            aOutput.put('(');
            PrintExpression(*((*iter)->SubList()), aOutput, aEnvironment, aDepth + 1);
            aOutput.put(')');
            item = 0;
        } else {
            aOutput << "[GenericObject]";
        }
        iter = &((*iter)->Nixed());
        item++;
    } // print next element
}

// does nothing in the LispPrinter but is used in derived classes

void LispPrinter::RememberLastChar(LispChar aChar)
{
}

