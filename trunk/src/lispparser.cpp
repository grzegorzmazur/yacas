
#include "yacasprivate.h"
#include "lispparser.h"
#include "lispatom.h"
#include "lisperror.h"

LispParser::LispParser(LispTokenizer& aTokenizer, LispInput& aInput,
           LispHashTable& aHashTable)
: iTokenizer(aTokenizer), iInput(aInput),iHashTable(aHashTable) {}

LispParser::~LispParser() {}
void LispParser::Parse(LispPtr& aResult)
{
    aResult.Set(NULL);

    LispStringPtr token;
    // Get token.
    token = iTokenizer.NextToken(iInput,iHashTable);
    if (token->String()[0] == '\0')
    {
        aResult.Set(LispAtom::New(iHashTable.LookUp("EndOfFile")));
        return;
    }
    ParseAtom(aResult,token);
}

void LispParser::ParseAtom(LispPtr& aResult, LispStringPtr aToken)
{
    // if token is empty string, return null pointer (no expression)
    if (!aToken->String()[0])
        return;
    // else if token is "(" read in a whole array of objects until ")",
    //   and make a sublist
    if (aToken == iHashTable.LookUp("("))
    {
        LispPtr subList;
        ParseList(subList);
        aResult.Set(LispSubList::New(subList.Get()));
        return;
    }
    // else make a simple atom, and return it.
    aResult.Set(LispAtom::New(aToken));
}

void LispParser::ParseList(LispPtr& aResult)
{
    LispStringPtr token;
    LispPtr* iter = &aResult;
    for (;;)
    {
        //Get token.
        token = iTokenizer.NextToken(iInput,iHashTable);
        // if token is empty string, error!
        Check(token->String()[0],KInvalidToken);
        // if token is ")" return result.
        if (token == iHashTable.LookUp(")"))
        {
            return;
        }
        // else parse simple atom with Parse, and append it to the
        // results list.

        ParseAtom(*iter,token);
        iter = &(iter->Get()->Next());
    }
}


LispPrinter::~LispPrinter() {}
void LispPrinter::Print(LispPtr& aExpression, LispOutput& aOutput)
{
    LispPtr* iter = &aExpression;

    while (iter->Get() != NULL)
    {
        // if String not null pointer: print string
        LispStringPtr string = iter->Get()->String();

        if (string != NULL)
        {
            aOutput.Write(string->String());
            aOutput.PutChar(' ');
        }
        // else print "(", print sublist, and print ")"
        else if (iter->Get()->SubList() != NULL)
        {
            aOutput.Write("(");
            Print(*(iter->Get()->SubList()),aOutput);
            aOutput.Write(")");
        }
        else
        {
            aOutput.Write("[GenericObject]");
        }
        iter = &(iter->Get()->Next());
    } // print next element
}

