
#include "yacasprivate.h"
#include "lispparser.h"
#include "lispatom.h"
#include "lisperror.h"
#include "lispenvironment.h"

LispParser::LispParser(LispTokenizer& aTokenizer, LispInput& aInput,
           LispEnvironment& aEnvironment)
    : iTokenizer(aTokenizer), iInput(aInput),iEnvironment(aEnvironment),
    iListed(LispFalse) {}

LispParser::~LispParser() {}
void LispParser::Parse(LispPtr& aResult)
{
    aResult.Set(NULL);

    LispStringPtr token;
    // Get token.
    token = iTokenizer.NextToken(iInput,iEnvironment.HashTable());
    if (token->String()[0] == '\0')
    {
        aResult.Set(LispAtom::New(iEnvironment,"EndOfFile"));
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
    if (aToken == iEnvironment.HashTable().LookUp("("))
    {
        LispPtr subList;
        ParseList(subList);
        aResult.Set(LispSubList::New(subList.Get()));
        return;
    }
    // else make a simple atom, and return it.
    aResult.Set(LispAtom::New(iEnvironment,aToken->String()));
}

void LispParser::ParseList(LispPtr& aResult)
{
    LispStringPtr token;

    LispPtr* iter = &aResult;
    if (iListed)
    {
        aResult.Set(LispAtom::New(iEnvironment,"List"));
        iter  = &(aResult.Get()->Next());
    }
    for (;;)
    {
        //Get token.
        token = iTokenizer.NextToken(iInput,iEnvironment.HashTable());
        // if token is empty string, error!
        Check(token->String()[0],KInvalidToken);
        // if token is ")" return result.
        if (token == iEnvironment.HashTable().LookUp(")"))
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


void LispPrinter::Print(LispPtr& aExpression, LispOutput& aOutput, LispEnvironment& aEnvironment)
{
  PrintExpression(aExpression, aOutput, aEnvironment,0);
}

void LispPrinter::Indent(LispOutput& aOutput, LispInt aDepth)
{
  aOutput.Write("\n");
  LispInt i;
  for (i=aDepth;i>0;i--)
  {
    aOutput.Write("  ");
  }
}

void LispPrinter::PrintExpression(LispPtr& aExpression, LispOutput& aOutput, 
                     LispEnvironment& aEnvironment,
    	             LispInt aDepth)
{
    LispPtr* iter = &aExpression;
    LispInt item = 0;
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
	    if (item != 0)
	    {
	      Indent(aOutput,aDepth+1);
	    }
            aOutput.Write("(");
            PrintExpression(*(iter->Get()->SubList()),aOutput, aEnvironment,aDepth+1);
            aOutput.Write(")");
	    item=0;
        }
        else
        {
            aOutput.Write("[GenericObject]");
        }
        iter = &(iter->Get()->Next());
	item++;
    } // print next element
}

// does nothing in the LispPrinter but is used in derived classes
void LispPrinter::RememberLastChar(LispChar aChar)
{
}
