
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
    aResult = (NULL);

    // Get token.
    LispString * token = iTokenizer.NextToken(iInput,iEnvironment.HashTable());
    if (token->c_str()[0] == '\0')
    {
        aResult = (LispAtom::New(iEnvironment,"EndOfFile"));
        return;
    }
    ParseAtom(aResult,token);
}

void LispParser::ParseAtom(LispPtr& aResult, LispString * aToken)
{
    // if token is empty string, return null pointer (no expression)
    if (!aToken->c_str()[0])
        return;
    // else if token is "(" read in a whole array of objects until ")",
    //   and make a sublist
    if (aToken == iEnvironment.HashTable().LookUp("("))
    {
        LispPtr subList;
        ParseList(subList);
        aResult = (LispSubList::New(subList));
        return;
    }
    // else make a simple atom, and return it.
    aResult = (LispAtom::New(iEnvironment,aToken->c_str()));
}

void LispParser::ParseList(LispPtr& aResult)
{
    LispPtr* iter = &aResult;
    if (iListed)
    {
        aResult = (LispAtom::New(iEnvironment,"List"));
        iter  = &(aResult->Nixed());
    }
    for (;;)
    {
        //Get token.
      LispString * token = iTokenizer.NextToken(iInput,iEnvironment.HashTable());
        // if token is empty string, error!
        Check(token->c_str()[0],KInvalidToken);
        // if token is ")" return result.
        if (token == iEnvironment.HashTable().LookUp(")"))
        {
            return;
        }
        // else parse simple atom with Parse, and append it to the
        // results list.

        ParseAtom(*iter,token);
        iter = &((*iter)->Nixed());
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
    while (!!(*iter))
    {
        // if String not null pointer: print string
        LispString * string = (*iter)->String();

        if (string)
        {
            aOutput.Write(string->c_str());
            aOutput.PutChar(' ');
        }
        // else print "(", print sublist, and print ")"
        else if ((*iter)->SubList())
        {
      if (item != 0)
      {
        Indent(aOutput,aDepth+1);
      }
            aOutput.Write("(");
            PrintExpression(*((*iter)->SubList()),aOutput, aEnvironment,aDepth+1);
            aOutput.Write(")");
      item=0;
        }
        else
        {
            aOutput.Write("[GenericObject]");
        }
        iter = &((*iter)->Nixed());
  item++;
    } // print next element
}

// does nothing in the LispPrinter but is used in derived classes
void LispPrinter::RememberLastChar(LispChar aChar)
{
}

