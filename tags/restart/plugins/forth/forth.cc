
#include "lispstring.h"
#include "lisphash.h"
#include "forth.h"



void ForthStack::Push(const ForthValue& aValue)
{
    //TODO optimize?
    Append(aValue);
}

const ForthValue& ForthStack::Pop(void)
{
    const ForthValue& v = Item(NrItems()-1);
    //TODO optimize?
    SetNrItems(NrItems()-1);
    return v;
}

void ForthInterpreter::Run(LispInt method)
{
}

ForthStack& ForthInterpreter::Stack()
{
    return stack;
}
ForthVariables& ForthInterpreter::Variables()
{
    return variables;
}

ForthMethods&   ForthInterpreter::Methods()
{
    return programs;
}



//
// Command functions
//


void push_double(ForthInterpreter& aInterpreter)
{
    stack_push_double(aInterpreter, cmd_get_double(aInterpreter));
}
void push_int(ForthInterpreter& aInterpreter)
{
    stack_push_int(aInterpreter, cmd_get_int(aInterpreter));
}
void double_add_double(ForthInterpreter& aInterpreter)
{
    stack_push_double(aInterpreter,
                      aInterpreter.Stack().Pop().u.d+
                      aInterpreter.Stack().Pop().u.d);
}

void int_add_int(ForthInterpreter& aInterpreter)
{
    stack_push_int(aInterpreter,
                   aInterpreter.Stack().Pop().u.i+
                   aInterpreter.Stack().Pop().u.i);
}


void int_to_double(ForthInterpreter& aInterpreter)
{
    stack_push_double(aInterpreter,aInterpreter.Stack().Pop().u.i);
}


