
#include "lispstring.h"
#include "lisphash.h"
#include "forth.h"



void ForthStack::Push(const ForthValue& aValue)
{
  Append(aValue);
}

#if HAS_NEW_ForthStackPop == 0
const ForthValue& ForthStack::Pop(void)
{
	// works, but I don't like it.
	// after the Resize, v might no longer reference the element?
    const ForthValue& v = elements()[Size()-1];
    ResizeTo(Size()-1);
    return v;
}
#else
double ForthStack::PopDouble(void)
{
	double v = elements()[Size()-1].u.d;
	ResizeTo(Size()-1);
	return v;
}
int ForthStack::PopInt(void)
{
	int v = elements()[Size()-1].u.i;
	ResizeTo(Size()-1);
	return v;
}
#endif

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
                      aInterpreter.Stack().PopDouble()+
                      aInterpreter.Stack().PopDouble());
}

void int_add_int(ForthInterpreter& aInterpreter)
{
    stack_push_int(aInterpreter,
                   aInterpreter.Stack().PopInt()+
                   aInterpreter.Stack().PopInt());
}


void int_to_double(ForthInterpreter& aInterpreter)
{
    stack_push_double(aInterpreter,aInterpreter.Stack().PopInt());
}


