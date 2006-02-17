

#ifndef __forth_h__
#define __forth_h__

/*


- Small plugin: forth interpreter.
  - actually get it working in a small demo.
  - allow externalizing/internalizing.
  - allow values to be shared among different executors.
  - linking in other plugins into the forth interpreter, so the calls to 
    these can be fast.

    Do it right immediately. This means immediately being able to recode
    puppet in yacas.

    Two cases: use as stand-alone objects, and combined with other
    objects.
    
    */

/*
 This forth scheme is an opportunity to introduce something new.
 Basic requirements are:
 - as fast as possible (numeric code). No run-time type checking,
   assume the compiler generates correct code.
 - enable the puppet stuff I made before, but more general, objects
   and all.
 - compile into small objects, and then have a linking step, for speed
   of developing large software projects.

The idea I want to try out is to have the concept of constraints, from
which the software should follow automatically.
The thing is since it knows the final sequence the program will be
evaluating it can hardcode the correct calling sequences, faking
lazy evaluation on the fly. Also, things like garbage collecting
should be implicit, but hardcoded with the appropriate delete
at the right time.

The idea is to have object-oriented programming, but building the
entire system into one big object, so global optimizations can be
done.

Also I plan on supporting hints: tell the compiler what the expensive
parts are.

So we are going to use c_form. Go from a OO definition of a program
to c code.

first target: a 3d graph plotter. Later something more ambitious.
A game possibly.

 */

class ForthVariables;
struct ForthValue
{
public:
    union u
    {
        int           i;
        double        d;
        LispString * s;
        ForthVariables* a; //array or object
    } u;
};


class ForthVariables : public CArrayGrower<ForthValue>
{ 
};

#define HAS_NEW_ForthStackPop 1

class ForthStack : CArrayGrower<ForthValue>
{
public:
    void Push(const ForthValue& aValue);
#if HAS_NEW_ForthStackPop == 0
    const ForthValue& Pop(void);
#else
	double PopDouble(void);
	int PopInt(void);
#endif
};


class ForthInterpreter;
struct ForthCommand
{
    union
    {
        void (*command)(ForthInterpreter& aInterpreter);
        int int_value;
        double double_value;
        LispString * string_value;
    } u;
};

class ForthSubRoutine : public CArrayGrower<ForthCommand>
{
};

class ForthMethods : public CDeletingArrayGrower<ForthSubRoutine*>
{
};

class ForthInterpreter
{
public:
    void Run(LispInt method);
    ForthStack&     Stack();
    ForthVariables& Variables();
    ForthMethods&   Methods();
public:
    inline ForthCommand* NextCommand();
private:
    ForthStack     stack;
    ForthVariables variables;
    ForthMethods   programs;
    LispInt program_counter;
    ForthSubRoutine* current_subroutine;
};

//TODO inline
inline ForthCommand* ForthInterpreter::NextCommand()
{
    return &(*current_subroutine)[program_counter++];
}

inline double cmd_get_double(ForthInterpreter& aInterpreter)
{
    return aInterpreter.NextCommand()->u.double_value;
}

inline void stack_push_double(ForthInterpreter& aInterpreter, double aDouble)
{
    ForthValue v;
    v.u.d = aDouble;
    aInterpreter.Stack().Push(v);
}

inline LispInt cmd_get_int(ForthInterpreter& aInterpreter)
{
    return aInterpreter.NextCommand()->u.int_value;
}

inline void stack_push_int(ForthInterpreter& aInterpreter, LispInt aInt)
{
    ForthValue v;
    v.u.i = aInt;
    aInterpreter.Stack().Push(v);
}


void int_add_int(ForthInterpreter& aInterpreter);
void int_to_double(ForthInterpreter& aInterpreter);
void push_double(ForthInterpreter& aInterpreter);
void push_int(ForthInterpreter& aInterpreter);
void double_add_double(ForthInterpreter& aInterpreter);

#endif












