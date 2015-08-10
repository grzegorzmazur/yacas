=========================================
Functions related to programming in Yacas
=========================================

Introduction
------------

This document aims to be a reference for functions that are useful 
when programming in {Yacas}, but which are not necessarily useful when 
using {Yacas}. There is another document that describes the functions
that are useful from a users point of view.

Programming
-----------

This chapter describes functions useful for writing Yacas scripts.

::

   /* --- Start of comment
   */ --- end of comment
   // --- Beginning of one-line comment

    /* comment */
    // comment

Introduce a comment block in a source file, similar to C++ comments.
``//`` makes everything until the end of the line a comment, while ``/*``
and ``*/`` may delimit a multi-line comment.

:Example:

::

   a+b; // get result
   a + /* add them */ b;


.. function:: Prog(expr1, expr2, ...) 
   
   block of statements

    :param expr1: expression

    The {Prog} and the {[ ... ]} construct have the same effect: they evaluate all
    arguments in order and return the result of the last evaluated expression.

    {Prog(a,b);} is the same as typing {[a;b;];} and is very useful
    for writing out function bodies. The {[ ... ]} construct is a
    syntactically nicer version of the {Prog} call; it is converted
    into {Prog(...)} during the parsing stage.



.. function:: Bodied(op, precedence)

   declare op as :term:`bodied function`

   :param op: string, the name of a function
   :param precedence: nonnegative integer (evaluated)

   Declares a special syntax for the function to be parsed as a
   :term:`bodied function`.


     For(pre, condition, post) statement;

   Here the function ``For`` has 4 arguments and the last argument is
   placed outside the parentheses.

   The ``precedence`` of a "bodied" function refers to how tightly the
   last argument is bound to the parentheses.  This makes a difference
   when the last argument contains other operators.  For example, when
   taking the derivative D(x) Sin(x)+Cos(x) both {Sin} and {Cos} are
   under the derivative because the bodied function {D} binds less
   tightly than the infix operator "{+}".

   .. seealso:: :func:`IsBodied`, :func:`OpPrecedence`

.. function:: Infix(op[, precedence])

   define function syntax (infix operator)

   :param op: string, the name of a function
   :param precedence: nonnegative integer (evaluated)

   Declares a special syntax for the function to be parsed as a bodied, infix, postfix,
   or prefix operator.

   "Infix" functions must have two arguments and are syntactically
   placed between their arguments.  Names of infix functions can be
   arbitrary, although for reasons of readability they are usually
   made of non-alphabetic characters.

   .. seealso:: :func:`IsBodied`, :func:`OpPrecedence`

.. function:: Postfix(op[, precedence])

   define function syntax (postfix operator)

   :param op: string, the name of a function
   :param precedence: nonnegative integer (evaluated)

   Declares a special syntax for the function to be parsed as a bodied, infix, postfix,
   or prefix operator.

   "Postfix" functions must have one argument and are syntactically
   placed after their argument.

   .. seealso:: :func:`IsBodied`, :func:`OpPrecedence`

.. function:: Prefix(op[, precedence])

   define function syntax (prefix operator)

   :param op: string, the name of a function
   :param precedence: nonnegative integer (evaluated)

   Declares a special syntax for the function to be parsed as a bodied, infix, postfix,
   or prefix operator.

   "Prefix" functions must have one argument and are syntactically
   placed before their argument.

   Function name can be any string but meaningful usage and
   readability would require it to be either made up entirely of
   letters or entirely of non-letter characters (such as "+", ":"
   etc.).  Precedence is optional (will be set to 0 by default).

   :Example:

   ::

      In> YY x := x+1;
      CommandLine(1) : Error parsing expression
    
      In> Prefix("YY", 2)
      Out> True;
      In> YY x := x+1;
      Out> True;
      In> YY YY 2*3
      Out> 12;
      In> Infix("##", 5)
      Out> True;
      In> a ## b ## c
      Out> a##b##c;

   Note that, due to a current parser limitation, a function atom that
   is declared prefix cannot be used by itself as an argument. ::

     In> YY
     CommandLine(1) : Error parsing expression

   .. seealso:: :func:`IsBodied`, :func:`OpPrecedence`



.. function:: IsBodied(op)

   check for function syntax

   :param op: string, the name of a function

   Check whether the function with given name {"op"} has been declared as a
   "bodied", infix, postfix, or prefix operator, and  return :data:`True` or :data:`False`.

.. function:: IsInfix(op)

   check for function syntax

   :param op: string, the name of a function

   Check whether the function with given name {"op"} has been declared as a
   "bodied", infix, postfix, or prefix operator, and  return :data:`True` or :data:`False`.

.. function:: IsPostfix(op)

   check for function syntax

   :param op: string, the name of a function

   Check whether the function with given name {"op"} has been declared as a
   "bodied", infix, postfix, or prefix operator, and  return :data:`True` or :data:`False`.

.. function:: IsPrefix(op)

   check for function syntax

   :param op: string, the name of a function

   Check whether the function with given name {"op"} has been declared as a
   "bodied", infix, postfix, or prefix operator, and  return :data:`True` or :data:`False`.

   :Example:

   ::

      In> IsInfix("+");
      Out> True;
      In> IsBodied("While");
      Out> True;
      In> IsBodied("Sin");
      Out> False;
      In> IsPostfix("!");
      Out> True;

   .. seealso:: :func:`Bodied`, :func:`OpPrecedence`

.. function:: OpPrecedence(op)

   get operator precedence

   :param op: string, the name of a function

   Returns the precedence of the function named "op" which should have
   been declared as a bodied function or an infix, postfix, or prefix
   operator. Generates an error message if the string str does not
   represent a type of function that can have precedence.

   For infix operators, right precedence can differ from left
   precedence. Bodied functions and prefix operators cannot have left
   precedence, while postfix operators cannot have right precedence;
   for these operators, there is only one value of precedence.


.. function:: OpLeftPrecedence(op)

   get operator precedence

   :param op: string, the name of a function

   Returns the precedence of the function named "op" which should have
   been declared as a bodied function or an infix, postfix, or prefix
   operator. Generates an error message if the string str does not
   represent a type of function that can have precedence.

   For infix operators, right precedence can differ from left
   precedence. Bodied functions and prefix operators cannot have left
   precedence, while postfix operators cannot have right precedence;
   for these operators, there is only one value of precedence.


.. function:: OpRightPrecedence(op)

   get operator precedence

   :param string op: name of a function

   Returns the precedence of the function named "op" which should have
   been declared as a bodied function or an infix, postfix, or prefix
   operator. Generates an error message if the string str does not
   represent a type of function that can have precedence.

   For infix operators, right precedence can differ from left
   precedence. Bodied functions and prefix operators cannot have left
   precedence, while postfix operators cannot have right precedence;
   for these operators, there is only one value of precedence.

   :Example:

   ::

      In> OpPrecedence("+")
      Out> 6;
      In> OpLeftPrecedence("!")
      Out> 0;



.. function:: RightAssociative(op)

   declare associativity

   :param op: string, the name of a function


   This makes the operator right-associative. For example: ::

     RightAssociative("*")

   would make multiplication right-associative. Take care not to abuse
   this function, because the reverse, making an infix operator
   left-associative, is not implemented. (All infix operators are by
   default left-associative until they are declared to be
   right-associative.)

   .. seealso:: :func:`OpPrecedence`


.. function:: LeftPrecedence(op, precedence)

   set operator precedence

   :param op: string, the name of a function
   :param precedence: nonnegative integer

   {"op"} should be an infix operator. This function call tells the
   infix expression printer to bracket the left or right hand side of
   the expression if its precedence is larger than precedence.

   This functionality was required in order to display expressions
   like {a-(b-c)} correctly. Thus, {a+b+c} is the same as {a+(b+c)},
   but {a-(b-c)} is not the same as {a-b-c}.

   Note that the left and right precedence of an infix operator does
   not affect the way Yacas interprets expressions typed by the
   user. You cannot make Yacas parse {a-b-c} as {a-(b-c)} unless you
   declare the operator "{-}" to be right-associative.

   .. seealso:: :func:`OpPrecedence`, :func:`OpLeftPrecedence`,
                :func:`OpRightPrecedence`, :func:`RightAssociative`

.. function:: RightPrecedence

   set operator precedence(op, precedence)

   :param op: string, the name of a function
   :param precedence: nonnegative integer

   {"op"} should be an infix operator. This function call tells the
   infix expression printer to bracket the left or right hand side of
   the expression if its precedence is larger than precedence.

   This functionality was required in order to display expressions
   like {a-(b-c)} correctly. Thus, {a+b+c} is the same as {a+(b+c)},
   but {a-(b-c)} is not the same as {a-b-c}.

   Note that the left and right precedence of an infix operator does
   not affect the way Yacas interprets expressions typed by the
   user. You cannot make Yacas parse {a-b-c} as {a-(b-c)} unless you
   declare the operator "{-}" to be right-associative.

   .. seealso:: :func:`OpPrecedence`, :func:`OpLeftPrecedence`,
                :func:`OpRightPrecedence`, :func:`RightAssociative`

.. function:: RuleBase(name, params)

   define function with a fixed number of arguments

   :param name: string, name of function
   :param params: list of arguments to function

   Define a new rules table entry for a function "name", with {params}
   as the parameter list. Name can be either a string or simple atom.

   In the context of the transformation rule declaration facilities
   this is a useful function in that it allows the stating of argument
   names that can he used with HoldArg.

   Functions can be overloaded: the same function can be defined with
   different number of arguments.


   .. seealso:: :func:`MacroRuleBase`, :func:`RuleBaseListed`,
                :func:`MacroRuleBaseListed`, :func:`HoldArg`,
                :func:`Retract`



.. function:: RuleBaseListed(name, params)

   define function with variable number of arguments

   :param name: string, name of function
   :param params: list of arguments to function

   The command {RuleBaseListed} defines a new function. It essentially
   works the same way as {RuleBase}, except that it declares a new
   function with a variable number of arguments. The list of
   parameters {params} determines the smallest number of arguments
   that the new function will accept. If the number of arguments
   passed to the new function is larger than the number of parameters
   in {params}, then the last argument actually passed to the new
   function will be a list containing all the remaining arguments.

   A function defined using {RuleBaseListed} will appear to have the
   arity equal to the number of parameters in the {param} list, and it
   can accept any number of arguments greater or equal than that. As a
   consequence, it will be impossible to define a new function with
   the same name and with a greater arity.

   The function body will know that the function is passed more
   arguments than the length of the {param} list, because the last
   argument will then be a list. The rest then works like a
   {RuleBase}-defined function with a fixed number of
   arguments. Transformation rules can be defined for the new function
   as usual.

   :Example:



   The definitions ::

     RuleBaseListed("f",{a,b,c})
     10 # f(_a,_b,{_c,_d}) <--
       Echo({"four args",a,b,c,d});
     20 # f(_a,_b,c_IsList) <--
       Echo({"more than four args",a,b,c});
     30 # f(_a,_b,_c) <-- Echo({"three args",a,b,c});

   give the following interaction: ::

     In> f(A)
     Out> f(A);
     In> f(A,B)
     Out> f(A,B);
     In> f(A,B,C)
     three args A B C 
     Out> True;
     In> f(A,B,C,D)
     four args A B C D 
     Out> True;
     In> f(A,B,C,D,E)
     more than four args A B {C,D,E} 
     Out> True;
     In> f(A,B,C,D,E,E)
     more than four args A B {C,D,E,E} 
     Out> True;

   The function {f} now appears to occupy all arities greater than 3: ::

     In> RuleBase("f", {x,y,z,t});
     CommandLine(1) : Rule base with this arity already defined


   .. seealso:: :func:`RuleBase`, :func:`Retract`, :func:`Echo`


.. function:: bodied Rule(body, operator, arity, precedence, predicate)

   define a rewrite rule

   :param "operator": string, name of function
   :param arity:
   :param precedence: integers
   :param predicate: function returning boolean
   :param body: expression, body of rule

   Define a rule for the function "operator" with "arity",
   "precedence", "predicate" and "body". The "precedence" goes from
   low to high: rules with low precedence will be applied first.

   The arity for a rules database equals the number of
   arguments. Different rules data bases can be built for functions
   with the same name but with a different number of arguments.

   Rules with a low precedence value will be tried before rules with a
   high value, so a rule with precedence 0 will be tried before a rule
   with precedence 1.

.. function:: HoldArg(operator, parameters)

   mark argument as not evaluated

   {"operator"} -- string, name of a function
   {parameter} -- atom, symbolic name of parameter


   Specify that parameter should not be evaluated before used. This
   will be declared for all arities of "operator", at the moment this
   function is called, so it is best called after all {RuleBase} calls
   for this operator.  "operator" can be a string or atom specifying
   the function name.

   The {parameter} must be an atom from the list of symbolic arguments
   used when calling {RuleBase}.

   .. seealso:: :func:`RuleBase`, :func:`HoldArgNr`,
                :func:`RuleBaseArgList`

.. function:: Retract(function, arity)

   erase rules for a function

   {"function"} -- string, name of function
   {arity} -- positive integer

   Remove a rulebase for the function named {"function"} with the
   specific {arity}, if it exists at all. This will make Yacas forget
   all rules defined for a given function. Rules for functions with
   the same name but different arities are not affected.

   Assignment {:=} of a function does this to the function being
   (re)defined.

   .. seealso:: :func:`RuleBaseArgList`, :func:`RuleBase`, :func:`:=`

.. function:: UnFence(operator, arity)

   change local variable scope for a function

   {"operator"} -- string, name of function
   {arity} -- positive integers

   When applied to a user function, the bodies defined for the rules
   for "operator" with given arity can see the local variables from
   the calling function. This is useful for defining macro-like
   procedures (looping and such).

   The standard library functions {For} and {ForEach} use {UnFence}.

.. function:: HoldArgNr(function, arity, argNum)

   specify argument as not evaluated

   {"function"} -- string, function name
   {arity}, {argNum} -- positive integers

   Declares the argument numbered {argNum} of the function named
   {"function"} with specified {arity} to be unevaluated
   ("held"). Useful if you don't know symbolic names of parameters,
   for instance, when the function was not declared using an explicit
   {RuleBase} call. Otherwise you could use {HoldArg}.

   .. seealso:: :func:`HoldArg`, :func:`RuleBase`


.. function:: RuleBaseArgList(operator, arity)

   obtain list of arguments

   {"operator"} -- string, name of function
   {arity} -- integer

   Returns a list of atoms, symbolic parameters specified in the
   {RuleBase} call for the function named {"operator"} with the
   specific {arity}.

   .. seealso:: :func:`RuleBase`, :func:`HoldArgNr`, :func:`HoldArg`


.. function:: MacroSet

   define rules in functions

.. function:: MacroClear

   define rules in functions

.. function:: MacroLocal
              
   define rules in functions

.. function:: MacroRuleBase

   define rules in functions

.. function:: MacroRuleBaseListed

   define rules in functions

.. function:: MacroRule

   define rules in functions

   These functions have the same effect as their non-macro
   counterparts, except that their arguments are evaluated before the
   required action is performed.  This is useful in macro-like
   procedures or in functions that need to define new rules based on
   parameters.

   Make sure that the arguments of {Macro}... commands evaluate to
   expressions that would normally be used in the non-macro versions!

   .. seealso:: :func:`Set`, :func:`Clear`, :func:`Local`,
                :func:`RuleBase`, :func:`Rule`, :func:`Backquoting`

.. function:: Backquoting

   macro expansion (LISP-style backquoting)

   {expression} -- expression containing "{@var}" combinations to substitute the value of variable "{var}"

   Backquoting is a macro substitution mechanism. A backquoted
   {expression} is evaluated in two stages: first, variables prefixed
   by {@} are evaluated inside an expression, and second, the new
   expression is evaluated.

   To invoke this functionality, a backquote {`} needs to be placed in
   front of an expression. Parentheses around the expression are
   needed because the backquote binds tighter than other operators.

   The expression should contain some variables (assigned atoms) with
   the special prefix operator {@}. Variables prefixed by {@} will be
   evaluated even if they are inside function arguments that are
   normally not evaluated (e.g. functions declared with {HoldArg}). If
   the {@var} pair is in place of a function name, e.g. "{@f(x)}",
   then at the first stage of evaluation the function name itself is
   replaced, not the return value of the function (see example); so at
   the second stage of evaluation, a new function may be called.

   One way to view backquoting is to view it as a parametric
   expression generator. {@var} pairs get substituted with the value
   of the variable {var} even in contexts where nothing would be
   evaluated. This effect can be also achieved using {UnList} and
   {Hold} but the resulting code is much more difficult to read and
   maintain.

   This operation is relatively slow since a new expression is built
   before it is evaluated, but nonetheless backquoting is a powerful
   mechanism that sometimes allows to greatly simplify code.

   :Example:

   This example defines a function that automatically evaluates to
   a number as soon as the argument is a number (a lot of functions
   do this only when inside a {N(...)} section). ::

     In> Decl(f1,f2) := \
     In>   `(@f1(x_IsNumber) <-- N(@f2(x)));
     Out> True;
     In> Decl(nSin,Sin)
     Out> True;
     In> Sin(1)
     Out> Sin(1);
     In> nSin(1)
     Out> 0.8414709848;

   This example assigns the expression {func(value)} to variable
   {var}. Normally the first argument of {Set} would be unevaluated. ::

     In> SetF(var,func,value) := \
     In>     `(Set(@var,@func(@value)));
     Out> True;
     In> SetF(a,Sin,x)
     Out> True;
     In> a
     Out> Sin(x);


   .. seealso:: :func:`MacroSet`, :func:`MacroLocal`,
                :func:`MacroRuleBase`, :func:`Hold`, :func:`HoldArg`,
                :func:`DefMacroRuleBase`



.. function:: DefMacroRuleBase(name,params)

   define a function as a macro

   {name} -- string, name of a function
   {params} -- list of arguments

   {DefMacroRuleBase} is similar to {RuleBase}, with the difference
   that it declares a macro, instead of a function.  After this call,
   rules can be defined for the function "{name}", but their
   interpretation will be different.

   With the usual functions, the evaluation model is that of the
   <i>applicative-order model of substitution</i>, meaning that first
   the arguments are evaluated, and then the function is applied to
   the result of evaluating these arguments. The function is entered,
   and the code inside the function can not access local variables
   outside of its own local variables.

   With macros, the evaluation model is that of the <i>normal-order
   model of substitution</i>, meaning that all occurrences of
   variables in an expression are first substituted into the body of
   the macro, and only then is the resulting expression evaluated
   <i>in its calling environment</i>. This is important, because then
   in principle a macro body can access the local variables from the
   calling environment, whereas functions can not do that.

   As an example, suppose there is a function {square}, which squares
   its argument, and a function {add}, which adds its
   arguments. Suppose the definitions of these functions are::

     add(x,y) <-- x+y;

   and ::

     square(x) <-- x*x;

   In applicative-order mode (the usual way functions are evaluated),
   in the following expression ::

     add(square(2),square(3))

   first the arguments to {add} get evaluated. So, first {square(2)}
   is evaluated.  To evaluate this, first {2} is evaluated, but this
   evaluates to itself. Then the {square} function is applied to it,
   {2*2}, which returns 4. The same is done for {square(3)}, resulting
   in {9}. Only then, after evaluating these two arguments, {add} is
   applied to them, which is equivalent to ``add(4,9)`` resulting in
   calling {4+9}, which in turn results in {13}.

   In contrast, when {add} is a macro, the arguments to {add} are first
   expanded. So ::

     add(square(2),square(3))

   first expands to ::

     square(2) + square(3)

   and then this expression is evaluated, as if the user had written
   it directly.  In other words, {square(2)} is not evaluated before
   the macro has been fully expanded.


   Macros are useful for customizing syntax, and compilers can
   potentially greatly optimize macros, as they can be inlined in the
   calling environment, and optimized accordingly.

   There are disadvantages, however. In interpreted mode, macros are
   slower, as the requirement for substitution means that a new
   expression to be evaluated has to be created on the fly. Also, when
   one of the parameters to the macro occur more than once in the body
   of the macro, it is evaluated multiple times.

   When defining transformation rules for macros, the variables to be
   substituted need to be preceded by the {@} operator, similar to the
   back-quoting mechanism.  Apart from that, the two are similar, and
   all transformation rules can also be applied to macros.

   Macros can co-exist with functions with the same name but different
   arity.  For instance, one can have a function {foo(a,b)} with two
   arguments, and a macro {foo(a,b,c)} with three arguments.


   :Example:

      The following example defines a function {myfor}, and shows one
      use, referencing a variable {a} from the calling environment. ::

        In> DefMacroRuleBase("myfor",{init,pred,inc,body})
        Out> True;
        In> myfor(_init,_pred,_inc,_body)<--[@init;While(@pred)[@body;@inc;];True;];
        Out> True;
        In> a:=10
        Out> 10;
        In> myfor(i:=1,i<10,i++,Echo(a*i))
        10 
        20 
        30 
        40 
        50 
        60 
        70 
        80 
        90 
        Out> True;
        In> i
        Out> 10;

   .. seealso:: :func:`RuleBase`, :func:`Backquoting`,
                :func:`DefMacroRuleBaseListed`

.. function:: DefMacroRuleBaseListed(name, params)

   define macro with variable number of arguments

   {"name"} -- string, name of function
   {params} -- list of arguments to function

   This does the same as {DefMacroRuleBase} (define a macro), but with a variable
   number of arguments, similar to {RuleBaseListed}.

   .. seealso:: :func:`RuleBase`, :func:`RuleBaseListed`,
                :func:`Backquoting`, :func:`DefMacroRuleBase`


.. function:: ExtraInfo'Set(expr,tag)
              ExtraInfo'Get(expr)

   annotate objects with additional information

   {expr} -- any expression
   {tag} -- tag information (any other expression)

   Sometimes it is useful to be able to add extra tag information to
   "annotate" objects or to label them as having certain
   "properties". The functions {ExtraInfo'Set} and {ExtraInfo'Get}
   enable this.

   The function {ExtraInfo'Set} returns the tagged expression, leaving
   the original expression alone. This means there is a common
   pitfall: be sure to assign the returned value to a variable, or the
   tagged expression is lost when the temporary object is destroyed.

   The original expression is left unmodified, and the tagged
   expression returned, in order to keep the atomic objects small. To
   tag an object, a new type of object is created from the old object,
   with one added property (the tag). The tag can be any expression
   whatsoever.

   The function {ExtraInfo'Get(x)} retrieves this tag expression from
   an object {x}. If an object has no tag, it looks the same as if it
   had a tag with value :data:`False`.

   No part of the Yacas core uses tags in a way that is visible to the
   outside world, so for specific purposes a programmer can devise a
   format to use for tag information. Association lists (hashes) are a
   natural fit for this, although it is not required and a tag can be
   any object (except the atom :data:`False` because it is indistinguishable
   from having no tag information). Using association lists is highly
   advised since it is most likely to be the format used by other
   parts of the library, and one needs to avoid clashes with other
   library code.  Typically, an object will either have no tag or a
   tag which is an associative list (perhaps empty). A script that
   uses tagged objects will check whether an object has a tag and if
   so, will add or modify certain entries of the association list,
   preserving any other tag information.

   Note that {FlatCopy} currently does <i>not</i> copy the tag
   information (see examples).

   :Example:

   ::

      In> a:=2*b
      Out> 2*b;
      In> a:=ExtraInfo'Set(a,{{"type","integer"}})
      Out> 2*b;
      In> a
      Out> 2*b;
      In> ExtraInfo'Get(a)
      Out> {{"type","integer"}};
      In> ExtraInfo'Get(a)["type"]
      Out> "integer";
      In> c:=a
      Out> 2*b;
      In> ExtraInfo'Get(c)
      Out> {{"type","integer"}};
      In> c
      Out> 2*b;
      In> d:=FlatCopy(a);
      Out> 2*b;
      In> ExtraInfo'Get(d)
      Out> False;

   .. seealso:: :func:`Assoc`, :func:`:=`

.. function:: GarbageCollect()

   do garbage collection on unused memory

   {GarbageCollect} garbage-collects unused memory. The Yacas system
   uses a reference counting system for most objects, so this call is
   usually not necessary.

   Reference counting refers to bookkeeping where in each object a
   counter is held, keeping track of the number of parts in the system
   using that object. When this count drops to zero, the object is
   automatically removed. Reference counting is not the fastest way of
   doing garbage collection, but it can be implemented in a very clean
   way with very little code.

   Among the most important objects that are not reference counted are
   the strings. {GarbageCollect} collects these and disposes of them
   when they are not used any more.

   {GarbageCollect} is useful when doing a lot of text processing, to
   clean up the text buffers. It is not highly needed, but it keeps
   memory use low.


.. function:: FindFunction(function)

   find the library file where a function is defined

   {function} -- string, the name of a function

   This function is useful for quickly finding the file where a
   standard library function is defined. It is likely to only be
   useful for developers. The function {FindFunction} scans the {.def}
   files that were loaded at start-up.  This means that functions that
   are not listed in {.def} files will not be found with
   {FindFunction}.

   :Example:

   ::

      In> FindFunction("Sum")
      Out> "sums.rep/code.ys";
      In> FindFunction("Integrate")
      Out> "integrate.rep/code.ys";

   .. seealso:: :func:`Vi`

.. function:: Secure(body)

   guard the host OS

   {body} -- expression

   {Secure} evaluates {body} in a "safe" environment, where files
   cannot be opened and system calls are not allowed. This can help
   protect the system when e.g. a script is sent over the Internet to
   be evaluated on a remote computer, which is potentially unsafe.

   .. seealso:: :func:`SystemCall`

Arbitrary-precision numerical programming
-----------------------------------------

This chapter contains functions that help programming numerical
calculations with arbitrary precision.

.. function:: MultiplyNum(x,y[,...])

   optimized numerical multiplication

   {x}, {y}, {z} -- integer, rational or floating-point numbers to multiply


   The function {MultiplyNum} is used to speed up multiplication of
   floating-point numbers with rational numbers. Suppose we need to
   compute $(p/q)*x$ where $p$, $q$ are integers and $x$ is a
   floating-point number. At high precision, it is faster to multiply
   $x$ by an integer $p$ and divide by an integer $q$ than to compute
   $p/q$ to high precision and then multiply by $x$. The function
   {MultiplyNum} performs this optimization.

   The function accepts any number of arguments (not less than two) or
   a list of numbers. The result is always a floating-point number
   (even if {InNumericMode()} returns False).

   .. seealso:: :func:`MathMultiply`

.. function:: CachedConstant(cache, Cname, Cfunc)

   precompute multiple-precision constants

   {cache} -- atom, name of the cache
   {Cname} -- atom, name of the constant
   {Cfunc} -- expression that evaluates the constant

   This function is used to create precomputed multiple-precision
   values of constants. Caching these values will save time if they
   are frequently used.

   The call to {CachedConstant} defines a new function named {Cname()}
   that returns the value of the constant at given precision. If the
   precision is increased, the value will be recalculated as
   necessary, otherwise calling {Cname()} will take very little time.

   The parameter {Cfunc} must be an expression that can be evaluated
   and returns the value of the desired constant at the current
   precision. (Most arbitrary-precision mathematical functions do this
   by default.)

   The associative list {cache} contains elements of the form {{Cname,
   prec, value}}, as illustrated in the example. If this list does not
   exist, it will be created.

   This mechanism is currently used by {N()} to precompute the values
   of $Pi$ and $gamma$ (and the golden ratio through {GoldenRatio},
   and {Catalan}).  The name of the cache for {N()} is
   {CacheOfConstantsN}.  The code in the function {N()} assigns
   unevaluated calls to {Internal'Pi()} and {Internal'gamma()} to the
   atoms {Pi} and {gamma} and declares them to be lazy global
   variables through {SetGlobalLazyVariable} (with equivalent
   functions assigned to other constants that are added to the list of
   cached constants).

   The result is that the constants will be recalculated only when
   they are used in the expression under {N()}.  In other words, the
   code in {N()} does the equivalent of ::

     SetGlobalLazyVariable(mypi,Hold(Internal'Pi()));
     SetGlobalLazyVariable(mygamma,Hold(Internal'gamma()));

   After this, evaluating an expression such as {1/2+gamma} will call
   the function {Internal'gamma()} but not the function
   {Internal'Pi()}.

   :Example:

   ::

      In> CachedConstant( my'cache, Ln2, Internal'LnNum(2) )
      Out> True;
      In> Internal'Ln2()
      Out> 0.6931471806;
      In> V(N(Internal'Ln2(),20))
      CachedConstant: Info: constant Ln2 is being recalculated at precision 20 
      Out> 0.69314718055994530942;
      In> my'cache
      Out> {{"Ln2",20,0.69314718055994530942}};

   .. seealso:: :func:`N`, :func:`Builtin'Precision'Set`, :func:`Pi`,
                :func:`GoldenRatio`, :func:`Catalan`, :func:`gamma`

.. function:: NewtonNum(func, x0[, prec0[, order]])

   low-level optimized Newton's iterations

   {func} -- a function specifying the iteration sequence
   {x0} -- initial value (must be close enough to the root)
   {prec0} -- initial precision (at least 4, default 5)
   {order} -- convergence order (typically 2 or 3, default 2)

   This function is an optimized interface for computing Newton's
   iteration sequences for numerical solution of equations in
   arbitrary precision.

   {NewtonNum} will iterate the given function starting from the
   initial value, until the sequence converges within current
   precision.  Initially, up to 5 iterations at the initial precision
   {prec0} is performed (the low precision is set for speed). The
   initial value {x0} must be close enough to the root so that the
   initial iterations converge. If the sequence does not produce even
   a single correct digit of the root after these initial iterations,
   an error message is printed. The default value of the initial
   precision is 5.

   The {order} parameter should give the convergence order of the
   scheme.  Normally, Newton iteration converges quadratically (so the
   default value is {order}=2) but some schemes converge faster and
   you can speed up this function by specifying the correct
   order. (Caution: if you give {order}=3 but the sequence is actually
   quadratic, the result will be silently incorrect. It is safe to use
   {order}=2.)

   The verbose option {V} can be used to monitor the convergence. The
   achieved exact digits should roughly form a geometric progression.

   :Example:

   ::

      In> Builtin'Precision'Set(20)
      Out> True;
      In> NewtonNum({{x}, x+Sin(x)}, 3, 5, 3)
      Out> 3.14159265358979323846;

   .. seealso:: :func:`Newton`

.. function:: SumTaylorNum

   optimized numerical evaluation of Taylor series

   SumTaylorNum(x, NthTerm, order)
   SumTaylorNum(x, NthTerm, TermFactor, order)
   SumTaylorNum(x, ZerothTerm, TermFactor, order)

   {NthTerm} -- a function specifying $n$-th coefficient of the series
   {ZerothTerm} -- value of the $0$-th coefficient of the series
   {x} -- number, value of the expansion variable
   {TermFactor} -- a function specifying the ratio of $n$-th term to the previous one
   {order} -- power of $x$ in the last term

   {SumTaylorNum} computes a Taylor series $Sum(k,0,n,a[k]*x^k)$
   numerically. This function allows very efficient computations of
   functions given by Taylor series, although some tweaking of the
   parameters is required for good results.

   The coefficients $a[k]$ of the Taylor series are given as functions
   of one integer variable ($k$). It is convenient to pass them to
   {SumTaylorNum} as closures.  For example, if a function {a(k)} is
   defined, then ::

    SumTaylorNum(x, {{k}, a(k)}, n)

   computes the series $Sum(k, 0, n, a(k)*x^k)$.

   Often a simple relation between successive coefficients $a[k-1]$,
   $a[k]$ of the series is available; usually they are related by a
   rational factor. In this case, the second form of {SumTaylorNum}
   should be used because it will compute the series faster. The
   function {TermFactor} applied to an integer $k>=1$ must return the
   ratio $a[k]$/$a[k-1]$. (If possible, the function {TermFactor}
   should return a rational number and not a floating-point number.)
   The function {NthTerm} may also be given, but the current
   implementation only calls {NthTerm(0)} and obtains all other
   coefficients by using {TermFactor}.  Instead of the function
   {NthTerm}, a number giving the $0$-th term can be given.

   The algorithm is described elsewhere in the documentation.  The
   number of terms {order}+1 must be specified and a sufficiently high
   precision must be preset in advance to achieve the desired
   accuracy.  (The function {SumTaylorNum} does not change the current
   precision.)

   :Example:

   To compute 20 digits of $Exp(1)$ using the Taylor series, one needs
   21 digits of working precision and 21 terms of the series. ::

     In> Builtin'Precision'Set(21)
     Out> True;
     In> SumTaylorNum(1, {{k},1/k!}, 21)
     Out> 2.718281828459045235351;
     In> SumTaylorNum(1, 1, {{k},1/k}, 21)
     Out> 2.71828182845904523535;
     In> SumTaylorNum(1, {{k},1/k!}, {{k},1/k}, 21)
     Out> 2.71828182845904523535;
     In> RoundTo(N(Ln(%)),20)
     Out> 1;

   .. seealso:: :func:`Taylor`

.. function:: IntPowerNum(x, n, mult, unity)

   optimized computation of integer powers

   {x} -- a number or an expression
   {n} -- a non-negative integer (power to raise {x} to)
   {mult} -- a function that performs one multiplication
   {unity} -- value of the unity with respect to that multiplication

   {IntPowerNum} computes the power $x^n$ using the fast binary
   algorithm.  It can compute integer powers with $n>=0$ in any ring
   where multiplication with unity is defined.  The multiplication
   function and the unity element must be specified.  The number of
   multiplications is no more than $2*Ln(n)/Ln(2)$.

   Mathematically, this function is a generalization of {MathPower} to
   rings other than that of real numbers.

   In the current implementation, the {unity} argument is only used
   when the given power {n} is zero.

   :Example:

   For efficient numerical calculations, the {MathMultiply} function can be passed: ::

     In> IntPowerNum(3, 3, MathMultiply,1)
     Out> 27;

   Otherwise, the usual {*} operator suffices: ::

     In> IntPowerNum(3+4*I, 3, *,1)
     Out> Complex(-117,44);
     In> IntPowerNum(HilbertMatrix(2), 4, *, Identity(2))
     Out> {{289/144,29/27},{29/27,745/1296}};

   Compute $Mod(3^100,7)$: ::

     In> IntPowerNum(3,100,{{x,y},Mod(x*y,7)},1)
     Out> 4;

   .. seealso:: :func:`MultiplyNum`, :func:`MathPower`,
                :func:`MatrixPower`

.. function:: BinSplitNum(n1, n2, a, b, c, d)

   computations of series by the binary splitting method

.. function:: BinSplitData(n1,n2, a, b, c, d)

   computations of series by the binary splitting method

.. function:: BinSplitFinal({P,Q,B,T})

   computations of series by the binary splitting method

   {n1}, {n2} -- integers, initial and final indices for summation
   {a}, {b}, {c}, {d} -- functions of one argument, coefficients of the series
   {P}, {Q}, {B}, {T} -- numbers, intermediate data as returned by {BinSplitData}

   The binary splitting method is an efficient way to evaluate many
   series when fast multiplication is available and when the series
   contains only rational numbers.  The function {BinSplitNum}
   evaluates a series of the form $$ S(n[1],n[2])=Sum(k,n[1],n[2],
   a(k)/b(k)*(p(0)/q(0)) * ... * p(k)/q(k)) $$.  Most series for
   elementary and special functions at rational points are of this
   form when the functions $a(k)$, $b(k)$, $p(k)$, $q(k)$ are chosen
   appropriately.

   The last four arguments of {BinSplitNum} are functions of one
   argument that give the coefficients $a(k)$, $b(k)$, $p(k)$, $q(k)$.
   In most cases these will be short integers that are simple to
   determine.  The binary splitting method will work also for
   non-integer coefficients, but the calculation will take much longer
   in that case.

   Note: the binary splitting method outperforms the straightforward
   summation only if the multiplication of integers is faster than
   quadratic in the number of digits.  See <*the algorithm
   documentation|yacasdoc://Algo/3/14/*> for more information.

   The two other functions are low-level functions that allow a finer
   control over the calculation.  The use of the low-level routines
   allows checkpointing or parallelization of a binary splitting
   calculation.

   The binary splitting method recursively reduces the calculation of
   $S(n[1],n[2])$ to the same calculation for the two halves of the
   interval [$n[1]$, $n[2]$].  The intermediate results of a binary
   splitting calculation are returned by {BinSplitData} and consist of
   four integers $P$, $Q$, $B$, $T$.  These four integers are
   converted into the final answer $S$ by the routine {BinSplitFinal}
   using the relation $$ S = T / (B*Q) $$.

   :Example:

   Compute the series for $e=Exp(1)$ using binary splitting.
   (We start from $n=1$ to simplify the coefficient functions.)::

     In> Builtin'Precision'Set(21)
     Out> True;
     In>  BinSplitNum(1,21, {{k},1}, {{k},1},{{k},1},{{k},k})
     Out> 1.718281828459045235359;
     In> N(Exp(1)-1)
     Out> 1.71828182845904523536;
     In>  BinSplitData(1,21, {{k},1}, {{k},1},{{k},1},{{k},k})
     Out> {1,51090942171709440000,1, 87788637532500240022};
     In> BinSplitFinal(%)
     Out> 1.718281828459045235359;

   .. seealso:: :func:`SumTaylorNum`

.. function:: MathSetExactBits(x)

   manipulate precision of floating-point numbers

.. function:: MathGetExactBits(x,bits)

   manipulate precision of floating-point numbers

   {x} -- an expression evaluating to a floating-point number
   {bits} -- integer, number of bits 

   Each floating-point number in Yacas has an internal precision
   counter that stores the number of exact bits in the mantissa.  The
   number of exact bits is automatically updated after each arithmetic
   operation to reflect the gain or loss of precision due to
   round-off.  The functions {MathGetExactBits}, {MathSetExactBits}
   allow to query or set the precision flags of individual number
   objects.

   {MathGetExactBits(x)} returns an integer number $n$ such that {x}
   represents a real number in the interval [$x*(1-2^(-n))$,
   $x*(1+2^(-n))$] if $x!=0$ and in the interval [$-2^(-n)$, $2^(-n)$]
   if $x=0$.  The integer $n$ is always nonnegative unless {x} is zero
   (a "floating zero").  A floating zero can have a negative value of
   the number $n$ of exact bits.

   These functions are only meaningful for floating-point numbers.
   (All integers are always exact.)  For integer {x}, the function
   {MathGetExactBits} returns the bit count of {x} and the function
   {MathSetExactBits} returns the unmodified integer {x}.

   .. todo:: FIXME - these examples currently do not work because of bugs

   :Example:

   The default precision of 10 decimals corresponds to 33 bits::

     In> MathGetExactBits(1000.123)
     Out> 33;
     In> x:=MathSetExactBits(10., 20)
     Out> 10.;
     In> MathGetExactBits(x)
     Out> 20;

   Prepare a "floating zero" representing an interval [-4, 4]::

     In> x:=MathSetExactBits(0., -2)
     Out> 0.;
     In> x=0
     Out> True;

   .. seealso:: :func:`Builtin'Precision'Set`, :func:`Builtin'Precision'Get`


.. function:: InNumericMode()

   determine if currently in numeric mode

.. function:: NonN(expr)

   calculate part in non-numeric mode

   {expr} -- expression to evaluate
   {prec} -- integer, precision to use

   When in numeric mode, {InNumericMode()} will return :data:`True`, else it
   will return :data:`False`. {Yacas} is in numeric mode when evaluating an
   expression with the function {N}. Thus when calling {N(expr)},
   {InNumericMode()} will return :data:`True` while {expr} is being
   evaluated.

   {InNumericMode()} would typically be used to define a
   transformation rule that defines how to get a numeric approximation
   of some expression. One could define a transformation rule::

     f(_x)_InNumericMode() <- [... some code to get a numeric approximation of f(x) ... ];

   {InNumericMode()} usually returns :data:`False`, so transformation rules
   that check for this predicate are usually left alone.

   When in numeric mode, {NonN} can be called to switch back to non-numeric
   mode temporarily.

   {NonN} is a macro. Its argument {expr} will only be evaluated after
   the numeric mode has been set appropriately.

   :Example:

   ::

      In> InNumericMode()
      Out> False
      In> N(InNumericMode())
      Out> True
      In> N(NonN(InNumericMode()))
      Out> False

   .. seealso:: :func:`N`, :func:`Builtin'Precision'Set`,
                :func:`Builtin'Precision'Get`, :func:`Pi`,
                :func:`CachedConstant`

.. function:: IntLog(n, base)

   integer part of logarithm

   {n}, {base} -- positive integers

   {IntLog} calculates the integer part of the logarithm of {n} in
   base {base}. The algorithm uses only integer math and may be faster
   than computing $$Ln(n)/Ln(base)$$ with multiple precision
   floating-point math and rounding off to get the integer part.

   This function can also be used to quickly count the digits in a
   given number.

   :Example:

   Count the number of bits::

     In> IntLog(257^8, 2)
     Out> 64;

   Count the number of decimal digits::

     In> IntLog(321^321, 10)
     Out> 804;

   .. seealso:: :func:`IntNthRoot`, :func:`Div`, :func:`Mod`,
                :func:`Ln`

.. function:: IntNthRoot(x, n)

   integer part of $n$-th root

   {x}, {n} -- positive integers

   {IntNthRoot} calculates the integer part of the $n$-th root of
   $x$. The algorithm uses only integer math and may be faster than
   computing $x^(1/n)$ with floating-point and rounding.

   This function is used to test numbers for prime powers.

   :Example:

   ::

      In> IntNthRoot(65537^111, 37)
      Out> 281487861809153;

   .. seealso:: :func:`IntLog`, :func:`MathPower`, :func:`IsPrimePower`



.. function:: NthRoot(m,n)

   calculate/simplify nth root of an integer

   {m} -- a non-negative integer ($m>0$)
   {n} -- a positive integer greater than 1 ($n>1$)

   {NthRoot(m,n)} calculates the integer part of the $n$-th root
   $m^(1/n)$ and returns a list {{f,r}}. {f} and {r} are both positive
   integers that satisfy :math:`f^nr=m`.  In other words, $f$ is the
   largest integer such that $m$ divides $f^n$ and $r$ is the
   remaining factor.

   For large {m} and small {n} {NthRoot} may work quite slowly. Every
   result {{f,r}} for given {m}, {n} is saved in a lookup table, thus
   subsequent calls to {NthRoot} with the same values {m}, {n} will be
   executed quite fast.

   :Example:

   ::

      In> NthRoot(12,2)
      Out> {2,3};
      In> NthRoot(81,3)
      Out> {3,3};
      In> NthRoot(3255552,2)
      Out> {144,157};
      In> NthRoot(3255552,3)
      Out> {12,1884};

   .. seealso:: :func:`IntNthRoot`, :func:`Factors`, :func:`MathPower`


.. function:: ContFracList(frac[,depth])

   manipulate continued fractions

.. function:: ContFracEval(list[,rest])

   manipulate continued fractions

   {frac} -- a number to be expanded
   {depth} -- desired number of terms
   {list} -- a list of coefficients
   {rest} -- expression to put at the end of the continued fraction

   The function {ContFracList} computes terms of the continued
   fraction representation of a rational number {frac}.  It returns a
   list of terms of length {depth}. If {depth} is not specified, it
   returns all terms.

   The function {ContFracEval} converts a list of coefficients into a
   continued fraction expression. The optional parameter {rest}
   specifies the symbol to put at the end of the expansion. If it is
   not given, the result is the same as if {rest=0}.

   :Example:

   ::

      In> A:=ContFracList(33/7 + 0.000001)
      Out> {4,1,2,1,1,20409,2,1,13,2,1,4,1,1,3,3,2};
      In> ContFracEval(Take(A, 5))
      Out> 33/7;
      In> ContFracEval(Take(A,3), remainder)
      Out> 1/(1/(remainder+2)+1)+4;
    
   .. seealso:: :func:`ContFrac`, :func:`GuessRational`

.. function:: GuessRational(x[,digits])

   find optimal rational approximations

.. function:: NearRational(x,[digits])

   find optimal rational approximations

.. function:: BracketRational(x,eps)

   find optimal rational approximations

   {x} -- a number to be approximated (must be already evaluated to floating-point)
   {digits} -- desired number of decimal digits (integer)
   {eps} -- desired precision

   The functions {GuessRational(x)} and {NearRational(x)} attempt to
   find "optimal" rational approximations to a given value {x}. The
   approximations are "optimal" in the sense of having smallest
   numerators and denominators among all rational numbers close to
   {x}. This is done by computing a continued fraction representation
   of {x} and truncating it at a suitably chosen term.  Both functions
   return a rational number which is an approximation of {x}.

   Unlike the function {Rationalize()} which converts floating-point
   numbers to rationals without loss of precision, the functions
   {GuessRational()} and {NearRational()} are intended to find the
   best rational that is <i>approximately</i> equal to a given value.

   The function {GuessRational()} is useful if you have obtained a
   floating-point representation of a rational number and you know
   approximately how many digits its exact representation should
   contain.  This function takes an optional second parameter {digits}
   which limits the number of decimal digits in the denominator of the
   resulting rational number. If this parameter is not given, it
   defaults to half the current precision. This function truncates the
   continuous fraction expansion when it encounters an unusually large
   value (see example).  This procedure does not always give the
   "correct" rational number; a rule of thumb is that the
   floating-point number should have at least as many digits as the
   combined number of digits in the numerator and the denominator of
   the correct rational number.

   The function {NearRational(x)} is useful if one needs to
   approximate a given value, i.e. to find an "optimal" rational
   number that lies in a certain small interval around a certain value
   {x}. This function takes an optional second parameter {digits}
   which has slightly different meaning: it specifies the number of
   digits of precision of the approximation; in other words, the
   difference between {x} and the resulting rational number should be
   at most one digit of that precision. The parameter {digits} also
   defaults to half of the current precision.

   The function {BracketRational(x,eps)} can be used to find
   approximations with a given relative precision from above and from
   below.  This function returns a list of two rational numbers
   {{r1,r2}} such that $r1<x<r2$ and $Abs(r2-r1)<Abs(x*eps)$.  The
   argument {x} must be already evaluated to enough precision so that
   this approximation can be meaningfully found.  If the approximation
   with the desired precision cannot be found, the function returns an
   empty list.

   :Example:

   Start with a rational number and obtain a floating-point approximation::

     In> x:=N(956/1013)
     Out> 0.9437314906
     In> Rationalize(x)
     Out> 4718657453/5000000000;
     In> V(GuessRational(x))
     GuessRational: using 10 terms of the continued fraction
     Out> 956/1013;
     In> ContFracList(x)
     Out> {0,1,16,1,3,2,1,1,1,1,508848,3,1,2,1,2,2};

   The first 10 terms of this continued fraction correspond to the
   correct continued fraction for the original rational number::

     In> NearRational(x)
     Out> 218/231;

   This function found a different rational number closeby because the
   precision was not high enough::

     In> NearRational(x, 10)
     Out> 956/1013;

   Find an approximation to $Ln(10)$ good to 8 digits::

     In> BracketRational(N(Ln(10)), 10^(-8))
     Out> {12381/5377,41062/17833};


   .. seealso:: :func:`ContFrac`, :func:`ContFracList`,
                :func:`Rationalize`


.. function:: TruncRadian(r)

   remainder modulo :math:`2*Pi`

   {r} -- a number

   {TruncRadian} calculates $Mod(r,2*Pi)$, returning a value between
   $0$ and $2*Pi$. This function is used in the trigonometry
   functions, just before doing a numerical calculation using a Taylor
   series. It greatly speeds up the calculation if the value passed is
   a large number.

   The library uses the formula $$TruncRadian(r) = r - Floor( r/(2*Pi)
   )*2*Pi$$, where $r$ and $2*Pi$ are calculated with twice the
   precision used in the environment to make sure there is no rounding
   error in the significant digits.

   :Example:

   ::

      In> 2*Internal'Pi()
      Out> 6.283185307;
      In> TruncRadian(6.28)
      Out> 6.28;
      In> TruncRadian(6.29)
      Out> 0.0068146929;

   .. seealso:: :func:`Sin`, :func:`Cos`, :func:`Tan`


.. function:: Builtin'Precision'Set(n)

   set the precision

   {n} -- integer, new value of precision

   This command sets the number of decimal digits to be used in
   calculations.  All subsequent floating point operations will allow
   for at least {n} digits of mantissa.

   This is not the number of digits after the decimal point.  For
   example, {123.456} has 3 digits after the decimal point and 6
   digits of mantissa.  The number {123.456} is adequately computed by
   specifying {Builtin'Precision'Set(6)}.

   The call {Builtin'Precision'Set(n)} will not guarantee that all
   results are precise to {n} digits.

   When the precision is changed, all variables containing previously
   calculated values remain unchanged.  The {Builtin'Precision'Set}
   function only makes all further calculations proceed with a
   different precision.

   Also, when typing floating-point numbers, the current value of
   {Builtin'Precision'Set} is used to implicitly determine the number
   of precise digits in the number.

   :Example:

   ::

      In> Builtin'Precision'Set(10)
      Out> True;
      In> N(Sin(1))
      Out> 0.8414709848;
      In> Builtin'Precision'Set(20)
      Out> True;
      In> x:=N(Sin(1))
      Out> 0.84147098480789650665;

   The value {x} is not changed by a {Builtin'Precision'Set()} call::

      In> [ Builtin'Precision'Set(10); x; ]
      Out> 0.84147098480789650665;

   The value {x} is rounded off to 10 digits after an arithmetic
   operation::

     In> x+0.
     Out> 0.8414709848;

   In the above operation, {0.} was interpreted as a number which is
   precise to 10 digits (the user does not need to type {0.0000000000}
   for this to happen).  So the result of {x+0.} is precise only to 10
   digits.

   .. seealso:: :func:`Builtin'Precision'Get`, :func:`N`

.. function:: Builtin'Precision'Get()

   get the current precision

   This command returns the current precision, as set by
   {Builtin'Precision'Set}.

   :Example:

   ::

      In> Builtin'Precision'Get();
      Out> 10;
      In> Builtin'Precision'Set(20);
      Out> True;
      In> Builtin'Precision'Get();
      Out> 20;

   .. seealso:: :func:`Builtin'Precision'Set`, :func:`N`




Error reporting
---------------

This chapter contains commands useful for reporting errors to the user.

.. function:: Check(predicate,"error text")

   report "hard" errors

.. function:: TrapError(expression,errorHandler)

   trap "hard" errors

.. function:: GetCoreError()

   get "hard" error string

   {predicate} -- expression returning :data:`True` or :data:`False`
   {"error text"} -- string to print on error
   {expression} -- expression to evaluate (causing potential error)
   {errorHandler} -- expression to be called to handle error

   If {predicate} does not evaluate to :data:`True`, the current operation
   will be stopped, the string {"error text"} will be printed, and
   control will be returned immediately to the command line. This
   facility can be used to assure that some condition is satisfied
   during evaluation of expressions (guarding against critical
   internal errors).

   A "soft" error reporting facility that does not stop the execution
   is provided by the function {Assert}.

   :Example:

      In> [Check(1=0,"bad value"); Echo(OK);]
      In function "Check" : 
      CommandLine(1) : "bad value"

   Note that {OK} is not printed.

   TrapError evaluates its argument {expression}, returning the result
   of evaluating {expression}. If an error occurs, {errorHandler} is
   evaluated, returning its return value in stead.

   GetCoreError returns a string describing the core error.  TrapError
   and GetCoreError can be used in combination to write a custom error
   handler.


   .. seealso:: :func:`Assert`

.. function:: Assert(pred, str, expr) 
              Assert(pred, str) pred
              Assert(pred)

   signal "soft" custom error


   Precedence:
   EVAL OpPrecedence("Assert")

   {pred} -- predicate to check
   {"str"} -- string to classify the error
   {expr} -- expression, error object

   {Assert} is a global error reporting mechanism. It can be used to
   check for errors and report them. An error is considered to occur
   when the predicate {pred} evaluates to anything except :data:`True`. In
   this case, the function returns :data:`False` and an error object is
   created and posted to the global error tableau.  Otherwise the
   function returns :data:`True`.

   Unlike the "hard" error function {Check}, the function {Assert}
   does not stop the execution of the program.

   The error object consists of the string {"str"} and an arbitrary
   expression {expr}. The string should be used to classify the kind
   of error that has occurred, for example "domain" or "format". The
   error object can be any expression that might be useful for
   handling the error later; for example, a list of erroneous values
   and explanations.  The association list of error objects is
   currently obtainable through the function {GetErrorTableau()}.

   If the parameter {expr} is missing, {Assert} substitutes :data:`True`. If
   both optional parameters {"str"} and {expr} are missing, {Assert}
   creates an error of class {"generic"}.

   Errors can be handled by a custom error handler in the portion of
   the code that is able to handle a certain class of errors. The
   functions {IsError}, {GetError} and {ClearError} can be used.

   Normally, all errors posted to the error tableau during evaluation
   of an expression should be eventually printed to the screen. This
   is the behavior of prettyprinters {DefaultPrint}, {Print},
   {PrettyForm} and {TeXForm} (but not of the inline prettyprinter,
   which is enabled by default); they call {DumpErrors} after
   evaluating the expression.

   :Example:

   ::

      In> Assert("bad value", "must be zero") 1=0
      Out> False;
      In> Assert("bad value", "must be one") 1=1
      Out> True;
      In> IsError()
      Out> True;
      In> IsError("bad value")
      Out> True;
      In> IsError("bad file")
      Out> False;
      In> GetError("bad value");
      Out> "must be zero";
      In> DumpErrors()
      Error: bad value: must be zero
      Out> True;

   No more errors left::

     In> IsError()
     Out> False;
     In> DumpErrors()
     Out> True;

   .. seealso:: :func:`IsError`, :func:`DumpErrors`, :func:`Check`,
                :func:`GetError`, :func:`ClearError`,
                :func:`ClearErrors`, :func:`GetErrorTableau`

.. function:: DumpErrors()

   simple error handlers

.. function:: ClearErrors()

   simple error handlers

   {DumpErrors} is a simple error handler for the global error
   reporting mechanism. It prints all errors posted using {Assert} and
   clears the error tableau.

   {ClearErrors} is a trivial error handler that does nothing except
   it clears the tableau.

   .. seealso:: :func:`Assert`, :func:`IsError`

.. function:: IsError()
              IsError(str)

   check for custom error

   {"str"} -- string to classify the error

   {IsError()} returns :data:`True` if any custom errors have been reported
   using {Assert}.  The second form takes a parameter {"str"} that
   designates the class of the error we are interested in. It returns
   :data:`True` if any errors of the given class {"str"} have been reported.

   .. seealso:: :func:`GetError`, :func:`ClearError`, :func:`Assert`,
                :func:`Check`


.. function:: GetError(str)

   custom errors handlers

.. function:: ClearError(str)

   custom errors handlers

.. function:: GetErrorTableau()

   custom errors handlers

   {"str"} -- string to classify the error

   These functions can be used to create a custom error handler.

   {GetError} returns the error object if a custom error of class
   {"str"} has been reported using {Assert}, or :data:`False` if no errors
   of this class have been reported.

   {ClearError("str")} deletes the same error object that is returned
   by {GetError("str")}. It deletes at most one error object. It
   returns :data:`True` if an object was found and deleted, and :data:`False`
   otherwise.

   {GetErrorTableau()} returns the entire association list of
   currently reported errors.

   :Example:

   ::

      In> x:=1
      Out> 1;
      In> Assert("bad value", {x,"must be zero"}) x=0
      Out> False;
      In> GetError("bad value")
      Out> {1, "must be zero"};
      In> ClearError("bad value");
      Out> True;
      In> IsError()
      Out> False;
      
   .. seealso:: :func:`IsError`, :func:`Assert`, :func:`Check`,
                :func:`ClearErrors`

.. function:: CurrentFile()

   return current input file

.. function:: CurrentLine()

   return current line number on input

   The functions {CurrentFile} and {CurrentLine} return a string
   with the file name of the current file and the current line 
   of input respectively.

   These functions are most useful in batch file calculations, where
   there is a need to determine at which line an error occurred.
   One can define a function::

     tst() := Echo({CurrentFile(),CurrentLine()});

   which can then be inserted into the input file at various places,
   to see how far the interpreter reaches before an error occurs.

   .. seealso:: :func:`Echo`






Built-in (core) functions
-------------------------

Yacas comes with a small core of built-in functions and a large
library of user-defined functions. Some of these core functions are
documented in this chapter.

It is important for a developer to know which functions are built-in
and cannot be redefined or {Retract}-ed. Also, core functions may be
somewhat faster to execute than functions defined in the script
library. All core functions are listed in the file {corefunctions.h}
in the {src/} subdirectory of the Yacas source tree. The declarations
typically look like this::

  SetCommand(LispSubtract, "MathSubtract");

Here {LispSubtract} is the Yacas internal name for the function and
{MathSubtract} is the name visible to the Yacas language.  Built-in
bodied functions and infix operators are declared in the same file.


.. function:: MathNot(expression)

   built-in logical "not"

   Returns "False" if "expression" evaluates to "True", and vice
   versa.

.. function:: MathAnd()

   built-in logical "and"

   Lazy logical {And}: returns :data:`True` if all args evaluate to :data:`True`,
   and does this by looking at first, and then at the second argument,
   until one is :data:`False`.  If one of the arguments is :data:`False`, {And}
   immediately returns :data:`False` without evaluating the rest. This is
   faster, but also means that none of the arguments should cause side
   effects when they are evaluated.

.. function:: MathOr()

   built-in logical "or"

   {MathOr} is the basic logical "or" function. Similarly to {And}, it
   is lazy-evaluated. {And(...)} and {Or(...)} do also exist, defined
   in the script library. You can redefine them as infix operators
   yourself, so you have the choice of precedence. In the standard
   scripts they are in fact declared as infix operators, so you can
   write {expr1 And expr}.

.. function:: BitAnd(n,m)

   bitwise and operation

.. function:: BitOr(n,m)

   bitwise or operation

.. function:: BitXor(n,m)

   bitwise xor operation

   These functions return bitwise "and", "or" and "xor" of two
   numbers.

.. function:: Equals(a,b)

   check equality

   Compares evaluated {a} and {b} recursively (stepping into
   expressions). So "Equals(a,b)" returns "True" if the expressions
   would be printed exactly the same, and "False" otherwise.

.. function:: GreaterThan(a,b)

   comparison predicate

.. function:: LessThan(a,b)

   comparison predicate


   {a}, {b} -- numbers or strings

   Comparing numbers or strings (lexicographically).

   :Example:

   ::

      In> LessThan(1,1)
      Out> False;
      In> LessThan("a","b")
      Out> True;


.. function:: MathExp()


.. function:: MathLog()


.. function:: MathPower()


.. function:: MathSin()


.. function:: MathCos()


.. function:: MathTan()


.. function:: MathArcSin()


.. function:: MathArcCos()


.. function:: MathArcTan()


.. function:: MathSinh()


.. function:: MathCosh()


.. function:: MathTanh()


.. function:: MathArcSinh()


.. function:: MathArcCosh()


.. function:: MathArcTanh()


.. function:: MathGcd()


.. function:: MathAdd()


.. function:: MathSubtract()


.. function:: MathMultiply()


.. function:: MathDivide()


.. function:: MathSqrt()


.. function:: MathFloor()


.. function:: MathCeil()


.. function:: MathAbs()


.. function:: MathMod()


.. function:: MathDiv()


.. function:: MathGcd(n,m)

   Greatest Common Divisor

.. function:: MathAdd(x,y)
   (add two numbers)

.. function:: MathSubtract(x,y)
   (subtract two numbers)

.. function:: MathMultiply(x,y)
   
   (multiply two numbers)

.. function:: MathDivide(x,y)

   (divide two numbers)

.. function:: MathSqrt(x)

   (square root, must be x>=0)

.. function:: MathFloor(x)

   (largest integer not larger than x)

.. function:: MathCeil(x)

   (smallest integer not smaller than x)

.. function:: MathAbs(x)

   (absolute value of x, or ``|x|`` )

.. function:: MathExp(x)

   (exponential, base 2.718...)

.. function:: MathLog(x)

   (natural logarithm, for x>0)

.. function:: MathPower(x,y)

   (power, x ^ y)

.. function:: MathSin(x)

   (sine)

.. function:: MathCos(x)

   (cosine)

.. function:: MathTan(x)

   (tangent)

.. function:: MathSinh(x)

   (hyperbolic sine)

.. function:: MathCosh(x)

   (hyperbolic cosine)

.. function:: MathTanh(x)

   (hyperbolic tangent)

.. function:: MathArcSin(x)

   (inverse sine)

.. function:: MathArcCos(x)

   (inverse cosine)

.. function:: MathArcTan(x)

   (inverse tangent)

.. function:: MathArcSinh(x)

   (inverse hyperbolic sine)

.. function:: MathArcCosh(x)

   (inverse hyperbolic cosine)

.. function:: MathArcTanh(x)

   (inverse hyperbolic tangent)

.. function:: MathDiv(x,y)

   (integer division, result is an integer)

.. function:: MathMod(x,y)

   (remainder of division, or x mod y)

   These commands perform the calculation of elementary mathematical
   functions.  The arguments <i>must</i> be numbers.  The reason for
   the prefix {Math} is that the library needs to define equivalent
   non-numerical functions for symbolic computations, such as {Exp},
   {Sin} and so on.

   Note that all functions, such as the {MathPower}, {MathSqrt},
   {MathAdd} etc., accept integers as well as floating-point numbers.
   The resulting values may be integers or floats.  If the
   mathematical result is an exact integer, then the integer is
   returned.  For example, {MathSqrt(25)} returns the integer {5}, and
   {MathPower(2,3)} returns the integer {8}.  In such cases, the
   integer result is returned even if the calculation requires more
   digits than set by {Builtin'Precision'Set}.  However, when the
   result is mathematically not an integer, the functions return a
   floating-point result which is correct only to the current
   precision.

   :Example:

   ::

      In> Builtin'Precision'Set(10)
      Out> True
      In> Sqrt(10)
      Out> Sqrt(10)
      In> MathSqrt(10)
      Out> 3.16227766
      In> MathSqrt(490000*2^150)
      Out> 26445252304070013196697600
      In> MathSqrt(490000*2^150+1)
      Out> 0.264452523e26
      In> MathPower(2,3)
      Out> 8
      In> MathPower(2,-3)
      Out> 0.125


.. function:: FastLog(x)

   (natural logarithm),

.. function:: FastPower(x,y)


.. function:: FastArcSin(x)

   double-precision math functions

   Versions of these functions using the C++ library. These should
   then at least be faster than the arbitrary precision versions.

.. function:: ShiftLeft(expr, bits)

   built-in bitwise shift left operation

.. function:: ShiftRight(expr, bits)

   built-in bitwise shift right operation

   ShiftLeft(expr,bits)
   ShiftRight(expr,bits)

   Shift bits to the left or to the right.

.. function:: IsPromptShown()

   test for the Yacas prompt option

   Returns :data:`False` if Yacas has been started with the option to
   suppress the prompt, and :data:`True` otherwise.


.. function:: GetTime(expr)

   measure the time taken by an evaluation

   {expr} -- any expression


   The function {GetTime(expr)} evaluates the expression {expr} and
   returns the time needed for the evaluation.  The result is returned
   as a floating-point number of seconds.  The value of the expression
   {expr} is lost.

   The result is the "user time" as reported by the OS, not the real
   ("wall clock") time.  Therefore, any CPU-intensive processes
   running alongside Yacas will not significantly affect the result of
   {GetTime}.

   :Example:

   ::

      In> GetTime(Simplify((a*b)/(b*a)))
      Out> 0.09;

   .. seealso:: :func:`Time`



Generic objects
---------------

Generic objects are objects that are implemented in C++, but
can be accessed through the Yacas interpreter.

.. function:: IsGeneric(object)

   check for generic object

   Returns :data:`True` if an object is of a generic object type.

.. function:: GenericTypeName(object)

   get type name

   Returns a string representation of the name of a generic object.

   :Example:

   ::

      In> GenericTypeName(Array'Create(10,1))
      Out> "Array";

.. function:: Array'Create(size, init)

   create array

   :param size: size of the array
   :param init: initial value

   Creates an array with ``size`` elements, all initialized to the
   value ``init``.

.. function:: Array'Size(array)

   array size

   :param array: an array
   :returns: array size (number of elements in the array)

.. function:: Array'Get(array,index)

   fetch array element

   :param array: an array
   :param index: an index

   :returns: the element of ``array`` at position ``index``

   .. note::

      Array indices are one-based, which means that the first element
      is indexed by 1.

   Arrays can also be accessed through the ``[]`` operators. So
   ``array[index]`` would return the same as ``Array'Get(array,
   index)``.

.. function:: Array'Set(array,index,element)

   set array element

   Sets the element at position index in the array passed to the value
   passed in as argument to element. Arrays are treated as base-one,
   so {index} set to 1 would set first element.

   Arrays can also be accessed through the {[]} operators. So
   {array[index] := element} would do the same as {Array'Set(array,
   index,element)}.

.. function:: Array'CreateFromList(list)

   convert list to array

   Creates an array from the contents of the list passed in.

.. function:: Array'ToList(array)

   convert array to list

   Creates a list from the contents of the array passed in.



The Yacas test suite
--------------------

This chapter describes commands used for verifying correct performance
of Yacas.

Yacas comes with a test suite which can be found in
the directory {tests/}. Typing 

    make test

on the command line after Yacas was built will run the test.
This test can be run even before {make install}, as it only 
uses files in the local directory of the Yacas source tree.
The default extension for test scripts is {.yts} (Yacas test script).

The verification commands described in this chapter only  display the
expressions that do not evaluate correctly. Errors do not terminate the
execution of the Yacas script that uses these testing commands, since they are
meant to be used in test scripts.


.. function:: Verify(question,answer)

   verifying equivalence of two expressions

.. function:: TestYacas(question,answer)

   verifying equivalence of two expressions

.. function:: LogicVerify(question,answer)

   verifying equivalence of two expressions

.. function:: LogicTest(variables,expr1,expr2)

   verifying equivalence of two expressions

   {question} -- expression to check for
   {answer} -- expected result after evaluation
   {variables} -- list of variables
   {exprN} -- Some boolean expression

   The commands {Verify}, {TestYacas}, {LogicVerify} and {LogicTest}
   can be used to verify that an expression is <I>equivalent</I> to a
   correct answer after evaluation. All three commands return :data:`True`
   or :data:`False`.

   For some calculations, the demand that two expressions are
   <I>identical</I> syntactically is too stringent. The Yacas system
   might change at various places in the future, but $ 1+x $ would
   still be equivalent, from a mathematical point of view, to $ x+1 $.

   The general problem of deciding that two expressions $ a $ and $ b
   $ are equivalent, which is the same as saying that $ a-b=0 $ , is
   generally hard to decide on. The following commands solve this
   problem by having domain-specific comparisons.

   The comparison commands do the following comparison types:

   * {Verify} -- verify for literal equality. 
     This is the fastest and simplest comparison, and can be 
     used, for example, to test that an expression evaluates to $ 2 $.
   * {TestYacas} -- compare two expressions after simplification as
     multivariate polynomials. If the two arguments are equivalent
     multivariate polynomials, this test succeeds. {TestYacas} uses
     {Simplify}. Note: {TestYacas} currently should not be used to
     test equality of lists.
   * {LogicVerify} -- Perform a test by using {CanProve} to verify
     that from {question} the expression {answer} follows. This test
     command is used for testing the logic theorem prover in Yacas.
   * {LogicTest} -- Generate a truth table for the two expressions and
     compare these two tables. They should be the same if the two
     expressions are logically the same.

   :Example:

   ::

    In> Verify(1+2,3)
    Out> True;
    In> Verify(x*(1+x),x^2+x)
    ******************
    x*(x+1) evaluates to x*(x+1) which differs
      from x^2+x
    ******************
    Out> False;
    In> TestYacas(x*(1+x),x^2+x)
    Out> True;
    In> Verify(a And c Or b And Not c,a Or b)
    ******************
     a And c Or b And Not c evaluates to  a And c
      Or b And Not c which differs from  a Or b
    ******************
    Out> False;
    In> LogicVerify(a And c Or b And Not c,a Or b)
    Out> True;
    In> LogicVerify(a And c Or b And Not c,b Or a)
    Out> True;
    In> LogicTest({A,B,C},Not((Not A) And (Not B)),A Or B)
    Out> True
    In> LogicTest({A,B,C},Not((Not A) And (Not B)),A Or C)
    ******************
    CommandLine: 1 
    
    $TrueFalse4({A,B,C},Not(Not A And Not B))
     evaluates to 
    {{{False,False},{True,True}},{{True,True},{True,True}}}
     which differs from 
    {{{False,True},{False,True}},{{True,True},{True,True}}}
    ******************
    Out> False

   .. seealso:: :func:`Simplify`, :func:`CanProve`,
                :func:`KnownFailure`


.. function:: KnownFailure(test)

   Mark a test as a known failure

   {test} -- expression that should return :data:`False` on failure

   The command {KnownFailure} marks a test as known to fail by
   displaying a message to that effect on screen.

   This might be used by developers when they have no time to fix the
   defect, but do not wish to alarm users who download Yacas and type
   {make test}.

   :Example:

   ::

      In> KnownFailure(Verify(1,2))
      Known failure:
      ******************
      1 evaluates to  1 which differs from  2
      ******************
      Out> False;
      In> KnownFailure(Verify(1,1))
      Known failure:
      Failure resolved!
      Out> True;

   .. seealso:: :func:`Verify`, :func:`TestYacas`, :func:`LogicVerify`

.. function:: RoundTo(number,precision)

   Round a real-valued result to a set number of digits

   {number} -- number to round off
   {precision} -- precision to use for round-off

   The function {RoundTo} rounds a floating point number to a
   specified precision, allowing for testing for correctness using the
   {Verify} command.

   :Example:

   ::

      In> N(RoundTo(Exp(1),30),30)
      Out> 2.71828182110230114951959786552;
      In> N(RoundTo(Exp(1),20),20)
      Out> 2.71828182796964237096;

   .. seealso:: :func:`Verify`, :func:`VerifyArithmetic`, :func:`VerifyDiv`



.. function:: VerifyArithmetic(x,n,m)

   Special purpose arithmetic verifiers

.. function:: RandVerifyArithmetic(n)

   Special purpose arithmetic verifiers

.. function:: VerifyDiv(u,v)

   Special purpose arithmetic verifiers


   {x}, {n}, {m}, {u}, {v} -- integer arguments

   The commands {VerifyArithmetic} and {VerifyDiv} test a mathematic
   equality which should hold, testing that the result returned by the
   system is mathematically correct according to a mathematically
   provable theorem.

   {VerifyArithmetic} verifies for an arbitrary set of numbers
   $ x $, $ n $ and $ m $ that
   $$ (x^n-1)*(x^m-1) = x^(n+m)-(x^n)-(x^m)+1 $$.

   The left and right side represent two ways to arrive at the
   same result, and so an arithmetic module actually doing the
   calculation does the calculation in two different ways. 
   The results should be exactly equal.

   {RandVerifyArithmetic(n)} calls {VerifyArithmetic} with
   random values, {n} times.

   {VerifyDiv(u,v)} checks that 
   $$ u = v*Div(u,v) + Mod(u,v) $$.

   :Example:

   ::

      In> VerifyArithmetic(100,50,60)
      Out> True;
      In> RandVerifyArithmetic(4)
      Out> True;
      In> VerifyDiv(x^2+2*x+3,x+1)
      Out> True;
      In> VerifyDiv(3,2)
      Out> True;

   .. seealso:: :func:`Verify`
