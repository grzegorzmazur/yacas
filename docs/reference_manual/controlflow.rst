======================
Control flow functions
======================


.. function:: MaxEvalDepth(n)

   set the maximum evaluation depth

   :param n: new maximum evaluation depth

   Use this command to set the maximum evaluation depth to ``n``. The default
   value is 1000.

   The point of having a maximum evaluation depth is to catch any
   infinite recursion. For example, after the definition ``f(x) := f(x)``,
   evaluating the expression ``f(x)`` would call ``f(x)``, which
   would call ``f(x)``, etc. The interpreter will halt if the maximum
   evaluation depth is reached. Also indirect recursion, e.g. the pair
   of definitions ``f(x) := g(x)`` and ``g(x) := f(x)``, will be caught.

   An example of an infinite recursion, caught because the maximum
   evaluation depth is reached ::

      In> f(x) := f(x)
      Out> True;
      In> f(x)

      Error on line 1 in file [CommandLine]
      Max evaluation stack depth reached.
      Please use MaxEvalDepth to increase the stack
      size as needed.

   However, a long calculation may cause the maximum evaluation depth to
   be reached without the presence of infinite recursion. The function 
   :func:`MaxEvalDepth` is meant for these cases ::

      In> 10 # g(0) <-- 1;
      Out> True;
      In> 20 # g(n_IsPositiveInteger) <-- \
      2 * g(n-1);
      Out> True;
      In> g(1001);
      Error on line 1 in file [CommandLine]
      Max evaluation stack depth reached.
      Please use MaxEvalDepth to increase the stack
      size as needed.
      In> MaxEvalDepth(10000);
      Out> True;
      In> g(1001);
      Out> 21430172143725346418968500981200036211228096234
      1106721488750077674070210224987224498639675763139171
      6255189345835106293650374290571384628087196915514939
      7149607869135549648461970842149210124742283755908364
      3060929499671638825347975351183310878921541258291423
      92955373084335320859663305248773674411336138752;

.. function:: Hold(expr)

   keep expression unevaluated

   :param expr: expression to keep unevaluated

   The expression ``expr`` is returned unevaluated. This is useful to
   prevent the evaluation of a certain expression in a context in
   which evaluation normally takes place. 

   :Example:

   ::

      In> Echo({ Hold(1+1), "=", 1+1 });
      1+1 = 2
      Out> True;

   .. seealso:: :func:`Eval`, :func:`HoldArg`, :func:`UnList`


.. function:: Eval(expr)

   force evaluation of expression

   :param expr: expression to evaluate

   This function explicitly requests an evaluation of the expression
   ``expr``, and returns the result of this evaluation.

   :Example:

   ::

      In> a := x;
      Out> x;
      In> x := 5;
      Out> 5;
      In> a;
      Out> x;
      In> Eval(a);
      Out> 5;

   The variable ``a`` is bound to ``x``, and ``x`` is bound
   to 5. Hence evaluating ``a`` will give ``x``. Only when an extra
   evaluation of ``a`` is requested, the value 5 is returned.  Note
   that the behavior would be different if we had exchanged the
   assignments. If the assignment ``a := x`` were given while ``x``
   had the value 5, the variable ``a`` would also get the value 5
   because the assignment operator :func:`:=` evaluates the right-hand
   side.

   .. seealso:: :func:`Hold`, :func:`HoldArg`, :func:`:=`


.. function:: bodied While(expr, pred)

   loop while a condition is met

   :param pred: predicate deciding whether to keep on looping
   :param expr: expression to loop over

   Keep on evaluating ``expr`` while ``pred`` evaluates to
   ``True``. More precisely, :func:`While` evaluates the predicate
   ``pred``, which should evaluate to either ``True`` or ``False``. If
   the result is ``True``, the expression ``expr`` is evaluated and
   then the predicate ``pred`` is evaluated again. If it is still
   ``True``, the expressions ``expr`` and ``pred`` are again evaluated
   and so on until ``pred`` evaluates to ``False``. At that point, the
   loop terminates and :func:`While` returns ``True``.

   In particular, if ``pred`` immediately evaluates to ``False``, the
   body is never executed. :func:`While` is the fundamental looping
   construct on which all other loop commands are based. It is
   equivalent to the ``while`` command in the programming language C.

   :Example:

   ::

      In> x := 0;
      Out> 0;
      In> While (x! < 10^6) \
      [ Echo({x, x!}); x++; ];
      0  1
      1  1
      2  2
      3  6
      4  24
      5  120
      6  720
      7  5040
      8  40320
      9  362880
      Out> True;
      

   .. seealso:: :func:`Until`, :func:`For`


.. function:: bodied Until(expr, pred)

   loop until a condition is met

   :param pred: predicate deciding whether to stop
   :param expr: expression to loop over

   Keep on evaluating ``expr`` until ``pred`` becomes ``True``. More
   precisely, :func:`Until` first evaluates the expression
   ``body``. Then the predicate ``pred`` is evaluated, which should yield
   either ``True`` or ``False``. In the latter case, the expressions
   ``expr`` and ``pred`` are again evaluated and this continues as
   long as "pred" is ``False``. As soon as ``pred`` yields ``True``,
   the loop terminates and :func:`Until` returns ``True``.

   The main difference with :func:`While` is that :func:`Until` always
   evaluates ``expr`` at least once, but :func:`While` may not
   evaluate it at all. Besides, the meaning of the predicate is
   reversed: :func:`While` stops if ``pred`` is ``False`` while
   :func:`Until` stops if ``pred`` is ``True``. The command
   ``Until(pred) expr;`` is equivalent to ``pred; While(Not pred)
   body;``. In fact, the implementation of :func:`Until` is based on
   the internal command :func:`While`. The :func:`Until` command can
   be compared to the ``do ... while`` construct in the programming
   language C.

   :Example:

   ::

      In> x := 0;
      Out> 0;
      In> Until (x! > 10^6) \
      [ Echo({x, x!}); x++; ];
      0  1
      1  1
      2  2
      3  6
      4  24
      5  120
      6  720
      7  5040
      8  40320
      9  362880
      Out> True;
      

   .. seealso:: :func:`While`, :func:`For`


.. function:: If(pred,then,[else])

   branch point

   :param pred: predicate to test
   :param then: expression to evaluate if ``pred`` is ``True``
   :param else: expression to evaluate if ``pred`` is ``False``

   This command implements a branch point. The predicate ``pred`` is
   evaluated, which should result in either ``True`` or ``False``. In
   the first case, the expression ``then`` is evaluated and
   returned. If the predicate yields ``False``, the expression ``else``
   (if present) is evaluated and returned. If there is no ``else``
   branch, the :func:`If` expression returns ``False``.

   The sign function is defined to be 1 if its argument is positive and
   -1 if its argument is negative. A possible implementation is::

      In> mysign(x) := If (IsPositiveReal(x), 1, -1);
      Out> True;
      In> mysign(Pi);
      Out> 1;
      In> mysign(-2.5);
      Out> -1;

   Note that this will give incorrect results, if ``x`` cannot be
   numerically approximated::

      In> mysign(a);
      Out> -1;

   Hence a better implementation would be::

      In> mysign(_x)_IsNumber(N(x)) <-- If(IsPositiveReal(x), 1, -1);
      Out> True;


.. function:: SystemCall(str)

   pass a command to the shell

   :param str: the command to call

   The command contained in the string ``str`` is executed by the
   underlying operating system. The return value of :func:`SystemCall`
   is ``True`` or ``False`` according to the exit code of the command.

   The :func:`SystemCall` function is not allowed in the body of the
   :func:`Secure` command.

   In a UNIX environment, the command ``SystemCall("ls")`` would print
   the contents of the current directory::

      In> SystemCall("ls")
      AUTHORS
      COPYING
      ChangeLog
      ... (truncated to save space)
      Out> True;

   The standard UNIX command ``test`` returns success or failure
   depending on conditions.  For example, the following command will
   check if a directory exists::

      In> SystemCall("test -d scripts/")
      Out> True;

   Check that a file exists::

      In> SystemCall("test -f COPYING")
      Out> True;
      In> SystemCall("test -f nosuchfile.txt")
      Out> False;

   .. seealso:: :func:`Secure`


.. function:: bodied Function(func(args))
              bodied Function(body, funcname, {args})

   declare or define a function

   :param func(args): function declaration, e.g. ``f(x,y)``
   :param args: list of atoms, formal arguments to the function
   :param body: expression comprising the body of the function

   This command can be used to define a new function with named
   arguments.

   The number of arguments of the new function and their names are
   determined by the list ``args``. If the ellipsis ``...`` follows
   the last atom in ``args``, a function with a variable number of
   arguments is declared (using :func:`RuleBaseListed`). Note that the
   ellipsis cannot be the only element of ``args`` and *must* be
   preceded by an atom.

   A function with variable number of arguments can take more
   arguments than elements in ``args``; in this case, it obtains its
   last argument as a list containing all extra arguments.

   The short form of the :func:`Function` call merely declares a 
   :func:`RuleBase` for the new function but does not define any 
   function body. This is a convenient shorthand for :func:`RuleBase`
   and :func:`RuleBaseListed`, when definitions of the function are to
   be supplied by rules. If the new function has been already declared
   with the same number of arguments (with or without variable arguments),
   :func:`Function` returns false and does nothing.

   The second, longer form of the :func:`Function` call declares a function
   and also defines a function body. It is equivalent to a single rule
   such as ``funcname(_arg1, _arg2) <-- body``. The rule will be declared at
   precedence 1025. Any previous rules associated with ``funcname`` (with
   the same arity) will be discarded. More complicated functions (with
   more than one body) can be defined by adding more rules.

   :Example:

   This will declare a new function with two or more arguments, but
   define no rules for it. This is equivalent to ``RuleBase ("f1", {x,
   y, ...})``::

      In> Function() f1(x,y,...);
      Out> True;
      In> Function() f1(x,y);
      Out> False;

   This defines a function ``FirstOf`` which returns the first element
   of a list. Equivalent definitions would be ``FirstOf(_list) <--
   list[1]`` or ``FirstOf(list) := list[1]``::

      In> Function("FirstOf", {list})  list[1];
      Out> True;
      In> FirstOf({a,b,c});
      Out> a;

   The following function will print all arguments to a string::

      In> Function("PrintAll",{x, ...}) If(IsList(x), PrintList(x), ToString()Write(x));
      Out> True;
      In> PrintAll(1):
      Out> " 1";
      In> PrintAll(1,2,3);
      Out> " 1 2 3";

   .. seealso:: :func:`TemplateFunction`, :func:`Rule`,
                :func:`RuleBase`, :func:`RuleBaseListed`, :func:`:=`,
                :func:`Retract`


.. function:: bodied Macro(func(args))
              bodied Macro(body, funcname, {args})

   declare or define a macro

   :param func(args): function declaration, e.g. ``f(x,y)``
   :param args: list of atoms, formal arguments to the function
   :param body: expression comprising the body of the function

   This does the same as :func:`Function`, but for macros. One can
   define a macro easily with this function, instead of having to use
   :func:`DefMacroRuleBase`.

   :Example:

   The following example defines a looping function ::

      In> Macro("myfor",{init,pred,inc,body}) [@init;While(@pred)[@body;@inc;];True;];
      Out> True;
      In> a:=10
      Out> 10;

   Here this new macro ``myfor`` is used to loop, using a variable ``a``
   from the calling environment ::

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
      

   .. seealso:: :func:`Function`, :func:`DefMacroRuleBase`

.. function:: bodied For(expr, init, pred, incr)

   C-style ``for`` loop

   :param init: expression for performing the initialization
   :param pred: predicate deciding whether to continue the loop
   :param incr: expression to increment the counter
   :param expr: expression to loop over

   This commands implements a C style ``for`` loop. First of all, the
   expression ``init`` is evaluated. Then the predicate ``pred`` is
   evaluated, which should return ``True`` or ``False``. Next, the
   loop is executed as long as the predicate yields ``True``. One
   traversal of the loop consists of the subsequent evaluations of
   ``expr``, ``incr``, and ``pred``. Finally, ``True`` is returned.

   This command is most often used in a form such as ``For(i=1, i<=10,
   i++) expr``, which evaluates ``expr`` with ``i`` subsequently set
   to 1, 2, 3, 4, 5, 6, 7, 8, 9, and 10.

   The expression ``For(init, pred, incr) expr`` is equivalent to
   ``init; While(pred) [expr; incr;]``.

   :Example:

   ::

      In> For (i:=1, i<=10, i++) Echo({i, i!});
      1  1
      2  2
      3  6
      4  24
      5  120
      6  720
      7  5040
      8  40320
      9  362880
      10  3628800
      Out> True;
      

   .. seealso:: :func:`While`, :func:`Until`, :func:`ForEach`


.. function:: bodied ForEach(expr, var, list)

   loop over all entries in list

   :param var: looping variable
   :param list: list of values to assign to ``var``
   :param expr: expression to evaluate with different values of ``var``

   The expression ``expr`` is evaluated multiple times. The first
   time, ``var`` has the value of the first element of "list", then it
   gets the value of the second element and so on. :func:`ForEach`
   returns ``True``.

   :Example:

   ::

      In> ForEach(i,{2,3,5,7,11}) Echo({i, i!});
      2  2
      3  6
      5  120
      7  5040
      11  39916800
      Out> True;
      

   .. seealso:: :func:`For`


.. function:: Apply(fn, arglist)

   apply a function to arguments

   :param fn: function to apply
   :param arglist: list of arguments

   This function applies the function "fn" to the arguments in
   "arglist" and returns the result. The first parameter "fn" can
   either be a string containing the name of a function  or a pure
   function. Pure functions, modeled after lambda-expressions, have
   the form "{varlist,body}", where "varlist" is the list of formal
   parameters. Upon application, the formal parameters are assigned
   the values in "arglist" (the second parameter of {Apply}) and the
   "body" is evaluated.

   Another way to define a pure function is with the Lambda construct.
   Here, in stead of passing in "{varlist,body}", one can pass in
   "Lambda(varlist,body)". Lambda has the advantage that its arguments
   are not evaluated (using lists can have undesirable effects because
   lists are evaluated). Lambda can be used everywhere a pure function
   is expected, in principle, because the function Apply is the only
   function dealing with pure functions. So all places where a pure
   function can be passed in will also accept Lambda.

   An shorthand for {Apply} is provided by the {@} operator.

   :Example:

   ::

      In> Apply("+", {5,9});
      Out> 14;
      In> Apply({{x,y}, x-y^2}, {Cos(a), Sin(a)});
      Out> Cos(a)-Sin(a)^2;
      In>  Apply(Lambda({x,y}, x-y^2), {Cos(a), Sin(a)});
      Out> Cos(a)-Sin(a)^2
      In>  Lambda({x,y}, x-y^2) @ {Cos(a), Sin(a)}
      Out> Cos(a)-Sin(a)^2
      

   .. seealso:: :func:`Map`, :func:`MapSingle`, :func:`@`


.. function:: MapArgs(expr, fn)

   apply a function to all top-level arguments

   :param expr: an expression to work on
   :param fn: an operation to perform on each argument

   Every top-level argument in ``expr`` is substituted by the result
   of applying ``fn`` to this argument. Here ``fn`` can be either the
   name of a function or a pure function (see :func:`Apply` for more
   information on pure functions).

   :Example:

   ::

      In> MapArgs(f(x,y,z),"Sin");
      Out> f(Sin(x),Sin(y),Sin(z));
      In> MapArgs({3,4,5,6}, {{x},x^2});
      Out> {9,16,25,36};
      

   .. seealso:: :func:`MapSingle`, :func:`Map`, :func:`Apply`


.. function:: bodied Subst(expr, from, to)

   perform a substitution

   :param from: expression to be substituted
   :param to: expression to substitute for "from"
   :param expr: expression in which the substitution takes place

   This function substitutes every occurrence of ``from`` in ``expr``
   by ``to``. This is a syntactical substitution: only places where
   ``from`` occurs as a subexpression are affected.

   :Example:

   ::

      In> Subst(x, Sin(y)) x^2+x+1;
      Out> Sin(y)^2+Sin(y)+1;
      In> Subst(a+b, x) a+b+c;
      Out> x+c;
      In> Subst(b+c, x) a+b+c;
      Out> a+b+c;

   The explanation for the last result is that the expression
   ``a+b+c`` is internally stored as ``(a+b)+c``. Hence ``a+b`` is a
   subexpression, but ``b+c`` is not.
      

   .. seealso:: :func:`WithValue`, :func:`/:`


.. function:: WithValue(var, val, expr)

   temporary assignment during an evaluation

   :param var: variable to assign to
   :param val: value to be assigned to "var"
   :param expr: expression to evaluate with "var" equal to "val"

   First, the expression "val" is assigned to the variable "var".
   Then, the expression "expr" is evaluated and returned. Finally, the
   assignment is reversed so that the variable "var" has the same
   value as it had before {WithValue} was evaluated.

   The second calling sequence assigns the first element in the list
   of values to the first element in the list of variables, the second
   value to the second variable, etc.

   :Example:

   ::

      In> WithValue(x, 3, x^2+y^2+1);
      Out> y^2+10;
      In> WithValue({x,y}, {3,2}, x^2+y^2+1);
      Out> 14;
      

   .. seealso:: :func:`Subst`, :func:`/:`

.. function:: infix /:(expression,patterns)

   local simplification rules

   :param expression: an expression
   :param patterns: a list of patterns

   Sometimes you have an expression, and you want to use specific
   simplification rules on it that are not done by default. This can
   be done with the {/:} and the {/::} operators. Suppose we have the
   expression containing things such as {Ln(a*b)}, and we want to
   change these into {Ln(a)+Ln(b)}, the easiest way to do this is
   using the {/:} operator, as follows: ::

     In> Sin(x)*Ln(a*b)
     Out> Sin(x)*Ln(a*b);
     In> % /: { Ln(_x*_y) <- Ln(x)+Ln(y) }
     Out> Sin(x)*(Ln(a)+Ln(b));

   A whole list of simplification rules can be built up in the list,
   and they will be applied to the expression on the left hand side of
   {/:} .

   The forms the patterns can have are one of: ::
           pattern <- replacement         {pattern,replacement}
           {pattern,postpredicate,replacement}

   Note that for these local rules, {<-} should be used instead of
   {<--} which would be used in a global rule.

   The {/:} operator traverses an expression much as {Subst} does,
   that is, top down, trying to apply the rules from the beginning of
   the list of rules to the end of the list of rules. If the rules
   cannot be applied to an expression, it will try subexpressions of
   that expression and so on.

   It might be necessary sometimes to use the {/::} operator, which
   repeatedly applies the {/:} operator until the result doesn't
   change any more. Caution is required, since rules can contradict
   each other, which could result in an infinite loop. To detect this
   situation, just use /: repeatedly on the expression. The repetitive
   nature should become apparent.

   :Example:

   ::

      In> Sin(u)*Ln(a*b) /: {Ln(_x*_y) <- Ln(x)+Ln(y)}
      Out> Sin(u)*(Ln(a)+Ln(b));
      In> Sin(u)*Ln(a*b) /:: { a <- 2, b <- 3 }
      Out> Sin(u)*Ln(6);
      

   .. seealso:: :func:`Subst`


.. function:: TraceStack(expression)

   show calling stack after an error occurs

   :param expression: an expression to evaluate

   TraceStack shows the calling stack after an error occurred. It
   shows the last few items on the stack, not to flood the screen.
   These are usually the only items of interest on the stack. This is
   probably by far the most useful debugging function in Yacas. It
   shows the last few things it did just after an error was generated
   somewhere.

   For each stack frame, it shows if the function evaluated was a
   built-in function or a user-defined function, and for the
   user-defined function, the number of the rule it is trying whether
   it was evaluating the pattern matcher of the rule, or the body code
   of the rule.

   This functionality is not offered by default because it slows down
   the evaluation code.

   :Example:

   ::

      Here is an example of a function calling itself recursively,
      causing Yacas to flood its stack:
      In> f(x):=f(Sin(x))
      Out> True;
      In> TraceStack(f(2))
      Debug> 982 :  f (Rule # 0 in body)
      Debug> 983 :  f (Rule # 0 in body)
      Debug> 984 :  f (Rule # 0 in body)
      Debug> 985 :  f (Rule # 0 in body)
      Debug> 986 :  f (Rule # 0 in body)
      Debug> 987 :  f (Rule # 0 in body)
      Debug> 988 :  f (Rule # 0 in body)
      Debug> 989 :  f (Rule # 0 in body)
      Debug> 990 :  f (Rule # 0 in body)
      Debug> 991 :  f (Rule # 0 in body)
      Debug> 992 :  f (Rule # 0 in body)
      Debug> 993 :  f (Rule # 0 in body)
      Debug> 994 :  f (Rule # 0 in body)
      Debug> 995 :  f (User function)
      Debug> 996 :  Sin (Rule # 0 in pattern)
      Debug> 997 :  IsList (Internal function)
      Error on line 1 in file [CommandLine]
      Max evaluation stack depth reached.
      Please use MaxEvalDepth to increase the stack
      size as needed.
      

   .. seealso:: :func:`TraceExp`, :func:`TraceRule`


.. function:: TraceExp(expr)

   evaluate with tracing enabled

   :param expr: expression to trace

   The expression "expr" is evaluated with the tracing facility turned
   on. This means that every subexpression, which is evaluated, is
   shown before and after evaluation. Before evaluation, it is shown
   in the form {TrEnter(x)}, where {x} denotes the subexpression being
   evaluated. After the evaluation the line {TrLeave(x,y)} is printed,
   where {y} is the result of the evaluation. The indentation shows
   the nesting level.

   Note that this command usually generates huge amounts of output. A
   more specific form of tracing (eg. {TraceRule}) is probably more
   useful  for all but very simple expressions.

   :Example:

   ::

      In> TraceExp(2+3);
      TrEnter(2+3);
      TrEnter(2);
      TrLeave(2, 2);
      TrEnter(3);
      TrLeave(3, 3);
      TrEnter(IsNumber(x));
      TrEnter(x);
      TrLeave(x, 2);
      TrLeave(IsNumber(x),True);
      TrEnter(IsNumber(y));
      TrEnter(y);
      TrLeave(y, 3);
      TrLeave(IsNumber(y),True);
      TrEnter(True);
      TrLeave(True, True);
      TrEnter(MathAdd(x,y));
      TrEnter(x);
      TrLeave(x, 2);
      TrEnter(y);
      TrLeave(y, 3);
      TrLeave(MathAdd(x,y),5);
      TrLeave(2+3, 5);
      Out> 5;
      

   .. seealso:: :func:`TraceStack`, :func:`TraceRule`


.. function:: bodied TraceRule(expr, template)

   turn on tracing for a particular function

   :param template: template showing the operator to trace
   :param expr: expression to evaluate with tracing on

   The tracing facility is turned on for subexpressions of the form
   "template", and the expression "expr" is evaluated. The template
   "template" is an example of the function to trace on. Specifically,
   all subexpressions with the same top-level operator and arity as
   "template" are shown. The subexpressions are displayed before
   (indicated with {TrEnter}) and after ({TrLeave}) evaluation. In
   between, the arguments are shown before and after evaluation
   ({TrArg}). Only functions defined in scripts can be traced.

   This is useful for tracing a function that is called from within
   another function. This way you can see how your function behaves in
   the environment it is used in.

   :Example:

   ::

      In> TraceRule(x+y) 2+3*5+4;
      TrEnter(2+3*5+4);
      TrEnter(2+3*5);
      TrArg(2, 2);
      TrArg(3*5, 15);
      TrLeave(2+3*5, 17);
      TrArg(2+3*5, 17);
      TrArg(4, 4);
      TrLeave(2+3*5+4, 21);
      Out> 21;
      

   .. seealso:: :func:`TraceStack`, :func:`TraceExp`


.. function:: Time(expr)

   measure the time taken by a function

   :param expr: any expression

   The function {Time(expr)} evaluates the expression {expr} and
   prints the time in seconds needed for the evaluation. The time is
   printed to the current output stream. The built-in function
   {GetTime} is used for timing.

   The result is the "user time" as reported by the OS, not the real
   ("wall clock") time. Therefore, any CPU-intensive processes running
   alongside Yacas will not significantly affect the result of {Time}.

   :Example:

   ::

      In> Time(N(MathLog(1000),40))
      0.34 seconds taken
      Out> 6.9077552789821370520539743640530926228033;
      

   .. seealso:: :func:`GetTime`

