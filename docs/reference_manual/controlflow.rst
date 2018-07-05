======================
Control flow functions
======================

Evaluation control
------------------

.. function:: MaxEvalDepth(n)

   set the maximum evaluation depth

   Use this command to set the maximum evaluation depth to ``n``. The default
   value is 1000.

   The point of having a maximum evaluation depth is to catch any infinite
   recursion. For example, after the definition ``f(x) := f(x)``, evaluating the
   expression ``f(x)`` would call ``f(x)``, which would call ``f(x)``, etc. The
   interpreter will halt if the maximum evaluation depth is reached. Also
   indirect recursion, e.g. the pair of definitions ``f(x) := g(x)`` and ``g(x)
   := f(x)``, will be caught.

   An example of an infinite recursion, caught because the maximum
   evaluation depth is reached::

      In> f(x) := f(x)
      Out> True;
      In> f(x)

      Error on line 1 in file [CommandLine]
      Max evaluation stack depth reached.
      Please use MaxEvalDepth to increase the stack
      size as needed.

   However, a long calculation may cause the maximum evaluation depth to
   be reached without the presence of infinite recursion. The function
   :func:`MaxEvalDepth` is meant for these cases::

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


Conditional execution
---------------------

.. function:: If(pred,then,[else])

   branch point

   This command implements a branch point. The predicate ``pred`` is evaluated,
   which should result in either :const:`True` or :const:`False`. In the first
   case, the expression ``then`` is evaluated and returned. If the predicate
   yields :const:`False`, the expression ``else`` (if present) is evaluated and
   returned. If there is no ``else`` branch, the :func:`If` expression returns
   :const:`False`.

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

Loops
-----

.. function:: bodied While(expr, pred)

   loop while a condition is met

   Keep on evaluating ``expr`` while ``pred`` evaluates to :const:`True`. More
   precisely, :func:`While` evaluates the predicate ``pred``, which should
   evaluate to either :const:`True` or :const:`False`. If the result is
   :const:`True`, the expression ``expr`` is evaluated and then the predicate
   ``pred`` is evaluated again. If it is still :const:`True`, the expressions
   ``expr`` and ``pred`` are again evaluated and so on until ``pred`` evaluates
   to :const:`False`. At that point, the loop terminates and :func:`While`
   returns :const:`True`.

   In particular, if ``pred`` immediately evaluates to :const:`False`, the body
   is never executed. :func:`While` is the fundamental looping construct on
   which all other loop commands are based. It is equivalent to the ``while``
   command in the programming language C.

   :Example:

   ::

      In> x := 0;
      Out> 0;
      In> While (x! < 10^6) [ Echo({x, x!}); x++; ];
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

   Keep on evaluating ``expr`` until ``pred`` becomes :const:`True`. More
   precisely, :func:`Until` first evaluates the expression ``body``. Then the
   predicate ``pred`` is evaluated, which should yield either :const:`True` or
   :const:`False`. In the latter case, the expressions ``expr`` and ``pred`` are
   again evaluated and this continues as long as "pred" is :const:`False`. As
   soon as ``pred`` yields :const:`True`, the loop terminates and :func:`Until`
   returns :const:`True`.

   The main difference with :func:`While` is that :func:`Until` always evaluates
   ``expr`` at least once, but :func:`While` may not evaluate it at all.
   Besides, the meaning of the predicate is reversed: :func:`While` stops if
   ``pred`` is :const:`False` while :func:`Until` stops if ``pred`` is
   :const:`True`. The command ``Until(pred) expr;`` is equivalent to ``pred;
   While(Not pred) body;``. In fact, the implementation of :func:`Until` is
   based on the internal command :func:`While`. The :func:`Until` command can be
   compared to the ``do ... while`` construct in the programming language C.

   :Example:

   ::

      In> x := 0;
      Out> 0;
      In> Until (x! > 10^6) [ Echo({x, x!}); x++; ];
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

.. function:: bodied For(expr, init, pred, incr)

   C-style ``for`` loop

   This commands implements a C style ``for`` loop. First of all, the
   expression ``init`` is evaluated. Then the predicate ``pred`` is
   evaluated, which should return :const:`True` or :const:`False`. Next, the
   loop is executed as long as the predicate yields :const:`True`. One
   traversal of the loop consists of the subsequent evaluations of
   ``expr``, ``incr``, and ``pred``. Finally, :const:`True` is returned.

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

   The expression ``expr`` is evaluated multiple times. The first
   time, ``var`` has the value of the first element of "list", then it
   gets the value of the second element and so on. :func:`ForEach`
   returns :const:`True`.

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


.. function:: bodied Function(func(args))
              bodied Function(body, funcname, {args})

   declare or define a function

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



.. function:: Apply(fn, arglist)

   apply a function to arguments

   This function applies the function ``fn`` to the arguments in ``arglist`` and
   returns the result. The first parameter ``fn`` can either be a string
   containing the name of a function  or a pure function. Pure functions,
   modeled after lambda-expressions, have the form ``{varlist,body}``, where
   ``varlist`` is the list of formal parameters. Upon application, the formal
   parameters are assigned the values in ``arglist`` (the second parameter of
   :func:`Apply`) and the ``body`` is evaluated.

   Another way to define a pure function is with the Lambda construct. Here,
   instead of passing in ``{varlist,body}``, one can pass in
   ``Lambda(varlist,body)``. Lambda has the advantage that its arguments are not
   evaluated (using lists can have undesirable effects because lists are
   evaluated). Lambda can be used everywhere a pure function is expected, in
   principle, because the function :func:`Apply` is the only function dealing
   with pure functions. So all places where a pure function can be passed in
   will also accept Lambda.

   An shorthand for :func:`Apply` is provided by the :func:`@` operator.

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
              WithValue(varlist, vallist, expr)

   temporary assignment during an evaluation

   First, the expression ``val`` is assigned to the variable ``var``. Then, the
   expression ``expr`` is evaluated and returned. Finally, the assignment is
   reversed so that the variable ``var`` has the same value as it had before
   :func:`WithValue` was evaluated.

   The second calling sequence assigns the first element in the list of values
   to the first element in the list of variables, the second value to the second
   variable, etc.

   :Example:

   ::

      In> WithValue(x, 3, x^2+y^2+1);
      Out> y^2+10;
      In> WithValue({x,y}, {3,2}, x^2+y^2+1);
      Out> 14;

   .. seealso:: :func:`Subst`, :func:`/:`

.. function:: infix /:(expression,patterns)

   local simplification rules

   Sometimes you have an expression, and you want to use specific simplification
   rules on it that are not done by default. This can be done with the ``/:``
   and the ``/::`` operators. Suppose we have the expression containing things
   such as ``Ln(a*b)``, and we want to change these into ``Ln(a)+Ln(b)``, the
   easiest way to do this is using the ``/:`` operator, as follows::

     In> Sin(x)*Ln(a*b)
     Out> Sin(x)*Ln(a*b);
     In> % /: { Ln(_x*_y) <- Ln(x)+Ln(y) }
     Out> Sin(x)*(Ln(a)+Ln(b));

   A whole list of simplification rules can be built up in the list,
   and they will be applied to the expression on the left hand side of
   ``/:``.

   The forms the patterns can have are one of::

           pattern <- replacement
           {pattern,replacement}
           {pattern,postpredicate,replacement}

   Note that for these local rules, ``<-`` should be used instead of
   ``<--`` which would be used in a global rule.

   The ``/:`` operator traverses an expression much as :func:`Subst` does, that
   is, top down, trying to apply the rules from the beginning of the list of
   rules to the end of the list of rules. If the rules cannot be applied to an
   expression, it will try subexpressions of that expression and so on.

   It might be necessary sometimes to use the ``/::`` operator, which repeatedly
   applies the ``/:`` operator until the result doesn't change any more. Caution
   is required, since rules can contradict each other, which could result in an
   infinite loop. To detect this situation, just use ``/:`` repeatedly on the
   expression. The repetitive nature should become apparent.

   :Example:

   ::

      In> Sin(u)*Ln(a*b) /: {Ln(_x*_y) <- Ln(x)+Ln(y)}
      Out> Sin(u)*(Ln(a)+Ln(b));
      In> Sin(u)*Ln(a*b) /:: { a <- 2, b <- 3 }
      Out> Sin(u)*Ln(6);


   .. seealso:: :func:`Subst`



