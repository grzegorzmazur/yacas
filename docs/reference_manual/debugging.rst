=========
Debugging
=========

.. function:: TraceStack(expression)

   show calling stack after an error occurs

   :func:`TraceStack` shows the calling stack after an error occurred. It shows
   the last few items on the stack, not to flood the screen. These are usually
   the only items of interest on the stack. This is probably by far the most
   useful debugging function in yacas. It shows the last few things it did just
   after an error was generated somewhere.

   For each stack frame, it shows if the function evaluated was a built-in
   function or a user-defined function, and for the user-defined function, the
   number of the rule it is trying whether it was evaluating the pattern matcher
   of the rule, or the body code of the rule.

   This functionality is not offered by default because it slows down the
   evaluation code.

   Here is an example of a function calling itself recursively, causing yacas to
   flood its stack::

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

