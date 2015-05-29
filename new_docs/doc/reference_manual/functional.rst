====================
Functional operators
====================

These operators can help the user to program in the style of
functional programming languages such as Miranda or Haskell.

.. function:: infix :(item, list)

   prepend item to list, or concatenate strings

   :param item: an item to be prepended to a list
   :param list: a list
   :param string1: a string
   :param string2: a string

   The first form prepends "item" as the first entry to the list
   "list". The second form concatenates the strings "string1" and
   "string2".

   :Example:

   ::

      In> a:b:c:{}
      Out> {a,b,c};
      In> "This":"Is":"A":"String"
      Out> "ThisIsAString";
      

   .. seealso:: :func:`Concat`, :func:`ConcatStrings`

.. function:: infix @(fn, arglist)

   apply a function

   :param fn: function to apply
   :param arglist: single argument, or a list of arguments

   This function is a shorthand for :func:`Apply`. It applies the  function
   "fn" to the argument(s) in "arglist" and returns the  result. The
   first parameter "fn" can either be a string containing  the name of
   a function or a pure function.

   :Example:

   ::

      In> "Sin" @ a
      Out> Sin(a);
      In> {{a},Sin(a)} @ a
      Out> Sin(a);
      In> "f" @ {a,b}
      Out> f(a,b);
      

   .. seealso:: :func:`Apply`

.. function:: infix /@(fn, list)

   apply a function to all entries in a list

   :param fn: function to apply
   :param list: list of arguments

   This function is a shorthand for {MapSingle}. It  successively
   applies the function "fn" to all the entries in  "list" and returns
   a list contains the results. The parameter "fn"  can either be a
   string containing the name of a function or a pure  function.

   :Example:

   ::

      In> "Sin" /@ {a,b}
      Out> {Sin(a),Sin(b)};
      In> {{a},Sin(a)*a} /@ {a,b}
      Out> {Sin(a)*a,Sin(b)*b};
      

   .. seealso:: :func:`MapSingle`, :func:`Map`, :func:`MapArgs`

.. function:: infix .. (n, m)

   construct a list of consecutive integers

   :param n: integer. the first entry in the list
   :param m: integer, the last entry in the list

   This command returns the list {{n, n+1, n+2, ..., m}}. If {m} is
   smaller than {n}, the empty list is returned. Note that the  {..}
   operator should be surrounded by spaces to keep the  parser happy,
   if "n" is a number. So one should write "{1 .. 4}" instead of
   "{1..4}".

.. function:: NFunction("newname","funcname", {arglist})

   make wrapper for numeric functions

   :param "newname": name of new function
   :param "funcname": name of an existing function
   :param arglist: symbolic list of arguments

   This function will define a function named "newname"  with the same
   arguments as an existing function named "funcname". The new
   function will evaluate and return the expression
   "funcname(arglist)" only when  all items in the argument list
   {arglist} are numbers, and return unevaluated otherwise.    This
   can be useful when plotting functions defined through other Yacas
   routines that cannot return unevaluated.    If the numerical
   calculation does not return a number (for example,  it might return
   the atom {nan}, "not a number", for some arguments),  then the new
   function will return {Undefined}.

   :Example:

   ::

      In> f(x) := N(Sin(x));
      Out> True;
      In> NFunction("f1", "f", {x});
      Out> True;
      In> f1(a);
      Out> f1(a);
      In> f1(0);
      Out> 0;
      
   Suppose we need to define a complicated function {t(x)} which cannot be evaluated unless {x} is a number::
   
      In> t(x) := If(x<=0.5, 2*x, 2*(1-x));
      Out> True;
      In> t(0.2);
      Out> 0.4;
      In> t(x);
      In function "If" :
      bad argument number 1 (counting from 1)
      CommandLine(1) : Invalid argument
      
   Then, we can use {NFunction()} to define a wrapper {t1(x)} around {t(x)} which will not try to evaluate {t(x)} unless {x} is a number::
   
      In> NFunction("t1", "t", {x})
      Out> True;
      In> t1(x);
      Out> t1(x);
      In> t1(0.2);
      Out> 0.4;
      
   Now we can plot the function.
   
      In> Plot2D(t1(x), -0.1: 1.1)
      Out> True;
      

   .. seealso:: :func:`MacroRule`

.. function:: infix Where(expr, x==v)

   substitute result into expression

   :param expr: expression to evaluate
   :param x: variable to set
   :param v: value to substitute for variable

   The operator {Where} fills in values for variables, in its simplest
   form.  It accepts sets of variable/value pairs defined as
   var1==val1 And var2==val2 And ...    and fills in the corresponding
   values. Lists of value pairs are  also possible, as:
   {var1==val1 And var2==val2, var1==val3           And var2==val4}
   These values might be obtained through {Solve}.

   :Example:

   ::

      In> x^2+y^2 Where x==2
      Out> y^2+4;
      In> x^2+y^2 Where x==2 And y==3
      Out> 13;
      In> x^2+y^2 Where {x==2 And y==3}
      Out> {13};
      In> x^2+y^2 Where {x==2 And y==3,x==4 And y==5}
      Out> {13,41};
      

   .. seealso:: :func:`Solve`, :func:`AddTo`

.. function:: infix AddTo(eq1,eq2)

   add an equation to a set of equations or set of set of equations

   :param eq: (set of) set of equations

   Given two (sets of) sets of equations, the command AddTo combines
   multiple sets of equations into one.     A list {a,b} means that a
   is a solution, OR b is a solution.  AddTo then acts as a AND
   operation:           (a or b) and (c or d) =>          (a or b)
   Addto (c or d) =>          (a and c) or (a and d) or (b and c)
   or (b and d)    This function is useful for adding an identity to
   an already  existing set of equations. Suppose a solve command
   returned  {a>=0 And x==a,a<0 And x== -a} from an expression
   x==Abs(a),  then a new identity a==2 could be added as follows:
   In> a==2 AddTo {a>=0 And x==a,a<0 And x== -a}         Out> {a==2
   And a>=0 And x==a,a==2 And a<0           And x== -a};    Passing
   this set of set of identities back to solve, solve  should
   recognize that the second one is not a possibility  any more, since
   a==2 And a<0 can never be true at the same time.

   :Example:

   ::

      In> {A==2,c==d} AddTo {b==3 And d==2}
      Out> {A==2 And b==3 And d==2,c==d
      And b==3 And d==2};
      In> {A==2,c==d} AddTo {b==3, d==2}
      Out> {A==2 And b==3,A==2 And d==2,c==d
      And b==3,c==d And d==2};
      

   .. seealso:: :func:`Where`, :func:`Solve`

