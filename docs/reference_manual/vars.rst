=========
Variables
=========

.. function:: infix :=(var,expr)
              infix :=(var[i],expr)
              infix :=(varlist,exprlist)
              infix :=(fn,expr)

   assign a variable or a list; define a function

   var := expr
   {var1, var2, ...} := {expr1, expr2, ...}
   var[i] := expr
   fn(arg1, arg2, ...) := expr

   :param var: atom, variable which should be assigned
   :param expr: expression to assign to the variable or body of function
   :param i: index (can be integer or string)
   :param fn: atom, name of a new function to define
   :param arg1, arg2: atoms, names of arguments of the new function {fn}

   The :func:`:=` operator can be used in a number of ways. In all
   cases, some sort of assignment or definition takes place.  The
   first form is the most basic one. It evaluates the expression on
   the right-hand side and assigns it to the variable named on the
   left-hand side. The left-hand side is not evaluated. The evaluated
   expression is also returned.  The second form is a small extension,
   which allows one to do multiple assignments. The first entry in the
   list on the right-hand side is assigned to the first variable
   mentioned in the left-hand side, the second entry on the right-hand
   side to the second variable on the left-hand side, etc.  The list
   on the right-hand side must have at least as many entries as the
   list on the left-hand side. Any excess entries are silently
   ignored. The result of the expression is the list of values that
   have been assigned.  The third form allows one to change an entry
   in the list. If the index "i" is an integer, the "i"-th entry in
   the list is changed to the expression on the right-hand side. It is
   assumed that the length of the list is at least "i". If the index
   "i" is a string, then "var" is considered to be an associative list
   (sometimes called hash table), and the key "i" is paired with the
   value "exp". In both cases, the right-hand side is evaluated before
   the assignment and the result of the assignment is :data:`True`.  The
   last form defines a function. For example, the assignment {fn(x) :=
   x^2} removes any rules previously associated with {fn(x)} and
   defines the rule {fn(_x) <-- x^2}. Note that the left-hand side may
   take a different form if {fn} is defined to be a prefix, infix or
   bodied function. This case is special since the right-hand side is
   not evaluated immediately, but only when the function {fn} is
   used. If this takes time, it may be better to force an immediate
   evaluation with {Eval} (see the last example).  If the expression
   on the right hand side begins with {Eval()}, then it <i>will</i> be
   evaluated before defining the new function. A variant of the
   function definition can be used to make a function accepting a
   variable number of arguments. The last argument

   Simple assignment::

      In> a := Sin(x) + 3;
      Out> Sin(x)+3;
      In> a;
      Out> Sin(x)+3;

   Multiple assignments::

      In> {a,b,c} := {1,2,3};
      Out> {1,2,3};
      In> a;
      Out> 1;
      In> b+c;
      Out> 5;

   Assignment to a list::

      In> xs := { 1,2,3,4,5 };
      Out> {1,2,3,4,5};
      In> xs[3] := 15;
      Out> True;
      In> xs;
      Out> {1,2,15,4,5};

   Building an associative list::

      In> alist := {};
      Out> {};
      In> alist["cherry"] := "red";
      Out> True;
      In> alist["banana"] := "yellow";
      Out> True;
      In> alist["cherry"];
      Out> "red";
      In> alist;
      Out> {{"banana","yellow"},{"cherry","red"}};

   Defining a function::

      In> f(x) := x^2;
      Out> True;
      In> f(3);
      Out> 9;
      In> f(Sin(a));
      Out> Sin(a)^2;

   Defining a function with variable number of arguments::

      In> f(x, ...) := If(IsList(x),Sum(x),x);
      Out> True;
      In> f(2);
      Out> 2;
      In> f(1,2,3);
      Out> 6;

   Defining a new infix operator::

      In> Infix("*&*",10);
      Out> True;
      In> x1 *&* x2 := x1/x2 + x2/x1;
      Out> True;
      In> Sin(a) *&* Cos(a);
      Out> Tan(1)+Cos(1)/Sin(1);
      In> Clear(a);
      Out> True;
      In> Sin(a) *&* Exp(a);
      Out> Sin(a)/Exp(a)+Exp(a)/Sin(a);

   In the following example, it may take some time to compute the Taylor
   expansion. This has to be done every time the function {f} is called::

      In> f(a) := Taylor(x,0,25) Sin(x);
      Out> True;
      In> f(1);
      Out> x-x^3/6+x^5/120-x^7/5040+x^9/362880-
      x^11/39916800+x^13/6227020800-x^15/
      1307674368000+x^17/355687428096000-x^19/
      121645100408832000+x^21/51090942171709440000
      -x^23/25852016738884976640000+x^25
      /15511210043330985984000000;
      In> f(2);
      Out> x-x^3/6+x^5/120-x^7/5040+x^9/362880-
      x^11/39916800+x^13/6227020800-x^15
      /1307674368000+x^17/355687428096000-x^19/
      121645100408832000+x^21/51090942171709440000
      -x^23/25852016738884976640000+x^25/
      15511210043330985984000000;

   The remedy is to evaluate the Taylor expansion immediately. Now the
   expansion is computed only once::

      In> f(a) := Eval(Taylor(x,0,25) Sin(x));
      Out> True;
      In> f(1);
      Out> x-x^3/6+x^5/120-x^7/5040+x^9/362880-
      x^11/39916800+x^13/6227020800-x^15/
      1307674368000+x^17/355687428096000-x^19/
      121645100408832000+x^21/51090942171709440000
      -x^23/25852016738884976640000+x^25
      /15511210043330985984000000;
      In> f(2);
      Out> x-x^3/6+x^5/120-x^7/5040+x^9/362880-
      x^11/39916800+x^13/6227020800-x^15
      /1307674368000+x^17/355687428096000-x^19/
      121645100408832000+x^21/51090942171709440000
      -x^23/25852016738884976640000+x^25/
      15511210043330985984000000;
      

   .. seealso:: :func:`Set`, :func:`Clear`, :func:`[]`, :func:`Rule`, :func:`Infix`, :func:`Eval`, :func:`Function`

.. function:: Set(var, exp)

   assignment

   :param var: variable which should be assigned
   :param exp: expression to assign to the variable

   The expression "exp" is evaluated and assigned it to the variable
   named "var". The first argument is not evaluated. The value True
   is returned.    The statement {Set(var, exp)} is equivalent to {var
   := exp}, but the {:=} operator  has more uses, e.g. changing
   individual entries in a list.

   :Example:

   ::

      In> Set(a, Sin(x)+3);
      Out> True;
      In> a;
      Out> Sin(x)+3;
      

   .. seealso:: :func:`Clear`, :func:`:=`

.. function:: Clear(var, ...)

   undo an assignment

   :param var: name of the variable to be cleared

   All assignments made to the variables listed as arguments are
   undone. From now on, all these variables remain unevaluated (until
   a  subsequent assignment is made). The result of the expression is
   True.

   :Example:

   ::

      In> a := 5;
      Out> 5;
      In> a^2;
      Out> 25;
      In> Clear(a);
      Out> True;
      In> a^2;
      Out> a^2;
      

   .. seealso:: :func:`Set`, :func:`:=`

.. function:: Local(var, ...)

   declare new local variables

   :param var: name of the variable to be declared as local

   All variables in the argument list are declared as local
   variables. The arguments are not evaluated. The value ``True`` is
   returned.  By default, all variables in Yacas are global. This
   means that the variable has the same value everywhere. But
   sometimes it is useful to have a private copy of some variable,
   either to prevent the outside world from changing it or to prevent
   accidental changes to the outside world. This can be achieved by
   declaring the variable local. Now only expressions within the
   :func:`Prog` block (or its syntactic equivalent, the ``[]``
   block) can access and change it. Functions called within this block
   cannot access the local copy unless this is specifically allowed
   with :func:`UnFence`.

   :Example:

   ::

      In> a := 3;
      Out> 3;
      In> [ a := 4; a; ];
      Out> 4;
      In> a;
      Out> 4;
      In> [ Local(a); a := 5; a; ];
      Out> 5;
      In> a;
      Out> 4;

   In the first block, ``a`` is not declared local and hence defaults
   to be a global variable. Indeed, changing the variable inside the
   block also changes the value of ``a`` outside the block. However,
   in the second block ``a`` is defined to be local and now the value
   outside the block stays the same, even though ``a`` is assigned the
   value 5 inside the block.
      

   .. seealso:: :func:`LocalSymbols`, :func:`Prog`, :func:`[]`, :func:`UnFence`

.. function:: postfix ++(var)

   increment variable

   :param var: variable to increment

   The variable with name ``var`` is incremented, i.e. the number 1 is
   added to it. The expression ``x++`` is equivalent to the assignment
   ``x := x + 1``, except that the assignment returns the new value of
   ``x`` while ``x++`` always returns ``True``. In this respect,
   Yacas' ``++`` differs from the corresponding operator in the
   programming language C.

   :Example:

   ::

      In> x := 5;
      Out> 5;
      In> x++;
      Out> True;
      In> x;
      Out> 6;
      

   .. seealso:: :func:`--`, :func:`:=`

.. function:: postfix --(var)

   decrement variable

   :param var: variable to decrement

   The variable with name ``var`` is decremented, i.e. the number 1 is
   subtracted from it. The expression ``x--`` is equivalent to the
   assignment ``x := x - 1``, except that the assignment returns the
   new value of ``x`` while ``x--`` always returns ``True``. In this
   respect, Yacas' ``--`` differs from the corresponding operator in
   the programming language C.

   :Example:

   ::

      In> x := 5;
      Out> 5;
      In> x--;
      Out> True;
      In> x;
      Out> 4;
      

   .. seealso:: :func:`++`, :func:`:=`

.. function:: Object("pred", expr)

   create an incomplete type

   :param pred: name of the predicate to apply
   :param expr: expression on which ``pred`` should be applied

   This function returns "obj" as soon as "pred" returns :data:`True` when
   applied on "obj". This is used to declare  so-called incomplete
   types.

   :Example:

   ::

      In> a := Object("IsNumber", x);
      Out> Object("IsNumber",x);
      In> Eval(a);
      Out> Object("IsNumber",x);
      In> x := 5;
      Out> 5;
      In> Eval(a);
      Out> 5;
      

   .. seealso:: :func:`IsNonObject`

.. function:: SetGlobalLazyVariable(var,value)

   global variable is to be evaluated lazily

   :param var: variable (held argument)
   :param value: value to be set to (evaluated before it is assigned)

   :func:`SetGlobalLazyVariable` enforces that a global variable will
   re-evaluate when used. This functionality doesn't survive if
   ``Clear(var)`` is called afterwards.  Places where this is used
   include the global variables ``%`` and ``I``.  The use of lazy in
   the name stems from the concept of lazy evaluation.  The object the
   global variable is bound to will only be evaluated when called.
   The {SetGlobalLazyVariable} property only holds once: after that,
   the result of evaluation is stored in the global variable, and it
   won't be reevaluated again::

     In> SetGlobalLazyVariable(a,Hold(Taylor(x,0,30)Sin(x)))
     Out> True

   Then the first time you call ``a`` it evaluates ``Taylor(...)`` and
   assigns the result to ``a``. The next time you call ``a`` it
   immediately returns the result.  :func:`SetGlobalLazyVariable` is
   called for ``%`` each time ``%`` changes.  The following example
   demonstrates the sequence of execution::

     In> SetGlobalLazyVariable(test,Hold(Write("hello")))
     Out> True
   
   The text "hello" is not written out to screen yet. However,
   evaluating the variable ``test`` forces the expression to be
   evaluated::

     In> test = "hello"
     Out> True

   :Example:

   ::

      In> Set(a,Hold(2+3))
      Out> True
      In> a
      Out> 2+3
      In> SetGlobalLazyVariable(a,Hold(2+3))
      Out> True
      In> a
      Out> 5
      

   .. seealso:: :func:`Set`, :func:`Clear`, :func:`Local`, :func:`%`, :func:`I`

.. function:: UniqueConstant()

   create a unique identifier


   This function returns a unique constant atom each time you call
   it. The atom starts with a C character, and a unique number is
   appended to it.

   :Example:

   ::

      In> UniqueConstant()
      Out> C9
      In>  UniqueConstant()
      Out> C10
      

   .. seealso:: :func:`LocalSymbols`

.. function:: bodied LocalSymbols(expr, var1, var2, ...)

   create unique local symbols with given prefix

   :param var1, var2, ..: atoms, symbols to be made local
   :param expr: expression to execute

   Given the symbols passed as the first arguments to
   :func:`LocalSymbols`, a set of unique local symbols will be
   created, typically of the form ``$<symbol><number>``, where
   ``symbol`` was the symbol entered by the user, and ``number`` is a
   unique number. This scheme is used to ensure that a generated
   symbol can not accidentally be entered by a user.  This is useful
   in cases where a guaranteed free variable is needed, for example,
   in the macro-like functions (:func:`For`, :func:`While` etc.).

   :Example:

   ::

      In> LocalSymbols(a,b)a+b
      Out> $a6+ $b6;
      

   .. seealso:: :func:`UniqueConstant`

