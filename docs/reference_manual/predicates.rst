==========
Predicates
==========

A predicate is a function that returns a boolean value, i.e. :data:`True` or
:data:`False`. Predicates are often used in patterns, For instance, a rule
that only holds for a positive integer would use a pattern such as
{n_IsPositiveInteger}.

.. function:: e1 != e2

   test for "not equal"

   :param e1}, {e2: expressions to be compared

   Both expressions are evaluated and compared. If they turn out to be
   equal, the result is :data:`False`. Otherwise, the result  is :data:`True`.
   The expression {e1 != e2} is equivalent to {Not(e1 = e2)}.

   :Example:

   ::

      In> 1 != 2;
      Out> True;
      In> 1 != 1;
      Out> False;
      

   .. seealso:: :func:`=`

.. function:: e1 = e2

   test for equality of expressions

   :param e1}, {e2: expressions to be compared

   Both expressions are evaluated and compared. If they turn out to be
   equal, the  result is :data:`True`. Otherwise, the result is :data:`False`. The
   function {Equals} does  the same.    Note that the test is on
   syntactic equality, not mathematical equality. Hence  even if the
   result is :data:`False`, the expressions can still be
   <i>mathematically</i> equal; see the examples below. Put otherwise,
   this  function tests whether the two expressions would be displayed
   in the same way  if they were printed.

   :Example:

   ::

      In> e1 := (x+1) * (x-1);
      Out> (x+1)*(x-1);
      In> e2 := x^2 - 1;
      Out> x^2-1;
      In> e1 = e2;
      Out> False;
      In> Expand(e1) = e2;
      Out> True;
      

   .. seealso:: :func:`!=`, :func:`Equals`

.. function:: Not expr

   logical negation

   :param expr: a boolean expression

   Not returns the logical negation of the argument expr. If {expr} is
   :data:`False` it returns :data:`True`, and if {expr} is :data:`True`, {Not expr}
   returns :data:`False`.  If the argument is neither :data:`True` nor :data:`False`, it
   returns the entire  expression with evaluated arguments.

   :Example:

   ::

      In> Not True
      Out> False;
      In> Not False
      Out> True;
      In> Not(a)
      Out> Not a;
      

   .. seealso:: :func:`And`, :func:`Or`

.. function:: a1 And a2

   logical conjunction

   :param a}1, ..., {a}: boolean values (may evaluate to :data:`True` or :data:`False`)

   This function returns :data:`True` if all arguments are true. The  {And}
   operation is "lazy", i.e. it returns :data:`False` as soon as a :data:`False`
   argument  is found (from left to right). If an argument other than
   :data:`True` or  :data:`False` is encountered a new {And} expression is
   returned with all  arguments that didn't evaluate to :data:`True` or
   :data:`False` yet.

   :Example:

   ::

      In> True And False
      Out> False;
      In> And(True,True)
      Out> True;
      In> False And a
      Out> False;
      In> True And a
      Out> And(a);
      In> And(True,a,True,b)
      Out> b And a;
      

   .. seealso:: :func:`Or`, :func:`Not`

.. function:: a1 Or a2

   logical disjunction

   :param a}1, ..., {a}: boolean expressions (may evaluate to :data:`True` or :data:`False`)

   This function returns :data:`True` if an argument is encountered  that is
   true (scanning from left to right). The  {Or} operation is "lazy",
   i.e. it returns :data:`True` as soon as a :data:`True` argument  is found (from
   left to right). If an argument other than :data:`True` or  :data:`False` is
   encountered, an unevaluated {Or} expression is returned with all
   arguments that didn't evaluate to :data:`True` or :data:`False` yet.

   :Example:

   ::

      In> True Or False
      Out> True;
      In> False Or a
      Out> Or(a);
      In> Or(False,a,b,True)
      Out> True;
      

   .. seealso:: :func:`And`, :func:`Not`

.. function:: IsFreeOf(var, expr)

   test whether expression depends on variable

   :param expr: expression to test
   :param var: variable to look for in "expr"

   This function checks whether the expression "expr" (after being
   evaluated) depends on the variable "var". It returns :data:`False` if
   this is the case and :data:`True`  otherwise.    The second form test
   whether the expression depends on <i>any</i> of  the variables
   named in the list. The result is :data:`True` if none of the variables
   appear in the expression and :data:`False` otherwise.

   :Example:

   ::

      In> IsFreeOf(x, Sin(x));
      Out> False;
      In> IsFreeOf(y, Sin(x));
      Out> True;
      In> IsFreeOf(x, D(x) a*x+b);
      Out> True;
      In> IsFreeOf({x,y}, Sin(x));
      Out> False;
      The third command returns :data:`True` because the
      expression {D(x) a*x+b} evaluates to {a}, which does not depend on {x}.
      

   .. seealso:: :func:`Contains`

.. function:: IsZeroVector(list)

   test whether list contains only zeroes

   :param list: list to compare against the zero vector

   The only argument given to {IsZeroVector} should be  a list. The
   result is :data:`True` if the list contains  only zeroes and :data:`False`
   otherwise.

   :Example:

   ::

      In> IsZeroVector({0, x, 0});
      Out> False;
      In> IsZeroVector({x-x, 1 - D(x) x});
      Out> True;
      

   .. seealso:: :func:`IsList`, :func:`ZeroVector`

.. function:: IsNonObject(expr)

   test whether argument is not an {Object()}

   :param expr: the expression to examine

   This function returns :data:`True` if "expr" is not of  the form
   {Object(...)} and :data:`False`  otherwise.

.. function:: IsEven(n)

   test for an even integer

   :param n: integer to test

   This function tests whether the integer "n" is even. An integer is
   even if it is divisible by two. Hence the even numbers are 0, 2, 4,
   6,  8, 10, etc., and -2, -4, -6, -8, -10, etc.

   :Example:

   ::

      In> IsEven(4);
      Out> True;
      In> IsEven(-1);
      Out> False;
      

   .. seealso:: :func:`IsOdd`, :func:`IsInteger`

.. function:: IsOdd(n)

   test for an odd integer

   :param n: integer to test

   This function tests whether the integer "n" is odd. An integer is
   odd if it is not divisible by two. Hence the odd numbers are 1, 3,
   5,  7, 9, etc., and -1, -3, -5, -7, -9, etc.

   :Example:

   ::

      In> IsOdd(4);
      Out> False;
      In> IsOdd(-1);
      Out> True;
      

   .. seealso:: :func:`IsEven`, :func:`IsInteger`

.. function:: IsEvenFunction(expression,variable)

   Return true if function is an even function, False otherwise

   :param expression: mathematical expression
   :param variable: variable

   These functions return True if Yacas can determine that the
   function is even or odd respectively. Even functions are  defined
   to be functions that have the property:    $$ f(x) = f(-x) $$
   And odd functions have the property:    $$ f(x) = -f(-x) $$
   {Sin(x)} is an example of an odd function, and {Cos(x)}  is an
   example of an even function.    
   
   
   .. note::

      One can decompose a
      function into an  even and an odd part $$ f(x) = f_{even}(x) +
      f_{odd}(x) $$    where     $$ f_{even}(x) = (f(x)+f(-x))/2 $$    and
      $$ f_{odd}(x) = (f(x)-f(-x))/2 $$

.. function:: IsFunction(expr)

   test for a composite object

   :param expr: expression to test

   This function tests whether "expr" is a composite object, i.e. not
   an  atom. This includes not only obvious functions such as {f(x)},
   but also expressions such as ``x+5`` and lists.

   :Example:

   ::

      In> IsFunction(x+5);
      Out> True;
      In> IsFunction(x);
      Out> False;
      

   .. seealso:: :func:`IsAtom`, :func:`IsList`, :func:`Type`

.. function:: IsAtom(expr)

   test for an atom

   :param expr: expression to test

   This function tests whether "expr" is an atom. Numbers, strings,
   and  variables are all atoms.

   :Example:

   ::

      In> IsAtom(x+5);
      Out> False;
      In> IsAtom(5);
      Out> True;
      

   .. seealso:: :func:`IsFunction`, :func:`IsNumber`, :func:`IsString`

.. function:: IsString(expr)

   test for an string

   :param expr: expression to test

   This function tests whether "expr" is a string. A string is a text
   within quotes, e.g. {"duh"}.

   :Example:

   ::

      In> IsString("duh");
      Out> True;
      In> IsString(duh);
      Out> False;
      

   .. seealso:: :func:`IsAtom`, :func:`IsNumber`

.. function:: IsNumber(expr)

   test for a number

   :param expr: expression to test

   This function tests whether "expr" is a number. There are two kinds
   of numbers, integers (e.g. 6) and reals (e.g. -2.75 or 6.0). Note
   that a  complex number is represented by the {Complex}  function,
   so {IsNumber} will return :data:`False`.

   :Example:

   ::

      In> IsNumber(6);
      Out> True;
      In> IsNumber(3.25);
      Out> True;
      In> IsNumber(I);
      Out> False;
      In> IsNumber("duh");
      Out> False;
      

   .. seealso:: :func:`IsAtom`, :func:`IsString`, :func:`IsInteger`, :func:`IsPositiveNumber`, :func:`IsNegativeNumber`, :func:`Complex`

.. function:: IsList(expr)

   test for a list

   :param expr: expression to test

   This function tests whether "expr" is a list. A list is a sequence
   between curly braces, e.g. {{2, 3, 5}}.

   :Example:

   ::

      In> IsList({2,3,5});
      Out> True;
      In> IsList(2+3+5);
      Out> False;
      

   .. seealso:: :func:`IsFunction`

.. function:: IsNumericList({list})

   test for a list of numbers

   :param {list}: a list

   Returns :data:`True` when called on a list of numbers or expressions that
   evaluate to numbers using {N()}. Returns :data:`False` otherwise.

   .. seealso:: :func:`N`, :func:`IsNumber`

.. function:: IsBound(var)

   test for a bound variable

   :param var: variable to test

   This function tests whether the variable "var" is bound, i.e.
   whether  it has been assigned a value. The argument "var" is not
   evaluated.

   :Example:

   ::

      In> IsBound(x);
      Out> False;
      In> x := 5;
      Out> 5;
      In> IsBound(x);
      Out> True;
      

   .. seealso:: :func:`IsAtom`

.. function:: IsBoolean(expression)

   test for a Boolean value

   :param expression: an expression

   IsBoolean returns True if the argument is of a boolean type.  This
   means it has to be either True, False, or an expression involving
   functions that return a boolean result, e.g.  {=}, {>}, {<}, {>=},
   {<=}, {!=}, {And}, {Not}, {Or}.

   :Example:

   ::

      In> IsBoolean(a)
      Out> False;
      In> IsBoolean(True)
      Out> True;
      In> IsBoolean(a And b)
      Out> True;
      

   .. seealso:: :func:`True`, :func:`False`

.. function:: IsNegativeNumber(n)

   test for a negative number

   :param n: number to test

   {IsNegativeNumber(n)} evaluates to :data:`True` if $n$ is (strictly)
   negative, i.e.  if $n<0$. If {n} is not a number, the functions
   return :data:`False`.

   :Example:

   ::

      In> IsNegativeNumber(6);
      Out> False;
      In> IsNegativeNumber(-2.5);
      Out> True;
      

   .. seealso:: :func:`IsNumber`, :func:`IsPositiveNumber`, :func:`IsNotZero`, :func:`IsNegativeInteger`, :func:`IsNegativeReal`

.. function:: IsNegativeInteger(n)

   test for a negative integer

   :param n: integer to test

   This function tests whether the integer {n} is (strictly)
   negative. The negative integers are -1, -2, -3, -4, -5, etc. If
   {n} is not a integer, the function returns :data:`False`.

   :Example:

   ::

      In> IsNegativeInteger(31);
      Out> False;
      In> IsNegativeInteger(-2);
      Out> True;
      

   .. seealso:: :func:`IsPositiveInteger`, :func:`IsNonZeroInteger`, :func:`IsNegativeNumber`

.. function:: IsPositiveNumber(n)

   test for a positive number

   :param n: number to test

   {IsPositiveNumber(n)} evaluates to :data:`True` if $n$ is (strictly)
   positive, i.e.  if $n>0$. If {n} is not a number the function
   returns :data:`False`.

   :Example:

   ::

      In> IsPositiveNumber(6);
      Out> True;
      In> IsPositiveNumber(-2.5);
      Out> False;
      

   .. seealso:: :func:`IsNumber`, :func:`IsNegativeNumber`, :func:`IsNotZero`, :func:`IsPositiveInteger`, :func:`IsPositiveReal`

.. function:: IsPositiveInteger(n)

   test for a positive integer

   :param n: integer to test

   This function tests whether the integer {n} is (strictly) positive.
   The  positive integers are 1, 2, 3, 4, 5, etc. If {n} is not a
   integer, the  function returns :data:`False`.

   :Example:

   ::

      In> IsPositiveInteger(31);
      Out> True;
      In> IsPositiveInteger(-2);
      Out> False;
      

   .. seealso:: :func:`IsNegativeInteger`, :func:`IsNonZeroInteger`, :func:`IsPositiveNumber`

.. function:: IsNotZero(n)

   test for a nonzero number

   :param n: number to test

   {IsNotZero(n)} evaluates to :data:`True` if {n} is not zero. In case {n}
   is not a  number, the function returns :data:`False`.

   :Example:

   ::

      In> IsNotZero(3.25);
      Out> True;
      In> IsNotZero(0);
      Out> False;
      

   .. seealso:: :func:`IsNumber`, :func:`IsPositiveNumber`, :func:`IsNegativeNumber`, :func:`IsNonZeroInteger`

.. function:: IsNonZeroInteger(n)

   test for a nonzero integer

   :param n: integer to test

   This function tests whether the integer {n} is not zero. If {n} is
   not an integer, the result is :data:`False`.

   :Example:

   ::

      In> IsNonZeroInteger(0)
      Out> False;
      In> IsNonZeroInteger(-2)
      Out> True;
      

   .. seealso:: :func:`IsPositiveInteger`, :func:`IsNegativeInteger`, :func:`IsNotZero`

.. function:: IsInfinity(expr)

   test for an infinity

   :param expr: expression to test

   This function tests whether {expr} is an infinity. This is only the
   case if {expr} is either {Infinity} or {-Infinity}.

   :Example:

   ::

      In> IsInfinity(10^1000);
      Out> False;
      In> IsInfinity(-Infinity);
      Out> True;
      

   .. seealso:: :func:`Integer`

.. function:: IsPositiveReal(expr)

   test for a numerically positive value

   :param expr: expression to test

   This function tries to approximate "expr" numerically. It returns
   :data:`True` if this approximation is positive. In case no  approximation
   can be found, the function returns :data:`False`. Note that round-off
   errors may cause incorrect  results.

   :Example:

   ::

      In> IsPositiveReal(Sin(1)-3/4);
      Out> True;
      In> IsPositiveReal(Sin(1)-6/7);
      Out> False;
      In> IsPositiveReal(Exp(x));
      Out> False;
      The last result is because {Exp(x)} cannot be
      numerically approximated if {x} is not known. Hence
      Yacas can not determine the sign of this expression.
      

   .. seealso:: :func:`IsNegativeReal`, :func:`IsPositiveNumber`, :func:`N`

.. function:: IsNegativeReal(expr)

   test for a numerically negative value

   :param expr: expression to test

   This function tries to approximate {expr} numerically. It returns
   :data:`True` if this approximation is negative. In case no  approximation
   can be found, the function returns :data:`False`. Note that round-off
   errors may cause incorrect  results.

   :Example:

   ::

      In> IsNegativeReal(Sin(1)-3/4);
      Out> False;
      In> IsNegativeReal(Sin(1)-6/7);
      Out> True;
      In> IsNegativeReal(Exp(x));
      Out> False;
      The last result is because {Exp(x)} cannot be
      numerically approximated if {x} is not known. Hence
      Yacas can not determine the sign of this expression.
      

   .. seealso:: :func:`IsPositiveReal`, :func:`IsNegativeNumber`, :func:`N`

.. function:: IsConstant(expr)

   test for a constant

   :param expr: some expression

   {IsConstant} returns :data:`True` if the  expression is some constant or
   a function with constant arguments. It  does this by checking that
   no variables are referenced in the  expression. {Pi} is considered
   a constant.

   :Example:

   ::

      In> IsConstant(Cos(x))
      Out> False;
      In> IsConstant(Cos(2))
      Out> True;
      In> IsConstant(Cos(2+x))
      Out> False;
      

   .. seealso:: :func:`IsNumber`, :func:`IsInteger`, :func:`VarList`

.. function:: IsGaussianInteger(z)

    test for a Gaussian integer

   :param z: a complex or real number

   This function returns :data:`True` if the argument is a Gaussian integer
   and :data:`False` otherwise.  A Gaussian integer is a generalization  of
   integers into the complex plane. A complex number $a+b*I$ is a
   Gaussian  integer if and only if $a$ and $b$ are integers.

   :Example:

   ::

      In> IsGaussianInteger(5)
      Out> True;
      In> IsGaussianInteger(5+6*I)
      Out> True;
      In> IsGaussianInteger(1+2.5*I)
      Out> False;
      

   .. seealso:: :func:`IsGaussianUnit`, :func:`IsGaussianPrime`

.. function:: MatchLinear(x,expr)

   match an expression to a polynomial of degree one in a variable

   :param x: variable to express the univariate polynomial in
   :param expr: expression to match

   {MatchLinear} tries to match an expression to a linear (degree less
   than  two) polynomial. The function returns :data:`True` if it could
   match, and  it stores the resulting coefficients in the variables
   "{a}" and "{b}"  as a side effect. The function calling this
   predicate should declare  local variables "{a}" and "{b}" for this
   purpose.  {MatchLinear} tries to match to constant coefficients
   which don't  depend on the variable passed in, trying to find a
   form "{a*x+b}"  with "{a}" and "{b}" not depending on {x} if {x} is
   given as the variable.

   :Example:

   ::

      In> MatchLinear(x,(R+1)*x+(T-1))
      Out> True;
      In> {a,b};
      Out> {R+1,T-1};
      In> MatchLinear(x,Sin(x)*x+(T-1))
      Out> False;
      

   .. seealso:: :func:`Integrate`

.. function:: HasExpr(expr, x)

   check for expression containing a subexpression

   :param expr: an expression
   :param x: a subexpression to be found
   :param list: list of function atoms to be considered "transparent"

   The command {HasExpr} returns :data:`True` if the expression {expr}
   contains a literal subexpression {x}. The expression is recursively
   traversed.    The command {HasExprSome} does the same, except it
   only looks at arguments of a given {list} of functions. All other
   functions become "opaque" (as if they do not contain anything).
   {HasExprArith} is defined through {HasExprSome} to look only at
   arithmetic operations {+}, {-}, {*}, {/}.    Note that since the
   operators "{+}" and "{-}" are prefix as well as infix operators, it
   is currently required to use {Atom("+")} to obtain the unevaluated
   atom "{+}".

   :Example:

   ::

      In> HasExpr(x+y*Cos(Ln(z)/z), z)
      Out> True;
      In> HasExpr(x+y*Cos(Ln(z)/z), Ln(z))
      Out> True;
      In> HasExpr(x+y*Cos(Ln(z)/z), z/Ln(z))
      Out> False;
      In> HasExprArith(x+y*Cos(Ln(x)/x), z)
      Out> False;
      In> HasExprSome({a+b*2,c/d},c/d,{List})
      Out> True;
      In> HasExprSome({a+b*2,c/d},c,{List})
      Out> False;
      

   .. seealso:: :func:`FuncList`, :func:`VarList`, :func:`HasFunc`

.. function:: HasFunc(expr, func)

   check for expression containing a function

   :param expr: an expression
   :param func: a function atom to be found
   :param list: list of function atoms to be considered "transparent"

   The command {HasFunc} returns :data:`True` if the expression {expr}
   contains a function {func}. The expression is recursively
   traversed.    The command {HasFuncSome} does the same, except it
   only looks at arguments of a given {list} of functions. Arguments
   of all other functions become "opaque" (as if they do not contain
   anything).    {HasFuncArith} is defined through {HasFuncSome} to
   look only at arithmetic operations {+}, {-}, {*}, {/}.    Note that
   since the operators "{+}" and "{-}" are prefix as well as infix
   operators, it is currently required to use {Atom("+")} to obtain
   the unevaluated atom "{+}".

   :Example:

   ::

      In> HasFunc(x+y*Cos(Ln(z)/z), Ln)
      Out> True;
      In> HasFunc(x+y*Cos(Ln(z)/z), Sin)
      Out> False;
      In> HasFuncArith(x+y*Cos(Ln(x)/x), Cos)
      Out> True;
      In> HasFuncArith(x+y*Cos(Ln(x)/x), Ln)
      Out> False;
      In> HasFuncSome({a+b*2,c/d},/,{List})
      Out> True;
      In> HasFuncSome({a+b*2,c/d},*,{List})
      Out> False;
      

   .. seealso:: :func:`FuncList`, :func:`VarList`, :func:`HasExpr`

