=================================
Calculus and elementary functions
=================================

In this chapter, some facilities for doing calculus are
described. These include functions implementing differentiation,
integration, standard mathematical functions, and solving of
equations.

.. function:: Sin(x)

   trigonometric sine function

   :param x: argument to the function, in radians

   This function represents the trigonometric function sine. Yacas
   leaves   expressions alone even if x is a number, trying to keep
   the result as   exact as possible. The floating point
   approximations of these functions   can be forced by using the {N}
   function.    Yacas knows some trigonometric identities, so it can
   simplify to exact  results even if {N} is not used. This is the
   case, for instance,  when the argument is a multiple of $Pi$/6 or
   $Pi$/4.
   
   :func:`Sin` is :term:`threaded <threaded function>`.

   :Example:

   ::

      In> Sin(1)
      Out> Sin(1);
      In> N(Sin(1),20)
      Out> 0.84147098480789650665;
      In> Sin(Pi/4)
      Out> Sqrt(2)/2;
      

   .. seealso:: :func:`Cos`, :func:`Tan`, :func:`ArcSin`, :func:`ArcCos`, :func:`ArcTan`, :func:`N`, :func:`Pi`

.. function:: Cos(x)

   trigonometric cosine function

   :param x: argument to the function, in radians

   This function represents the trigonometric function cosine. Yacas
   leaves   expressions alone even if x is a number, trying to keep
   the result as   exact as possible. The floating point
   approximations of these functions   can be forced by using the {N}
   function.    Yacas knows some trigonometric identities, so it can
   simplify to exact  results even if {N} is not used. This is the
   case, for instance,  when the argument is a multiple of $Pi$/6 or
   $Pi$/4.    These functions are threaded, meaning that if the
   argument {x} is a  list, the function is applied to all entries in
   the list.

    :func:`Cos` is :term:`threaded <threaded function>`.

   :Example:

   ::

      In> Cos(1)
      Out> Cos(1);
      In> N(Cos(1),20)
      Out> 0.5403023058681397174;
      In> Cos(Pi/4)
      Out> Sqrt(1/2);
      

   .. seealso:: :func:`Sin`, :func:`Tan`, :func:`ArcSin`, :func:`ArcCos`, :func:`ArcTan`, :func:`N`, :func:`Pi`

.. function:: Tan(x)

   trigonometric tangent function

   :param x: argument to the function, in radians

   This function represents the trigonometric function tangent. Yacas
   leaves   expressions alone even if x is a number, trying to keep
   the result as   exact as possible. The floating point
   approximations of these functions   can be forced by using the {N}
   function.    Yacas knows some trigonometric identities, so it can
   simplify to exact  results even if {N} is not used. This is the
   case, for instance,  when the argument is a multiple of $Pi$/6 or
   $Pi$/4.    These functions are threaded, meaning that if the
   argument {x} is a  list, the function is applied to all entries in
   the list.

   :Example:

   ::

      In> Tan(1)
      Out> Tan(1);
      In> N(Tan(1),20)
      Out> 1.5574077246549022305;
      In> Tan(Pi/4)
      Out> 1;
      

   .. seealso:: :func:`Sin`, :func:`Cos`, :func:`ArcSin`, :func:`ArcCos`, :func:`ArcTan`, :func:`N`, :func:`Pi`

.. function:: ArcSin(x)

   inverse trigonometric function arc-sine

   :param x: argument to the function

   This function represents the inverse trigonometric function
   arcsine. For  instance, the value of $ArcSin(x)$ is a number $y$
   such that  $Sin(y)$ equals $x$.    Note that the number $y$ is not
   unique. For instance, $Sin(0)$ and  $Sin(Pi)$ both equal 0, so what
   should $ArcSin(0)$ be? In Yacas,  it is agreed that the value of
   $ArcSin(x)$ should be in the interval  [-$Pi$/2,$Pi$/2].
   Usually, Yacas leaves this function alone unless it is forced to do
   a numerical evaluation by the {N} function. If the  argument is -1,
   0, or 1 however, Yacas will simplify the  expression. If the
   argument is complex,  the expression will be  rewritten using the
   {Ln} function.    This function is threaded, meaning that if the
   argument {x} is a  list, the function is applied to all entries in
   the list.

   :Example:

   ::

      In> ArcSin(1)
      Out> Pi/2;
      In> ArcSin(1/3)
      Out> ArcSin(1/3);
      In> Sin(ArcSin(1/3))
      Out> 1/3;
      In> x:=N(ArcSin(0.75))
      Out> 0.848062;
      In> N(Sin(x))
      Out> 0.7499999477;
      

   .. seealso:: :func:`Sin`, :func:`Cos`, :func:`Tan`, :func:`N`, :func:`Pi`, :func:`Ln`, :func:`ArcCos`, :func:`ArcTan`

.. function:: ArcCos(x)

   inverse trigonometric function arc-cosine

   :param x: argument to the function

   This function represents the inverse trigonometric function
   arc-cosine. For  instance, the value of $ArcCos(x)$ is a number $y$
   such that  $Cos(y)$ equals $x$.    Note that the number $y$ is not
   unique. For instance, $Cos(Pi/2)$ and  $Cos(3*Pi/2)$ both equal 0,
   so what should $ArcCos(0)$ be? In Yacas,  it is agreed that the
   value of $ArcCos(x)$ should be in the interval [0,$Pi$] .
   Usually, Yacas leaves this function alone unless it is forced to do
   a numerical evaluation by the {N} function. If the  argument is -1,
   0, or 1 however, Yacas will simplify the  expression. If the
   argument is complex,  the expression will be  rewritten using the
   {Ln} function.    This function is threaded, meaning that if the
   argument {x} is a  list, the function is applied to all entries in
   the list.

   :Example:

   ::

      In> ArcCos(0)
      Out> Pi/2
      In> ArcCos(1/3)
      Out> ArcCos(1/3)
      In> Cos(ArcCos(1/3))
      Out> 1/3
      In> x:=N(ArcCos(0.75))
      Out> 0.7227342478
      In> N(Cos(x))
      Out> 0.75
      

   .. seealso:: :func:`Sin`, :func:`Cos`, :func:`Tan`, :func:`N`, :func:`Pi`, :func:`Ln`, :func:`ArcSin`, :func:`ArcTan`

.. function:: ArcTan(x)

   inverse trigonometric function arc-tangent

   :param x: argument to the function

   This function represents the inverse trigonometric function
   arctangent. For  instance, the value of $ArcTan(x)$ is a number $y$
   such that  $Tan(y)$ equals $x$.    Note that the number $y$ is not
   unique. For instance, $Tan(0)$ and  $Tan(2*Pi)$ both equal 0, so
   what should $ArcTan(0)$ be? In Yacas,  it is agreed that the value
   of $ArcTan(x)$ should be in the interval  [-$Pi$/2,$Pi$/2].
   Usually, Yacas leaves this function alone unless it is forced to do
   a numerical evaluation by the {N} function. Yacas will try to
   simplify  as much as possible while keeping the result exact. If
   the argument is   complex,  the expression will be rewritten using
   the {Ln} function.    This function is threaded, meaning that if
   the argument {x} is a  list, the function is applied to all entries
   in the list.

   :Example:

   ::

      In> ArcTan(1)
      Out> Pi/4
      In> ArcTan(1/3)
      Out> ArcTan(1/3)
      In> Tan(ArcTan(1/3))
      Out> 1/3
      In> x:=N(ArcTan(0.75))
      Out> 0.643501108793285592213351264945231378078460693359375
      In> N(Tan(x))
      Out> 0.75
      

   .. seealso:: :func:`Sin`, :func:`Cos`, :func:`Tan`, :func:`N`, :func:`Pi`, :func:`Ln`, :func:`ArcSin`, :func:`ArcCos`

.. function:: Exp(x)

   exponential function

   :param x: argument to the function

   This function calculates :math:`e^x` where :math:`e` is the
   mathematic constant 2.71828... One can use ``Exp(1)`` to represent
   :math:`e`.  This function is :term:`threaded function`, meaning
   that if the argument ``x`` is a list, the function is applied to
   all entries in the list.

   :Example:

   ::

      In> Exp(0)
      Out> 1;
      In> Exp(I*Pi)
      Out> -1;
      In> N(Exp(1))
      Out> 2.7182818284;
      

   .. seealso:: :func:`Ln`, :func:`Sin`, :func:`Cos`, :func:`Tan`, :func:`N`

.. function:: Ln(x)

   natural logarithm

   :param x: argument to the function

   This function calculates the natural logarithm of "x". This is the
   inverse function of the exponential function, {Exp}, i.e. $Ln(x) =
   y$ implies that $Exp(y) = x$. For complex  arguments, the imaginary
   part of the logarithm is in the interval  (-$Pi$,$Pi$]. This is
   compatible with the branch cut of {Arg}.    This function is
   threaded, meaning that if the argument {x} is a  list, the function
   is applied to all entries in the list.

   :Example:

   ::

      In> Ln(1)
      Out> 0;
      In> Ln(Exp(x))
      Out> x;
      In> D(x) Ln(x)
      Out> 1/x;
      

   .. seealso:: :func:`Exp`, :func:`Arg`

.. function:: Sqrt(x)

   square root

   :param x: argument to the function

   This function calculates the square root of "x". If the result is
   not rational, the call is returned unevaluated unless a numerical
   approximation is forced with the {N} function. This  function can
   also handle negative and complex arguments.    This function is
   threaded, meaning that if the argument {x} is a  list, the function
   is applied to all entries in the list.

   :Example:

   ::

      In> Sqrt(16)
      Out> 4;
      In> Sqrt(15)
      Out> Sqrt(15);
      In> N(Sqrt(15))
      Out> 3.8729833462;
      In> Sqrt(4/9)
      Out> 2/3;
      In> Sqrt(-1)
      Out> Complex(0,1);
      

   .. seealso:: :func:`Exp`, :func:`^`, :func:`N`

.. function:: Abs(x)

   absolute value or modulus of complex number

   :param x: argument to the function

   This function returns the absolute value (also called the modulus)
   of "x". If "x" is positive, the absolute value is "x" itself; if
   "x" is negative, the absolute value is "-x". For complex "x", the
   modulus is the "r" in the polar decomposition :math:`x =
   re^{\imath\phi}`.  This function is connected to the {Sign}
   function by the identity ``Abs(x) * Sign(x) = x`` for real "x".
   This function is threaded, meaning that if the argument {x} is a
   list, the function is applied to all entries in the list.

   :Example:

   ::

      In> Abs(2);
      Out> 2;
      In> Abs(-1/2);
      Out> 1/2;
      In> Abs(3+4*I);
      Out> 5;
      

   .. seealso:: :func:`Sign`, :func:`Arg`

.. function:: Sign(x)

   sign of a number

   :param x: argument to the function

   This function returns the sign of the real number $x$. It is "1"
   for positive numbers and "-1" for negative numbers. Somewhat
   arbitrarily, {Sign(0)} is defined to be 1.    This function is
   connected to the {Abs} function by  the identity $Abs(x) * Sign(x)
   = x$ for real $x$.    This function is threaded, meaning that if
   the argument {x} is a  list, the function is applied to all entries
   in the list.

   :Example:

   ::

      In> Sign(2)
      Out> 1;
      In> Sign(-3)
      Out> -1;
      In> Sign(0)
      Out> 1;
      In> Sign(-3) * Abs(-3)
      Out> -3;
      

   .. seealso:: :func:`Arg`, :func:`Abs`


.. function:: bodied D(expression, variable[,n=1])

   derivative

   :param variable: variable
   :param expression: expression to take derivatives of
   :param n: order

   :returns: ``n``-th derivative of ``expression`` with respect to ``variable``

.. function:: bodied D(expression, variable)

   derivative

   :param variable: variable
   :param list: a list of variables
   :param expression: expression to take derivatives of
   :param n: order of derivative

   :returns: derivative of ``expression`` with respect to ``variable``

   This function calculates the derivative of the expression {expr}
   with  respect to the variable {var} and returns it. If the third
   calling  format is used, the {n}-th derivative is determined. Yacas
   knows  how to differentiate standard functions such as {Ln}  and
   {Sin}.    The {D} operator is threaded in both {var} and  {expr}.
   This means that if either of them is a list, the function is
   applied to each entry in the list. The results are collected in
   another list which is returned. If both {var} and {expr} are a
   list, their lengths should be equal. In this case, the first entry
   in  the list {expr} is differentiated with respect to the first
   entry in  the list {var}, the second entry in {expr} is
   differentiated with  respect to the second entry in {var}, and so
   on.    The {D} operator returns the original function if $n=0$, a
   common  mathematical idiom that simplifies many formulae.

   :Example:

   ::

      In> D(x)Sin(x*y)
      Out> y*Cos(x*y);
      In> D({x,y,z})Sin(x*y)
      Out> {y*Cos(x*y),x*Cos(x*y),0};
      In> D(x,2)Sin(x*y)
      Out> -Sin(x*y)*y^2;
      In> D(x){Sin(x),Cos(x)}
      Out> {Cos(x),-Sin(x)};
      

   .. seealso:: :func:`Integrate`, :func:`Taylor`, :func:`Diverge`, :func:`Curl`

.. function:: Curl(vector, basis)

   curl of a vector field

   :param vector: vector field to take the curl of
   :param basis: list of variables forming the basis

   This function takes the curl of the vector field "vector" with
   respect to the variables "basis". The curl is defined in the usual
   way,           Curl(f,x) = {             D(x[2]) f[3] - D(x[3])
   f[2],             D(x[3]) f[1] - D(x[1]) f[3],             D(x[1])
   f[2] - D(x[2]) f[1]         }  Both "vector" and "basis" should be
   lists of length 3.

.. function:: Diverge(vector, basis)

   divergence of a vector field

   :param vector: vector field to calculate the divergence of
   :param basis: list of variables forming the basis

   This function calculates the divergence of the vector field
   "vector"  with respect to the variables "basis". The divergence is
   defined as           Diverge(f,x) = D(x[1]) f[1] + ...
   + D(x[n]) f[n],  where {n} is the length of the lists "vector" and
   "basis". These lists should have equal length.

.. function:: bodied Integrate(expr, var)
              bodied Integrate(expr, var, x1, x2)

   integral

   :param expr: expression to integrate
   :param var: atom, variable to integrate over
   :param x1: first point of definite integration
   :param x2: second point of definite integration

   This function integrates the expression {expr} with respect to the
   variable {var}. In the case of definite integral, the integration
   is carried out from $var=x1$ to $var=x2$". Some simple integration
   rules have currently been implemented.  Polynomials, some quotients
   of polynomials, trigonometric functions and their inverses,
   hyperbolic functions and their inverses, {Exp}, and {Ln}, and
   products of these functions with polynomials can be integrated.

   :Example:

   ::

      In> Integrate(x,a,b) Cos(x)
      Out> Sin(b)-Sin(a);
      In> Integrate(x) Cos(x)
      Out> Sin(x);
      

   .. seealso:: :func:`D`, :func:`UniqueConstant`

.. function:: bodied Limit(expr, var, val)

   limit of an expression

   :param var: variable
   :param val: number or ``Infinity``
   :param dir: direction (``Left`` or ``Right``)
   :param expr: an expression

   This command tries to determine the value that the expression
   "expr"  converges to when the variable "var" approaches "val". One
   may use  {Infinity} or {-Infinity} for  "val". The result of
   {Limit} may be one of the  symbols {Undefined} (meaning that the
   limit does not  exist), {Infinity}, or {-Infinity}.    The second
   calling sequence is used for unidirectional limits. If one  gives
   "dir" the value {Left}, the limit is taken as  "var" approaches
   "val" from the positive infinity; and {Right} will take the limit
   from the negative infinity.

   :Example:

   ::

      In> Limit(x,0) Sin(x)/x
      Out> 1;
      In> Limit(x,0) (Sin(x)-Tan(x))/(x^3)
      Out> -1/2;
      In> Limit(x,0) 1/x
      Out> Undefined;
      In> Limit(x,0,Left) 1/x
      Out> -Infinity;
      In> Limit(x,0,Right) 1/x
      Out> Infinity;
      Random numbers
      

.. function:: Random()

   (pseudo-) random number generator

   :param init: integer, initial seed value
   :param option: atom, option name
   :param value: atom, option value
   :param r: a list, RNG object

   These commands are an object-oriented interface to (pseudo-)random
   number generators (RNGs).    {RngCreate} returns a list which is a
   well-formed RNG object.  Its value should be saved in a variable
   and used to call {Rng} and {RngSeed}.    {Rng(r)} returns a
   floating-point random number between 0 and 1 and updates the RNG
   object {r}.  (Currently, the Gaussian option makes a RNG return a
   <i>complex</i> random number instead of a real random number.)
   {RngSeed(r,init)} re-initializes the RNG object {r} with the seed
   value {init}.  The seed value should be a positive integer.    The
   {RngCreate} function accepts several options as arguments.
   Currently the following options are available:

.. function:: RandomIntegerMatrix(rows,cols,from,to)

   generate a matrix of random integers

   :param rows: number of rows in matrix
   :param cols: number of cols in matrix
   :param from: lower bound
   :param to: upper bound

   This function generates a {rows x cols} matrix of random integers.
   All  entries lie between "from" and "to", including the boundaries,
   and  are uniformly distributed in this interval.

   :Example:

   ::

      In> PrettyForm( RandomIntegerMatrix(5,5,-2^10,2^10) )
      /                                               \
      | ( -506 ) ( 749 )  ( -574 ) ( -674 ) ( -106 )  |
      |                                               |
      | ( 301 )  ( 151 )  ( -326 ) ( -56 )  ( -277 )  |
      |                                               |
      | ( 777 )  ( -761 ) ( -161 ) ( -918 ) ( -417 )  |
      |                                               |
      | ( -518 ) ( 127 )  ( 136 )  ( 797 )  ( -406 )  |
      |                                               |
      | ( 679 )  ( 854 )  ( -78 )  ( 503 )  ( 772 )   |
      \                                               /
      

   .. seealso:: :func:`RandomIntegerVector`, :func:`RandomPoly`

.. function:: RandomIntegerVector(nr, from, to)

   generate a vector of random integers

   :param nr: number of integers to generate
   :param from: lower bound
   :param to: upper bound

   This function generates a list with "nr" random integers. All
   entries lie between "from" and "to", including the boundaries, and
   are uniformly distributed in this interval.

   :Example:

   ::

      In> RandomIntegerVector(4,-3,3)
      Out> {0,3,2,-2};
      

   .. seealso:: :func:`Random`, :func:`RandomPoly`

.. function:: RandomPoly(var,deg,coefmin,coefmax)

   construct a random polynomial

   :param var: free variable for resulting univariate polynomial
   :param deg: degree of resulting univariate polynomial
   :param coefmin: minimum value for coefficients
   :param coefmax: maximum value for coefficients

   RandomPoly generates a random polynomial in variable "var", of
   degree "deg", with integer coefficients ranging from "coefmin" to
   "coefmax" (inclusive). The coefficients are uniformly distributed
   in  this interval, and are independent of each other.

   :Example:

   ::

      In> RandomPoly(x,3,-10,10)
      Out> 3*x^3+10*x^2-4*x-6;
      In> RandomPoly(x,3,-10,10)
      Out> -2*x^3-8*x^2+8;
      

   .. seealso:: :func:`Random`, :func:`RandomIntegerVector`

.. function:: Add(val1, val2, ...)

   find sum of a list of values

   :param val1}, {val2: expressions
   :param {list}: list of expressions to add

   This function adds all its arguments and returns their sum. It
   accepts any  number of arguments. The arguments can be also passed
   as a list.

   :Example:

   ::

      In> Add(1,4,9);
      Out> 14;
      In> Add(1 .. 10);
      Out> 55;
      

.. function:: Sum(var, from, to, body)

   find sum of a sequence

   :param var: variable to iterate over
   :param from: integer value to iterate from
   :param to: integer value to iterate up to
   :param body: expression to evaluate for each iteration

   The command finds the sum of the sequence generated by an iterative
   formula.   The expression "body" is  evaluated while the variable
   "var" ranges over all integers from  "from" up to "to", and the sum
   of all the results is  returned. Obviously, "to" should be greater
   than or equal to  "from".    Warning: {Sum} does not evaluate its
   arguments {var} and {body} until the actual loop is run.

   :Example:

   ::

      In> Sum(i, 1, 3, i^2);
      Out> 14;
      

   .. seealso:: :func:`Factorize`

.. function:: Factorize(list)

   product of a list of values

   :param list: list of values to multiply
   :param var: variable to iterate over
   :param from: integer value to iterate from
   :param to: integer value to iterate up to
   :param body: expression to evaluate for each iteration

   The first form of the {Factorize} command simply  multiplies all
   the entries in "list" and returns their product.    If the second
   calling sequence is used, the expression "body" is  evaluated while
   the variable "var" ranges over all integers from  "from" up to
   "to", and the product of all the results is  returned. Obviously,
   "to" should be greater than or equal to  "from".

   :Example:

   ::

      In> Factorize({1,2,3,4});
      Out> 24;
      In> Factorize(i, 1, 4, i);
      Out> 24;
      

   .. seealso:: :func:`Sum`, :func:`Apply`

.. function:: Taylor(var, at, order) expr

   univariate Taylor series expansion

   :param var: variable
   :param at: point to get Taylor series around
   :param order: order of approximation
   :param expr: expression to get Taylor series for

   This function returns the Taylor series expansion of the expression
   "expr" with respect to the variable "var" around "at" up to order
   "order". This is a polynomial which agrees with "expr" at the
   point "var = at", and furthermore the first "order" derivatives of
   the polynomial at this point agree with "expr". Taylor expansions
   around removable singularities are correctly handled by taking the
   limit as "var" approaches "at".

   :Example:

   ::

      In> PrettyForm(Taylor(x,0,9) Sin(x))
      3    5      7       9
      x    x      x       x
      x - -- + --- - ---- + ------
      6    120   5040   362880
      Out> True;
      

   .. seealso:: :func:`D`, :func:`InverseTaylor`, :func:`ReversePoly`, :func:`BigOh`

.. function:: InverseTaylor(var, at, order) expr

   Taylor expansion of inverse

   :param var: variable
   :param at: point to get inverse Taylor series around
   :param order: order of approximation
   :param expr: expression to get inverse Taylor series for

   This function builds the Taylor series expansion of the inverse of
   the  expression "expr" with respect to the variable "var" around
   "at"  up to order "order". It uses the function {ReversePoly} to
   perform the task.

   :Example:

   ::

      In> PrettyPrinter'Set("PrettyForm")
      True
      In> exp1 := Taylor(x,0,7) Sin(x)
      3    5      7
      x    x      x
      x - -- + --- - ----
      6    120   5040
      In> exp2 := InverseTaylor(x,0,7) ArcSin(x)
      5      7     3
      x      x     x
      --- - ---- - -- + x
      120   5040   6
      In> Simplify(exp1-exp2)
      0
      

   .. seealso:: :func:`ReversePoly`, :func:`Taylor`, :func:`BigOh`

.. function:: ReversePoly(f, g, var, newvar, degree)

   solve $h(f(x)) = g(x) + O(x^n)$ for $h$

   :param f: function of ``var``
   :param g: function of ``var``
   :param var: a variable
   :param newvar: a new variable to express the result in
   :param degree: the degree of the required solution

   This function returns a polynomial in "newvar", say "h(newvar)",
   with the property that "h(f(var))" equals "g(var)" up to order
   "degree". The degree of the result will be at most "degree-1". The
   only requirement is that the first derivative of "f" should not be
   zero.    This function is used to determine the Taylor series
   expansion of the  inverse of a function "f": if we take
   "g(var)=var", then  "h(f(var))=var" (up to order "degree"), so "h"
   will be the  inverse of "f".

   :Example:

   ::

      In> f(x):=Eval(Expand((1+x)^4))
      Out> True;
      In> g(x) := x^2
      Out> True;
      In> h(y):=Eval(ReversePoly(f(x),g(x),x,y,8))
      Out> True;
      In> BigOh(h(f(x)),x,8)
      Out> x^2;
      In> h(x)
      Out> (-2695*(x-1)^7)/131072+(791*(x-1)^6)/32768 +(-119*(x-1)^5)/4096+(37*(x-1)^4)/1024+(-3*(x-1)^3)/64+(x-1)^2/16;
      

   .. seealso:: :func:`InverseTaylor`, :func:`Taylor`, :func:`BigOh`

.. function:: BigOh(poly, var, degree)

   drop all terms of a certain order in a polynomial

   :param poly: a univariate polynomial
   :param var: a free variable
   :param degree: positive integer

   This function drops all terms of order "degree" or higher in
   "poly", which is a polynomial in the variable "var".

   :Example:

   ::

      In> BigOh(1+x+x^2+x^3,x,2)
      Out> x+1;
      

   .. seealso:: :func:`Taylor`, :func:`InverseTaylor`

.. function:: LagrangeInterpolant(xlist, ylist, var)

   polynomial interpolation

   :param xlist: list of argument values
   :param ylist: list of function values
   :param var: free variable for resulting polynomial

   This function returns a polynomial in the variable "var" which
   interpolates the points "(xlist, ylist)". Specifically, the value
   of  the resulting polynomial at "xlist[1]" is "ylist[1]", the value
   at  "xlist[2]" is "ylist[2]", etc. The degree of the polynomial is
   not  greater than the length of "xlist".    The lists "xlist" and
   "ylist" should be of equal  length. Furthermore, the entries of
   "xlist" should be all distinct  to ensure that there is one and
   only one solution.    This routine uses the Lagrange interpolant
   formula to build up the  polynomial.

   :Example:

   ::

      In> f := LagrangeInterpolant({0,1,2}, \
      {0,1,1}, x);
      Out> (x*(x-1))/2-x*(x-2);
      In> Eval(Subst(x,0) f);
      Out> 0;
      In> Eval(Subst(x,1) f);
      Out> 1;
      In> Eval(Subst(x,2) f);
      Out> 1;
      In> PrettyPrinter'Set("PrettyForm");
      True
      In> LagrangeInterpolant({x1,x2,x3}, {y1,y2,y3}, x)
      y1 * ( x - x2 ) * ( x - x3 )
      ----------------------------
      ( x1 - x2 ) * ( x1 - x3 )
      y2 * ( x - x1 ) * ( x - x3 )
      + ----------------------------
      ( x2 - x1 ) * ( x2 - x3 )
      y3 * ( x - x1 ) * ( x - x2 )
      + ----------------------------
      ( x3 - x1 ) * ( x3 - x2 )
      

   .. seealso:: :func:`Subst`

.. function:: postfix !(n)

   factorial

   :param m: integer
   :param n: integer, half-integer, or list
   :param a}, {b: numbers

   The factorial function {n!} calculates the factorial of integer or
   half-integer numbers. For  nonnegative integers, $n! :=
   n*(n-1)*(n-2)*...*1$. The factorial of  half-integers is defined
   via Euler's Gamma function, $z! := Gamma(z+1)$. If $n=0$ the
   function returns $1$.    The "double factorial" function {n!!}
   calculates $n*(n-2)*(n-4)*...$. This product terminates either with
   $1$ or with $2$ depending on whether $n$ is odd or even. If $n=0$
   the function returns $1$.    The "partial factorial" function {a
   *** b} calculates the product $a*(a+1)*...$ which is terminated at
   the least integer not greater than $b$. The arguments $a$ and $b$
   do not have to be integers; for integer arguments, {a *** b} = $b!
   / (a-1)!$. This function is sometimes a lot faster than evaluating
   the two factorials, especially if $a$ and $b$ are close together.
   If $a>b$ the function returns $1$.    The {Subfactorial} function
   can be interpreted as the  number of permutations of {m} objects in
   which no object   appears in its natural place, also called
   "derangements."     The factorial functions are threaded, meaning
   that if the argument {n} is a  list, the function will be applied
   to each element of the list.    Note: For reasons of Yacas syntax,
   the factorial sign {!} cannot precede other  non-letter symbols
   such as {+} or {*}. Therefore, you should enter a space  after {!}
   in expressions such as {x! +1}.    The factorial functions
   terminate and print an error message if the arguments are too large
   (currently the limit is $n < 65535$) because exact factorials of
   such large numbers are computationally expensive and most probably
   not useful. One can call {Internal'LnGammaNum()} to evaluate
   logarithms of such factorials to desired precision.

   :Example:

   ::

      In> 5!
      Out> 120;
      In> 1 * 2 * 3 * 4 * 5
      Out> 120;
      In> (1/2)!
      Out> Sqrt(Pi)/2;
      In> 7!!;
      Out> 105;
      In> 1/3 *** 10;
      Out> 17041024000/59049;
      In> Subfactorial(10)
      Out> 1334961;
      

   .. seealso:: :func:`Bin`, :func:`Factorize`, :func:`Gamma`, :func:`!!`, :func:`***`, :func:`Subfactorial`

.. function:: postfix !!(n)

   double factorial

.. function:: infix ***(x,y)

   whatever

.. function:: Bin(n, m)

   binomial coefficients

   :param n}, {m: integers

   This function calculates the binomial coefficient "n" above  "m",
   which equals $$n! / (m! * (n-m)!)$$    This is equal to the number
   of ways  to choose "m" objects out of a total of "n" objects if
   order is  not taken into account. The binomial coefficient is
   defined to be zero  if "m" is negative or greater than "n";
   {Bin(0,0)}=1.

   :Example:

   ::

      In> Bin(10, 4)
      Out> 210;
      In> 10! / (4! * 6!)
      Out> 210;
      

   .. seealso:: :func:`!`, :func:`Eulerian`

.. function:: Eulerian(n,m)

   Eulerian numbers

   :param n}, {m: integers

   The Eulerian numbers can be viewed as a generalization of the
   binomial coefficients,  and are given explicitly by $$
   Sum(j,0,k+1,(-1)^j*Bin(n+1,j)*(k-j+1)^n) $$ .

   :Example:

   ::

      In> Eulerian(6,2)
      Out> 302;
      In> Eulerian(10,9)
      Out> 1;
      

   .. seealso:: :func:`Bin`

.. function:: LeviCivita(list)

   totally anti-symmetric Levi-Civita symbol

   :param list: a list of integers 1 .. n in some order

   {LeviCivita} implements the Levi-Civita symbol. This is generally
   useful for tensor calculus.  {list}  should be a list of integers,
   and this function returns 1 if the integers are in successive
   order,  eg. {LeviCivita( {1,2,3,...} )}  would return 1. Swapping
   two elements of this  list would return -1. So, {LeviCivita(
   {2,1,3} )} would evaluate  to -1.

   :Example:

   ::

      In> LeviCivita({1,2,3})
      Out> 1;
      In> LeviCivita({2,1,3})
      Out> -1;
      In> LeviCivita({2,2,3})
      Out> 0;
      

   .. seealso:: :func:`Permutations`

.. function:: Permutations(list)

   get all permutations of a list

   :param list: a list of elements

   Permutations returns a list with all the permutations of  the
   original list.

   :Example:

   ::

      In> Permutations({a,b,c})
      Out> {{a,b,c},{a,c,b},{c,a,b},{b,a,c},
      {b,c,a},{c,b,a}};
      

   .. seealso:: :func:`LeviCivita`

.. function:: Gamma(x)

   Euler's Gamma function

   :param x: expression
   :param number: expression that can be evaluated to a number

   {Gamma(x)} is an interface to Euler's Gamma function $Gamma(x)$. It
   returns exact values on integer and half-integer arguments.
   {N(Gamma(x)} takes a numeric parameter and always returns a
   floating-point number in the current precision.    Note that
   Euler's constant $gamma<=>0.57722$ is the lowercase {gamma} in
   Yacas.

   :Example:

   ::

      In> Gamma(1.3)
      Out> Gamma(1.3);
      In> N(Gamma(1.3),30)
      Out> 0.897470696306277188493754954771;
      In> Gamma(1.5)
      Out> Sqrt(Pi)/2;
      In> N(Gamma(1.5),30);
      Out> 0.88622692545275801364908374167;
      

   .. seealso:: :func:`!`, :func:`N`, :func:`gamma`

.. function:: Zeta(x)

   Riemann's Zeta function

   :param x: expression
   :param number: expression that can be evaluated to a number

   {Zeta(x)} is an interface to Riemann's Zeta function $zeta(s)$. It
   returns exact values on integer and half-integer arguments.
   {N(Zeta(x)} takes a numeric parameter and always returns a
   floating-point number in the current precision.

   :Example:

   ::

      In> Precision(30)
      Out> True;
      In> Zeta(1)
      Out> Infinity;
      In> Zeta(1.3)
      Out> Zeta(1.3);
      In> N(Zeta(1.3))
      Out> 3.93194921180954422697490751058798;
      In> Zeta(2)
      Out> Pi^2/6;
      In> N(Zeta(2));
      Out> 1.64493406684822643647241516664602;
      

   .. seealso:: :func:`!`, :func:`N`

.. function:: Bernoulli(index)

   Bernoulli numbers and polynomials

   :param x: expression that will be the variable in the polynomial
   :param index: expression that can be evaluated to an integer

   {Bernoulli(n)} evaluates the $n$-th Bernoulli number. {Bernoulli(n,
   x)} returns the $n$-th Bernoulli polynomial in the variable $x$.
   The polynomial is returned in the Horner form.

.. function:: Euler(index)

   Euler numbers and polynomials

   :param x: expression that will be the variable in the polynomial
   :param index: expression that can be evaluated to an integer

   {Euler(n)} evaluates the $n$-th Euler number. {Euler(n,x)} returns
   the $n$-th Euler polynomial in the variable $x$.

   :Example:

   ::

      In> Euler(6)
      Out> -61;
      In> A:=Euler(5,x)
      Out> (x-1/2)^5+(-10*(x-1/2)^3)/4+(25*(x-1/2))/16;
      In> Simplify(A)
      Out> (2*x^5-5*x^4+5*x^2-1)/2;
      

   .. seealso:: :func:`Bin`

.. function:: LambertW(x)

   Lambert's :math:`W` function

   :param x: expression, argument of the function

   Lambert's :math:`W` function is (a multiple-valued, complex
   function) defined for any (complex) :math:`z` by

   .. math:: W(z)e^{W(z)}=z

   The :math:`W` function is sometimes useful to represent solutions
   of transcendental equations. For example, the equation $Ln(x)=3*x$
   can be "solved" by writing $x= -3*W(-1/3)$. It is also possible to
   take a derivative or integrate this function "explicitly".  For
   real arguments $x$, $W(x)$ is real if $x>= -Exp(-1)$.  To compute
   the numeric value of the principal branch of Lambert's $W$ function
   for real arguments $x>= -Exp(-1)$ to current precision, one can
   call {N(LambertW(x))} (where the function {N} tries to approximate
   its argument with a real value).

   :Example:

   ::

      In> LambertW(0)
      Out> 0;
      In> N(LambertW(-0.24/Sqrt(3*Pi)))
      Out> -0.0851224014;
      

   .. seealso:: :func:`Exp`

