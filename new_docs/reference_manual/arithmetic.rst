==========================================
Arithmetic and other operations on numbers
==========================================

.. function:: infix +(x,y)

  addition

  Addition can work on integers, rational numbers,
  complex numbers, vectors, matrices and lists.

  .. hint::
    Addition is implemented in the standard math library (as
    opposed to being built-in). This means that it can be extended by the user.


  :Example:

  ::

    In> 2+3
    Out> 5

.. function:: prefix -(x)

  negation

  Negation can work on integers, rational numbers,
  complex numbers, vectors, matrices and lists.

  .. hint::
    Negation is implemented in the standard math library (as
    opposed to being built-in). This means that it can be extended by the user.


  :Example:

  ::

    In> - 3
    Out> -3

.. function:: infix -(x,y)

  subtraction

  Subtraction can work on integers, rational numbers,
  complex numbers, vectors, matrices and lists.

  .. hint::
    Subtraction is implemented in the standard math library (as
    opposed to being built-in). This means that it can be extended by the user.


  :Example:

  ::

    In> 2-3
    Out> -1

.. function:: infix *(x,y)

  multiplication

  Multiplication can work on integers, rational numbers,
  complex numbers, vectors, matrices and lists.

  .. note::
    In the case of matrices, multiplication is defined in
    terms of standard matrix product.

  .. hint::
    Multiplication is implemented in the standard math library (as
    opposed to being built-in). This means that it can be extended by the user.


  :Example:

  ::

    In> 2*3
    Out> 6

.. function:: infix /(x,y)

  division

  Division can work on integers, rational numbers,
  complex numbers, vectors, matrices and lists.

  .. note::
    For matrices division is element-wise.

  .. hint::
    Division is implemented in the standard math library (as
    opposed to being built-in). This means that it can be extended by the user.


  :Example:

  ::

    In> 6/2
    Out> 3

.. function:: infix ^(x,y)

  exponentiation

  Exponentiation can work on integers, rational numbers,
  complex numbers, vectors, matrices and lists.

  .. note::
    In the case of matrices, exponentiation is defined in
    terms of standard matrix product.

  .. hint::
    Exponentiation is implemented in the standard math library (as
    opposed to being built-in). This means that it can be extended by the user.


  :Example:

  ::

    In> 2^3
    Out> 8

.. function:: Div(x,y)

   determine divisor

  :func:`Div` performs integer division.
  If ``Div(x,y)`` returns ``a`` and ``Mod(x,y)`` equals ``b``, then these
  numbers satisfy :math:`x =ay + b` and :math:`0 \leq b < y`.

  :Example:

  ::

    In> Div(5,3)
    Out> 1

  .. seealso:: :func:`Mod`, :func:`Gcd`, :func:`Lcm`

.. function:: Mod(x,y)

   determine remainder

  :func:`Mod` returns the division remainder.
  If ``Div(x,y)`` returns ``a`` and ``Mod(x,y)`` equals ``b``, then these
  numbers satisfy :math:`x =ay + b` and :math:`0 \leq b < y`.

  :Example:

  ::

    In> Div(5,3)
    Out> 1
    In> Mod(5,3)
    Out> 2
      

  .. seealso:: :func:`Div`, :func:`Gcd`, :func:`Lcm`

.. function:: Gcd(n,m)
              Gcd(list)

  greatest common divisor

  This function returns the `greatest common divisor 
  <https://en.wikipedia.org/wiki/Greatest_common_divisor>`_ of ``n`` and ``m``
  or of all elements of ``list``.

  .. seealso:: :func:`Lcm`

.. function:: Lcm(n,m)
              Lcm(list)

  least common multiple

  This command returns the `least common multiple 
  <https://en.wikipedia.org/wiki/Least_common_multiple>`_ of ``n`` and ``m`` or
  of all elements of ``list``.

  :Example:

  ::

    In> Lcm(60,24)
    Out> 120
    In> Lcm({3,5,7,9})
    Out> 315
      

  .. seealso:: :func:`Gcd`

.. function:: infix <<(n, m)
              infix >>(n, m)

  binary shift operators

  These operators shift integers to the left or to the right.  They
  are similar to the C shift operators. These are sign-extended
  shifts, so they act as multiplication or division by powers of 2.

  :Example:

  ::

    In> 1 << 10
    Out> 1024
    In> -1024 >> 10
    Out> -1      

.. function:: FromBase(base,"string")

   conversion of a number from non-decimal base to decimal base

   :param base: integer, base to convert to/from
   :param number: integer, number to write out in a different base
   :param "string": string representing a number in a different base

   In Yacas, all numbers are written in decimal notation (base 10).
   The two functions {FromBase}, {ToBase} convert numbers between base
   10 and a different base.  Numbers in non-decimal notation are
   represented by strings.    {FromBase} converts an integer, written
   as a string in base  {base}, to base 10. {ToBase} converts
   {number},  written in base 10, to base {base}.

.. function:: N(expression)

   try determine numerical approximation of expression

   :param expression: expression to evaluate
   :param precision: integer, precision to use

   The function :func:`N` instructs yacas to try to coerce an expression
   in to a numerical approximation to the  expression ``expr``, using
   ``prec`` digits precision if the second calling  sequence is used,
   and the default precision otherwise. This overrides the normal
   behaviour, in which expressions are kept in symbolic form (eg.
   ``Sqrt(2)`` instead of ``1.41421``). Application of the :func:`N` operator
   will make yacas  calculate floating point representations of
   functions whenever  possible. In addition, the variable :data:`Pi` is
   bound to  the value of :math:`\pi` calculated at the current precision.

   .. note::
     :func:`N` is a macro. Its argument ``expr`` will only be evaluated after
     switching to numeric mode.

   :Example:

   ::

      In> 1/2
      Out> 1/2;
      In> N(1/2)
      Out> 0.5;
      In> Sin(1)
      Out> Sin(1);
      In> N(Sin(1),10)
      Out> 0.8414709848;
      In> Pi
      Out> Pi;
      In> N(Pi,20)
      Out> 3.14159265358979323846;
      

   .. seealso:: :func:`Pi`

.. function:: Rationalize(expr)

   convert floating point numbers to fractions

   :param expr: an expression containing real numbers

   This command converts every real number in the expression "expr"
   into a rational number. This is useful when a calculation needs to
   be  done on floating point numbers and the algorithm is unstable.
   Converting the floating point numbers to rational numbers will
   force  calculations to be done with infinite precision (by using
   rational  numbers as representations).    It does this by finding
   the smallest integer $n$ such that multiplying  the number with
   $10^n$ is an integer. Then it divides by $10^n$ again,  depending
   on the internal gcd calculation to reduce the resulting  division
   of integers.

   :Example:

   ::

      In> {1.2,3.123,4.5}
      Out> {1.2,3.123,4.5};
      In> Rationalize(%)
      Out> {6/5,3123/1000,9/2};
      

   .. seealso:: :func:`IsRational`

.. function:: ContFrac(x[,depth=6])

   continued fraction expansion

   :param x: number or polynomial to expand in continued fractions
   :param depth: positive integer, maximum required depth

   This command returns the continued fraction expansion of ``x``,
   which should be either a floating point number or a polynomial. The
   remainder is denoted by {rest}.  This is especially useful for
   polynomials, since series expansions that converge slowly will
   typically converge a lot faster if calculated using a continued
   fraction expansion.

   :Example:

   ::

      In> PrettyForm(ContFrac(N(Pi)))
                    1
      --------------------------- + 3
                 1
      ----------------------- + 7
              1
      ------------------ + 15
           1
      -------------- + 1
         1
      -------- + 292
      rest + 1
      Out> True;
      In> PrettyForm(ContFrac(x^2+x+1, 3))
      x
      ---------------- + 1
      x
      1 - ------------
      x
      -------- + 1
      rest + 1
      Out> True;
      

   .. seealso:: :func:`PAdicExpand`, :func:`N`

.. function:: Decimal(frac)

   decimal representation of a rational

   :param frac: a rational number

   This function returns the infinite decimal representation of a
   rational number {frac}.  It returns a list, with the first element
   being the number before the decimal point and the last element the
   sequence of digits that will repeat forever. All the intermediate
   list  elements are the initial digits before the period sets in.

   :Example:

   ::

      In> Decimal(1/22)
      Out> {0,0,{4,5}};
      In> N(1/22,30)
      Out> 0.045454545454545454545454545454;
      

   .. seealso:: :func:`N`

.. function:: Floor(x)

   round a number downwards

   :param x: a number

   This function returns :math:`\left \lfloor{x}\right \rfloor`, the largest
   integer smaller than or equal to ``x``.

   :Example:

   ::

      In> Floor(1.1)
      Out> 1;
      In> Floor(-1.1)
      Out> -2;
      

   .. seealso:: :func:`Ceil`, :func:`Round`

.. function:: Ceil(x)

   round a number upwards

   :param x: a number

   This function returns :math:`\left \lceil{x}\right \rceil`, the smallest 
   integer larger than or equal to ``x``.

   :Example:

   ::

      In> Ceil(1.1)
      Out> 2;
      In> Ceil(-1.1)
      Out> -1;
      

   .. seealso:: :func:`Floor`, :func:`Round`

.. function:: Round(x)

   round a number to the nearest integer

   :param x: a number

   This function returns the integer closest to $x$. Half-integers
   (i.e. numbers of the form $n + 0.5$, with $n$ an integer) are
   rounded upwards.

   :Example:

   ::

      In> Round(1.49)
      Out> 1;
      In> Round(1.51)
      Out> 2;
      In> Round(-1.49)
      Out> -1;
      In> Round(-1.51)
      Out> -2;
      

   .. seealso:: :func:`Floor`, :func:`Ceil`

.. function:: Min(x,y)

   minimum of a number of values

   :param x}, {y: pair of values to determine the minimum of
   :param list: list of values from which the minimum is sought

   This function returns the minimum value of its argument(s). If the
   first calling sequence is used, the smaller of "x" and "y" is
   returned. If one uses the second form, the smallest of the entries
   in  "list" is returned. In both cases, this function can only be
   used  with numerical values and not with symbolic arguments.

   :Example:

   ::

      In> Min(2,3);
      Out> 2;
      In> Min({5,8,4});
      Out> 4;
      

   .. seealso:: :func:`Max`, :func:`Sum`

.. function:: Max(x,y)

   maximum of a number of values

   :param x}, {y: pair of values to determine the maximum of
   :param list: list of values from which the maximum is sought

   This function returns the maximum value of its argument(s). If the
   first calling sequence is used, the larger of "x" and "y" is
   returned. If one uses the second form, the largest of the entries
   in  "list" is returned. In both cases, this function can only be
   used  with numerical values and not with symbolic arguments.

   :Example:

   ::

      In> Max(2,3);
      Out> 3;
      In> Max({5,8,4});
      Out> 8;
      

   .. seealso:: :func:`Min`, :func:`Sum`

.. function:: Numer(expr)

   numerator of an expression

   :param expr: expression to determine numerator of

   This function determines the numerator of the rational expression
   ``expr`` and returns it. As a special case, if its argument is
   numeric  but not rational, it returns this number. If ``expr`` is
   neither  rational nor numeric, the function returns unevaluated.

   :Example:

   ::

      In> Numer(2/7)
      Out> 2;
      In> Numer(a / x^2)
      Out> a;
      In> Numer(5)
      Out> 5;
      

   .. seealso:: :func:`Denom`, :func:`IsRational`, :func:`IsNumber`

.. function:: Denom(expr)

   denominator of an expression

   :param expr: expression to determine denominator of

   This function determines the denominator of the rational expression
   ``expr`` and returns it. As a special case, if its argument is
   numeric but not rational, it returns ``1``. If ``expr`` is  neither
   rational nor numeric, the function returns unevaluated.

   :Example:

   ::

      In> Denom(2/7)
      Out> 7;
      In> Denom(a / x^2)
      Out> x^2;
      In> Denom(5)
      Out> 1;
      

   .. seealso:: :func:`Numer`, :func:`IsRational`, :func:`IsNumber`

.. function:: Pslq(xlist,precision)

   search for integer relations between reals

   :param xlist: list of numbers
   :param precision: required number of digits precision of calculation

   This function is an integer relation detection algorithm. This
   means  that, given the numbers :math:`x_i` in the list ``xlist``, it tries
   to find integer coefficients 
   :math:`a_i` such that  :math:`a_1*x_`+\ldots+a_n*x_n = 0`. The list of
   integer coefficients is returned.
   The numbers in "xlist" must evaluate to floating point numbers when
   the :func:`N` operator is applied to them.

.. function:: infix <(e1, e2)

   test for "less than"

   :param e1: expression to be compared
   :param e2: expression to be compared

   The two expression are evaluated. If both results are numeric, they
   are compared. If the first expression is smaller than the second
   one,  the result is :data:`True` and it is :data:`False` otherwise. If either
   of the expression is not numeric, after  evaluation, the expression
   is returned with evaluated arguments.    The word "numeric" in the
   previous paragraph has the following  meaning. An expression is
   numeric if it is either a number (i.e. {IsNumber} returns :data:`True`),
   or the  quotient of two numbers, or an infinity (i.e. {IsInfinity}
   returns :data:`True`). Yacas will try to   coerce the arguments passed to
   this comparison operator to a real value before making the
   comparison.

   :Example:

   ::

      In> 2 < 5;
      Out> True;
      In> Cos(1) < 5;
      Out> True;
      

   .. seealso:: :func:`IsNumber`, :func:`IsInfinity`, :func:`N`

.. function:: infix >(e1, e2)

   test for "greater than"

   :param e1: expression to be compared
   :param e2: expression to be compared

   The two expression are evaluated. If both results are numeric, they
   are compared. If the first expression is larger than the second
   one,  the result is :data:`True` and it is :data:`False` otherwise. If either
   of the expression is not numeric, after  evaluation, the expression
   is returned with evaluated arguments.    The word "numeric" in the
   previous paragraph has the following  meaning. An expression is
   numeric if it is either a number (i.e. {IsNumber} returns :data:`True`),
   or the  quotient of two numbers, or an infinity (i.e. {IsInfinity}
   returns :data:`True`). Yacas will try to   coerce the arguments passed to
   this comparison operator to a real value before making the
   comparison.

   :Example:

   ::

      In> 2 > 5;
      Out> False;
      In> Cos(1) > 5;
      Out> False
      

   .. seealso:: :func:`IsNumber`, :func:`IsInfinity`, :func:`N`

.. function:: infix <=(e1, e2)

   test for "less or equal"

   :param e1: expression to be compared
   :param e2: expression to be compared

   The two expression are evaluated. If both results are numeric, they
   are compared. If the first expression is smaller than or equals the
   second one, the result is :data:`True` and it is :data:`False` otherwise. If
   either of the expression is not  numeric, after evaluation, the
   expression is returned with evaluated  arguments.    The word
   "numeric" in the previous paragraph has the following  meaning. An
   expression is numeric if it is either a number (i.e. {IsNumber}
   returns :data:`True`), or the  quotient of two numbers, or an infinity
   (i.e. {IsInfinity} returns :data:`True`). Yacas will try to   coerce the
   arguments passed to this comparison operator to a real value before
   making the comparison.

   :Example:

   ::

      In> 2 <= 5;
      Out> True;
      In> Cos(1) <= 5;
      Out> True
      

   .. seealso:: :func:`IsNumber`, :func:`IsInfinity`, :func:`N`

.. function:: infix >=(e1, e2)

   test for "greater or equal"

   :param e1: expression to be compared
   :param e2: expression to be compared

   The two expression are evaluated. If both results are numeric, they
   are compared. If the first expression is larger than or equals the
   second one, the result is :data:`True` and it is :data:`False` otherwise. If
   either of the expression is not  numeric, after evaluation, the
   expression is returned with evaluated  arguments.    The word
   "numeric" in the previous paragraph has the following  meaning. An
   expression is numeric if it is either a number (i.e. {IsNumber}
   returns :data:`True`), or the  quotient of two numbers, or an infinity
   (i.e. {IsInfinity} returns :data:`True`). Yacas will try to   coerce the
   arguments passed to this comparison operator to a real value before
   making the comparison.

   :Example:

   ::

      In> 2 >= 5;
      Out> False;
      In> Cos(1) >= 5;
      Out> False
      

   .. seealso:: :func:`IsNumber`, :func:`IsInfinity`, :func:`N`

.. function:: IsZero(n)

   test whether argument is zero

   :param n: number to test

   ``IsZero(n)`` evaluates to :data:`True` if  ``n`` is zero. In case ``n`` is
   not a number, the function returns  :data:`False`.

   :Example:

   ::

      In> IsZero(3.25)
      Out> False;
      In> IsZero(0)
      Out> True;
      In> IsZero(x)
      Out> False;
      

   .. seealso:: :func:`IsNumber`, :func:`IsNotZero`

.. function:: IsRational(expr)

   test whether argument is a rational

   :param expr: expression to test

   This commands tests whether the expression "expr" is a rational
   number, i.e. an integer or a fraction of integers.

   :Example:

   ::

      In> IsRational(5)
      Out> False;
      In> IsRational(2/7)
      Out> True;
      In> IsRational(0.5)
      Out> False;
      In> IsRational(a/b)
      Out> False;
      In> IsRational(x + 1/x)
      Out> False;
      

   .. seealso:: :func:`Numer`, :func:`Denom`

