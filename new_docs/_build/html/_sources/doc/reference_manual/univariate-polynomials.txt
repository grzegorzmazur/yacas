=========================
Operations on polynomials
=========================

This chapter contains commands to manipulate polynomials. This
includes functions for constructing and evaluating orthogonal
polynomials.

.. function:: Expand(expr)
              Expand(expr,var)
              Expand(expr,varlist)

   transform a polynomial to an expanded form

   :param expr: a polynomial expression
   :param var: a variable
   :param varlist: a list of variables

   This command brings a polynomial in expanded form, in which
   polynomials are represented in the form   :math:`c_0 + c_1x + c_2x^2 + ...
   + c_nx^n`. In this form, it is   easier to test whether a
   polynomial is zero, namely by testing  whether all coefficients are
   zero.    If the polynomial {expr} contains only one variable, the
   first  calling sequence can be used. Otherwise, the second form
   should be  used which explicitly mentions that {expr} should be
   considered as a  polynomial in the variable {var}. The third
   calling form can be used  for multivariate polynomials. Firstly,
   the polynomial {expr} is  expanded with respect to the first
   variable in {varlist}. Then the  coefficients are all expanded with
   respect to the second variable, and  so on.

   :Example:

   ::

      In> Expand((1+x)^5)
      Out> x^5+5*x^4+10*x^3+10*x^2+5*x+1
      In> Expand((1+x-y)^2, x);
      Out> x^2+2*(1-y)*x+(1-y)^2
      In> Expand((1+x-y)^2, {x,y})
      Out> x^2+((-2)*y+2)*x+y^2-2*y+1
      

   .. seealso:: :func:`ExpandBrackets`

.. function:: Degree(expr,[var])

   degree of a polynomial

   :param expr: a polynomial
   :param var: a variable occurring in {expr}

   This command returns the
   `degree <http://en.wikipedia.org/wiki/Degree_of_a_polynomial>`_ of the
   polynomial ``expr`` with respect to the variable ``var``. If only one
   variable occurs in ``expr``, the first calling sequence can be used.
   Otherwise the user should use the second form in which the variable is
   explicitly mentioned.

   :Example:

   ::

      In> Degree(x^5+x-1);
      Out> 5;
      In> Degree(a+b*x^3, a);
      Out> 1;
      In> Degree(a+b*x^3, x);
      Out> 3;
      

   .. seealso:: :func:`Expand`, :func:`Coef`

.. function:: Coef(expr, var, order)

   coefficient of a polynomial

   :param expr: a polynomial
   :param var: a variable occurring in {expr}
   :param order: integer or list of integers

   This command returns the coefficient of {var} to the power {order}
   in the polynomial {expr}. The parameter {order} can also be a list
   of integers, in which case this function returns a list of
   coefficients.

   :Example:

   ::

      In> e := Expand((a+x)^4,x)
      Out> x^4+4*a*x^3+(a^2+(2*a)^2+a^2)*x^2+
      (a^2*2*a+2*a^3)*x+a^4;
      In> Coef(e,a,2)
      Out> 6*x^2;
      In> Coef(e,a,0 .. 4)
      Out> {x^4,4*x^3,6*x^2,4*x,1};
      

   .. seealso:: :func:`Expand`, :func:`Degree`, :func:`LeadingCoef`

.. function:: Content(expr)

   content of a univariate polynomial

   :param expr: univariate polynomial

   This command determines the 
   `content <http://en.wikipedia.org/wiki/Content_(algebra)>`_
   of a univariate polynomial.

   :Example:

   ::

      In> poly := 2*x^2 + 4*x;
      Out> 2*x^2+4*x;
      In> c := Content(poly);
      Out> 2*x;
      In> pp := PrimitivePart(poly);
      Out> x+2;
      In> Expand(pp*c);
      Out> 2*x^2+4*x;
      

   .. seealso:: :func:`PrimitivePart`, :func:`Gcd`

.. function:: PrimitivePart(expr)

   primitive part of a univariate polynomial

   :param expr: univariate polynomial

   This command determines the primitive part of a univariate
   polynomial. The primitive part is what remains after the content
   is divided out. So the  product of the content and the primitive part equals
   the original  polynomial.

   :Example:

   ::

      In> poly := 2*x^2 + 4*x;
      Out> 2*x^2+4*x;
      In> c := Content(poly);
      Out> 2*x;
      In> pp := PrimitivePart(poly);
      Out> x+2;
      In> Expand(pp*c);
      Out> 2*x^2+4*x;
      

   .. seealso:: :func:`Content`

.. function:: LeadingCoef(poly)

   leading coefficient of a polynomial

   :param poly: a polynomial
   :param var: a variable

   This function returns the leading coefficient of {poly}, regarded
   as  a polynomial in the variable {var}. The leading coefficient is
   the  coefficient of the term of highest degree. If only one
   variable  appears in the expression {poly}, it is obvious that it
   should be  regarded as a polynomial in this variable and the first
   calling  sequence may be used.

   :Example:

   ::

      In> poly := 2*x^2 + 4*x;
      Out> 2*x^2+4*x;
      In> lc := LeadingCoef(poly);
      Out> 2;
      In> m := Monic(poly);
      Out> x^2+2*x;
      In> Expand(lc*m);
      Out> 2*x^2+4*x;
      In> LeadingCoef(2*a^2 + 3*a*b^2 + 5, a);
      Out> 2;
      In> LeadingCoef(2*a^2 + 3*a*b^2 + 5, b);
      Out> 3*a;
      

   .. seealso:: :func:`Coef`, :func:`Monic`

.. function:: Monic(poly)

   monic part of a polynomial

   :param poly: a polynomial
   :param var: a variable

   This function returns the monic part of {poly}, regarded as a
   polynomial in the variable {var}. The monic part of a polynomial is
   the quotient of this polynomial by its leading coefficient. So the
   leading coefficient of the monic part is always one. If only one
   variable appears in the expression {poly}, it is obvious that it
   should be regarded as a polynomial in this variable and the first
   calling sequence may be used.

   :Example:

   ::

      In> poly := 2*x^2 + 4*x;
      Out> 2*x^2+4*x;
      In> lc := LeadingCoef(poly);
      Out> 2;
      In> m := Monic(poly);
      Out> x^2+2*x;
      In> Expand(lc*m);
      Out> 2*x^2+4*x;
      In> Monic(2*a^2 + 3*a*b^2 + 5, a);
      Out> a^2+(a*3*b^2)/2+5/2;
      In> Monic(2*a^2 + 3*a*b^2 + 5, b);
      Out> b^2+(2*a^2+5)/(3*a);
      

   .. seealso:: :func:`LeadingCoef`

.. function:: SquareFree(p)

   return the square-free part of polynomial

   :param p: a polynomial in {x}

   Given a polynomial   $$ p = p[1]^n[1]* ... * p[m]^n[m] $$  with
   irreducible polynomials $ p[i] $,  return the square-free version
   part (with all the factors having  multiplicity 1):  $$ p[1]* ... *
   p[m] $$

   :Example:

   ::

      In> Expand((x+1)^5)
      Out> x^5+5*x^4+10*x^3+10*x^2+5*x+1;
      In> SquareFree(%)
      Out> (x+1)/5;
      In> Monic(%)
      Out> x+1;
      

   .. seealso:: :func:`FindRealRoots`, :func:`NumRealRoots`, :func:`MinimumBound`, :func:`MaximumBound`, :func:`Factor`

.. function:: SquareFreeFactorize(p,x)

   return square-free decomposition of polynomial

   :param p: a polynomial in {x}

   Given a polynomial $p$ having square-free decomposition   $$ p =
   p[1]^n[1] * ... * p[m]^n[m] $$  where $p[i]$ are square-free and
   $n[i+1]>n[i]$,  return the list of pairs ($p[i]$, $n[i]$)

   :Example:

   ::

      In> Expand((x+1)^5)
      Out> x^5+5*x^4+10*x^3+10*x^2+5*x+1
      In> SquareFreeFactorize(%,x)
      Out> {{x+1,5}}
      

   .. seealso:: :func:`Factor`

.. function:: Horner(expr, var)

   convert a polynomial into the Horner form

   :param expr: a polynomial in {var}
   :param var: a variable

   This command turns the polynomial {expr}, considered as a
   univariate  polynomial in {var}, into Horner form. A polynomial in
   normal form  is an expression such as  $$c[0] + c[1]*x + ... +
   c[n]*x^n$$.    If one converts this polynomial into Horner form,
   one gets the  equivalent expression  $$(...( c[n] * x + c[n-1] ) *
   x + ...  + c[1] ) * x + c[0]$$.    Both expression are equal, but
   the latter form gives a more  efficient way to evaluate the
   polynomial as  the powers have  disappeared.

   :Example:

   ::

      In> expr1:=Expand((1+x)^4)
      Out> x^4+4*x^3+6*x^2+4*x+1;
      In> Horner(expr1,x)
      Out> (((x+4)*x+6)*x+4)*x+1;
      

   .. seealso:: :func:`Expand`, :func:`ExpandBrackets`, :func:`EvaluateHornerScheme`

.. function:: ExpandBrackets(expr)

   expand all brackets

   :param expr: an expression

   This command tries to expand all the brackets by repeatedly using
   the  distributive laws $a * (b+c) = a*b + a*c$ and $(a+b) * c = a*c
   + b*c$.  It goes further than {Expand}, in that it expands all
   brackets.

   :Example:

   ::

      In> Expand((a-x)*(b-x),x)
      Out> x^2-(b+a)*x+a*b;
      In> Expand((a-x)*(b-x),{x,a,b})
      Out> x^2-(b+a)*x+b*a;
      In> ExpandBrackets((a-x)*(b-x))
      Out> a*b-x*b+x^2-a*x;
      

   .. seealso:: :func:`Expand`

.. function:: EvaluateHornerScheme(coeffs,x)

   fast evaluation of polynomials

   :param coeffs: a list of coefficients
   :param x: expression

   This function evaluates a polynomial given as a list of its
   coefficients, using  the Horner scheme. The list of coefficients
   starts with the $0$-th power.

.. function:: OrthoP(n, x);

   Legendre and Jacobi orthogonal polynomials

   :param n: degree of polynomial
   :param x: point to evaluate polynomial at
   :param a}, {b: parameters for Jacobi polynomial

   The first calling format with two arguments evaluates the Legendre
   polynomial  of degree {n} at the point {x}. The second form does
   the same for the Jacobi  polynomial with parameters {a} and {b},
   which should be both greater than -1.    The Jacobi polynomials are
   orthogonal with respect to the weight  function $(1-x)^a *(1+x)^b$
   on the interval [-1,1]. They satisfy the  recurrence relation
   $$P(n,a,b,x) = (2*n+a+b-1)/(2*n+a+b-2) $$*  $$
   ((a^2-b^2+x*(2*n+a+b-2)*(n+a+b))/(2*n*(n+a+b))) * P(n-1,a,b,x)$$
   $$ -
   ((n+a-1)*(n+b-1)*(2*n+a+b))/(n*(n+a+b)*(2*n+a+b-2))*P(n-2,a,b,x)$$
   for $n > 1$, with  $P(0,a,b,x) = 1$,  $$P(1,a,b,x) =
   (a-b)/2+x*(1+(a+b)/2)$$.

.. function:: OrthoH(n, x);

   Hermite orthogonal polynomials

   :param n: degree of polynomial
   :param x: point to evaluate polynomial at

   This function evaluates the Hermite polynomial of degree {n} at the
   point {x}.    The Hermite polynomials are orthogonal with respect
   to the weight  function $Exp(-x^2/2)$ on the entire real axis. They
   satisfy the  recurrence relation  $$ H(n,x) = 2*x*H(n-1,x) -
   2*(n-1)*H(n-2,x) $$  for $n > 1$, with  $H(0,x) = 1$,  $H(1,x) =
   2*x$.    Most of the work is performed by the internal function
   {OrthoPoly}.

   :Example:

   ::

      In> OrthoH(3, x);
      Out> x*(8*x^2-12);
      In> OrthoH(6, 0.5);
      Out> 31;
      

   .. seealso:: :func:`OrthoHSum`, :func:`OrthoPoly`

.. function:: OrthoG(n, a, x);

   Gegenbauer orthogonal polynomials

   :param n: degree of polynomial
   :param a: parameter
   :param x: point to evaluate polynomial at

   This function evaluates the Gegenbauer (or ultraspherical)
   polynomial  with parameter {a} and degree {n} at the point {x}. The
   parameter {a} should be greater than -1/2.    The Gegenbauer
   polynomials are orthogonal with respect to the weight  function
   $(1-x^2)^(a-1/2)$ on the interval [-1,1]. Hence they are  connected
   to the Jacobi polynomials via   $$ G(n, a, x) = P(n, a-1/2, a-1/2,
   x) $$.  They satisfy the recurrence relation  $$ G(n,a,x) =
   2*(1+(a-1)/n)*x*G(n-1,a,x) $$  $$ -(1+2*(a-2)/n)*G(n-2,a,x) $$  for
   $n>1$, with  $G(0,a,x) = 1$,  $G(1,a,x) = 2*x$.

.. function:: OrthoL(n, a, x);

   Laguerre orthogonal polynomials

   :param n: degree of polynomial
   :param a: parameter
   :param x: point to evaluate polynomial at

   This function evaluates the Laguerre polynomial with parameter {a}
   and degree {n} at the point {x}. The parameter {a} should be
   greater than -1.    The Laguerre polynomials are orthogonal with
   respect to the weight  function $x^a * Exp(-x)$ on the positive
   real axis. They satisfy the  recurrence relation  $$ L(n,a,x) =
   (2+(a-1-x)/n)* L(n-1,a,x) $$  $$ -(1-(a-1)/n)*L(n-2,a,x) $$  for
   $n>1$, with   $L(0,a,x) = 1$,  $L(1,a,x) = a + 1 - x$.

.. function:: OrthoT(n, x);

   Chebyshev polynomials

   :param n: degree of polynomial
   :param x: point to evaluate polynomial at

   These functions evaluate the Chebyshev polynomials of the first
   kind  $T(n,x)$ and of the second kind $U(n,x)$, of degree {n} at
   the point {x}. (The  name of this Russian mathematician is also
   sometimes spelled {Tschebyscheff}.)    The Chebyshev polynomials
   are orthogonal with respect to the weight  function
   $(1-x^2)^(-1/2)$. Hence they are a special case of the Gegenbauer
   polynomials $G(n,a,x)$, with $a=0$. They satisfy the recurrence
   relations  $$ T(n,x) = 2* x* T(n-1,x) - T(n-2,x) $$,  $$ U(n,x) =
   2* x* U(n-1,x) - U(n-2,x) $$  for $n > 1$, with  $T(0,x) = 1$,
   $T(1,x) = x$,  $U(0,x) = 1$,  $U(1,x) = 2*x$.

   :Example:

   ::

      In> OrthoT(3, x);
      Out> 2*x*(2*x^2-1)-x;
      In> OrthoT(10, 0.9);
      Out> -0.2007474688;
      In> OrthoU(3, x);
      Out> 4*x*(2*x^2-1);
      In> OrthoU(10, 0.9);
      Out> -2.2234571776;
      

   .. seealso:: :func:`OrthoG`, :func:`OrthoTSum`, :func:`OrthoUSum`, :func:`OrthoPoly`

.. function:: OrthoPSum(c, x);

   sums of series of orthogonal polynomials

   :param c: list of coefficients
   :param a}, {b: parameters of specific polynomials
   :param x: point to evaluate polynomial at

   These functions evaluate the sum of series of orthogonal
   polynomials at the point {x}, with given list of coefficients {c}
   of the series and fixed polynomial parameters {a}, {b} (if
   applicable).    The list of coefficients starts with the lowest
   order, so that for example  OrthoLSum(c, a, x) = c[1] L[0](a,x) +
   c[2] L[1](a,x) + ... + c[N] L[N-1](a,x).    See pages for specific
   orthogonal polynomials for more details on the parameters of the
   polynomials.    Most of the work is performed by the internal
   function {OrthoPolySum}. The individual polynomials entering the
   series are not computed, only the sum of the series.

   :Example:

   ::

      In> Expand(OrthoPSum({1,0,0,1/7,1/8}, 3/2, \
      2/3, x));
      Out> (7068985*x^4)/3981312+(1648577*x^3)/995328+
      (-3502049*x^2)/4644864+(-4372969*x)/6967296
      +28292143/27869184;
      

   .. seealso:: :func:`OrthoP`, :func:`OrthoG`, :func:`OrthoH`, :func:`OrthoL`, :func:`OrthoT`, :func:`OrthoU`, :func:`OrthoPolySum`

.. function:: OrthoPoly(name, n, par, x)

   internal function for constructing orthogonal polynomials

   :param name: string containing name of orthogonal family
   :param n: degree of the polynomial
   :param par: list of values for the parameters
   :param x: point to evaluate at

   This function is used internally to construct orthogonal
   polynomials. It returns the {n}-th polynomial from the family
   {name} with parameters {par} at the point {x}.    All known
   families are stored in the association list returned by the
   function {KnownOrthoPoly()}. The name serves as key. At the moment
   the following names are known to Yacas: {"Jacobi"}, {"Gegenbauer"},
   {"Laguerre"}, {"Hermite"}, {"Tscheb1"},  and {"Tscheb2"}. The value
   associated to the key  is a pure function that takes two arguments:
   the order {n} and the  extra parameters {p}, and returns a list of
   two lists: the first list  contains the coefficients {A,B} of the
   n=1 polynomial, i.e. $A+B*x$;  the second list contains the
   coefficients {A,B,C} in the recurrence  relation, i.e. $P[n] =
   (A+B*x)*P[n-1]+C*P[n-2]$. (There are  only 3 coefficients in the
   second list, because none of the polynomials use $C+D*x$ instead of
   $C$ in the recurrence relation. This is assumed in the
   implementation!)    If the argument {x} is numerical, the function
   {OrthoPolyNumeric} is called. Otherwise, the function
   {OrthoPolyCoeffs} computes a list of coefficients, and
   {EvaluateHornerScheme} converts this list into a  polynomial
   expression.

   .. seealso:: :func:`OrthoP`, :func:`OrthoG`, :func:`OrthoH`, :func:`OrthoL`, :func:`OrthoT`, :func:`OrthoU`, :func:`OrthoPolySum`

.. function:: OrthoPolySum(name, c, par, x)

   internal function for computing series of orthogonal polynomials

   :param name: string containing name of orthogonal family
   :param c: list of coefficients
   :param par: list of values for the parameters
   :param x: point to evaluate at

   This function is used internally to compute series of orthogonal
   polynomials.  It is similar to the function {OrthoPoly} and returns
   the result of the  summation of series of polynomials from the
   family {name} with parameters {par}  at the point {x}, where {c} is
   the list of coefficients of the series.    The algorithm used to
   compute the series without first computing the individual
   polynomials is the Clenshaw-Smith recurrence scheme.  (See the
   algorithms book for explanations.)    If the argument {x} is
   numerical, the function {OrthoPolySumNumeric} is called.
   Otherwise, the function {OrthoPolySumCoeffs} computes the list of
   coefficients  of the resulting polynomial, and
   {EvaluateHornerScheme} converts this list into  a polynomial
   expression.

   .. seealso:: :func:`OrthoPSum`, :func:`OrthoGSum`, :func:`OrthoHSum`, :func:`OrthoLSum`, :func:`OrthoTSum`, :func:`OrthoUSum`, :func:`OrthoPoly`

