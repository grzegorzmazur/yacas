=======
Solvers
=======

By solving one tries to find a mathematical object that meets certain
criteria. This chapter documents the functions that are available to
help find solutions to specific types of problems.

Symbolic Solvers
----------------


.. function:: Solve(eq, var)

   solve an equation

   :param eq: equation to solve
   :param var: variable to solve for

   This command tries to solve an equation. If {eq} does not contain
   the  {==} operator, it is assumed that the user wants to solve $eq
   ==  0$. The result is a list of equations of the form {var ==
   value}, each  representing a solution of the given equation. The
   {Where} operator  can be used to substitute this solution in
   another expression. If the  given equation {eq} does not have any
   solutions, or if {Solve} is  unable to find any, then an empty list
   is returned.    The current implementation is far from perfect. In
   particular, the  user should keep the following points in mind:


.. function:: OldSolve(eq, var)

   old version of {Solve}

   :param eq: single identity equation
   :param var: single variable
   :param eqlist: list of identity equations
   :param varlist: list of variables

   This is an older version of {Solve}. It is retained for two
   reasons. The first one is philosophical: it is good to have
   multiple  algorithms available. The second reason is more
   practical: the newer  version cannot handle systems of equations,
   but {OldSolve} can.    This command tries to solve one or more
   equations. Use the first form  to solve a single equation and the
   second one for systems of  equations.    The first calling sequence
   solves the equation "eq" for the variable  "var". Use the {==}
   operator to form the equation.  The value of "var" which satisfies
   the equation, is returned. Note  that only one solution is found
   and returned.    To solve a system of equations, the second form
   should be used. It  solves the system of equations contained in the
   list "eqlist" for  the variables appearing in the list "varlist". A
   list of results is  returned, and each result is a list containing
   the values of the  variables in "varlist". Again, at most a single
   solution is  returned.    The task of solving a single equation is
   simply delegated to {SuchThat}. Multiple equations are solved
   recursively:  firstly, an equation is sought in which one of the
   variables occurs  exactly once; then this equation is solved with
   {SuchThat}; and finally the solution is substituted in the  other
   equations by {Eliminate} decreasing the number  of equations by
   one. This suffices for all linear equations and a  large group of
   simple nonlinear equations.

   :Example:

   ::

      In> OldSolve(a+x*y==z,x)
      Out> (z-a)/y;
      In> OldSolve({a*x+y==0,x+z==0},{x,y})
      Out> {{-z,z*a}};
      This means that "x = (z-a)/y" is a solution of the first equation
      and that "x = -z", "y = z*a" is a solution of the systems of
      equations in the second command.
      An example which {OldSolve} cannot solve:
      In> OldSolve({x^2-x == y^2-y,x^2-x == y^3+y},{x,y});
      Out> {};
      

   .. seealso:: :func:`Solve`, :func:`SuchThat`, :func:`Eliminate`, :func:`PSolve`, :func:`==`


.. function:: SuchThat(expr, var)

   special purpose solver

   :param expr: expression to make zero
   :param var: variable (or subexpression) to solve for

   This functions tries to find a value of the variable "var" which
   makes the expression "expr" zero. It is also possible to pass a
   subexpression as "var", in which case {SuchThat}  will try to solve
   for that subexpression.    Basically, only expressions in which
   "var" occurs only once are  handled; in fact, {SuchThat} may even
   give wrong  results if the variables occurs more than once. This is
   a consequence  of the implementation, which repeatedly applies the
   inverse of the top  function until the variable "var" is reached.

   :Example:

   ::

      In> SuchThat(a+b*x, x)
      Out> (-a)/b;
      In> SuchThat(Cos(a)+Cos(b)^2, Cos(b))
      Out> Cos(a)^(1/2);
      In> A:=Expand(a*x+b*x+c, x)
      Out> (a+b)*x+c;
      In> SuchThat(A, x)
      Out> (-c)/(a+b);
      

   .. seealso:: :func:`Solve`, :func:`OldSolve`, :func:`Subst`, :func:`Simplify`


.. function:: Eliminate(var, value, expr)

   substitute and simplify

   :param var: variable (or subexpression) to substitute
   :param value: new value of "var"
   :param expr: expression in which the substitution should take place

   This function uses {Subst} to replace all instances  of the
   variable (or subexpression) "var" in the expression "expr"  with
   "value", calls {Simplify} to simplify the  resulting expression,
   and returns the result.

   :Example:

   ::

      In> Subst(Cos(b), c) (Sin(a)+Cos(b)^2/c)
      Out> Sin(a)+c^2/c;
      In> Eliminate(Cos(b), c, Sin(a)+Cos(b)^2/c)
      Out> Sin(a)+c;
      

   .. seealso:: :func:`SuchThat`, :func:`Subst`, :func:`Simplify`


.. function:: PSolve(poly, var)

   solve a polynomial equation

   :param poly: a polynomial in "var"
   :param var: a variable

   This commands returns a list containing the roots of "poly",
   considered as a polynomial in the variable "var". If there is only
   one root, it is not returned as a one-entry list but just by
   itself. A double root occurs twice in the result, and similarly for
   roots of higher multiplicity. All polynomials of degree up to 4 are
   handled.

   :Example:

   ::

      In> PSolve(b*x+a,x)
      Out> -a/b;
      In> PSolve(c*x^2+b*x+a,x)
      Out> {(Sqrt(b^2-4*c*a)-b)/(2*c),(-(b+
      Sqrt(b^2-4*c*a)))/(2*c)};
      

   .. seealso:: :func:`Solve`, :func:`Factor`


.. function:: MatrixSolve(A,b)

   solve a system of equations

   :param A: coefficient matrix
   :param b: row vector

   {MatrixSolve} solves the matrix equations {A*x = b} using Gaussian
   Elimination  with Backward substitution. If your matrix is
   triangular or diagonal, it will  be recognized as such and a faster
   algorithm will be used.

   :Example:

   ::

      In> A:={{2,4,-2,-2},{1,2,4,-3},{-3,-3,8,-2},{-1,1,6,-3}};
      Out> {{2,4,-2,-2},{1,2,4,-3},{-3,-3,8,-2},{-1,1,6,-3}};
      In> b:={-4,5,7,7};
      Out> {-4,5,7,7};
      In> MatrixSolve(A,b);
      Out> {1,2,3,4};
      Numeric solvers
      

Numeric Solvers
----------------


.. function:: Newton(expr, var, initial, accuracy)

   solve an equation numerically with Newton's method

   :param expr: an expression to find a zero for
   :param var: free variable to adjust to find a zero
   :param initial: initial value for "var" to use in the search
   :param accuracy: minimum required accuracy of the result
   :param min: minimum value for "var" to use in the search
   :param max: maximum value for "var" to use in the search

   This function tries to numerically find a zero of the expression
   {expr}, which should depend only on the variable {var}. It uses
   the value {initial} as an initial guess.    The function will
   iterate using Newton's method until it estimates  that it has come
   within a distance {accuracy} of the correct  solution, and then it
   will return its best guess. In particular, it  may loop forever if
   the algorithm does not converge.    When {min} and {max} are
   supplied, the Newton iteration takes them  into account by
   returning {Fail} if it failed to find a root in  the given range.
   Note this doesn't mean there isn't a root, just  that this
   algorithm failed to find it due to the trial values  going outside
   of the bounds.

   :Example:

   ::

      In> Newton(Sin(x),x,3,0.0001)
      Out> 3.1415926535;
      In> Newton(x^2-1,x,2,0.0001,-5,5)
      Out> 1;
      In> Newton(x^2+1,x,2,0.0001,-5,5)
      Out> Fail;
      

   .. seealso:: :func:`Solve`, :func:`NewtonNum`


.. function:: FindRealRoots(p)

   find the real roots of a polynomial

   :param p: a polynomial in {x}

   Return a list with the real roots of $ p $. It tries to find the
   real-valued  roots, and thus requires numeric floating point
   calculations. The precision  of the result can be improved by
   increasing the calculation precision.

   :Example:

   ::

      In> p:=Expand((x+3.1)^5*(x-6.23))
      Out> x^6+9.27*x^5-0.465*x^4-300.793*x^3-
      1394.2188*x^2-2590.476405*x-1783.5961073;
      In> FindRealRoots(p)
      Out> {-3.1,6.23};
      

   .. seealso:: :func:`SquareFree`, :func:`NumRealRoots`, :func:`MinimumBound`, :func:`MaximumBound`, :func:`Factor`


.. function:: NumRealRoots(p)

   return the number of real roots of a polynomial

   :param p: a polynomial in {x}

   Returns the number of real roots of a polynomial $ p $.  The
   polynomial must use the variable {x} and no other variables.

   :Example:

   ::

      In> NumRealRoots(x^2-1)
      Out> 2;
      In> NumRealRoots(x^2+1)
      Out> 0;
      

   .. seealso:: :func:`FindRealRoots`, :func:`SquareFree`, :func:`MinimumBound`, :func:`MaximumBound`, :func:`Factor`


.. function:: MinimumBound(p)

   return lower bounds on the absolute values of real roots of a polynomial

   :param p: a polynomial in $x$

   Return minimum and maximum bounds for the absolute values of the
   real  roots of a polynomial {p}. The polynomial has to be converted
   to one with  rational coefficients first, and be made square-free.
   The polynomial must use the variable {x}.

   :Example:

   ::

      In> p:=SquareFree(Rationalize((x-3.1)*(x+6.23)))
      Out> (-40000*x^2-125200*x+772520)/870489;
      In> MinimumBound(p)
      Out> 5000000000/2275491039;
      In> N(%)
      Out> 2.1973279236;
      In> MaximumBound(p)
      Out> 10986639613/1250000000;
      In> N(%)
      Out> 8.7893116904;
      

   .. seealso:: :func:`SquareFree`, :func:`NumRealRoots`, :func:`FindRealRoots`, :func:`Factor`

