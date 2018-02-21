===========
Integration
===========

Integration can be performed by the function :func:`Integrate`, which has
two calling conventions::

    Integrate(variable) expression
    Integrate(variable, from, to) expression

:func:`Integrate` can have its own set of rules for specific integrals, which
might return a correct answer immediately. Alternatively, it calls the function
:func:`AntiDeriv`, to see if the anti-derivative can be determined for the
integral requested. If this is the case, the anti-derivative is used to compose
the output.

If the integration algorithm cannot perform the integral, the
expression is returned unsimplified.

The integration algorithm
-------------------------

General structure
^^^^^^^^^^^^^^^^^

The integration starts at the function :func:`Integrate`, but the task is
delegated to other functions, one after the other. Each function can deem the
integral unsolvable, and thus return the integral unevaluated. These different
functions offer hooks for adding new types of integrals to be handled.

Expression clean-up
^^^^^^^^^^^^^^^^^^^

Integration starts by first cleaning up the expression, by calling
:func:`TrigSimpCombine` to simplify expressions containing multiplications of
trigonometric functions into additions of trigonometric functions (for which the
integration rules are trivial), and then passing the result to :func:`Simplify`.

Generalized integration rules
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

For the function :func:`AntiDeriv`, which is responsible for finding the
anti-derivative of a function, the code splits up expressions
according to the additive properties of integration, eg. integration
of :math:`a+b` is the same as integrating :math:`a` and :math:`b` separately
and adding the integrals.

* Polynomials which can be expressed as univariate polynomials in the
  variable to be integrated over are handled by one integration rule.
* Expressions of the form :math:`pf(x)`, where :math:`p` represents a
  univariate polynomial, and :math:`f(x)` an integrable function, are
  handled by a special integration rule. This transformation rule has
  to be designed carefully not to invoke infinite recursion.
* Rational functions, :math:`f(x)/g(x)` with both :math:`f(x)` and :math:`g(x)`
  being univariate polynomials, is handled separately also, using partial
  fraction expansion to reduce rational function to a sum of simpler
  expressions.

Integration tables
^^^^^^^^^^^^^^^^^^

For elementary functions, yacas uses integration tables. For instance,
the fact that the anti-derivative of :math:`\cos(x)` is :math:`\sin(x)` is
declared in an integration table.

For the purpose of setting up the integration table, a few declaration
functions have been defined, which use some generalized pattern
matchers to be more flexible in recognizing expressions that are
integrable.

Integrating simple functions of a variable
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

For functions like :math:`\sin(x)` the anti-derivative can be declared with
the function :func:`IntFunc`.

The calling sequence for :func:`IntFunc` is::

    IntFunc(variable,pattern,antiderivative)

For instance, for the function :func:`Cos` there is a declaration::

    IntFunc(x,Cos(_x),Sin(x));

The fact that the second argument is a pattern means that each
occurrence of the variable to be matched should be referred to as
``_x``, as in the example above.

:func:`IntFunc` generalizes the integration implicitly, in that it will set up
the system to actually recognize expressions of the form :math:`\cos(ax+b)`,
and return :math:`\sin(ax+b)/a` automatically. This means that the
variables ``a`` and ``b`` are reserved, and can not be used in the
pattern. Also, the variable used (in this case, ``_x`` is actually
matched to the expression passed in to the function, and the variable
``var`` is the real variable being integrated over. To clarify: suppose
the user wants to integrate :math:`\cos(cy+d)` over :math:`y`, then the
following variables are set:

* ``a`` = :math:`c`
* ``b`` = :math:`d`
* ``x`` = :math:`ay+b`
* ``var`` = :math:`x`

When functions are multiplied by constants, that situation is handled
by the integration rule that can deal with univariate polynomials
multiplied by functions, as a constant is a polynomial of degree zero.


Integrating functions containing expressions of the form :math:`ax^2+b`
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

There are numerous expressions containing sub-expressions of the form
:math:`ax^2+b` which can easily be integrated.

The general form for declaring anti-derivatives for such expressions
is::

  IntPureSquare(variable, pattern, sign2, sign0, antiderivative)

Here :func:`IntPureSquare` uses :func:`MatchPureSquared` to match the expression.

The expression is searched for the pattern, where the variable can
match to a sub-expression of the form :math:`ax^2+b`, and for which both
:math:`a` and :math:`b` are numbers and :math:`a*sign2>0` and :math:`b*sign0>0`.

As an example::

    IntPureSquare(x,num_IsFreeOf(var)/(_x),1,1,
                    (num/(a*Sqrt(b/a)))*ArcTan(var/Sqrt(b/a)));

declares that the anti-derivative of :math:`\frac{c}{a*x^2+b}` is

.. math::

   \frac{c}{a\sqrt{\frac{b}{a}}}\arctan{\frac{x}{\sqrt{\frac{b}{a}}}},

if both :math:`a` and :math:`b` are positive numbers.
