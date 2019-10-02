=============================
Simplification of expressions
=============================

Simplification of expression is a big and non-trivial
subject. Simplification implies that there is a preferred form. In
practice the preferred form depends on the calculation at hand. This
chapter describes the functions offered that allow simplification of
expressions.

.. function:: Simplify(expr)

   try to simplify an expression

   This function tries to simplify the expression ``expr`` as much  as
   possible. It does this by grouping powers within terms, and then
   grouping similar terms.

   :Example:

   ::

      In> a*b*a^2/b-a^3
      Out> (b*a^3)/b-a^3;
      In> Simplify(a*b*a^2/b-a^3)
      Out> 0;


   .. seealso:: :func:`FactorialSimplify`, :func:`LnCombine`, :func:`LnExpand`, :func:`RadSimp`, :func:`TrigSimpCombine`

.. function:: RadSimp(expr)

   simplify expression with nested radicals

   This function tries to write the expression ``expr`` as a sum of roots  of
   integers: :math:`\sqrt{e_1} + \sqrt{e_2} + ...`, where :math:`e_1,e_2` and so
   on are natural numbers. The expression ``expr`` may not contain  free
   variables.

   It does this by trying all possible combinations for :math:`e1,e2,\ldots`.
   Every possibility is numerically evaluated using :func:`N` and compared with
   the numerical evaluation of ``expr``. If the approximations are equal (up to
   a certain margin), this possibility is returned. Otherwise, the expression is
   returned unevaluated.

   .. note::
     Due to the use of numerical approximations, there is a small chance that
     the expression returned by :func:`RadSimp` is close but not equal to
     ``expr``. Furthermore, if the numerical value of ``expr`` is large, the
     number of possibilities becomes exorbitantly big so the evaluation may take
     very long.

   :Example:

   ::

      In> RadSimp(Sqrt(9+4*Sqrt(2)))
      Out> Sqrt(8)+1;
      In> RadSimp(Sqrt(5+2*Sqrt(6)) + Sqrt(5-2*Sqrt(6)))
      Out> Sqrt(12);
      In> RadSimp(Sqrt(14+3*Sqrt(3+2*Sqrt(5-12*Sqrt(3-2*Sqrt(2))))))
      Out> Sqrt(2)+3;

   But this command may yield incorrect results::

      In> RadSimp(Sqrt(1+10^(-6)))
      Out> 1;

   .. seealso:: :func:`Simplify`, :func:`N`

.. function:: FactorialSimplify(expression)

   simplify hypergeometric expressions containing factorials

   :func:`FactorialSimplify` takes an expression that may contain factorials,
   and tries to simplify it. An expression like :math:`\frac{(n+1)!}{n!}` would
   simplify to :math:`(n+1)`.

   .. seealso:: :func:`Simplify`, :func:`!`

.. function:: LnExpand(expr)

   expand a logarithmic expression using standard logarithm rules

   :func:`LnExpand` takes an expression of the form :math:`\ln(expr)`, and
   applies logarithm  rules to expand this into multiple :func:`Ln` expressions
   where possible.  An  expression like :math:`\ln(ab^n)` would be expanded to
   :math:`\ln(a)+n\ln(b)`. If the logarithm of an integer is discovered, it is
   factorised using :func:`Factors` and expanded as though :func:`LnExpand` had
   been given the factorised form.  So :math:`\ln(18)` goes to
   :math:`\ln(2)+2\ln(3)`.

   .. seealso:: :func:`LnCombine`, :func:`Simplify`, :func:`Ln`, :func:`Expand`

.. function:: LnCombine(expr)

   combine logarithmic expressions using standard logarithm rules

   :func:`LnCombine` finds :func:`Ln` terms in the expression it is given, and
   combines them  using logarithm rules.  It is intended to be the converse of
   :func:`LnExpand`.

   .. seealso:: :func:`LnExpand`, :func:`Simplify`, :func:`Ln`

.. function:: TrigSimpCombine(expr)

   combine products of trigonometric functions

   This function applies the product rules of trigonometry, e.g.
   :math:`\cos{u}\sin{v} = \frac{1}{2}(\sin(v-u) + \sin(v+u))`. As a result, all
   products of the trigonometric functions :func:`Cos` and :func:`Sin`
   disappear. The function also tries to simplify the resulting expression as
   much as  possible by combining all similar terms. This function is used in
   for instance :func:`Integrate`, to bring down the expression into a simpler
   form that hopefully can be  integrated easily.

   :Example:

   ::

      In> PrettyPrinter'Set("PrettyForm");
      True
      In> TrigSimpCombine(Cos(a)^2+Sin(a)^2)
      1
      In> TrigSimpCombine(Cos(a)^2-Sin(a)^2)
      Cos( -2 * a )
      Out>
      In> TrigSimpCombine(Cos(a)^2*Sin(b))
      Sin( b )   Sin( -2 * a + b )
      -------- + -----------------
         2               4
        Sin( -2 * a - b )
      - -----------------
               4


   .. seealso:: :func:`Simplify`, :func:`Integrate`, :func:`Expand`, :func:`Sin`, :func:`Cos`, :func:`Tan`

