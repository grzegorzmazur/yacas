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

   :param expr: expression to simplify

   This function tries to simplify the expression {expr} as much  as
   possible. It does this by grouping powers within terms, and then
   grouping similar terms.

   :Example:

   ::

      In> a*b*a^2/b-a^3
      Out> (b*a^3)/b-a^3;
      In> Simplify(a*b*a^2/b-a^3)
      Out> 0;
      

   .. seealso:: :func:`TrigSimpCombine`, :func:`RadSimp`

.. function:: RadSimp(expr)

   simplify expression with nested radicals

   :param expr: an expression containing nested radicals

   This function tries to write the expression "expr" as a sum of
   roots  of integers: $Sqrt(e1) + Sqrt(e2) + ...$, where $e1$, $e2$
   and  so on are natural numbers. The expression "expr" may not
   contain  free variables.    It does this by trying all possible
   combinations for $e1$, $e2$, ...  Every possibility is numerically
   evaluated using {N} and compared with the numerical evaluation of
   "expr". If the approximations are equal (up to a certain margin),
   this possibility is returned. Otherwise, the expression is returned
   unevaluated.    Note that due to the use of numerical
   approximations, there is a small  chance that the expression
   returned by {RadSimp} is  close but not equal to {expr}. The last
   example underneath  illustrates this problem. Furthermore, if the
   numerical value of  {expr} is large, the number of possibilities
   becomes exorbitantly  big so the evaluation may take very long.

   :Example:

   ::

      In> RadSimp(Sqrt(9+4*Sqrt(2)))
      Out> Sqrt(8)+1;
      In> RadSimp(Sqrt(5+2*Sqrt(6)) \
      +Sqrt(5-2*Sqrt(6)))
      Out> Sqrt(12);
      In> RadSimp(Sqrt(14+3*Sqrt(3+2
      *Sqrt(5-12*Sqrt(3-2*Sqrt(2))))))
      Out> Sqrt(2)+3;
      But this command may yield incorrect results:
      In> RadSimp(Sqrt(1+10^(-6)))
      Out> 1;
      

   .. seealso:: :func:`Simplify`, :func:`N`

.. function:: FactorialSimplify(expression)

   Simplify hypergeometric expressions containing factorials

   :param expression: expression to simplify

   {FactorialSimplify} takes an expression that may contain
   factorials,  and tries to simplify it. An expression like $ (n+1)!
   / n! $ would  simplify to $(n+1)$.     The following steps are
   taken to simplify:

.. function:: LnExpand(expr)

   expand a logarithmic expression using standard logarithm rules

   :param expr: the logarithm of an expression

   {LnExpand} takes an expression of the form $Ln(expr)$, and applies
   logarithm  rules to expand this into multiple {Ln} expressions
   where possible.  An  expression like $Ln(a*b^n)$ would be expanded
   to $Ln(a)+n*Ln(b)$.    If the logarithm of an integer is
   discovered, it is factorised using {Factors}  and expanded as
   though {LnExpand} had been given the factorised form.  So
   $Ln(18)$ goes to $Ln(x)+2*Ln(3)$.

.. function:: LnCombine(expr)

   combine logarithmic expressions using standard logarithm rules

   :param expr: an expression possibly containing multiple {Ln} terms to be combined

   {LnCombine} finds {Ln} terms in the expression it is given, and
   combines them  using logarithm rules.  It is intended to be the
   exact converse of {LnExpand}.

.. function:: TrigSimpCombine(expr)

   combine products of trigonometric functions

   :param expr: expression to simplify

   This function applies the product rules of trigonometry, e.g.
   $Cos(u)*Sin(v) = (1/2)*(Sin(v-u) + Sin(v+u))$. As a  result, all
   products of the trigonometric functions {Cos} and {Sin} disappear.
   The function also tries to simplify the resulting expression as
   much as  possible by combining all similar terms.    This function
   is used in for instance {Integrate},  to bring down the expression
   into a simpler form that hopefully can be  integrated easily.

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

