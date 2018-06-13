==================================
Propositional logic theorem prover
==================================

.. function:: CanProve(proposition)

   try to prove statement

   :param proposition: an expression with logical operations

   Yacas has a small built-in propositional logic theorem prover. It can be
   invoked with a call to :func:`CanProve`. An example of a proposition is: "if
   a implies b and b implies c then a implies c". Yacas supports the following
   logical operations:

    ``Not``
       negation, read as "not"

    ``And``
        conjunction, read as "and"

    ``Or``
        disjunction, read as "or"
    ``=>``
        implication, read as "implies"

   The abovementioned proposition would be represented by the following
   expression::

        ( (a=>b) And (b=>c) ) => (a=>c)

   Yacas can prove that is correct by applying :func:`CanProve` to it::

        In> CanProve(( (a=>b) And (b=>c) ) => (a=>c))
        Out> True;

   It does this in the following way: in order to prove a proposition :math:`p`,
   it suffices to prove that :math:`\neg p` is false. It continues to simplify
   :math:`\neg p` using the rules:

    * :math:`\neg\neg x \to x` (eliminate double negation),
    * :math:`x\Rightarrow y \to \neg x \vee y` (eliminate implication),
    * :math:`\neg (x \wedge y) \to \neg x \vee \neg y` (`De Morgan's law`_),
    * :math:`\neg (x \vee y) \to \neg x \wedge \neg y` (`De Morgan's law`_),
    * :math:`(x \wedge y) \vee z \to (x \vee z) \wedge (y \vee z)` (distribution),
    * :math:`x \vee (y \wedge z) \to (x \vee y) \wedge (x \vee z)` (distribution),

   and the obvious other rules, such as, :math:`1 \vee x \to 1` etc. The above
   rules will translate a proposition into a form::

        (p1 Or p2 Or ...) And (q1 Or q2 Or ...) And ...

   If any of the clauses is false, the entire expression will be false. In the
   next step, clauses are scanned for situations of the form: :math:`(p \vee Y)
   \wedge (\neg p \vee Z) \to (Y \vee Z)`. If this combination :math:`(Y\vee Z)`
   is empty, it is false, and thus the entire proposition is false. As a last
   step, the algorithm negates the result again. This has the added advantage of
   simplifying the expression further.

   :Example:

   ::

      In> CanProve(a Or Not a)
      Out> True;
      In> CanProve(True Or a)
      Out> True;
      In> CanProve(False Or a)
      Out> a;
      In> CanProve(a And Not a)
      Out> False;
      In> CanProve(a Or b Or (a And b))
      Out> a Or b;


   .. seealso:: :func:`True`, :func:`False`, :func:`And`, :func:`Or`, :func:`Not`

.. _De Morgan's law: https://en.wikipedia.org/wiki/De_Morgan%27s_laws
