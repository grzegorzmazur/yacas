=========
Constants
=========

Yacas-specific constants
------------------------

.. data:: %

   previous result

   :data:`%` evaluates to the previous result on the command
   line. :data:`%` is a global variable that is bound to the previous
   result from the command line.  Using :data:`%` will evaluate the
   previous result. (This uses the functionality offered by the
   {SetGlobalLazyVariable} command).

   Typical examples are ``Simplify(%)`` and ``PrettyForm(%)`` to
   simplify and show the result in a nice form respectively.

   :Example:

   ::

      In> Taylor(x,0,5)Sin(x)
      Out> x-x^3/6+x^5/120;
      In> PrettyForm(%)
    
           3    5
          x    x
      x - -- + ---
          6    120
    
   .. seealso:: :func:`SetGlobalLazyVariable`

.. data:: EndOfFile

   end-of-file marker

   End of file marker when reading from file. If a file contains the
   expression {EndOfFile;} the operation will stop reading the file at
   that point.

Mathematical constants
----------------------

.. data:: True
          False

   boolean constants representing true and false

   :data:`True` and :data:`False` are typically a result of boolean
   expressions such as ``2 < 3`` or ``True And False``.

.. seealso:: :func:`And`, :func:`Or`, :func:`Not`

.. data:: Infinity

   constant representing mathematical infinity

   :data:`Infinity` represents infinitely large values. It can be the
   result of certain calculations.

   Note that for most analytic functions yacas understands
   :data:`Infinity` as a positive number.  Thus ``Infinity*2`` will
   return ``Infinity``, and ``a < Infinity`` will evaluate to
   :data:`True`.

   :Example:

   ::

      In> 2*Infinity
      Out> Infinity;
      In> 2<Infinity
      Out> True;

.. data:: Pi

   mathematical constant, :math:`\pi`

   The :term:`constant` represents the `number π <https://en.wikipedia.org/wiki/Pi>`_.
   It is available symbolically as ``Pi`` or numerically through ``N(Pi)``.

   This is a :term:`cached constant` which is recalculated only when
   precision is increased.

   :Example:

   ::

      In> Sin(3*Pi/2)
      Out> -1;
      In> Pi+1
      Out> Pi+1;
      In> N(Pi)
      Out> 3.14159265358979323846;

.. seealso:: :func:`Sin`, :func:`Cos`, :func:`N`, :func:`CachedConstant`

.. data:: Undefined

   constant signifying an undefined result

   :data:`Undefined` is a token that can be returned by a function
   when it considers its input to be invalid or when no meaningful
   answer can be given. The result is then undefined.

   Most functions also return :data:`Undefined` when evaluated on it.

   :Example:

   ::

      In> 2*Infinity
      Out> Infinity;
      In> 0*Infinity
      Out> Undefined;
      In> Sin(Infinity);
      Out> Undefined;
      In> Undefined+2*Exp(Undefined);
      Out> Undefined;

.. seealso:: :data:`Infinity`

.. data:: GoldenRatio

   the golden ratio

   The :term:`constant` represents the `golden ratio <https://en.wikipedia.org/wiki/Golden_ratio>`_

   .. math::
      \phi := \frac{1+\sqrt{5}}{2} = 1.6180339887\ldots

   It is available symbolically as ``GoldenRatio`` or
   numerically through ``N(GoldenRatio)``.

   This is a :term:`cached constant` which is recalculated only when precision
   is increased.

   :Example:

   ::

      In> x:=GoldenRatio - 1
      Out> GoldenRatio-1;
      In> N(x)
      Out> 0.6180339887;
      In> N(1/GoldenRatio)
      Out> 0.6180339887;
      In> V(N(GoldenRatio,20));
      
      CachedConstant: Info: constant GoldenRatio is
      being recalculated at precision 20 
      Out> 1.6180339887498948482;


.. seealso:: :func:`N`, :func:`CachedConstant`


.. data:: Catalan

   Catalan's constant

   The :term:`constant` represents the `Catalan's constant <https://en.wikipedia.org/wiki/Catalan%27s_constant>`_

   .. math::
      G := \beta(2) = \sum_{n=0}^\infty\frac{-1^n}{(2n+1)^2}=0.9159655941\ldots

   It is available symbolically as ``Catalan`` or numerically
   through ``N(Catalan)``.

   This is a :term:`cached constant` which is recalculated only
   when precision is increased.

   :Example:

   ::

      In> N(Catalan)
      Out> 0.9159655941;
      In> DirichletBeta(2)
      Out> Catalan;
      In> V(N(Catalan,20))

      CachedConstant: Info: constant Catalan is
      being recalculated at precision 20
      Out> 0.91596559417721901505;

.. seealso:: :func:`N`, :func:`CachedConstant`

.. data:: gamma

   Euler–Mascheroni constant :math:`\gamma`

   The :term:`constant` represents the `Euler–Mascheroni constant <https://en.wikipedia.org/wiki/Euler–Mascheroni_constant>`_

   .. math::
      \gamma := \lim_{n\to\infty}\left(-\ln(n)+\sum_{k=1}^n\frac{1}{k}\right)=0.5772156649\ldots

   It is available symbolically as ``gamma`` or numerically
   through ``N(gamma)``.

   This is a :term:`cached constant` which is recalculated only
   when precision is increased.

   .. note::
      Euler's :math:`\Gamma(x)` function is the capitalized :func:`Gamma` in yacas.

   :Example:

   ::

      In> gamma+Pi
      Out> gamma+Pi;
      In> N(gamma+Pi)
      Out> 3.7188083184;
      In> V(N(gamma,20))
    
      CachedConstant: Info: constant gamma is being
        recalculated at precision 20 
      GammaConstNum: Info: used 56 iterations at
        working precision 24 
      Out> 0.57721566490153286061;

.. seealso:: :func:`Gamma`, :func:`N`, :func:`CachedConstant`
