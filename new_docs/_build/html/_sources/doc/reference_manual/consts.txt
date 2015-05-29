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

   Note that for most analytic functions Yacas understands
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

   :data:`Pi` symbolically represents the exact value of
   :math:`\pi`. When the :func:`N()` function is used, :data:`Pi`
   evaluates to a numerical value according to the current precision.
   It is better to use :data:`Pi` than ``N(Pi)`` except in numerical
   calculations, because exact simplification will be possible.

   This is a "cached constant" which is recalculated only when
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
   answer can be given. The result is then "undefined".

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

   the Golden Ratio

   These functions compute the "golden ratio"
   $$phi <=> 1.6180339887 <=> (1+Sqrt(5))/2 $$.

   The ancient Greeks defined the "golden ratio" as follows: If one
   divides a length 1 into two pieces $x$ and $1-x$, such that the
   ratio of 1 to $x$ is the same as the ratio of $x$ to $1-x$, then
   $1/x <=> 1.618$... is the "golden ratio".

   The constant is available symbolically as :data:`GoldenRatio` or
   numerically through ``N(GoldenRatio)``.  This is a "cached
   constant" which is recalculated only when precision is increased.

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

   Catalan's Constant

   These functions compute Catalan's Constant
   $Catalan<=>0.9159655941$.

   The constant is available symbolically as {Catalan} or numerically
   through {N(Catalan)} with {N(...)} the usual operator used to try
   to coerce an expression in to a numeric approximation of that
   expression.  This is a "cached constant" which is recalculated only
   when precision is increased.  The numerical value of the constant
   can also be obtained as {N(Catalan)}.  The low-level numerical
   computations are performed by the routine {CatalanConstNum}.

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

   Euler's constant :math:`\gamma`

   The constant represents the Euler's constant :math:`\gamma\approx
   0.57722...`

   The constant is available symbolically as :data:`gamma` or
   numerically through using the usual function :func:`N` to get the
   numeric result, ``N(gamma)``.  This is a "cached constant" which is
   recalculated only when precision is increased. The low-level
   numerical computations are performed by the routine
   :func:`GammaConstNum`.

   Note that Euler's :math:`\Gamma(x)` function is the capitalized
   :func:`Gamma` in Yacas.

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
