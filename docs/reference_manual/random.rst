==============
Random numbers
==============

Simple interface
----------------

.. function:: RandomSeed(seed)

   seed the global pseudo-random generator

   .. seealso:: :func:`Random`

.. function:: Random()

   generate pseudo-random number

   :func:`Random` generates a uniformly-distributed pseudo-random number
   between 0 and 1.

   .. seealso:: :func:`RandomSeed`

Advanced interface
------------------

.. function:: RngCreate()
              RngCreate(seed)
              RngCreate([seed=seed], [engine=engine], [dist=dist])
              RngCreate({seed, engine, dist})

   create a pseudo-random generator

   :func:`RngCreate` returns a list which is a well-formed RNG object.  Its
   value should be saved in a variable and used to call :func:`Rng` and
   :func:`RngSeed`.

   Engines:

    * ``default``
    * ``advanced``

   Distributions:

    * ``default`` (the same as ``flat``)
    * ``flat`` (uniform)
    * ``gauss`` (normal)

.. seealso:: :func:`Rng`, :func:`RngSeed`

.. function:: RngSeed(r, seed)

   (re)seed pseudo-random number generator

   :func:`RngSeed` re-initializes the RNG object ``r`` with the seed value
   ``seed``. The seed value should be a positive integer.

.. seealso:: :func:`RngCreate`, :func:`Rng`, :func:`RandomSeed`

.. function:: Rng(r)

   generate pseudo-random number

   :func:`Rng(r)` returns a floating-point random number between 0 and 1 and
   updates the RNG object ``r``.  (Currently, the Gaussian option makes a RNG
   return a *complex* random number instead of a real random number.)

   .. seealso:: :func:`RngCreate`, :func:`RngSeed`, :func:`Random`


Auxilliary functions
--------------------

.. function:: RandomIntegerMatrix(rows,cols,from,to)

   generate a matrix of random integers

   This function generates a ``rows x cols`` matrix of random integers.
   All  entries lie between ``from`` and ``to``, including the boundaries,
   and are uniformly distributed in this interval.

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

.. function:: RandomIntegerVector(n, from, to)

   generate a vector of random integers

   This function generates a list with ``n`` random integers. All
   entries lie between ``from`` and ``to``, including the boundaries, and
   are uniformly distributed in this interval.

   :Example:

   ::

      In> RandomIntegerVector(4,-3,3)
      Out> {0,3,2,-2};


   .. seealso:: :func:`Random`, :func:`RandomPoly`

.. function:: RandomPoly(var,deg,coefmin,coefmax)

   construct a random polynomial

   :func:`RandomPoly` generates a random polynomial in the variable ``var``, of
   degree ``deg``, with integer coefficients ranging from ``coefmin`` to
   ``coefmax`` (inclusive). The coefficients are uniformly distributed in  this
   interval, and are independent of each other.

   :Example:

   ::

      In> RandomPoly(x,3,-10,10)
      Out> 3*x^3+10*x^2-4*x-6;
      In> RandomPoly(x,3,-10,10)
      Out> -2*x^3-8*x^2+8;


   .. seealso:: :func:`Random`, :func:`RandomIntegerVector`
