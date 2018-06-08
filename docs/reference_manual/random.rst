==============
Random numbers
==============

.. function:: Random()

   (pseudo-) random number generator

   :param init: integer, initial seed value
   :param option: atom, option name
   :param value: atom, option value
   :param r: a list, RNG object

   These commands are an object-oriented interface to (pseudo-)random
   number generators (RNGs).    {RngCreate} returns a list which is a
   well-formed RNG object.  Its value should be saved in a variable
   and used to call {Rng} and {RngSeed}.    {Rng(r)} returns a
   floating-point random number between 0 and 1 and updates the RNG
   object {r}.  (Currently, the Gaussian option makes a RNG return a
   *complex* random number instead of a real random number.)
   {RngSeed(r,init)} re-initializes the RNG object {r} with the seed
   value {init}.  The seed value should be a positive integer.    The
   {RngCreate} function accepts several options as arguments.
   Currently the following options are available:

.. function:: RandomIntegerMatrix(rows,cols,from,to)

   generate a matrix of random integers

   :param rows: number of rows in matrix
   :param cols: number of cols in matrix
   :param from: lower bound
   :param to: upper bound

   This function generates a {rows x cols} matrix of random integers.
   All  entries lie between "from" and "to", including the boundaries,
   and  are uniformly distributed in this interval.

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

.. function:: RandomIntegerVector(nr, from, to)

   generate a vector of random integers

   :param nr: number of integers to generate
   :param from: lower bound
   :param to: upper bound

   This function generates a list with "nr" random integers. All
   entries lie between "from" and "to", including the boundaries, and
   are uniformly distributed in this interval.

   :Example:

   ::

      In> RandomIntegerVector(4,-3,3)
      Out> {0,3,2,-2};


   .. seealso:: :func:`Random`, :func:`RandomPoly`

.. function:: RandomPoly(var,deg,coefmin,coefmax)

   construct a random polynomial

   :param var: free variable for resulting univariate polynomial
   :param deg: degree of resulting univariate polynomial
   :param coefmin: minimum value for coefficients
   :param coefmax: maximum value for coefficients

   RandomPoly generates a random polynomial in variable "var", of
   degree "deg", with integer coefficients ranging from "coefmin" to
   "coefmax" (inclusive). The coefficients are uniformly distributed
   in  this interval, and are independent of each other.

   :Example:

   ::

      In> RandomPoly(x,3,-10,10)
      Out> 3*x^3+10*x^2-4*x-6;
      In> RandomPoly(x,3,-10,10)
      Out> -2*x^3-8*x^2+8;


   .. seealso:: :func:`Random`, :func:`RandomIntegerVector`
