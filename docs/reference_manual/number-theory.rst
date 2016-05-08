=============
Number theory
=============

This chapter describes functions that are of interest in number
theory.  These functions typically operate on integers.  Some of these
functions work quite slowly.


.. function:: IsPrime(n)

   test for a prime number

   :param n: integer to test



.. function:: IsComposite(n)

   test for a composite number

   :param n: positive integer



.. function:: IsCoprime(m,n)

   test if integers are coprime

   :param m: positive integer
   :param n: positive integer
   :param list: list of positive integers



.. function:: IsSquareFree(n)

   test for a square-free number

   :param n: positive integer



.. function:: IsPrimePower(n)

   test for a power of a prime number

   :param n: integer to test



.. function:: NextPrime(i)

   generate a prime following a number

   :param i: integer value



.. function:: IsTwinPrime(n)

   test for a twin prime

   :param n: positive integer



.. function:: IsIrregularPrime(n)

   test for an irregular prime

   :param n: positive integer



.. function:: IsCarmichaelNumber(n)

   test for a Carmichael number

   :param n: positive integer



.. function:: Factors(x)

   factorization

   :param x: integer or univariate polynomial



.. function:: IsAmicablePair(m,n)

   test for a pair of amicable numbers

   :param m: positive integer
   :param n: positive integer



.. function:: Factor(x)

   factorization, in pretty form

   :param x: integer or univariate polynomial



.. function:: Divisors(n)

   number of divisors

   :param n: positive integer



.. function:: DivisorsSum(n)

   the sum of  divisors

   :param n: positive integer



.. function:: ProperDivisors(n)

   the number of proper divisors

   :param n: positive integer



.. function:: ProperDivisorsSum(n)

   the sum of proper divisors

   :param n: positive integer



.. function:: Moebius(n)

   the Moebius function

   :param n: positive integer



.. function:: CatalanNumber(n)

   return the ``n``-th Catalan Number

   :param n: positive integer



.. function:: FermatNumber(n)

   return the ``n``-th Fermat Number

   :param n: positive integer



.. function:: HarmonicNumber(n)

   return the ``n``-th Harmonic Number

   :param n: positive integer
   :param r: positive integer



.. function:: StirlingNumber1(n,m)

   return the ``n,m``-th Stirling Number of the first kind

   :param n: positive integers
   :param m: positive integers



.. function:: StirlingNumber1(n,m)

   return the ``n,m``-th Stirling Number of the second kind

   :param n: positive integer
   :param m: positive integer



.. function:: DivisorsList(n)

   the list of divisors

   :param n: positive integer



.. function:: SquareFreeDivisorsList(n)

   the list of square-free divisors

   :param n: positive integer



.. function:: MoebiusDivisorsList(n)

   the list of divisors and Moebius values

   :param n: positive integer



.. function:: SumForDivisors(var,n,expr)

   loop over divisors

   :param var: atom, variable name
   :param n: positive integer
   :param expr: expression depending on ``var``



.. function:: RamanujanSum(k,n)

   compute the Ramanujan's sum

   :param k: positive integer
   :param n: positive integer

   This function computes the Ramanujan's sum, i.e. the sum of the
   ``n``-th powers of the ``k``-th primitive roots of the unit:

   .. math:: \sum_{l=1}^k\frac{\exp(2ln\pi\imath)}{k}
 
   where :math:`l` runs thought the integers between 1 and ``k-1``
   that are coprime to :math:`l`. The computation is done by using the
   formula in T. M. Apostol, <i>Introduction to Analytic Theory</i>
   (Springer-Verlag), Theorem 8.6.

   .. todo:: check the definition


.. function:: PAdicExpand(n, p)

   p-adic expansion

   :param n: number or polynomial to expand
   :param p: base to expand in



.. function:: IsQuadraticResidue(m,n)

   functions related to finite groups

   :param m: integer
   :param n: odd positive integer



.. function:: GaussianFactors(z)

   factorization in Gaussian integers

   :param z: Gaussian integer



.. function:: GaussianNorm(z)

   norm of a Gaussian integer

   :param z: Gaussian integer



.. function:: IsGaussianUnit(z)

   test for a Gaussian unit

   :param z: a Gaussian integer



.. function:: IsGaussianPrime(z)

   test for a Gaussian prime

   :param z: a complex or real number



.. function:: GaussianGcd(z,w)

   greatest common divisor in Gaussian integers

   :param z: Gaussian integer
   :param w: Gaussian integer


