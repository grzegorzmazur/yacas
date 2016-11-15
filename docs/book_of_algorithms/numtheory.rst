========================
Number theory algorithms
========================

This chapter describes the algorithms used for computing various
number-theoretic functions.  We call "number-theoretic" any function
that takes integer arguments, produces integer values, and is of
interest to number theory.

Euclidean GCD algorithms
------------------------

The main algorithm for the calculation of the GCD of two integers is
the binary Euclidean algorithm.  It is based on the following
identities: :math:`\gcd(a,b) = \gcd(b,a)`, :math:`\gcd(a,b) = \gcd(a-b,b)`,
and for odd :math:`b`, :math:`\gcd(2a,b) = \gcd(a,b)`. Thus we can produce
a sequence of pairs with the same GCD as the original two numbers, and each
pair will be at most half the size of the previous pair. The number of
steps is logarithmic in the number of digits in :math:`a`, :math:`b`. The only
operations needed for this algorithm are binary shifts and
subtractions (no modular division is necessary).  The low-level
function for this is :func:`MathGcd`.

To speed up the calculation when one of the numbers is much larger
than another, one could use the property :math:`\gcd(a,b)=\gcd(a,a \bmod b)`.
This will introduce an additional modular division into the algorithm;
this is a slow operation when the numbers are large.

Prime numbers: the Miller-Rabin test and its improvements
---------------------------------------------------------

Small prime numbers are simply stored in a precomputed table as an array of
bits; the bits corresponding to prime numbers are set to 1.  This makes
primality testing on small numbers very quick.  This is implemented by the
function :func:`FastIsPrime`.

Primality of larger numbers is tested by the function :func:`IsPrime` that
uses the Miller-Rabin algorithm. [#miller-rabin]_ This algorithm is
deterministic (guaranteed correct within a certain running time) for
small numbers :math:`n<3.4\cdot10^{13}` and probabilistic (correct with high
probability, but not guaranteed) for larger numbers.  In other words,
the Miller-Rabin test could sometimes flag a large number :math:`n` as prime
when in fact :math:`n` is composite; but the probability for this to happen
can be made extremely small. The basic reference is
:cite:`rabin1980:probabilistic_algorithm_testing`.  We also implemented some
of the improvements suggested in :cite:`davenport1992:primality_test_revisited`.

.. [#miller-rabin] Initial implementation and documentation was supplied by Christian Obrecht.

The idea of the Miller-Rabin algorithm is to improve on the Fermat
primality test. If :math:`n` is prime, then for any :math:`x` we have
:math:`\gcd(n,x)=1`. Then by `Fermat's little theorem`_,
:math:`x^{n-1}:=1 \bmod n`. (This is really a simple statement; if :math:`n` is
prime, then :math:`n-1` nonzero remainders modulo :math:`n`: :math:`1, 2, 
\ldots, n-1` form a cyclic multiplicative group.) Therefore we pick some 
"base" integer :math:`x` and compute :math:`x^{n-1} \bmod n`; this is a quick
computation even if :math:`n` is large. If this value is not equal to 1 for 
some base :math:`x`, then :math:`n` is definitely not prime.  However, we
cannot test *every* base :math:`x<n`; instead we test only some :math:`x`, so
it may happen that we miss the right values of :math:`x` that would expose the
non-primality of :math:`n`.  So Fermat's test sometimes fails, i.e. says
that :math:`n` is a prime when :math:`n` is in fact not a prime.  Also there
are infinitely many integers called `Carmichael numbers`_ which are not
prime but pass the Fermat test for every base.

.. _Fermat's little theorem: https://en.wikipedia.org/wiki/Fermat%27s_little_theorem
.. _Carmichael numbers: https://en.wikipedia.org/wiki/Carmichael_number

The Miller-Rabin algorithm improves on this by using the property that
for prime :math:`n` there are no nontrivial square roots of unity in the
ring of integers modulo :math:`n` (this is Lagrange's theorem). In other
words, if :math:`x^2:=1 \bmod n` for some :math:`x`, then :math:`x` must
be equal to 1 or -1 modulo :math:`n`. (Since :math:`n-1` is equal to -1
modulo :math:`n`, we have :math:`n-1` as a trivial square root of unity
modulo :math:`n`.  Note that even if :math:`n` is prime there may be
nontrivial divisors of 1, for example, :math:`2\dot49:=1 \bmod 97`.)

We can check that :math:`n` is odd before applying any primality test. (A
test :math:`n^2:=1 \bmod 24` guarantees that :math:`n` is not divisible by
2 or 3.  For large :math:`n` it is faster to first compute :math:`n \bmod 24`
rather than :math:`n^2`, or test :math:`n` directly.)  Then we note that in
Fermat's test the number :math:`n-1` is certainly a composite number because
:math:`n-1` is even. So if we first find the largest power of 2 in :math:`n-1`
and decompose :math:`n-1=2^rq` with :math:`q` odd, then
:math:`x^{n-1}:=a^{2^r}\bmod n` where :math:`a:=x^q \bmod n`. (Here
:math:`r\ge 1` since :math:`n` is odd.) In other words,
the number :math:`x^{n-1}\bmod n` is obtained by repeated squaring of the
number :math:`a`.  We get a sequence of :math:`r` repeated squares:
:math:`a, a^2,\ldots,a^{2^r}`.  The last element of this sequence must be 1 if
:math:`n` passes the Fermat test.  (If it does not pass, :math:`n` is
definitely a composite number.)  If :math:`n` passes the Fermat test, the
last-but-one element :math:`a^{2^{r-1}}` of the sequence of squares is a
square root of unity modulo :math:`n`.  We can check whether this square
root is non-trivial (i.e. not equal to 1 or -1 modulo :math:`n`). If it is
non-trivial, then :math:`n` definitely cannot be a prime. If it is trivial
and equal to 1, we can check the preceding element, and so on. If an
element is equal to -1, we cannot say anything, i.e. the test passes
(:math:`n` is "probably a prime").
 
This procedure can be summarized like this:

1. Find the largest power of 2 in :math:`n-1` and an odd number :math:`q`
   such that :math:`n-1=2^rq`.
2. Select the "base number" :math:`x<n`. Compute the sequence
   :math:`a:=x^q \bmod n`, :math:`a^2, a^4,\ldots, a^{2^r}` by repeated
   squaring modulo :math:`n`. This sequence contains at least two elements
   since :math:`r\ge 1`.
3. If :math:`a=1` or :math:`a=n-1`, the test passes on the base number
   :math:`x`. Otherwise, the test passes if at least one of the elements of
   the sequence is equal to :math:`n-1` and fails if none of them are equal
   to :math:`n-1`. This simplified procedure works because the first element
   that is equal to 1 *must* be preceded by a -1, or else we would find a
   nontrivial root of unity.

Here is a more formal definition. An odd integer :math:`n` is called
*strongly-probably-prime* for base :math:`b` if :math:`b^q:=1 \bmod n` or
:math:`b^{q2^i}:=n-1 \bmod n` for some :math:`i` such that :math:`0\le i < r`,
where :math:`q` and :math:`r` are such that :math:`q` is odd and
:math:`n-1 = q2^r`.

A practical application of this procedure needs to select particular
base numbers.  It is advantageous (according to
:cite:`pomerance1980:pseudoprimes` to choose *prime* numbers :math:`b`
as bases, because for a composite base :math:`b=pq`, if :math:`n` is
a strong pseudoprime for both :math:`p` and :math:`q`, then it is very
probable that :math:`n` is a strong pseudoprime also for :math:`b`,
so composite bases rarely give new information.

An additional check suggested by :cite:`davenport1992:primality_test_revisited`
is activated if :math:`r>2` (i.e. if :math:`n:=1\bmod 8` which is true for
only 1/4 of all odd numbers).  If :math:`i\ge 1` is found such that
:math:`b^{q2^i}:=n-1\bmod n`, then :math:`b^{q2^{i-1}}` is a square root
of -1 modulo :math:`n`. If :math:`n` is prime, there may be only two different
square roots of -1.  Therefore we should store the set of found values
of roots of -1; if there are more than two such roots, then we will find
some roots :math:`s_1`, :math:`s_2` of -1 such that
:math:`s_1+s_2\ne 0 \bmod n`. But :math:`s_1^2-s_2^2:=0 \bmod n`.
Therefore :math:`n` is definitely composite, e.g. :math:`\gcd(s_1+s_2,n)>1`.
This check costs very little computational effort but guards against some
strong pseudoprimes.

Yet another small improvement comes from :cite:`damgard1993:average_case_error`.
They found that the strong primality test sometimes (rarely) passes on
composite numbers :math:`n` for more than 1/8 of all bases :math:`x<n`
if :math:n` is such that either :math:`3n+1` or :math:`8n+1` is a perfect
square, or if :math:`n` is a Carmichael number. Checking Carmichael numbers
is slow, but it is easy to show that if :math:`n` is a large enough prime
number, then neither :math:`3n+1`, nor :math:`8n+1`, nor any :math:`sn+1`
with small integer :math:`s` can be a perfect square.  [If :math:`sn+1=r^2`,
then :math:`sn=(r-1)(r+1)`.]  Testing for a perfect square is quick and does
not slow down the algorithm. This is however not implemented in yacas because
it seems that perfect squares are too rare for this improvement to be
significant.

If an integer is not "strongly-probably-prime" for a given base :math:`b`,
then it is a composite number.  However, the converse statement is
false, i.e. "strongly-probably-prime" numbers can actually be
composite.  Composite strongly-probably-prime numbers for base :math:`b` are
called *strong pseudoprimes* for base :math:`b`. There is a theorem
that if :math:`n` is composite, then among all numbers :math:`b` such that
:math:`1 < b < n`, at most one fourth are such that :math:`n` is a strong
pseudoprime for base :math:`b`.  Therefore if :math:`n` is 
strongly-probably-prime for many bases, then the probability for :math:`n`
to be composite is very small.

For numbers less than :math:`B=34155071728321`, exhaustive [#exhaustive]_
computations have shown that there are no strong
pseudoprimes simultaneously for bases 2, 3, 5, 7, 11, 13 and 17. This
gives a simple and reliable primality test for integers below :math:`B`.
If :math:`n\ge B`, the Rabin-Miller method consists of checking if
:math:`n` is strongly-probably-prime for :math:`k` base numbers :math:`b`.
The base numbers are chosen to be consecutive "weak pseudoprimes" that are
easy to generate (see below the function :func:`NextPseudoPrime`).

.. [#exhaustive] And surely exhausting.

In the implemented routine :func:`RabinMiller`, the number of bases :math:`k`
is chosen to make the probability of erroneously passing the test
:math:`p < 10^{-25}`. (Note that this is *not* the same as the probability
to give an incorrect answer, because all numbers that do not pass the
test are definitely composite.) The probability for the test to pass
mistakenly on a given number is found as follows.  Suppose the number
of bases :math:`k` is fixed. Then the probability for a given composite
number to pass the test is less than :math:`p_f=4^{-k}`. The probability
for a given number :math:`n` to be prime is roughly
:math:`p_p=\frac{1}{\ln{n}}` and to be composite
:math:`p_c=1-\frac{1}{\ln{n}}`. Prime numbers never fail the test.
Therefore, the probability for the test to pass is :math:`p_fp_c+p_p`
and the probability to pass erroneously is

.. math:: p = \frac{p_fp_c}{p_fp_c+p_p} < 4^{-k}\ln(n).

To make :math:`p<\epsilon`, it is enough to select
:math:`k=\frac{1}{\ln{4}(\ln{n}-\ln{\epsilon})}`.

Before calling :func:`MillerRabin`, the function :func:`IsPrime` performs two
quick checks: first, for :math:`n\ge4` it checks that :math:`n` is not divisible by
2 or 3 (all primes larger than 4 must satisfy this); second, for
:math:`n>257`, it checks that :math:`n` does not contain small prime factors
:math:`p\le257`.  This is checked by evaluating the GCD of :math:`n` with the
precomputed product of all primes up to 257.  The computation of the
GCD is quick and saves time in case a small prime factor is present.

There is also a function :func:`NextPrime` that returns the smallest
prime number larger than :math:`n`.  This function uses a sequence
:math:`5,7,11,13,\ldots` generated by the function :func:`NextPseudoPrime`.
This sequence contains numbers not divisible by 2 or 3 (but perhaps
divisible by 5,7,...). The function :func:`NextPseudoPrime` is very fast
because it does not perform a full primality test.

The function :func:`NextPrime` however does check each of these pseudoprimes
using :func:`IsPrime` and finds the first prime number.


Factorization of integers
-------------------------

When we find from the primality test that an integer :math:`n` is composite,
we usually do not obtain any factors of :math:`n`.  Factorization is
implemented by functions :func:`Factor` and :func:`Factors`.  Both functions
use the same algorithms to find all prime factors of a given integer :math:`n`.
(Before doing this, the primality checking algorithm is used to detect
whether :math:`n` is a prime number.)  Factorization consists of repeatedly
finding a factor, i.e. an integer :math:`f` such that :math:`n\bmod f=0`, and
dividing :math:`n` by :math:`f`.  (Of course, each fastor :math:`f` needs to be
factorized too.)

small prime factors
^^^^^^^^^^^^^^^^^^^

First we determine whether the number :math:`n` contains "small" prime
factors :math:`p\le257`. A quick test is to find the GCD of :math:`n` and the
product of all primes up to 257: if the GCD is greater than 1, then
:math:`n` has at least one small prime factor. (The product of primes is
precomputed.) If this is the case, the trial division algorithm is
used: :math:`n` is divided by all prime numbers :math:`p\le257` until a factor is
found. :func:`NextPseudoPrime` is used to generate the sequence of candidate
divisors :math:`p`.

checking for prime powers
^^^^^^^^^^^^^^^^^^^^^^^^^

After separating small prime factors, we test whether the number :math:`n`
is an integer power of a prime number, i.e. whether :math:`n=p^s` for some
prime number :math:`p` and an integer :math:`s\ge1`. This is tested by the
following algorithm. We already know that :math:`n` is not prime and that
:math:`n` does not contain any small prime factors up to 257. Therefore if
:math:`n=p^s`, then :math:`p>257` and :math:`2\le s<s_0=\frac{\ln{n}}{\ln{257}}`.
In other words, we only need to look for powers not greater than :math:`s_0.`
This number can be approximated by the "integer logarithm" of :math:`n` in
base 257 (routine ``IntLog(n, 257)``).

Now we need to check whether :math:`n` is of the form :math:`p^s` for
:math:`s=2,3,\ldots,s_0`. Note that if for example :math:`n=p^{24}` for some
:math:`p`, then the square root of :math:`n` will already be an integer,
:math:`n^\frac{1}{2}=p^{12}`. Therefore it is enough to test whether
:math:`n^\frac{1}{s}` is an integer for all *prime* values of :math:`s` up to
:math:`s_0`, and then we will definitely discover whether :math:`n` is a power
of some other integer. The testing is performed using the integer :math:`n`-th
root function :func:`IntNthRoot` which quickly computes the integer part of
:math:`n`-th root of an integer number. If we discover that :math:`n` has an
integer root :math:`p` of order :math:`s`, we have to check that :math:`p`
itself is a prime power (we use the same algorithm recursively). The number
:math:`n` is a prime power if and only if :math:`p` is itself a prime power.
If we find no integer roots of orders :math:`s\le s_0`, then :math:`n` is not
a prime power.

Pollard's rho algorithm
^^^^^^^^^^^^^^^^^^^^^^^

If the number :math:`n` is not a prime power, the `Pollard's rho algorithm`_
is applied :cite:`pollard1975:theory_algebraic_numbers`. The Pollard rho
algorithm takes an irreducible polynomial, e.g. :math:`p(x)=x^2+1` and builds
a sequence of integers :math:`x_{k+1}:=p(x_k)\bmod n`, starting from
:math:`x_0=2`. For each :math:`k`, the value :math:`x_{2k}-x_k` is attempted
as possibly containing a common factor with :math:`n`. The GCD of
:math:`x_{2k}-x_k` with :math:`n` is computed, and if
:math:`\gcd(x_{2k}-x_k,n)>1`, then that GCD value divides :math:`n`.

.. _Pollard's rho algorithm: https://en.wikipedia.org/wiki/Pollard%27s_rho_algorithm

The idea behind the rho algorithm is to generate an effectively
random sequence of trial numbers :math:`t_k` that may have a common factor
with :math:`n`. The efficiency of this algorithm is determined by the size
of the smallest factor :math:`p` of :math:`n`. Suppose :math:`p` is the
smallest prime factor of :math:`n` and suppose we generate a random sequence
of integers :math:`t_k` such that :math:`1\leq t_k<n`. It is clear that, on
the average, a fraction :math:`\frac{1}{p}` of these integers will be divisible
by :math:`p`. Therefore (if :math:`t_k` are truly random) we should need on
the average :math:`p` tries until we find :math:`t_k` which is accidentally
divisible by :math:`p`. In practice, of course, we do not use a truly random
sequence and the number of tries before we find a factor :math:`p` may be
significantly different from :math:`p`. The quadratic polynomial seems to
help reduce the number of tries in most cases.

But the Pollard "rho" algorithm may actually enter an infinite loop
when the sequence :math:`x_k` repeats itself without giving any factors of
:math:`n`. For example, the unmodified rho algorithm starting from
:math:`x_0=2` loops on the number 703. The loop is detected by comparing
:math:`x_{2k}` and :math:`x_k`. When these two quantities become equal to each
other for the first time, the loop may not yet have occurred so the
value of GCD is set to 1 and the sequence is continued. But when the
equality of :math:`x_{2k}` and :math:`x_k` occurs many times, it indicates that
the algorithm has entered a loop. A solution is to randomly choose a
different starting number :math:`x_0` when a loop occurs and try factoring
again, and keep trying new random starting numbers between 1 and :math:`n`
until a non-looping sequence is found. The current implementation
stops after 100 restart attempts and prints an error message, "failed
to factorize number".

A better (and faster) integer factoring algorithm needs to be
implemented in yacas.

overview of algorithms
^^^^^^^^^^^^^^^^^^^^^^

Modern factoring algorithms are all probabilistic (i.e. they do not
guarantee a particular finishing time) and fall into three categories:

1. Methods that work well (i.e. quickly) if there is a relatively
   small factor :math:`p` of :math:`n` (even if :math:`n` itself is large).
   Pollard's rho algorithm belongs to this category. The fastest in this
   category is `Lenstra's elliptic curves method`_ (ECM).
2. Methods that work equally quickly regardless of the size of
   factors (but slower with larger :math:`n`). These are the continued
   fractions method and the various sieve methods. The current best
   is the `General Number Field Sieve`_ (GNFS) but it is quite a
   complicated algorithm requiring operations with high-order algebraic
   numbers. The next best one is the `Multiple Polynomial Quadratic
   Sieve`_ (MPQS).
3. Methods that are suitable only for numbers of special
   interesting form, e.g. Fermat numbers :math:`2^{2^k}-1` or generally
   numbers of the form :math:`r^s+a` where :math:`s` is large but :math:`r`
   and :math:`a` are very small integers. The best method seems to be the
   `Special Number Field Sieve`_ which is a faster variant of the GNFS adapted
   to the problem.

.. _Lenstra's elliptic curves method: https://en.wikipedia.org/wiki/Lenstra_elliptic_curve_factorization
.. _General Number Field Sieve: https://en.wikipedia.org/wiki/General_number_field_sieve
.. _Multiple Polynomial Quadratic Sieve: http://www.mersennewiki.org/index.php/Multiple_polynomial_quadratic_sieve
.. _Special Number Field Sieve: https://en.wikipedia.org/wiki/Special_number_field_sieve

There is ample literature describing these algorithms.

The Jacobi symbol
-----------------

A number :math:`m` is a *quadratic residue modulo* :math:`n` if there exists a
number :math:`k` such that :math:`k^2:=m\bmod n`.

The `Legendre symbol`_ :math:`(\frac{m}{n})` is defined as :math:`+1` if 
:math:`m` is a quadratic residue modulo :math:`n` and :math:`-1` if it is a
non-residue. The Legendre symbol is equal to :math:`0` if :math:`\frac{m}{n}`
is an integer.

.. _Legendre symbol: https://en.wikipedia.org/wiki/Legendre_symbol

The `Jacobi symbol` :math:`(\frac{m}{n})` is defined as the product of the
Legendre symbols of the prime factors :math:`f_i` of 
:math:`n=f_1^{p_1}\ldots f_s^{p_s}`

.. math:: \left(\frac{m}{n}\right) := \left(\frac{m}{f_1}\right)^{p_1}\ldots \left(\frac{m}{f}\right)^{p_s}

(Here we used the same notation :math:`(\frac{a}{b})` for the Legendre and the
Jacobi symbols; this is confusing but seems to be the current practice.)  The
Jacobi symbol is equal to :math:`0` if :math:`m`, :math:`n` are not mutually
prime (have a common factor). The Jacobi symbol and the Legendre symbol have
values :math:`+1`, :math:`-1` or :math:`0`.

.. _Jacobi symbol: https://en.wikipedia.org/wiki/Jacobi_symbol

The Jacobi symbol can be efficiently computed without knowing the full
factorization of the number :math:`n`.  The currently used method is based
on the following identities for the Jacobi symbol:

1. :math:`(\frac{a}{1}) = 1`,
2. :math:`(\frac{2}{b}) = (-1)^{\frac{b^2-1}{8}}`,
3. :math:`(\frac{ab}{c}) = (\frac{a}{c})(\frac{b}{c})`,
4. If :math:`a:=b\bmod c`, then :math:`(\frac{a}{c})=(\frac{b}{c})`,
5. If :math:`a`, :math:`b` are both odd, then
   :math:`(\frac{a}{b})=(\frac{b}{a}) (-1)^\frac{(a-1)(b-1)}{4}`.

Using these identities, we can recursively reduce the computation of
the Jacobi symbol :math:`(\frac{a}{b})` to the computation of the Jacobi
symbol for numbers that are on the average half as large.  This is similar
to the fast binary Euclidean algorithm for the computation of the GCD.  The
number of levels of recursion is logarithmic in the arguments :math:`a`,
:math:`b`.

More formally, Jacobi symbol :math:`(\frac{a}{b})` is computed by the following
algorithm.  (The number :math:`b` must be an odd positive integer, otherwise
the result is undefined.)

1. If :math:`b=1`, return :math:`1` and stop. If :math:`a=0`, return :math:`0`
   and stop. Otherwise, replace :math:`(\frac{a}{b})` by
   :math:`(\frac{a\bmod b}{b})` (identity 4).
2. Find the largest power of :math:`2` that divides :math:`a`. Say, 
   :math:`a=2^{sc}` where :math:`c` is odd.  Replace :math:`(\frac{a}{b})` by
   :math:`(\frac{c}{b})(-1)^\frac{s(b^2-1)}{8}`
   (identities 2 and 3).
3. Now that :math:`c<b`, replace :math:`(\frac{c}{b})` by 
   :math:`(\frac{b}{c})(-1)^\frac{(b-1)(c-1)}{4}` (identity 5).
4. Continue to step 1.

Note that the arguments :math:`a`, :math:`b` may be very large integers and we
should avoid performing multiplications of these numbers.  We can
compute :math:`(-1)^\frac{(b-1)(c-1)}{4}` without multiplications. This
expression is equal to :math:`1` if either :math:`b` or :math:`c` is equal to 1
mod 4; it is equal to :math:`-1` only if both :math:`b` and :math:`c` are equal
to 3 mod 4. Also, :math:`(-1)^\frac{b^2-1}{8}` is equal to :math:`1` if either
:math:`b=1` or :math:`b=7` mod 8, and it is equal to :math:`-1` if :math:`b=3`
or :math:`b=5` mod 8.  Of course, if :math:`s` is even, none of this needs
to be computed.


Integer partitions
------------------

partitions of an integer
^^^^^^^^^^^^^^^^^^^^^^^^

A partition of an integer :math:`n` is a way of writing :math:`n` as the sum of
positive integers, where the order of these integers is unimportant.
For example, there are 3 ways to write the number 3 in this way:
:math:`3=1+1+1`, :math:`3=1+2`, :math:`3=3`.  The function :func:`PartitionsP`
counts the number of such partitions.

partitions of an integer by Rademacher-Hardy-Ramanujan series
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

Large :math:`n`

The first algorithm used to compute this function uses the
Rademacher-Hardy-Ramanujan (RHR) theorem and is efficient for large
:math:`n`.  (See for example [Ahlgren <i>et al.</i> 2001].)  The number of
partitions :math:`P(n)` is equal to an infinite sum:

.. math:: P(n) = \frac{1}{\pi\sqrt{2}}\sum_{k=1}^{\infty}\sqrt{k}A(k,n)S(k,n),

where the functions :math:`A` and :math:`S` are defined as follows:

.. math:: S(k,n) := \frac{d}{dn} \frac{\sinh(\frac{\pi}{k}\sqrt{\frac{2}{3}(n-\frac{1}{24})})}{\sqrt{n-\frac{1}{24}}}

.. math:: A(k,n) := \sum_{l=1}^{k} \delta_{\gcd(l,k),1}\exp(-2\pi i \frac{ln}{k}+\pi i B(k,l)),

where :math:`\delta_{x,y}` is the Kronecker delta function (so that the
summation goes only over integers :math:`l` which are mutually prime with
:math:`k`) and :math:`B` is defined by 

.. math:: B(k,l) := \sum_{j=1}^{k-1}\frac{j}{k}\left(\frac{lj}{k}-\left\lfloor\frac{lj}{k}\right\rfloor-\frac{1}{2}\right).

The first term of the series gives, at large :math:`n`, the Hardy-Ramanujan
asymptotic estimate,

.. math:: P(n) \sim P_0(n) := \frac{1}{4n\sqrt{3}}\exp\left(\pi\sqrt{\frac{2n}{3}}\right).

The absolute value of each term decays quickly, so after :math:`O(\sqrt{n})`
terms the series gives an answer that is very close to the integer result.

There exist estimates of the error of this series, but they are
complicated.  The series is sufficiently well-behaved and it is easier
to determine the truncation point heuristically.  Each term of the
series is either 0 (when all terms in :math:`A(k,n)` happen to cancel) or
has a magnitude which is not very much larger than the magnitude of
the previous nonzero term.  (But the series is not actually
monotonic.)  In the current implementation, the series is truncated
when :math:`|A(k,n)S(n)\sqrt{k}|` becomes smaller than :math:`0.1` for the
first time; in any case, the maximum number of calculated terms is
:math:`5+\frac{\sqrt{n}}{2}`.  One can show that asymptotically for large
:math:`n`, the required number of terms is less than
:math:`\frac{\mu}{\ln{\mu}}`, where :math:`\mu:=\pi\sqrt{\frac{2n}{3}}`.

[Ahlgren <i>et al.</i> 2001] mention that there exist explicit
constants :math:`B_1` and :math:`B_2` such that

.. math:: |P(n)-\sum_{k=1}^{B_1\sqrt{n}}A(k,n))| < B_2n^{-\frac{1}{4}}.

The floating-point precision necessary to obtain the integer result
must be at least the number of digits in the first term :math:`P_0(n)`, i.e.

.. math:: Prec > \frac{\pi\sqrt{\frac{2n}{3}}-\ln(4n\sqrt{3})}{\ln(10)}.

However, :program:`yacas` currently uses the fixed-point precision model.
Therefore, the current implementation divides the series by :math:`P_0(n)`
and computes all terms to :math:`Prec` digits.

The RHR algorithm requires :math:`O\left(\left(\frac{n}{\ln(n)}\right)^\frac{3}{2}\right)`
operations, of which :math:`O(\frac{n}{\ln(n)})` are long multiplications at
precision :math:`Prec\sim O(\sqrt{n})` digits.  The computational cost is
therefore :math:`O(\frac{n}{\ln(n)}M(\sqrt{n}))`.

partitions of an integer by recurrence relation
"""""""""""""""""""""""""""""""""""""""""""""""

Small :math:`n`

The second, simpler algorithm involves a recurrence relation

.. math:: P_n = \sum_{k=1}^n (-1)^{k+1}(P_{n-\frac{k(3k-1)}{2}}+P_{n-\frac{k(3k+1)}{2}}).

The sum can be written out as

.. math:: P_{n-1}+P_{n-2}-P_{n-5}-P_{n-7}+\ldots,

where :math:`1, 2, 5, 7, \ldots` is the `generalized pentagonal sequence`_
generated by the pairs :math:`\frac{k(3k-1)}{2}`, :math:`\frac{k(3k+1)}{2}`
for :math:`k=1,2,\ldots`. The recurrence starts from :math:`P_0=1`,
:math:`P_1=1`.  (This is implemented as :func:`PartitionsP'recur`.)

.. _generalized pentagonal sequence: https://oeis.org/A001318

The sum is actually not over all :math:`k` up to :math:`n` but is truncated when
the pentagonal sequence grows above :math:`n`.  Therefore, it contains only
:math:`O(\sqrt{n})` terms.  However, computing :math:`P_n` using the recurrence
relation requires computing and storing :math:`P_k` for all
:math:`1\le k\le n`. No long multiplications are necessary, but the number
of long additions of numbers with :math:`Prec\sim O(\sqrt{n})` digits is
:math:`O(n^\frac{3}{2})`. Therefore the computational cost is :math:`O(n^2)`.
This is asymptotically slower than the RHR algorithm even if a slow
:math:`O(n^2)` multiplication is used. With internal yacas math, the recurrence
relation is faster for :math:`n<300` or so, and for larger :math:`n` the RHR
algorithm is faster.


Miscellaneous functions
-----------------------

divisors
^^^^^^^^

The function :func:`Divisors` currently returns the number of divisors of
integer, while :func:`DivisorsSum` returns the sum of these divisors.  (The
current algorithms need to factor the number.) The following theorem
is used:

Let :math:`p_1^{k_1}\ldots p_r^{k_r}` be the prime factorization of :math:`n`,
where :math:`r` is the number of prime factors and :math:`k_i` is the
multiplicity of the :math:`i`-th factor. Then 

.. math::

   \mathrm{Divisors}(n) =(k_1+1)\ldots(k_r+1)

.. math::
   \mathrm{DivisorsSum}(n) = \frac{p_1^{k_1+1} -1}{p_1-1}\ldots\frac{p_r^{k_r+1} -1}{p_r-1}

proper divisors
"""""""""""""""

The functions :func:`ProperDivisors` and :func:`ProperDivisorsSum` are
functions that do the same as the above functions, except they do not consider
the number :math:`n` as a divisor for itself.  These functions are defined
by:

.. math::

   \mathrm{ProperDivisors}(n) := \mathrm{Divisors}(n) - 1;

.. math::
   \mathrm{ProperDivisorsSum}(n) := \mathrm{DivisorsSum}(n) - n;

Another number-theoretic function is :func:`Moebius`, defined as follows:
:math:`\mathrm{Moebius}(n)=(-1)^r` if no factors of :math:`n` are repeated,
:math:`\mathrm{Moebius}(n)=0` if some factors are repeated, and
:math:`\mathrm{Moebius}(n)=1` if :math:`n = 1`. This again requires to factor
the number :math:`n` completely and investigate the properties of its prime
factors. From the definition, it can be seen that if :math:`n` is prime,
then :math:`\mathrm{Moebius}(n) = -1`. The predicate :func:`IsSquareFree` then
reducess to :math:`\mathrm{Moebius}(n)\ne0`, which means that no factors of
:math:`n` are repeated.


Gaussian integers
-----------------

A *Gaussian integer* is a complex number of the form :math:`z =
a+b*\imath`, where :math:`a` and :math:`b` are ordinary (rational) integers.
[#rational_integers]_ The ring of Gaussian integers is usually denoted by
:math:`\mathbb{Z}[\imath]` in the mathematical literature. It is an example
of a ring of algebraic integers.

.. [#rational_integers] To distinguish ordinary integers from Gaussian
   integers, the ordinary integers (with no imaginary part) are called
   *rational integers*.

The function :func:`GaussianNorm` computes the norm :math:`N(z)=a^2+b^2` of
:math:`z`. The norm plays a fundamental role in the arithmetic of Gaussian
integers, since it has the multiplicative property

.. math:: N(zw) = N(z)N(w).

A unit of a ring is an element that divides any other element of the
ring.  There are four units in the Gaussian integers: :math:`1`, :math:`-1`,
:math:`\imath`, :math:`-\imath`. They are exactly the Gaussian integers whose
norm is :math:`1`. The predicate :func:`IsGaussianUnit` tests for a Gaussian
unit.

Two Gaussian integers :math:`z` and :math:`w` are *associated* is
:math:`\frac{z}{w}` is a unit. For example, :math:`2+\imath` and
:math:`-1+2\imath` are associated.

A Gaussian integer is called *prime* if it is only divisible by the
units and by its associates. It can be shown that the primes in the
ring of Gaussian integers are:

1. :math:`1+\imath` and its associates.
2. The rational (ordinary) primes of the form :math:`4n+3`.
3. The factors :math:`a+b\imath` of rational primes :math:`p` of the form
   :math:`p=4n+1`, whose norm is :math:`p=a^2+b^2`.

For example, :math:`7` is prime as a Gaussian integer, while :math:`5` is not,
since :math:`5 = (2+\imath)(2-\imath)`.  Here :math:`2+\imath` is a Gaussian
prime.

Factors
^^^^^^^

The ring of Gaussian integers is an example of an Euclidean ring,
i.e. a ring where there is a division algorithm.  This makes it
possible to compute the greatest common divisor using Euclid's
algorithm. This is what the function :func:GaussianGcd` computes.

As a consequence, one can prove a version of the fundamental theorem
of arithmetic for this ring: The expression of a Gaussian integer as a
product of primes is unique, apart from the order of primes, the
presence of units, and the ambiguities between associated primes.

The function :func:`GaussianFactors` finds this expression of a Gaussian
integer :math:`z` as the product of Gaussian primes, and returns the result
as a list of pairs :math:`(p,e)`, where :math:`p` is a Gaussian prime and
:math:`e` is the corresponding exponent.  To do that, an auxiliary function
called :func:`GaussianFactorPrime` is used. This function finds a factor of a
rational prime of the form :math:`4n+1`. We compute :math:`a := (2n)!\bmod p`.
By Wilson's theorem :math:`a^2` is congruent to :math:`-1` (mod :math:`p`),
and it follows that :math:`p` divides :math:`(a+\imath)(a-\imath)=a^2+1` in
the Gaussian integers. The desired factor is then the :func:`GaussianGcd` of
:math:`a+\imath` and :math:`p`. If the result is :math:`a+b\imath`, then
:math:`p=a^2+b^2`.

If :math:`z` is a rational (i.e. real) integer, we factor :math:`z` in the
Gaussian integers by first factoring it in the rational integers, and
after that by factoring each of the integer prime factors in the
Gaussian integers.
 
If :math:`z` is not a rational integer, we find its possible Gaussian prime
factors by first factoring its norm :math:`N(z)` and then computing the
exponent of each of the factors of :math:`N(z)` in the decomposition of
:math:`z`.

A simple factorization algorithm for univariate polynomials
-----------------------------------------------------------

This section discusses factoring polynomials using arithmetic modulo
prime numbers. Information was used from
:cite:`knuth1997:acp_seminumerical_algorithms` and
:cite:`davenport1988:computer_algebra`.

A simple factorization algorithm is developed for univariate
polynomials. This algorithm is implemented as the function
:func:`BinaryFactors`. The algorithm was named the binary factoring
algorithm since it determines factors to a polynomial modulo :math:`2^n` for
successive values of :math:`n`, effectively adding one binary digit to the
solution in each iteration. No reference to this algorithm has been
found so far in literature.

Berlekamp showed that polynomials can be efficiently factored when
arithmetic is done modulo a prime. The `Berlekamp algorithm`_ is only
efficient for small primes, but after that `Hensel lifting`_ can be used
to determine the factors modulo larger numbers.

.. _Berlekamp algorithm: https://en.wikipedia.org/wiki/Berlekamp%27s_algorithm
.. _Hensel lifting: https://en.wikipedia.org/wiki/Hensel%27s_lemma

The algorithm presented here is similar in approach to applying the
Berlekamp algorithm to factor modulo a small prime, and then factoring
modulo powers of this prime (using the solutions found modulo the
small prime by the Berlekamp algorithm) by applying Hensel lifting.
However it is simpler in set up. It factors modulo 2, by trying all
possible factors modulo 2 (two possibilities, if the polynomial is
monic). This performs the same action usually left to the Berlekamp
step. After that, given a solution modulo :math:`2^n`, it will test for a
solution :math:`f_i` modulo :math:`2^n` if :math:`f_i` or :math:`f_i + 2^n`
are a solution modulo :math:`2^{n+1}`.

This scheme raises the precision of the solution with one digit in
binary representation. This is similar to the linear Hensel lifting
algorithm, which factors modulo :math:`p^n` for some prime :math:`p`, where
:math:`n` increases by one after each iteration. There is also a quadratic
version of Hensel lifting which factors modulo :math:`p^{2^n}`, in effect
doubling the number of digits (in :math:`p`-adic expansion) of the solution
after each iteration. However, according to Davenport, the quadratic
algorithm is not necessarily faster.

The algorithm here thus should be equivalent in complexity to Hensel
lifting linear version. This has not been verified yet.


Modular arithmetic
------------------

This section copies some definitions and rules from <I>The Art of
Computer Programming, Volume 1, Fundamental Algorithms </I> regarding
arithmetic modulo an integer.

Arithmetic modulo an integer :math:`p` requires performing the arithmetic
operation and afterwards determining that integer modulo :math:`p`. A number
:math:`x` can be written as

.. math:: x=qp+r

where :math:`q` is called the quotient, and :math:`r` remainder.  There is some
liberty in the range one chooses :math:`r` to be in. If :math:`r` is an integer
in the range :math:`0,1,\ldots,p-1` then it is the *modulo*,
:math:`r = x \bmod p`.

When :math:`x\bmod p = y\bmod p`, the notation :math:`x=y\pmod p` is used. All
arithmetic calculations are done modulo an integer :math:`p` in that case.

For calculations modulo some :math:`p` the following rules hold:

* If :math:`a=b\pmod p` and :math:`x=y\pmod p`, then :math:`ax=by\pmod p`,
  :math:`a+x=b+y\pmod p`, and :math:`a-x=b-y\pmod p`.  This means that for
  instance also :math:`x^n\bmod p = (x\bmod p)^n\bmod p`.
* Two numbers :math:`x` and :math:`y` are *relatively prime* if they don't
  share a common factor, that is, if their greatest common denominator
  is one, :math:`\gcd(x,y)=1`.
* If :math:`ax=by\pmod p` and if :math:`a=b\pmod p`, and if :math:`a` and 
  :math:`p` are relatively prime, then :math:`x=y\pmod p`.  This is useful
  for dividing out common factors.
* :math:`a=b\pmod p` if and only if :math:`an=bn\pmod np` when :math:`n\ne0`.
  Also, if :math:`r` and :math:`s` are relatively prime, then
  :math:`a=b\pmod rs` only if :math:`a=b\pmod r` and :math:`a=b\pmod s`.
  These rules are useful when the modulus is changed.

For polynomials :math:`v_1(x)` and :math:`v_2(x)` it further holds that

.. math:: (v_1(x)+v_2(x))^p = v_1(x)^p + v_2(x)^p\pmod p

This follows by writing out the expression, noting that the binomial
coefficients that result are multiples of :math:`p`, and thus their value
modulo :math:`p` is zero (:math:`p` divides these coefficients), so only the
two terms on the right hand side remain.

Some corollaries
^^^^^^^^^^^^^^^^

One corollary of the rules for calculations modulo an integer is
`Fermat's little theorem`_: if :math:`p` is a prime number then
:math:`a^p=a\pmod p` for all integers :math:`a` (for a proof, see Knuth).

.. _Fermat's little theorem: https://en.wikipedia.org/wiki/Fermat%27s_little_theorem

An interesting corollary to this is that, for some prime integer :math:`p`:

.. math:: v(x)^p = v(x^p)\pmod p.

This follows from writing it out and using Fermat's little theorem to replace
:math:`a^p` with :math:`a` where appropriate (the coefficients to the
polynomial when written out, on the left hand side).

Factoring using modular arithmetic
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The task is to factor a polynomial 

.. math:: p(x) = a_nx^n + \ldots + a_0

into a form 

.. math p(x) = Cg(x)f_1(x)^p_1f_2(x)^p_2\ldots f_m(x)^p_m

Where :math:`f_i(x)` are irreducible polynomials of the form:

.. math:: f_i(x) = x+c_i

The part that could not be factorized is returned as :math:`g(x)`,
with a possible constant factor :math:`C`.

The factors :math:`f_i(x)` and :math:`g(x)` are determined uniquely by
requiring them to be monic. The constant :math:`C` accounts for a common factor.

The :math:`c_i` constants in the resulting solutions :math:`f_i(x)` can be 
rational numbers (or even complex numbers, if Gaussian integers
are used).

Preparing the polynomial for factorization
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The final factoring algorithm needs the input polynomial to be monic
with integer coefficients (a polynomial is monic if its leading
coefficient is one). Given a non-monic polynomial with rational
coefficients, the following steps are performed:

Convert polynomial with rational coefficients to polynomial with integer coefficients

First the least common multiple :math:`lcm` of the denominators of the
coefficients :math:`p(x)` has to be found, and the polynomial is multiplied
by this number.  Afterwards, the :math:`C` constant in the result should
have a factor :math:`\frac{1}{lcm}`.

The polynomial now only has integer coefficients.


Convert polynomial to a monic polynomial
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The next step is to convert the polynomial to one where the leading
coefficient is one. In order to do so, following "Davenport", the
following steps have to be taken:

1. Multiply the polynomial by :math:`a_n^{n-1}`
2. Perform the substitution :math:`x=\frac{y}{a_n}`

The polynomial is now a monic polynomial in :math:`y`.

After factoring, the irreducible factors of :math:`p(x)`  can be obtained by
multiplying :math:`C` with :math:`\frac{1}{a_n^{n-1}}`, and replacing
:math:`y` with :math:`a_nx`. The irreducible solutions :math:`a_nx+c_i`
can be replaced by :math:`x+\frac{c_i}{a_i}` after multiplying :math:`C`
by :math:`a_n`, converting the factors to monic factors.

After the steps described here the polynomial is now monic with
integer coefficients, and the factorization of this polynomial can be
used to determine the factors of the original polynomial :math:`p(x)`.


Definition of division of polynomials
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

To factor a polynomial a division operation for polynomials modulo
some integer is needed. This algorithm needs to return a quotient
:math:`q(x)` and remainder :math:`r(x)` such that:

.. math:: p(x) = q(r)d(x) + r(x)\pmod p

for some polymomial :math:`d(x)` to be divided by, modulo some integer
:math:`p`. :math:`d(x)` is said to divide :math:`p(x)` (modulo :math:`p`)
if :math:`r(x)` is zero.  It is then a factor modulo :math:`p`.

For binary factoring algorithm it is important that if some monic
:math:`d(x)` divides :math:`p(x)`, then it also divides :math:`p(x)` modulo
some integer :math:`p`.

Define :math:`\mathrm{deg}(f(x))` to be the `degree`_ of :math:`f(x)` and
:math:`\mathrm{lc}(f(x))` to be the leading coefficient of :math:`f(x)`.
Then, if :math:`\mathrm{deg}(p(x))\ge \mathrm{deg}(d(x))`, one
can compute an integer :math:`s` such that

.. _degree: https://en.wikipedia.org/wiki/Degree_of_a_polynomial

.. math:: \mathrm{lc}(d(x))s = lc(p(x)\pmod p

If :math:`p` is prime, then 

.. math:: s = \mathrm{lc}(p(x))\mathrm{lc}(d(x))^{p-2}\bmod p

Because :math:`a^{p-1} = 1\pmod p` for any :math:`a`. If :math:`p` is not
prime but :math:`d(x)` is monic (and thus :math:`\mathrm{lc}(d(x)) = 1`,

.. math:: s = \mathrm{lc}(p(x))

This identity can also be used when dividing in general (not modulo
some integer), since the divisor is monic.

The quotient can then be updated by adding a term:

.. math:: term = sx^{\mathrm{deg}(p(x))-\mathrm{deg}(d(x))}

and updating the polynomial to be divided, :math:`p(x)`, by subtracting
:math:`d(x)term`. The resulting polynomial to be divided now has a degree
one smaller than the previous.

When the degree of :math:`p(x)` is less than the degree of :math:`d(x)` it is
returned as the remainder.

A full division algorithm for arbitrary integer :math:`p>1` with
:math:`\mathrm{lc}(d(x)) = 1` would thus look like::

	divide(p(x),d(x),p)
	   q(x) = 0
	   r(x) = p(x)
	   while (deg(r(x)) >= deg(d(x)))
	      s = lc(r(x))
	      term = s*x^(deg(r(x))-deg(d(x)))
	      q(x) = q(x) + term
	      r(x) = r(x) - term*d(x) mod p
	   return (q(x),r(x))

The reason we can get away with factoring modulo :math:`2^n` as opposed to
factoring modulo some prime :math:`p` in later sections is that the divisor
:math:`d(x)` is monic. Its leading coefficient is one and thus :math:`q(x)` and
:math:`r(x)` can be uniquely determined. If :math:`p` is not prime and
:math:`\mathrm{lc}(d(x))` is not equal to one, there might be multiple
combinations for which :math:`p(x) = q(x)d(x)+r(x)`, and we are interested
in the combinations where :math:`r(x)` is zero. This can be costly to determine
unless :math:`(q(x),r(x))` is unique.  This is the case here because we are
factoring a monic polynomial, and are thus only interested in cases where
:math:`\mathrm{lc}(d(x)) = 1`.


Determining possible factors modulo 2
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We start with a polynomial :math:`p(x)` which is monic and has integer
coefficients.

It will be factored into a form:

.. math:: p(x) = g(x)f_1(x)^{p_1}f_2(x)^{p_2}\ldots f_m(x)^{p_m}

where all factors :math:`f_i(x)` are monic also.

The algorithm starts by setting up a test polynomial, :math:`p_{test}(x)`
which divides :math:`p(x)`, but has the property that

.. math:: p_{test}(x) = g(x)f_1(x)f_2(x)\ldots f_m(x)

Such a polynomial is said to be *square-free*.  It has the same
factors as the original polynomial, but the original might have
multiple of each factor, where :math:`p_{test}(x)` does not.

The square-free part of a polynomial can be obtained as follows:

.. math:: p_{test}(x) = \frac{p(x)}{\gcd(p(x),\frac{d}{dx}p(x))}

It can be seen by simply writing this out that :math:`p(x)` and
:math:`\frac{d}{dx}p(x)` will have factors :math:`f_i(x)^{p_i-1}` in common.
these can thus be divided out.

It is not a requirement of the algorithm that the algorithm being
worked with is square-free, but it speeds up computations to work with
the square-free part of the polynomial if the only thing sought after
is the set of factors. The multiplicity of the factors can be
determined using the original :math:`p(x)`.

Binary factoring then proceeds by trying to find potential solutions
modulo :math:`p=2` first. There can only be two such solutions: :math:`x+0`
and :math:`x+1`.

A list of possible solutions :math:`L` is set up with potential solutions.


Determining factors modulo :math:`2^n` given a factorization modulo 2
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

At this point there is a list :math:`L` with solutions modulo :math:`2^n` for
some :math:`n`. The solutions will be of the form: :math:`x+a`. The first step
is to determine if any of the elements in :math:`L` divides :math:`p(x)` (not
modulo any integer).  Since :math:`x+a` divides :math:`p_{test}(x)` modulo
:math:`2^n`, both :math:`x+a` and :math:`x+a-2^n` have to be checked.

If an element in :math:`L` divides :math:`p_{test}(x)`, :math:`p_{test}(x)`
is divided by it, and a loop is entered to test how often it divides
:math:`p(x)` to determine the multiplicity :math:`p_i` of the factor.
The found factor :math:`f_i(x) = x+c_i` is added as a combination
:math:`(x+c_i, p_i)`. :math:`p(x)` is divided by :math:`f_i(x)^p_i`.

At this point there is a list :math:`L` of factors that divide
:math:`p_{test}(x)` modulo :math:`2^n`. This implies that for each of the
elements :math:`u` in :math:`L`, either :math:`u` or :math:`u+2^n` should
divide :math:`p_{test}(x)` modulo :math:`2^{n+1}`. The following step is thus
to set up a new list with new elements that divide :math:`p_{test}(x)`
modulo :math:`2^{n+1}`.

The loop is re-entered, this time doing the calculation modulo
:math:`2^{n+1}` instead of modulo :math:`2^n`.

The loop is terminated if the number of factors found equals
:math:`\mathrm{deg}(p_{test}(x))`, or if :math:`2^n` is larger than the
smallest non-zero coefficient of :math:`p_test(x)` as this smallest non-zero
coefficient is the product of all the smallest non-zero coefficients of the
factors, or if the list of potential factors is zero.

The polynomial :math:`p(x)` can not be factored any further, and is added as
a factor :math:`(p(x), 1)`.

The function :func:`BinaryFactors`, yields the following interaction in yacas::

  In> BinaryFactors((x+1)^4*(x-3)^2)
  Out> {{x-3,2},{x+1,4}}
  In> BinaryFactors((x-1/5)*(2*x+1/3))
  Out> {{2,1},{x-1/5,1},{x+1/6,1}}
  In> BinaryFactors((x-1123125)*(2*x+123233))
  Out> {{2,1},{x-1123125,1},{x+123233/2,1}}

The binary factoring algorithm starts with a factorization modulo 2,
and then each time tries to guess the next bit of the solution,
maintaining a list of potential solutions.  This list can grow
exponentially in certain instances.  For instance, factoring
:math:`(x-a)(x-2a)(x-3a)\ldots(x-na)` implies a that the roots have common
factors. There are inputs where the number of potential solutions
(almost) doubles with each iteration.  For these inputs the algorithm
becomes exponential. The worst-case performance is therefore
exponential. The list of potential solutions while iterating will
contain a lot of false roots in that case.

Efficiently deciding if a polynomial divides another
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Given the polynomial :math:`p(x)`, and a potential divisor

.. math:: f_i(x) = x-p

modulo some :math:`q=2^n` an expression for the remainder after division
is

.. math:: \mathrm{rem}(p)=\sum_{i=0}^n a_ip^i

For the initial solutions modulo 2, where the possible solutions are
:math:`x` and :math:`x-1`. For :math:`p=0`, :math:`\mathrm{rem}(0) = a_0`.
For :math:`p=1`, :math:`\mathrm{rem}(1) = \sum_{i=0}^na_i`.

Given a solution :math:`x-p` modulo :math:`q=2^n`, we consider the possible
solutions :math:`x-p\bmod 2^{n+1}` and :math:`x-(p+2^n)\bmod 2^{n+1}`.

:math:`x-p` is a possible solution if :math:`\mathrm{rem}(p)\bmod 2^{n+1} = 0`.

:math:`x-(p+q)` is a possible solution if 
:math:`\mathrm{rem}(p+q)\bmod 2^{n+1} = 0`. Expanding
:math:`\mathrm{rem}(p+q)\bmod 2q` yields:

.. math:: \mathrm{rem}(p+q) = \mathrm{rem}(p) + \mathrm{extra}(p,q)\pmod{2q}

When expanding this expression, some terms grouped under
:math:`\mathrm{extra}(p,q)` have factors like :math:`2q` or :math:`q^2`.
Since :math:`q=2^n`, these terms vanish if the calculation is done modulo
:math:`2^{n+1}`.

The expression for :math:`\mathrm{extra}(p,q)` then becomes

.. math:: \mathrm{extra}(p,q) = q\sum_{i=1}^{\frac{n}{2}} (2i-1)a(2i)p^{2i-2}

An efficient approach to determining if :math:`x-p` or :math:`x-(p+q)` divides
:math:`p(x)` modulo :math:`2^{n+1}` is then to first calculate
:math:`\mathrm{rem}(p)\bmod 2q`. If this is zero, :math:`x-p` divides
:math:`p(x)`. In addition, if
:math:`\mathrm{rem}(p)+\mathrm{extra}(p,q)\bmod 2q` is zero, :math:`x-(p+q)`
is a potential candidate.

Other efficiencies are derived from the fact that the operations are
done in binary. Eg. if :math:`q=2^n`, then :math:`q_{next}=2^{n+1} = 2q = q<<1`
is used in the next iteration. Also, calculations modulo :math:`2^n` are
equivalent to performing a bitwise and with :math:`2^n-1`. These operations
can in general be performed efficiently on todays hardware which is
based on binary representations.


Extending the algorithm
^^^^^^^^^^^^^^^^^^^^^^^

Only univariate polynomials with rational coefficients have been
considered so far. This could be extended to allow for roots that are
complex numbers :math:`a+b\imath` where both :math:`a` and :math:`b` are
rational numbers.

For this to work the division algorithm would have to be extended to
handle complex numbers with integer :math:`a` and :math:`b` modulo some
integer, and the initial setup of the potential solutions would have to be
extended to try :math:`x+1+\imath` and :math:`x+\imath` also. The step
where new potential solutions modulo :math:`2^{n+1}` are determined should
then also test for :math:`x+2^n\imath` and :math:`x+2^n+2^n\imath`.

The same extension could be made for multivariate polynomials,
although setting up the initial irreducible polynomials that divide
:math:`p_{test}(x)` modulo 2 might become expensive if done on a polynomial
with many variables (:math:`2^{2^m-1}` trials for :math:`m` variables).

Lastly, polynomials with real-valued coefficients *could* be
factored, if the coefficients were first converted to rational
numbers. However, for real-valued coefficients there exist other
methods (Sturm sequences).

Newton iteration
^^^^^^^^^^^^^^^^

What the :func:`BinaryFactor` algorithm effectively does is finding a set of
potential solutions modulo :math:`2^{n+1}` when given a set of potential
solutions modulo :math:`2^n`.  There is a better algorithm that does
something similar: Hensel lifting. Hensel lifting is a generalized
form of Newton iteration, where given a factorization modulo :math:`p`, each
iteration returns a factorization modulo :math:`p^2`.

Newton iteration is based on the following idea: when one takes a
Taylor series expansion of a function:

.. math:: f(x_0+dx) := f(x_0) + (\frac{d}{dx}f(x_0))dx +\ldots

Newton iteration then proceeds by taking only the first two terms in
this series, the constant plus the constant times :math:`dx`. Given some
good initial value :math:`x_0`, the function will is assumed to be close to
a root, and the function is assumed to be almost linear, hence this
approximation.  Under these assumptions, if we want :math:`f(x_0+dx)` to be
zero,

.. math:: f(x_0+dx) = f(x_0) + (\frac{d}{dx}f(x_0))dx = 0

This yields:

.. math:: dx := -\frac{f(x_0)}{\frac{d}{dx}f(x_0)} = 0

And thus a next, better, approximation for the root is

.. math:: x_1=x_0-\frac{f(x_0)}{\frac{d}{dx}f(x_0)},

or more general:

.. math:: x_{n+1}=x_n-\frac{f(x_n)}{\frac{d}{dx}f(x_n)}.

If the root has multiplicity one, a Newton iteration can converge
*quadratically*, meaning the number of decimals precision for
each iteration doubles.

As an example, we can try to find a root of :math:`\sin x` near
:math:`3`, which should converge to :math:`\pi`.

Setting precision to 30 digits,::

  In> Builtin'Precision'Set(30)
  Out> True;

We first set up a function :math:`dx(x)`::

  In> dx(x):=Eval(-Sin(x)/(D(x)Sin(x)))
  Out> True;

And we start with a good initial approximation to :math:`\pi`, namely
:math:`3`. Note we should set ``x`` *after* we set ``dx(x)``, as the right
hand side of the function definition is evaluated. We could also have
used a different parameter name for the definition of the function
:math:`dx(x)`::

  In> x:=3
  Out> 3;

We can now start the iteration::

  In> x:=N(x+dx(x))
  Out> 3.142546543074277805295635410534;
  In> x:=N(x+dx(x))
  Out> 3.14159265330047681544988577172;
  In> x:=N(x+dx(x))
  Out> 3.141592653589793238462643383287;
  In> x:=N(x+dx(x))
  Out> 3.14159265358979323846264338328;
  In> x:=N(x+dx(x))
  Out> 3.14159265358979323846264338328;

As shown, in this example the iteration converges quite quickly.

Finding roots of multiple equations in multiple variables using Newton iteration
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

One generalization, mentioned in W.H. Press et al., <i>NUMERICAL
RECIPES in C, The Art of Scientific computing</i> is finding roots for
multiple functions in multiple variables.

Given :math:`N` functions in :math:`N` variables, we want to solve

.. math:: f_i(x_1,\ldots,x_N) = 0

for :math:`i = 1,\ldots N`. If de denote by :math:`X` the vector 

.. math:: X := (x_1,x_2,\ldots,x_N)

and by :math:`dX` the delta vector, then one can write

.. math:: f_i(X+dX) = f_i(X)+\sum_{j=1}^N\frac{d}{dx_j}f_i(X)dx_j

Setting :math:`f_i(X+dX)` to zero, one obtains

\sum_{j=1}^Na_{ij}dx_j = b_i

where

.. math:: a_{ij} := \frac{d}{dx_j}f_i(X)

and

.. math:: b_i := -f_i(X)

So the generalization is to first initialize :math:`X` to a good initial
value, calculate the matrix elements :math:`a_{ij}` and the vector :math:`b_i`,
and then to proceed to calculate :math:`dX` by solving the matrix equation,
and calculating

.. math:: X_{i+1} = X_i + dX_i

In the case of one function with one variable, the summation reduces
to one term, so this linear set of equations was a lot simpler in that
case. In this case we will have to solve this set of linear equations
in each iteration.

As an example, suppose we want to find the zeroes for the following
two functions:

.. math:: f_1(a,x) := \sin(ax)

and

.. math:: f_2(a,x) := a-2

It is clear that the solution to this is :math:`a=2` and
:math:`x:=N\frac{\pi}{2}` for any integer value :math:`N`.

We will do calculations with precision 30::

  In> Builtin'Precision'Set(30)
  Out> True;

And set up a vector of functions :math:`(f_1(X),f_2(X))`
where :math:`X:=(a,x)`::

  In> f(a,x):={Sin(a*x),a-2}
  Out> True;

Now we set up a function ``matrix(a,x)`` which returns the
matrix :math:`a_{ij}`::

  In> matrix(a,x):=Eval({D(a)f(a,x),D(x)f(a,x)})
  Out> True;

We now set up some initial values::

  In> {a,x}:={1.5,1.5}
  Out> {1.5,1.5};


The iteration converges a lot slower for this example, so we
will loop 100 times::

  In> For(ii:=1,ii<100,ii++)[{a,x}:={a,x}+\
        N(SolveMatrix(matrix(a,x),-f(a,x)));]
  Out> True;
  In> {a,x}
  Out> {2.,0.059667311457823162437151576236};


The value for :math:`a` has already been found. Iterating a
few more times::

  In> For(ii:=1,ii<100,ii++)[{a,x}:={a,x}+\
        N(SolveMatrix(matrix(a,x),-f(a,x)));]
  Out> True;
  In> {a,x}
  Out> {2.,-0.042792753588155918852832259721};
  In> For(ii:=1,ii<100,ii++)[{a,x}:={a,x}+\
	   N(SolveMatrix(matrix(a,x),-f(a,x)));]
  Out> True;
  In> {a,x}
  Out> {2.,0.035119151349413516969586788023};

the value for :math:`x` converges a lot slower this time, and to the
uninteresting value of zero (a rather trivial zero of this set of
functions).  In fact for all integer values :math:`N` the value
:math:`\frac{N\pi}{2}` is a solution.  Trying various initial values will
find them.


Newton iteration on polynomials
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

von zur Gathen *et al.*, :cite:`gathen1999:modern_computer_algebra` discusses
taking the inverse of a polynomial using Newton iteration.  The task is,
given a polynomial :math:`f(x)`, to find a polynomial :math:`g(x)` such that
:math:`f(x) = \frac{1}{g(x)}`, modulo some power in :math:`x`.  This implies
that we want to find a polynomial :math:`g` for which:

.. math:: h(g) = \frac{1}{g}-f = 0

Applying a Newton iteration step :math:`g_{i+1} = g_i -
\frac{h(g_i)}{\frac{d}{dg}h(g_i)}` to this expression yields:

.. math:: g_{i+1} = 2g_i - f(g_i)^2

von zur Gathen then proves by induction that for :math:`f(x)` monic, and
thus :math:`f(0)=1`, given initial value :math:`g_0(x) = 1`, that

.. math:: fg_i=1\pmod{x^{2^i}}

Example:

suppose we want to find the polynomial :math:`g(x)` up to the 7-th degree
for which :math:`f(x)g(x) = 1\pmod{x^8}`, for the function

.. math:: f(x):=1+x+\frac{1}{2}x^2+\frac{1}{6}x^3+\frac{1}{24}x^4

First we define the function f::

  In> f:=1+x+x^2/2+x^3/6+x^4/24
  Out> x+x^2/2+x^3/6+x^4/24+1

And initialize :math:`g` and :math:`i`::

  In> g:=1
  Out> 1
  In> i:=0
  Out> 0

Now we iterate, increasing :math:`i`, and replacing :math:`g` with the
new value for :math:`g`::

  In> [i++;g:=BigOh(2*g-f*g^2,x,2^i);]
  Out> 1-x;
  In> [i++;g:=BigOh(2*g-f*g^2,x,2^i);]
  Out> x^2/2-x^3/6-x+1;
  In> [i++;g:=BigOh(2*g-f*g^2,x,2^i);]
  Out> x^7/72-x^6/72+x^4/24-x^3/6+x^2/2-x+1;

The resulting expression must thus be:

.. math:: g(x):=\frac{1}{72}x^7-\frac{1}{72}x^6+\frac{1}{24}x^4-\frac{1}{6}x^3+\frac{1}{2}x^2-x+1

We can easily verify this::

  In> Expand(f*g)
  Out> x^11/1728+x^10/576+x^9/216+(5*x^8)/576+1

This expression is 1 modulo :math:`x^8`, as can easily be shown::

  In> BigOh(%,x,8)
  Out> 1;
