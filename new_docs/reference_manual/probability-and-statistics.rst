==========================
Probability and Statistics
==========================

Probability
-----------

Each distribution is represented as an entity. For each distribution
known to the system the consistency of parameters is checked. If the
parameters for a distribution are invalid, the functions return
``Undefined``.  For example, ``NormalDistribution(a,-1)`` evaluates to
``Undefined``, because of negative variance.


.. function:: BernoulliDistribution(p)

   Bernoulli distribution

   :param p: number, probability of an event in a single trial

   A random variable has a Bernoulli distribution with probability
   ``p`` if it can be interpreted as an indicator of an event, where
   ``p`` is the probability to observe the event in a single trial.
   Numerical value of ``p`` must satisfy ``0 < p < 1``.

   .. seealso:: :func:`BinomialDistribution`


.. function:: BinomialDistribution(p,n)

   binomial distribution

   :param p: number, probability to observe an event in single trial
   :param n: number of trials

   Suppose we repeat a trial ``n`` times, the probability to observe
   an event in a single trial is ``p`` and outcomes in all trials are
   mutually independent. Then the number of trials when the event
   occurred is distributed according to the binomial distribution.
   The probability of that is ``BinomialDistribution(p,n)``.
   Numerical value of ``p`` must satisfy ``0 < p < 1``. Numerical
   value of ``n`` must be a positive integer.

   .. seealso:: :func:`BernoulliDistribution`


.. function:: tDistribution(m)

   Student's $t$ distribution

   :param {m}: integer, number of degrees of freedom


.. function:: PDF(dist,x)

   probability density function

   :param dist: a distribution type
   :param x: a value of random variable

   If ``dist`` is a discrete distribution, then ``PDF`` returns the
   probability for a random variable with distribution ``dist`` to take
   a  value of ``x``. If ``dist`` is a continuous distribution, then ``PDF``
   returns the density function at point ``x``.

   .. seealso:: :FUNC:`CDF`

Statistics
----------


.. function:: ChiSquareTest(observed,expected)

   Pearson's ChiSquare test

   :param observed: list of observed frequencies
   :param expected: list of expected frequencies
   :param params: number of estimated parameters

   ``ChiSquareTest`` is intended to find out if our sample was drawn
   from a given distribution or not. To find this out, one has to
   calculate observed frequencies into certain intervals and expected
   ones. To calculate expected frequency the formula :math:`n_i=n p_i`
   must be used, where :math:`p_i` is the probability measure of
   :math:`i`-th interval, and :math:`n` is the total number of
   observations. If any of the parameters of the distribution were
   estimated, this number is given as ``params``.  The function
   returns a list of three local substitution rules. First of them
   contains the test statistic, the second contains the value of the
   parameters, and the last one contains the degrees of freedom. The
   test statistic is distributed as :func:`ChiSquareDistribution`.
