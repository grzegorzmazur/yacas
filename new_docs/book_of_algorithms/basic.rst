==========================
Adaptive function plotting
==========================

Here we consider plotting of functions :math:`y=f(x)`.

There are two tasks related to preparation of plots of functions:
first, to produce the numbers required for a plot, and second, to draw
a plot with axes, symbols, a legend, perhaps additional illustrations
and so on.  Here we only concern ourselves with the first task, that
of preparation of the numerical data for a plot.  There are many
plotting programs that can read a file with numbers and plot it in any
desired manner.

Generating data for plots of functions generally does not require
high-precision calculations.  However, we need an algorithm that can
be adjusted to produce data to different levels of precision.  In some
particularly ill-behaved cases, a precise plot will not be possible
and we would not want to waste time producing data that is too
accurate for what it is worth.

A simple approach to plotting would be to divide the interval into
many equal subintervals and to evaluate the function on the resulting
grid.  Precision of the plot can be adjusted by choosing a larger or a
smaller number of points.

However, this approach is not optimal. Sometimes a function changes
rapidly near one point but slowly everywhere else.  For example,
:math:`f(x)=\frac{1}{x}` changes very quickly at small :math:`x`.
Suppose we need to plot this function between :math:`0` and
:math:`100`.  It would be wasteful to use the same subdivision
interval everywhere: a finer grid is only required over a small
portion of the plotting range near :math:`x=0`.

The adaptive plotting routine :func:`Plot2D'adaptive` uses a simple
algorithm to select the optimal grid to approximate a function of one
argument :math:`f(x)`.  The algorithm repeatedly subdivides the grid
intervals near points where the existing grid does not represent the
function well enough.  A similar algorithm for adaptive grid
refinement could be used for numerical integration. The idea is that
plotting and numerical integration require the same kind of detailed
knowledge about the behavior of the function.

The algorithm first splits the interval into a specified initial
number of equal subintervals, and then repeatedly splits each
subinterval in half until the function is well enough approximated by
the resulting grid. The integer parameter {depth} gives the maximum
number of binary splittings for a given initial interval; thus, at
most :math:`2^depth` additional grid points will be generated. The
function {Plot2D'adaptive} should return a list of pairs of points
{{{x1,y1}, {x2,y2}, ...}} to be used directly for plotting.

The adaptive plotting algorithm works like this:

1. Given an interval (:math:`a`, :math:`c`), we split it in half,
   :math:`b:=(a+c)/2` and first compute :math:`f(x)` at five grid
   points :math:`a`, :math:`a_1:=(a+b)/2`, :math:`b`,
   :math:`b_1:=(b+c)/2`, :math:`c`.
2. If currently :math:`depth <= 0`, return this list of five points and
   values because we cannot refine the grid any more.
3. Otherwise, check that the function does not oscillate too rapidly
   on the interval :math:`[a, c]`.  The formal criterion is that the
   five values are all finite and do not make a "zigzag" pattern such
   as :math:`(1,3,2,3,1)`.  More formally, we use the following
   procedure: For each three consecutive values, write "1" if the
   middle value is larger than the other two, or if it is smaller than
   the other two, or if one of them is not a number (e.g. ``Infinity``
   or ``Undefined}``.  If we have at most two ones now, then we
   consider the change of values to be "slow enough". Otherwise it is
   not "slow enough".  In this case we need to refine the grid; go to
   step 5.  Otherwise, go to step 4.
4. Check that the function values are smooth enough through the
   interval. Smoothness is controlled by a parameter
   :math:`\epsilon`. The meaning of the parameter :math:`\epsilon` is
   the (relative) error of the numerical approximation of the integral
   of :math:`f(x)` by the grid. A good heuristic value of
   :math:`\epsilon` is 1/(the number of pixels on the screen) because
   it means that no pixels will be missing in the area under the
   graph. For this to work we need to make sure that we are actually
   computing the area *under* the graph; so we define
   :math:`g(x):=f(x)-f[0]` where :math:`f[0]` is the minimum of the
   values of :math:`f(x)` on the five grid points :math:`a`,
   :math:`a_1`, :math:`b`, :math:`b_1`, and :math:`c`; the function
   :math:`g(x)` is nonnegative and has the minimum value 0.  Then we
   compute two different Newton-Cotes quadratures for
   :math:`\int_b^{b_1}g(x)\,\mathrm{d}x` using these five
   points. (Asymmetric quadratures are chosen to avoid running into an
   accidental symmetry of the function; the first quadrature uses
   points :math:`a`, :math:`a_1`, :math:`b`, :math:`b_1` and the
   second quadrature uses :math:`b`, :math:`b_1`, :math:`c`.) If the
   absolute value of the difference between these quadratures is less
   than :math:`epsilon` * (value of the second quadrature), then we
   are done and we return the list of these five points and values.
5. Otherwise, we need to refine the grid. We compute
   :func:`Plot2D'adaptive` recursively for the two halves of the
   interval, that is, for :math:`[a, b]` and :math:`[b, c]`.  We also
   decrease :math:`depth` by 1 and multiply :math:`\epsilon` by 2 because we
   need to maintain a constant *absolute* precision and this means
   that the relative error for the two subintervals can be twice as
   large.  The resulting two lists for the two subintervals are
   concatenated (excluding the double value at point :math:`b`) and
   returned.

This algorithm works well if the initial number of points and the
:math:`depth` parameter are large enough.  These parameters can be
adjusted to balance the available computing time and the desired level
of detail in the resulting plot.

Singularities in the function are handled by the step 3.  Namely, the
change in the sequence :math:`a`, :math:`a_1`, :math:`b`, :math:`b_1`,
:math:`c` is always considered to be "too rapid" if one of these
values is a non-number (e.g. ``Infinity`` or ``Undefined``). Thus,
the interval immediately adjacent to a singularity will be plotted at
the highest allowed refinement level. When preparing the plotting
data, the singular points are simply not printed to the data file, so
that a plotting programs does not encounter any problems.

Newton-Cotes quadratures
------------------------

The meaning of Newton-Cotes quadrature coefficients is that an
integral of a function :math:`f(x)` is approximated by a sum

.. math::
   \int_{a_0}^{a_n}f(x)\,\mathrm{d}x\approx h\sum_{k=0}^nc_kf(a_k)

where :math:`a_k` are the grid points, :math:`h:=a_1-a_0` is the grid
step, and :math:`c_k` are the quadrature coefficients.  It may seem
surprising, but these coefficients :math:`c_k` are independent of the
function :math:`f(x)` and can be precomputed in advance for a given
grid :math:`a_k`.  [The quadrature coefficients do depend on the
relative separations of the grid.  Here we assume a uniform grid with
a constant step :math:`h=a_k-a_{k-1}`.  Quadrature coefficients can
also be found for non-uniform grids.]

The coefficients :math:`c_k` for grids with a constant step :math:`h`
can be found, for example, by solving the following system of
equations,

.. math::
   \sum_{k=0}^nc_kk^p=\frac{n^{p+1}}{p+1}

for :math:`p=0,1,\ldots,n`. This system of equations means that the
quadrature correctly gives the integrals of :math:`p+1` functions
:math:`f(x)=x^p`, :math:`p=0,1,\ldots,n` over the interval
:math:`(0,n)`.  The solution of this system always exists and gives
quadrature coefficients as rational numbers. For example, the
well-known Simpson quadrature :math:`c_0=1/3,c_1=4/3,c_2=1/3` is
obtained with :math:`n=2`.  An example of using this quadrature is the
approximation

.. math::
   \int_0^2f(x)\,\mathrm{d}x\approx(f(0)+f(2))/3+4/3*f(1)


Newton-Cotes quadratures for partial intervals
----------------------------------------------

In the same way it is possible to find quadratures for the integral
over a subinterval rather than over the whole interval of
:math:`x`. In the current implementation of the adaptive plotting
algorithm, two quadratures are used: the 3-point quadrature
(:math:`n=2`) and the 4-point quadrature (:math:`n=3`) for the
integral over the first subinterval,
:math:`\int_{a_0}^{a_1}(x,a[0],a[1])f(x)\,\mathrm{d}x`. Their
coefficients are
:math:`\left(\frac{5}{12},\frac{2}{3},-\frac{1}{12}\right)` and
:math:`\left(\frac{3}{8},\frac{19}{24},-\frac{5}{24},\frac{1}{24}\right)`.
An example of using the first of these subinterval quadratures would
be the approximation

.. math::
   \int_0^2f(x)\,\mathrm{d}x\approx \frac{5}{12}f(0)+\frac{2}{3}f(1)-\frac{1}{12}f(2).

These quadratures are intentionally chosen to be asymmetric to avoid
an accidental cancellation when the function :math:`f(x)` itself is
symmetric.  (Otherwise the error estimate could accidentally become
exactly zero.)

================
Surface plotting
================

Here we consider plotting of functions :math:`z=f(x,y)`.

The task of surface plotting is to obtain a picture of a
two-dimensional surface as if it were a solid object in three
dimensions.  A graphical representation of a surface is a complicated
task.  Sometimes it is required to use particular coordinates or
projections, to colorize the surface, to remove hidden lines and so
on.  We shall only be concerned with the task of obtaining the data
for a plot from a given function of two variables :math:`f(x,y)`.
Specialized programs can take a text file with the data and let the
user interactively produce a variety of surface plots.

The currently implemented algorithm in the function :func:`Plot3DS` is
very similar to the adaptive plotting algorithm for two-dimensional
plots.  A given rectangular plotting region :math:`a_1\leq x\leq a_2`,
:math:`b_1\leq y\leq b_2` is subdivided to produce an equally spaced
rectangular grid of points.  This is the initial grid which will be
adaptively refined where necessary.  The refinement algorithm will
divide a given rectangle in four quarters if the available function
values indicate that the function does not change smoothly enough on
that rectangle.

The criterion of a "smooth enough" change is very similar to the
procedure outlined in the previous section.  The change is "smooth
enough" if all points are finite, nonsingular values, and if the
integral of the function over the rectangle is sufficiently well
approximated by a certain low-order "cubature" formula.

The two-dimensional integral of the function is estimated using the
following 5-point Newton-Cotes cubature:

+------+----+------+
| 1/12 |  0 | 1/12 |
+------+----+------+
|   0  | 2/3|  0   |
+------+----+------+
| 1/12 |  0 | 1/12 |
+------+----+------+

An example of using this cubature would be the approximation

.. math::
   \int_0^1\mathrm{d}x\int_0^1\mathrm{d}y f(x,y)\approx \frac{f(0,0)+f(0,1)+f(1,0)+f(1,1)}{12}+\frac{2}{3}f\left(\frac{1}{2},\frac{1}{2}\right)

Similarly, an 8-point cubature with zero sum is used to estimate the
error:

+------+------+------+
| -1/3 |  2/3 |  1/6 |
+------+------+------+	
| -1/6 | -2/3 | -1/2 |
+------+------+------+
| 1/2  |   0  |  1/3 |
+------+------+------+

This set of coefficients was intentionally chosen to be asymmetric to
avoid possible exact cancellations when the function itself is
symmetric.

One minor problem with adaptive surface plotting is that the resulting
set of points may not correspond to a rectangular grid in the
parameter space :math:`(x,y)`.  This is because some rectangles from
the initial grid will need to be bisected more times than others.  So,
unless adaptive refinement is disabled, the function :func:`Plot3DS`
produces a somewhat disordered set of points.  However, most surface
plotting programs require that the set of data points be a rectangular
grid in the parameter space.  So a smoothing and interpolation
procedure is necessary to convert a non-gridded set of data points
("scattered" data) to a gridded set.

================
Parametric plots
================

Currently, parametric plots are not directly implemented in Yacas.
However, it is possible to use Yacas to obtain numerical data for such
plots.  One can then use external programs to produce actual graphics.

A two-dimensional parametric plot is a line in a two-dimensional
space, defined by two equations such as :math:`x=f(t),y=g(t)`.  Two
functions :math:`f, g` and a range of the independent variable
:math:`t`, for example, :math:`t_1\leq t\leq t_2`, need to be
specified.

Parametric plots can be used to represent plots of functions in
non-Euclidean coordinates.  For example, to plot the function
:math:`\rho=\cos(4\phi)^2` in polar coordinates :math:`(\rho,\phi)`,
one can rewrite the Euclidean coordinates through the polar
coordinates, :math:`x=\rho\cos(\phi),y = \rho\sin(\phi)`, and
use the equivalent parametric plot with :math:`\phi` as the parameter:
:math:`x=\cos(4\phi)^2\cos(\phi), y = \cos(4\phi)^2\sin(\phi)`.

Sometimes higher-dimensional parametric plots are required.  A line
plot in three dimensions is defined by three functions of one
variable, for example, :math:`x=f(t),y=g(t),z=h(t)`, and a range of
the parameter :math:`t`.  A surface plot in three dimensions is
defined by three functions of two variables each, for example,
:math:`x=f(u,v),y=g(u,v),z=h(u,v)`, and a rectangular domain in the
:math:`(u,v)` space.

The data for parametric plots can be generated separately using the
same adaptive plotting algorithms as for ordinary function plots, as
if all functions such as :math:`f(t)` or :math:`g(u,v)` were unrelated
functions.  The result would be several separate data sets for the
:math:`x,y,\ldots` coordinates.  These data sets could then be
combined using an interactive plotting program.

============================================
The cost of arbitrary-precision computations
============================================

A computer algebra system absolutely needs to be able to perform
computations with very large *integer* numbers. Without this
capability, many symbolic computations (such as exact GCD of
polynomials or exact solution of polynomial equations) would be
impossible.

A different question is whether a CAS really needs to be able to
evaluate, say, 10,000 digits of the value of a Bessel function of some
10,000-digit complex argument.  It seems likely that no applied
problem of natural sciences would need floating-point computations of
special functions with such a high precision. However,
arbitrary-precision computations are certainly useful in some
mathematical applications; e.g. some mathematical identities can be
first guessed by a floating-point computation with many digits and
then proved.

Very high precision computations of special functions *might* be
useful in the future.  But it is already quite clear that computations
with moderately high precision (say, 50 or 100 decimal digits) are
useful for applied problems.  For example, to obtain the leading
asymptotic of an analytic function, we could expand it in series and
take the first term.  But we need to check that the coefficient at
what we think is the leading term of the series does not vanish.  This
coefficient could be a certain "exact" number such as
:math:`(\cos(355)+1)^2`.  This number is "exact" in the sense that it
is made of integers and elementary functions.  But we cannot say *a
priori* that this number is nonzero.  The problem of "zero
determination" (finding out whether a certain "exact" number is zero)
is known to be algorithmically unsolvable if we allow transcendental
functions.  The only practical general approach seems to be to compute
the number in question with many digits.  Usually a few digits are
enough, but occasionally several hundred digits are needed.

Implementing an efficient algorithm that computes 100 digits of
:math:`\sin(\frac{3}{7})` already involves many of the issues that would also
be relevant for a 10,000 digit computation.  Modern algorithms allow
evaluations of all elementary functions in time that is asymptotically
logarithmic in the number of digits :math:`P` and linear in the cost
of long multiplication (usually denoted :math:`M(P)`).  Almost all
special functions can be evaluated in time that is asymptotically
linear in :math:`P` and in :math:`M(P)`.  (However, this asymptotic
cost sometimes applies only to very high precision, e.g.,
:math:`P>1000`, and different algorithms need to be implemented for
calculations in lower precision.)

In yacas we strive to implement all numerical functions to arbitrary
precision.  All integer or rational functions return exact results,
and all floating-point functions return their value with :math:`P`
correct decimal digits (assuming sufficient precision of the
arguments).  The current value of :math:`P` is accessed as
:func:`Builtin'Precision'Get` and may be changed by
:func:`Builtin'Precision'Set`.

Implementing an arbitrary-precision floating-point computation of a
function :math:`f(x)`, such as :math:`f(x)=\exp(x)`, typically needs
the following:

* An algorithm that will compute :math:`f(x)` for a given value
  :math:`x` to a user-specified precision of :math:`P` (decimal)
  digits. Often, several algorithms must be implemented for different
  subdomains of the (:math:`x`,:math:`P`) space.
* An estimate of the computational cost of the algorithm(s), as a
  function of :math:`x` and :math:`P`. This is needed to select the
  best algorithm for given :math:`x, P`.
* An estimate of the round-off error.  This is needed to select the
  "working precision" which will typically be somewhat higher than the
  precision of the final result.

In calculations with machine precision where the number of digits is
fixed, the problem of round-off errors is quite prominent.  Every
arithmetic operation causes a small loss of precision; as a result, a
few last digits of the final value are usually incorrect.  But if we
have an arbitrary precision capability, we can always increase
precision by a few more digits during intermediate computations and
thus eliminate all round-off error in the final result.  We should, of
course, take care not to increase the working precision unnecessarily,
because any increase of precision means slower calculations.  Taking
twice as many digits as needed and hoping that the result is precise
is not a good solution.

Selecting algorithms for computations is the most non-trivial part of
the implementation.  We want to achieve arbitrarily high precision, so
we need to find either a series, or a continued fraction, or a
sequence given by explicit formula, that converges to the function in
a controlled way.  It is not enough to use a table of precomputed
values or a fixed approximation formula that has a limited precision.

In the last 30 years, the interest in arbitrary-precision computations
grew and many efficient algorithms for elementary and special
functions were published.  Most algorithms are iterative.  Almost
always it is very important to know in advance how many iterations are
needed for given :math:`x`, :math:`P`.  This knowledge allows to
estimate the computational cost, in terms of the required precision
:math:`P` and of the cost of long multiplication :math:`M(P)`, and
choose the best algorithm.

Typically all operations will fall into one of the following
categories (sorted by the increasing cost):

* addition, subtraction: linear in :math:`P`;
* multiplication, division, integer power, integer root: linear in
  :math:`M(P)`;
* elementary functions: :math:`\exp(x)`, :math:`\ln(x)`, :math:`\sin(x)`,
  :math:`\arctan(x)` etc.: :math:`M(P)\ln(P)` or slower by some powers of
  :math:`\ln(P)`;
* transcendental functions: :math:`erf(x)`, :math:`\gamma(x)` etc.:
  typically :math:`PM(P)` or slower.

The cost of long multiplication :math:`M(P)` is between :math:`O(P^2)`
for low precision and :math:`O(P\ln(P))` for very high precision.  In
some cases, a different algorithm should be chosen if the precision is
high enough to allow :math:`M(P)` faster than :math:`O(P^2)`.

Some algorithms also need storage space (e.g. an efficient algorithm
for summation of the Taylor series uses :math:`O(\ln(P))` temporary
:math:`P`-digit numbers).

Below we shall normally denote by :math:`P` the required number of
decimal digits.  The formulae frequently contain conspicuous factors
of :math:`\ln(10)`, so it will be clear how to obtain analogous
expressions for another base.  (Most implementations use a binary base
rather than a decimal base since it is more convenient for many
calculations.)

==================================
Estimating convergence of a series
==================================

Analyzing convergence of a power series is usually not difficult.
Here is a worked-out example of how we could estimate the required
number of terms in the power series

.. math::
  \exp(x)=1+x+\frac{x^2}{2!} +\ldots +\frac{x^n}{n!} +O(x^{n+1})

if we need :math:`P` decimal digits of precision in the
result.  To be specific, assume that :math:`|x|<1`. (A similar
calculation can be done for any other bound on :math:`x`.)

Suppose we truncate the series after :math:`n`-th term and the series
converges "well enough" after that term. Then the error will be
approximately equal to the first term we dropped. (This is what we
really mean by "converges well enough" and this will generally be the
case in all applications, because we would not want to use a series
that does not converge well enough.)

The term we dropped is :math:`\frac{x^{n+1}}{(n+1)!}`.  To estimate :math:`n!`
for large :math:`n`, one can use the inequality

.. math::
  e^{e-1}*(n/e)^n < n! < (n/e)^{n+1}

(valid for all :math:`n\geq 47`) which provides tight
bounds for the growth of the factorial, or a weaker inequality which
is somewhat easier to use,


.. math::
  (n/e)^n < n! < ((n+1)/e)^{n+1}

(valid for all :math:`n\geq 6`). The latter inequality is sufficient for most
purposes.

If we use the upper bound on :math:`n!` from this estimate, we find
that the term we dropped is bounded by

.. math::
  x^{n+1}/(n+1)! < (e/(n+2))^{n+2}.

We need this number to be smaller than
:math:`10^{-P}`. This leads to an inequality

.. math::
   (e/(n+2))^(n+2) < 10^{-P},

which we now need to solve for :math:`n`. The left hand
side decreases with growing :math:`n`. So it is clear that the
inequality will hold for large enough :math:`n`, say for :math:`n\geq n_0`
where :math:`n_0` is an unknown (integer) value. We can take a
logarithm of both sides, replace :math:`n` with :math:`n_0` and obtain
the following equation for :math:`n_0`:

.. math::
  (n_0+2)\ln((n_0+2)/e) = P*\ln(10).

This equation cannot be solved exactly in terms of
elementary functions; this is a typical situation in such
estimates. However, we do not really need a very precise solution for
:math:`n_0`; all we need is an estimate of its integer part.  This is
also a typical situation.  It is acceptable if our approximate value
of :math:`n_0` comes out a couple of units higher than necessary,
because a couple of extra terms of the Taylor series will not
significantly slow down the algorithm (but it is important that we do
not underestimate :math:`n_0`).  Finally, we are mostly interested in
having a good enough answer for large values of :math:`P`.

We can try to guess the result.  The largest term on the LHS grows as
:math:`n_0\ln(n_0)` and it should be approximately equal to
:math:`P\ln(10)`; but :math:`\ln(n_0)` grows very slowly, so this gives
us a hint that :math:`n_0` is proportional to :math:`P\ln(10)`.  As a
first try, we set :math:`n_0=P\ln(10)-2` and compare the RHS with the
LHS; we find that we have overshot by a factor
:math:`\ln(P)-1+\ln(\ln(10))`, which is not a large factor. We can now
compensate and divide :math:`n_0` by this factor, so our second try is

.. math::
  n0 = (P\ln(10))/(\ln(P)-1+\ln(\ln(10)))-2.

(This approximation
procedure is equivalent to solving the equation

.. math::
  x = (P*\ln(10))/(\ln(x)-1)

by direct iteration, starting from
:math:`x=P\ln(10)`.)  If we substitute our second try for :math:`n_0`
into the equation, we shall find that we undershot a little bit
(i.e. the LHS is a little smaller than the RHS), but our :math:`n_0` is
now smaller than it should be by a quantity that is smaller than 1 for
large enough :math:`P`.  So we should stop at this point and simply
add 1 to this approximate answer. We should also replace
:math:`\ln(\ln(10))-1` by 0 for simplicity (this is safe because it will
slightly increase :math:`n_0`.)

Our final result is that it is enough to take

.. math::
  n=(P*\ln(10))/\ln(P)-1

terms in the Taylor series to compute :math:`\exp(x)` for
:math:`|x|<1` to :math:`P` decimal digits. (Of course, if :math:`x`
is much smaller than 1, many fewer terms will suffice.)

==============================
Estimating the round-off error
==============================

Unavoidable round-off errors
----------------------------

As the required precision :math:`P` grows, an arbitrary-precision
algorithm will need more iterations or more terms of the series. So
the round-off error introduced by every floating-point operation will
increase. When doing arbitrary-precision computations, we can always
perform all calculations with a few more digits and compensate for
round-off error.  It is however imperative to know in advance how many
more digits we need to take for our "working precision". We should
also take that increase into account when estimating the total cost of
the method.  (In most cases this increase is small.)

Here is a simple estimate of the normal round-off error in a
computation of :math:`n` terms of a power series.  Suppose that the
sum of the series is of order :math:`1`, that the terms monotonically
decrease in magnitude, and that adding one term requires two
multiplications and one addition. If all calculations are performed
with absolute precision :math:`\epsilon=10^{-P}`, then the total
accumulated round-off error is :math:`3n\epsilon`. If the relative
error is :math:`3n\epsilon`, it means that our answer is something
like :math:`a*(1+3n\epsilon)` where :math:`a` is the correct
answer. We can see that out of the total :math:`P` digits of this
answer, only the first :math:`k` decimal digits are correct, where
:math:`k= -\ln(3n\epsilon)/\ln(10)`. In other words, we have lost

.. math::
  P-k=\ln(3n)/\ln(10)

digits because of accumulated round-off
error. So we found that we need :math:`\ln(3*n)/\ln(10)` extra decimal
digits to compensate for this round-off error.

This estimate assumes several things about the series (basically, that
the series is "well-behaved").  These assumptions must be verified in
each particular case.  For example, if the series begins with some
large terms but converges to a very small value, this estimate is
wrong (see the next subsection).

In the previous exercise we found the number of terms :math:`n` for
:math:`\exp(x)`. So now we know how many extra digits of working
precision we need for this particular case.

Below we shall have to perform similar estimates of the required
number of terms and of the accumulated round-off error in our analysis
of the algorithms.

Catastrophic round-off error
----------------------------

Sometimes the round-off error of a particular method of computation
becomes so large that the method becomes highly inefficient.

Consider the computation of :math:`\sin(x)` by the truncated Taylor
series

.. math::
  \sin(x)\approx \sum_{k=0}^{N-1}(-1)^kx^{2k+1}/(2k+1)!),

when :math:`x` is large.  We know that this series converges for all
:math:`x`, no matter how large.  Assume that :math:`x=10^M` with
:math:`M\geq 1`, and that we need :math:`P` decimal digits of precision
in the result.

First, we determine the necessary number of terms :math:`N`.  The
magnitude of the sum is never larger than :math:`1`.  Therefore we
need the :math:`N`-th term of the series to be smaller than
:math:`10^{-P}`.  The inequality is :math:`(2N+1)! >
10^{P+M(2N+1)}`.  We obtain that :math:`2N+2>e10^M` is a
necessary condition, and if :math:`P` is large, we find approximately

.. math::
  2N+2 \approx ((P-M)\ln(10)) / (\ln(P-M)-1-M\ln(10)).

However, taking enough terms does not yet guarantee a good result.
The terms of the series grow at first and then start to decrease.  The
sum of these terms is, however, small.  Therefore there is some
cancellation and we need to increase the working precision to avoid
the round-off.  Let us estimate the required working precision.

We need to find the magnitude of the largest term of the series.  The
ratio of the next term to the previous term is :math:`x/(2k(2k+1))`
and therefore the maximum will be when this ratio becomes equal to
:math:`1`, i.e. for :math:`2k\approx\sqrt{x}`. Therefore the largest term
is of order :math:`x^{\sqrt{x}}/\sqrt{x}!` and so we need about
:math:`M/2\sqrt{x}` decimal digits before the decimal point to
represent this term.  But we also need to keep at least :math:`P`
digits after the decimal point, or else the round-off error will erase
the significant digits of the result.  In addition, we will have
unavoidable round-off error due to :math:`O(P)` arithmetic operations.
So we should increase precision again by :math:`P+\ln(P)/\ln(10)` digits
plus a few guard digits.

As an example, to compute :math:`\sin(10)` to :math:`P=50` decimal
digits with this method, we need a working precision of about
:math:`60` digits, while to compute :math:`\sin(10000)` we need to work
with about :math:`260` digits.  This shows how inefficient the Taylor
series for :math:`\sin(x)` becomes for large arguments :math:`x`.  A
simple transformation :math:`x=2\pi n+x'` would have reduced :math:`x`
to at most 7, and the unnecessary computations with :math:`260` digits
would be avoided.  The main cause of this inefficiency is that we have
to add and subtract extremely large numbers to get a relatively small
result of order :math:`1`.

We find that the method of Taylor series for :math:`\sin(x)` at large
:math:`x` is highly inefficient because of round-off error and should
be complemented by other methods.  This situation seems to be typical
for Taylor series.


====================================
Basic arbitrary-precision arithmetic
====================================

Yacas uses an internal math library (the ``yacasnumbers`` library) which
comes with the source code. This reduces the dependencies of the yacas
system and improves portability.  The internal math library is simple
and does not necessarily use the most optimal algorithms.

If :math:`P` is the number of digits of precision, then multiplication
and division take :math:`M(P)=O(P^2)` operations in the internal
math. (Of course, multiplication and division by a short integer takes
time linear in :math:`P`.)  Much faster algorithms (Karatsuba,
Toom-Cook, FFT multiplication, Newton-Raphson division etc.) are
implemented in {gmp}, {CLN} and some other libraries.  The asymptotic
cost of multiplication for very large precision is
:math:`M(P)\approx O(P^{1.6})` for the Karatsuba method and
:math:`M(P)=O(P\ln(P)\ln(\ln(P)))` for the FFT method.  In the
estimates of computation cost in this book we shall assume that
:math:`M(P)` is at least linear in :math:`P` and maybe a bit slower.

The costs of multiplication may be different in various
arbitrary-precision arithmetic libraries and on different computer
platforms.  As a rough guide, one can assume that the straightforward
:math:`O(P^2)` multiplication is good until 100-200 decimal digits,
the asymptotically fastest method of FFT multiplication is good at the
precision of about 5,000 or more decimal digits, and the Karatsuba
multiplication is best in the middle range.

Warning: calculations with internal yacas math using precision
exceeding 10,000 digits are currently impractically slow.

In some algorithms it is necessary to compute the integer parts of
expressions such as :math:`a\ln(b)/\ln(10)` or :math:`a\ln(10)/\ln(2)`,
where :math:`a`, :math:`b` are short integers of order
:math:`O(P)`. Such expressions are frequently needed to estimate the
number of terms in the Taylor series or similar parameters of the
algorithms. In these cases, it is important that the result is not
underestimated. However, it would be wasteful to compute
:math:`1000\ln(10)/\ln(2)` in great precision only to discard most of
that information by taking the integer part of that number.  It is
more efficient to approximate such constants from above by short
rational numbers, for example, :math:`\ln(10)/\ln(2) < 28738/8651` and
:math:`\ln(2) < 7050/10171`. The error of such an approximation will be
small enough for practical purposes. The function :func:`BracketRational`
can be used to find optimal rational approximations.

The function :func:`IntLog` (see below) efficiently computes the integer
part of a logarithm (for an integer base, not a natural logarithm). If
more precision is desired in calculating :math:`\ln(a)/\ln(b)` for
integer :math:`a`, :math:`b`, one can compute :math:`IntLog(a^k,b)`
for some integer :math:`k` and then divide by :math:`k`.

How many digits of :math:`\sin(\exp(\exp(1000)))` do we need?
-------------------------------------------------------------

Arbitrary-precision math is not omnipotent against overflow.  Consider
the problem of representing very large numbers such as
:math:`x=\exp(\exp(1000))`.  Suppose we need a floating-point
representation of the number :math:`x` with :math:`P` decimal digits
of precision.  In other words, we need to express :math:`x\approx M10^E`,
where the mantissa :math:`1<M<10` is a floating-point number and
the exponent :math:`E` is an integer, chosen so that the relative
precision is :math:`10^{-P}`.  How much effort is needed to find
:math:`M` and :math:`E`?

The exponent :math:`E` is easy to obtain:

.. math::
  E = \lfloor\ln(x)/\ln(10)\rfloor = \lfloor\exp(1000)/\ln(10)\rfloor\approx 8.55 * 10^{433}.

To compute the integer
part :math:`\lfloor y\rfloor` of a number :math:`y` exactly, we need to
approximate :math:`y` with at least :math:`\ln(y)/\ln(10)`
floating-point digits.  In our example, we find that we need 434
decimal digits to represent :math:`E`.

Once we found :math:`E`, we can write :math:`x=10^{E+m}` where
:math:`m=\exp(1000)/\ln(10)-E` is a floating-point number,
:math:`0<m<1`.  Then :math:`M=10^m`.  To find :math:`M` with :math:`P`
(decimal) digits, we need :math:`m` with also at least :math:`P`
digits.  Therefore, we actually need to evaluate
:math:`\exp(1000)/\ln(10)` with :math:`434+P` decimal digits before we
can find :math:`P` digits of the mantissa of :math:`x`.  We ran into a
perhaps surprising situation: one needs a high-precision calculation
even to find the first digit of :math:`x`, because it is necessary to
find the exponent :math:`E` exactly as an integer, and :math:`E` is a
rather large integer.  A normal double-precision numerical calculation
would give an overflow error at this point.

Suppose we have found the number :math:`x=\exp(\exp(1000))` with some
precision.  What about finding :math:`\sin(x)`?  Now, this is extremely
difficult, because to find even the first digit of :math:`\sin(x)` we
have to evaluate :math:`x` with *absolute* error of at most
:math:`0.5`.  We know, however, that the number :math:`x` has
approximately :math:`10^434` digits *before* the decimal point.
Therefore, we would need to calculate :math:`x` with at least that
many digits.  Computations with :math:`10^434` digits is clearly far
beyond the capability of modern computers.  It seems unlikely that
even the sign of :math:`\sin(\exp(\exp(1000)))` will be obtained in the
near future [#]_. 

.. [#] It seems even less likely that the sign of :math:`\sin(\exp(\exp(1000)))` would be of any use to anybody even if it could be computed.

Suppose that :math:`N` is the largest integer that our
arbitrary-precision facility can reasonably handle.  (For yacas
internal math library, :math:`N` is about :math:`10^10000`.)  Then it
follows that numbers :math:`x` of order :math:`10^N` can be calculated
with at most one (1) digit of floating-point precision, while larger
numbers cannot be calculated with any precision at all.

It seems that very large numbers can be obtained in practice only
through exponentiation or powers.  It is unlikely that such numbers
will arise from sums or products of reasonably-sized numbers in some
formula [#]_.

.. [#] A factorial function can produce rapidly growing results, but exact factorials :math:`n!` for large :math:`n` are well represented by the Stirling formula which involves powers and exponentials.  For example, suppose a program operates with numbers :math:`x` of size :math:`N` or smaller; a number such as :math:`10^N` can be obtained only by multiplying :math:`O(N)` numbers :math:`x` together.  But since :math:`N` is the largest representable number, it is certainly not feasible to perform :math:`O(N)` sequential operations on a computer.  However, it is feasible to obtain :math:`N`-th power of a small number, since it requires only :math:`O(Ln(N))` operations.

If numbers larger than :math:`10^N` are created only by exponentiation
operations, then special exponential notation could be used to
represent them.  For example, a very large number :math:`z` could be
stored and manipulated as an unevaluated exponential
:math:`z=\exp(M10^E)` where :math:`M\geq 1` is a floating-point number
with :math:`P` digits of mantissa and :math:`E` is an integer,
:math:`\ln(N)<E<N`.  Let us call such objects "exponentially large
numbers" or "exp-numbers" for short.

In practice, we should decide on a threshold value :math:`N` and
promote a number to an exp-number when its logarithm exceeds
:math:`N`.

Note that an exp-number :math:`z` might be positive or negative, e.g.
:math:`z= -\exp(M10^E)`.

Arithmetic operations can be applied to the exp-numbers.  However,
exp-large arithmetic is of limited use because of an almost certainly
critical loss of precision.  The power and logarithm operations can be
meaningfully performed on exp-numbers :math:`z`.  For example, if
:math:`z=\exp(M10^E)` and :math:`p` is a normal floating-point number,
then :math:`z^p=\exp(pM10^E)` and :math:`\ln(z)=M10^E`.  We can also
multiply or divide two exp-numbers.  But it makes no sense to multiply
an exp-number :math:`z` by a normal number because we cannot represent
the difference between :math:`z` and say :math:`2.52*z`.  Similarly,
adding :math:`z` to anything else would result in a total underflow,
since we do not actually know a single digit of the decimal
representation of :math:`z`.  So if :math:`z_1` and :math:`z_2` are
exp-numbers, then :math:`z_1+z_2` is simply equal to either :math:`z1`
or :math:`z2` depending on which of them is larger.

We find that an exp-number :math:`z` acts as an effective "infinity"
compared with normal numbers.  But exp-numbers cannot be used as a
device for computing limits: the unavoidable underflow will almost
certainly produce wrong results.  For example, trying to verify

.. math::
  \lim_{x\to 0} \frac{\exp(x)-1}{x} = 1

by substituting :math:`x=1/z` with
some exp-number :math:`z` gives 0 instead of 1.

Taking a logarithm of an exp-number brings it back to the realm of
normal, representable numbers.  However, taking an exponential of an
exp-number results in a number which is not representable even as an
exp-number.  This is because an exp-number :math:`z` needs to have its
exponent :math:`E` represented exactly as an integer, but
:math:`\exp(z)` has an exponent of order :math:`O(z)` which is not a
representable number.  The monstrous number :math:`\exp(z)` could be
only written as :math:`\exp(\exp(M10^E))`, i.e. as a "doubly
exponentially large" number, or "2-exp-number" for short.  Thus we
obtain a hierarchy of iterated exp-numbers.  Each layer is
"unrepresentably larger" than the previous one.

The same considerations apply to very small numbers of the order
:math:`10^{-N}` or smaller.  Such numbers can be manipulated as
"exponentially small numbers", i.e. expressions of the form
:math:`Exp(-M*10^E)` with floating-point mantissa :math:`M\geq 1` and
integer :math:`E` satisfying :math:`\ln(N)<E<N`.  Exponentially small
numbers act as an effective zero compared with normal numbers.

Taking a logarithm of an exp-small number makes it again a normal
representable number.  However, taking an exponential of an exp-small
number produces 1 because of underflow.  To obtain a "doubly
exponentially small" number, we need to take a reciprocal of a doubly
exponentially large number, or take the exponent of an exponentially
large negative power.  In other words, :math:`\exp(-M10^E)` is
exp-small, while :math:`\exp(-\exp(M10^E))` is 2-exp-small.

The practical significance of exp-numbers is rather limited.  We
cannot obtain even a single significant digit of an exp-number.  A
"computation" with exp-numbers is essentially a floating-point
computation with logarithms of these exp-numbers.  A practical problem
that needs numbers of this magnitude can probably be restated in terms
of more manageable logarithms of such numbers.  In practice,
exp-numbers could be useful not as a means to get a numerical answer,
but as a warning sign of critical overflow or underflow [#]_.

.. [#] Yacas currently does not implement exp-numbers or any other guards against overflow and underflow. If a decimal exponential becomes too large, an incorrect answer may result.
