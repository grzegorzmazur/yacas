.. _tutorial:

********
Tutorial
********

.. _syntax:

============
Yacas syntax
============

Expressions in Yacas are generally built up of words. We will not bore
you with the exact definitions of such words, but roughly speaking
they are either sequences of alphabetic letters, or a number, or a
bracket, or space to separate words, or a word built up from symbols
like ``+``, ``-``, ``*``, ``<``, *etc.*. If you want, you can mix
these different types of characters, by surrounding them with
quotes. Thus, ``"This text"`` is what is called one token, surrounded
by quotes.

The usual notation people use when writing down a calculation is
called the infix notation, and you can readily recognize it, as for
example ``2+3`` and ``3*4``. Prefix operators also exist. These
operators come before an expression, like for example the unary minus
sign (called unary because it accepts one argument), ``-(3*4)``. In
addition to prefix operators there are also postfix operators, like
the exclamation mark to calculate the factorial of a number, ``10!``.

Yacas understands standard simple arithmetic expressions. Some
examples:

* ``2+3`` (addition)
* ``2*3`` (multiplication)
* ``2-3`` (subtraction)
* ``2^3`` (raising powers)
* ``2+3*4``
* ``(2+3)*4``
* ``6/3`` (division)
* ``1/3``

Divisions are not reduced to real numbers, but kept as a rational for
as long as possible, since the rational is an exact correct expression
(and any real number would just be an approximation). Yacas is able to
change a rational in to a number with the function ``N``, for example
``N(1/3)``.

Operators have *precedence*, meaning that certain operations are done
first before others are done. For example, in ``2+3*4`` the
multiplication is performed before the addition. The usual way to
change the order of a calculation is with round brackets.  The round
brackets in the expression ``(2+3)*4`` will force Yacas to first add 2
and 3, and then multiply the result.

Simple function calls have their arguments between round brackets,
separated by commas. Examples are ``Sin(Pi)`` (which indicates that
you are interested in the value of the trigonometric function
:math:`\sin` applied to the constant :math:`\pi`), and
``Min(5,1,3,-5,10)`` (which should return the lowest of its arguments,
``-5`` in this case).  Functions usually have the form ``f()``,
``f(x)`` or ``f(x,y,z,...)`` depending on how many arguments the
function accepts. Functions always return a result.  For example,
``Cos(0)`` should return ``1``. Evaluating functions can be thought of
as simplifying an expression as much as possible. Sometimes further
simplification is not possible and a function returns itself
unsimplified, like taking the square root of an integer ``Sqrt(2)``. A
reduction to a number would be an approximation. We explain elsewhere
how to get Yacas to simplify an expression to a number.

Yacas allows for use of the infix notation, but with some
additions. Functions can be *bodied*, meaning that the last argument
is written past the close bracket. An example is ``ForEach``, where we
write ``ForEach(item, 1 .. 10) Echo(item);``.  ``Echo(item)`` is the
last argument to the function ``ForEach``.  <br /> A list is enclosed
with curly braces, and is written out with commas between the
elements, like for example ``{1,2,3}``.  items in lists (and things
that can be made to look like lists, like arrays and strings), can
then be accessed by indicating the index between square brackets after
the object. ``{a,b,c}[2]`` should return ``b``, as ``b`` is the second
element in the list (Yacas starts counting from 1 when accessing
elements). The same can be done with strings: ``"abc"[2]``.  <br />
And finally, function calls can be grouped together, where they get
executed one at a time, and the result of executing the last
expression is returned. This is done through square brackets, as ``[
Echo("Hello"); Echo("World"); True; ];``, which first writes ``Hello``
to screen, then ``World`` on the next line, and then returns ``True``.

When you type in an expression, you have to take in to account the
fact that Yacas is case-sensitive. This means that a function ``sin``
(with all lowercase) is a different function from ``Sin`` (which
starts with a capital S), and the variable ``v`` is a different one
from ``V``.

=======================================
Using Yacas from the calculation center
=======================================

As mentioned earlier, you can type in commands on the command line in
the calculation center. Typically, you would enter one statement per
line, for example, click on ``Sin(Pi/2);``. The has a memory, and
remembers results from calculations performed before.  For example, if
you define a function on a line (or set a variable to a value), the
defined function (or variable) are available to be used in following
lines. A session can be restarted (forgetting all previous definitions
and results) by typing ``restart``.  All memory is erased in that
case.

Statements should end with a semicolon ``;`` although this is not
required in interactive sessions (Yacas will append a semicolon at end
of line to finish the statement).

The command line has a history list, so it should be easy to browse
through the expressions you entered previously using the up and down
arrow keys.

When a few characters have been typed, the command line will use the
characters before the cursor as a filter into the history, and allow
you to browse through all the commands in the history that start with
these characters quickly, instead of browsing through the entire
history. If the system recognized the first few characters, it will
also show the commands that start with the sequence entered. You can
use the arrow keys to browse through this list, and then select the
intended function to be inserted by pressing enter.

Commands spanning multiple lines can (and actually have to) be entered
by using a trailing backslash \ at end of each continued line. For
example, clicking on ``2+3+`` will result in an
error, but entering the same with a backslash at the end and then
entering another expression will concatenate the two lines and
evaluate the concatenated input.

Incidentally, any text Yacas prints without a prompt is either
a message printed by a function as a side-effect, or an error
message. Resulting values of expressions are always printed after an
``Out>`` prompt.

==============================
Yacas as a symbolic calculator
==============================

We are ready to try some calculations. Yacas uses a C-like
infix syntax and is case-sensitive. Here are some exact manipulations
with fractions for a start: ``1/14+5/21*(30-(1+1/2)*5^2);``

The standard scripts already contain a simple math library for
symbolic simplification of basic algebraic functions. Any names such
as ``x`` are treated as independent, symbolic variables and are not
evaluated by default. Some examples to try:

* ``0+x``
* ``x+1*y``
* ``Sin(ArcSin(alpha))+Tan(ArcTan(beta))``

Note that the answers are not just simple numbers here, but actual
expressions. This is where Yacas shines. It was built specifically to
do calculations that have expressions as answers.

In Yacas after a calculation is done, you can refer to the previous
result with ``%``. For example, we could first type ``(x+1)*(x-1)``,
and then decide we would like to see a simpler version of that
expression, and thus type ``Simplify(%)``,
which should result in ``x^2-1``.

The special operator ``%`` automatically recalls the result from the
previous line.  The function ``Simplify`` attempts to reduce an
expression to a simpler form. Note that standard function names in
Yacas are typically capitalized. Multiple capitalization such as
``ArcSin`` is sometimes used. The underscore character ``_`` is a
reserved operator symbol and cannot be part of variable or function
names.

Yacas offers some more powerful symbolic manipulation
operations. A few will be shown here to wetten the appetite.

Some simple equation solving algorithms are in place:

* ``Solve(x/(1+x) == a, x);``
* ``Solve(x^2+x == 0, x);``
* ``Solve(a+x*y==z,x);``

(Note the use of the ``==`` operator, which does not evaluate to
anything, to denote an "equation" object.)

Taylor series are supported, for example: ``Taylor(x,0,3) Exp(x)`` is
a bodied operator that expands ``Exp(x)`` for ``x`` around ``x=0``, up
to order 3.

Symbolic manipulation is the main application of Yacas. This is
a small tour of the capabilities Yacas currently offers. Note
that this list of examples is far from complete. Yacas contains
a few hundred commands, of which only a few are shown here.

* ``Expand((1+x)^5);`` (expand the expression into a polynomial)
* ``Limit(x,0) Sin(x)/x;`` (calculate the limit of ``Sin(x)/x`` as
  ``x`` approaches zero)
* ``Newton(Sin(x),x,3,0.0001);`` (use Newton's method to find the
  value of ``x`` near ``3`` where ``Sin(x)`` equals zero numerically
  and stop if the result is closer than ``0.0001`` to the real result)
* ``DiagonalMatrix({a,b,c});`` (create a matrix with the elements
  specified in the vector on the diagonal)
* ``Integrate(x,a,b) x*Sin(x);`` (integrate a function over variable
  ``x``, from ``a`` to ``b``)
* ``Factor(x^2-1);`` (factorize a polynomial)
* ``Apart(1/(x^2-1),x);`` (create a partial fraction expansion of a
  polynomial)
* ``Simplify((x^2-1)/(x-1));`` (simplification of expressions)
* ``CanProve( (a And b) Or (a And Not b) );`` (special-purpose
  simplifier that tries to simplify boolean expressions as much as
  possible)
* ``TrigSimpCombine(Cos(a)*Sin(b));`` (special-purpose simplifier that
  tries to transform trigonometric expressions into a form where there
  are only additions of trigonometric functions involved and no
  multiplications)

===========================
Arbitrary precision numbers
===========================

Yacas can deal with arbitrary precision numbers. It can work with
large integers, like ``20!`` (The ! means factorial, thus
``1*2*3*...*20``).

As we saw before, rational numbers will stay rational as long as the
numerator and denominator are integers, so ``55/10`` will evaluate to
``11/2``. You can override this behavior by using the numerical
evaluation function ``N()``. For example, ``N(55/10)`` will evaluate
to ``5.5`` . This behavior holds for most math functions. Yacas will
try to maintain an exact answer (in terms of integers or fractions)
instead of using floating point numbers, unless ``N()`` is used. Where
the value for the constant pi is needed, use the built-in variable
``Pi``. It will be replaced by the (approximate) numerical value when
``N(Pi)`` is called.  Yacas knows some simplification rules using
``Pi`` (especially with trigonometric functions).

The function ``N`` takes either one or two arguments. It evaluates its
first argument and tries to reduce it as much as possible to a
real-valued approximation of the expression. If the second argument is
present, it states the number of digits precision required. Thus
``N(1/234)`` returns a number with the current default precision
(which starts at 20 digits), but you can request as many digits as you
like by passing a second argument, as in ``N(1/234, 10)``, ``N(1/234,
20)``, ``N(1/234, 30)``, etcetera.

Note that we need to enter ``N()`` to force the approximate
calculation, otherwise the fraction would have been left unevaluated.

Revisiting ``Pi``, we can get as many digits of ``Pi`` as we like, by
providing the precision required as argument to ``N``.  So to get 50
digits precision, we can evaluate ``N(Pi,50)``.

Taking a derivative of a function was amongst the very first of
symbolic calculations to be performed by a computer, as the operation
lends itself surprisingly well to being performed
automatically. Naturally, it is also implemented in Yacas, through the
function ``D``.  ``D`` is a <i>bodied</i> function, meaning that its
last argument is past the closing brackets. Where normal functions are
called with syntax similar to ``f(x,y,z)``, a bodied function would be
called with a syntax ``f(x,y)z``. Here are two examples of taking a
derivative: <ul> <li> ``D(x) Sin(x);`` (taking a derivative)</li>
<li>``D(x) D(x) Sin(x);`` (taking a derivative twice)</li> </ul> The
{D} function also accepts an argument specifying how often the
derivative has to be taken. In that case, the above expressions can
also be written as: <ul> <li> ``D(x,1) Sin(x);`` (taking a
derivative)</li> <li>``D(x,2) Sin(x);`` (taking a derivative
twice)</li> </ul>

==================
Analytic functions
==================

Many of the usual analytic functions have been defined in the Yacas
library. Examples are ``Exp(1)``, ``Sin(2)``, ``ArcSin(1/2)``,
``Sqrt(2)``.  These will not evaluate to a numeric result in general,
unless the result is an integer, like ``Sqrt(4)``. If asked to reduce
the result to a numeric approximation with the function ``N``, then
<i>Yacas will do so</i>, as for example in ``N(Sqrt(2),50)``.

=========
Variables
=========

Yacas supports variables. You can set the value of a variable with the
``:=`` infix operator, as in ``a:=1;``. The variable can then be used
in expressions, and everywhere where it is referred to, it will be
replaced by its value.

To clear a variable binding, execute ``Clear(a);``.  A variable will
evaluate to itself after a call to clear it (so after the call to
clear ``a`` above, calling <span class="commandlink">a`` should now
return ``a``).  This is one of the properties of the evaluation scheme
of Yacas; when some object can not be evaluated or transformed any
further, it is returned as the final result.

=========
Functions
=========

The ``:=`` operator can also be used to define simple functions:
``f(x):=2*x*x``.  will define a new function, ``f``, that accepts one
argument and returns twice the square of that argument.  This function
can now be called, ``f(a)``. You can change the definition of a
function by defining it again.

One and the same function name such as ``f`` may define different
functions if they take different numbers of arguments. One can define
a function ``f`` which takes one argument, as for example
``f(x):=x^2;``, or two arguments, ``f(x,y):=x*y;``.  If you clicked on
both links, both functions should now be defined, and ``f(a)`` calls
the one function whereas ``f(a,b)`` calls the other.

Yacas is very flexible when it comes to types of mathematical
objects. Functions can in general accept or return any type of
argument.

==================================
Boolean expressions and predicates
==================================

Yacas predefines ``True`` and ``False`` as boolean values. Functions
returning boolean values are called <i>predicates</i>. For example,
``IsNumber()`` and`` IsInteger()`` are predicates defined in the Yacas
environment. For example, try ``IsNumber(2+x);``, or
``IsInteger(15/5);``.

There are also comparison operators. Typing ``2 > 1`` would return
``True``. You can also use the infix operators ``And`` and ``Or``, and
the prefix operator ``Not``, to make more complex boolean
expressions. For example, try ``True And False``, ``True Or False``,
``True And Not(False)``.

=================
Strings and lists
=================

In addition to numbers and variables, Yacas supports strings and
lists. Strings are simply sequences of characters enclosed by double
quotes, for example: ``"this is a string with \"quotes\" in it"``.

Lists are ordered groups of items, as usual. Yacas represents lists by
putting the objects between braces and separating them with
commas. The list consisting of objects a, b, and c could be entered by
typing ``{a,b,c}``.  In Yacas, vectors are represented as lists and
matrices as lists of lists.

Items in a list can be accessed through the ``[ ]`` operator. The
first element has index one. Examples: when you enter
``uu:={a,b,c,d,e,f};`` then ``uu[2];`` evaluates to ``b``, and
``uu[2 .. 4];`` evaluates to ``{b,c,d}``. The "range" expression
``2 .. 4`` evaluates to ``{2,3,4}``. Note that spaces around the
``..`` operator are necessary, or else the parser will not be able to
distinguish it from a part of a number.

Lists evaluate their arguments, and return a list with results of
evaluating each element. So, typing ``{1+2,3};`` would evaluate to ``{3,3}``.

The idea of using lists to represent expressions dates back to the
language LISP developed in the 1970's. From a small set of operations
on lists, very powerful symbolic manipulation algorithms can be
built. Lists can also be used as function arguments when a variable
number of arguments are necessary.

Let's try some list operations now. First click on ``m:={a,b,c};`` to
set up an initial list to work on. Then click on links below: <ul>
<li>``Length(m);`` (return the length of a list)</li>
<li>``Reverse(m);`` (return the string reversed)</li>
<li>``Concat(m,m);`` (concatenate two strings)</li> <li>``m[1]:=d;``
(setting the first element of the list to a new value, d, as can be
verified by evaluating ``m``)</li> </ul> Many more list operations are
described in the reference manual.

============================
Writing simplification rules
============================

Mathematical calculations require versatile transformations on
symbolic quantities. Instead of trying to define all possible
transformations, Yacas provides a simple and easy to use pattern
matching scheme for manipulating expressions according to user-defined
<i>rules</i>. Yacas itself is designed as a small core engine
executing a large library of rules to match and replace patterns.

One simple application of pattern-matching rules is to define new
functions. (This is actually the only way Yacas can learn about new
functions.) As an example, let's define a function ``f`` that will
evaluate factorials of non-negative integers. We will define a
predicate to check whether our argument is indeed a non-negative
integer, and we will use this predicate and the obvious recursion
``f(n)=n*f(n-1) if n>0 and 1 if n=0`` to evaluate the factorial.

We start with the simple termination condition, which is that ``f(n)``
should return one if ``n`` is zero: <ul> <li>``10 # f(0) &lt;--
1;``</li> </ul> You can verify that this already works for input value
zero, with ``f(0)``.

Now we come to the more complex line, <ul> <li>``20 #
f(n_IsIntegerGreaterThanZero) &lt;-- n*f(n-1);``</li> </ul> Now we
realize we need a function ``IsGreaterThanZero``, so we define this
function, with <ul> <li>``IsIntegerGreaterThanZero(_n) &lt;--
(IsInteger(n) And n&gt;0);``</li> </ul> You can verify that it works
by trying ``f(5)``, which should return the same value as ``5!``.

In the above example we have first defined two "simplification rules"
for a new function ``f()``. Then we realized that we need to define a
predicate ``IsIntegerGreaterThanZero()``. A predicate equivalent to
``IsIntegerGreaterThanZero()`` is actually already defined in the
standard library and it's called ``IsPositiveInteger``, so it was not
necessary, strictly speaking, to define our own predicate to do the
same thing. We did it here just for illustration purposes.

The first two lines recursively define a factorial function
``f(n)=n*(n-1)*...*1``. The rules are given precedence values 10 and
20, so the first rule will be applied first.  Incidentally, the
factorial is also defined in the standard library as a postfix
operator ! and it is bound to an internal routine much faster than the
recursion in our example. The example does show how to create your own
routine with a few lines of code. One of the design goals of Yacas was
to allow precisely that, definition of a new function with very little
effort.

The operator ``&lt;--`` defines a rule to be applied to a specific
function. (The ``&lt;--`` operation cannot be applied to an atom.)
The ``_n`` in the rule for ``IsIntegerGreaterThanZero()`` specifies
that any object which happens to be the argument of that predicate is
matched and assigned to the local variable ``n``. The expression to
the right of ``&lt;--`` can use n (without the underscore) as a
variable.

Now we consider the rules for the function ``f``. The first rule just
specifies that ``f(0)`` should be replaced by 1 in any expression. The
second rule is a little more involved.  ``n_IsIntegerGreaterThanZero``
is a match for the argument of ``f``, with the proviso that the
predicate ``IsIntegerGreaterThanZero(n)`` should return ``True``,
otherwise the pattern is not matched. The underscore operator is to be
used only on the left hand side of the rule definition operator
``&lt;--``.

There is another, slightly longer but equivalent way of writing the
second rule: <ul> <li>``20 # f(_n)_(IsIntegerGreaterThanZero(n))
&lt;-- n*f(n-1);``</li> </ul> The underscore after the function object
denotes a "postpredicate" that should return ``True`` or else there is
no match. This predicate may be a complicated expression involving
several logical operations, unlike the simple checking of just one
predicate in the ``n_IsIntegerGreaterThanZero`` construct. The
postpredicate can also use the variable ``n`` (without the
underscore).

Precedence values for rules are given by a number followed by the
``#`` infix operator (and the transformation rule after it). This
number determines the ordering of precedence for the pattern matching
rules, with 0 the lowest allowed precedence value, i.e. rules with
precedence 0 will be tried first. Multiple rules can have the same
number: this just means that it doesn't matter what order these
patterns are tried in. If no number is supplied, 0 is assumed. In our
example, the rule ``f(0) &lt;-- 1`` must be applied earlier than the
recursive rule, or else the recursion will never terminate. But as
long as there are no other rules concerning the function ``f``, the
assignment of numbers 10 and 20 is arbitrary, and they could have been
500 and 501 just as well.  It is usually a good idea however to keep
some space between these numbers, so you have room to insert new
transformation rules later on.

Predicates can be combined: for example, {IsIntegerGreaterThanZero()}
could also have been defined as: <ul> <li>``10 #
IsIntegerGreaterThanZero(n_IsInteger)_(n&gt;0) &lt;-- True;``</li>
<li>``20 # IsIntegerGreaterThanZero(_n) &lt;-- False;``</li> </ul> The
first rule specifies that if n is an integer, and is greater than
zero, the result is ``True``, and the second rule states that
otherwise (when the rule with precedence 10 did not apply) the
predicate returns ``False``.

In the above example, the expression ``n &gt; 0`` is added after the
pattern and allows the pattern to match only if this predicate return
``True``. This is a useful syntax for defining rules with complicated
predicates. There is no difference between the rules``
F(n_IsPositiveInteger) &lt;--...`` and ``F(_n)_(IsPositiveInteger(n))
&lt;-- ...`` except that the first syntax is a little more concise.

The left hand side of a rule expression has the following form: ::

  <i>precedence</i> # <i>pattern</i> _ <i>postpredicate</i> <-- <i>replacement</i> ;

The optional *precedence* must be
a positive integer.

Some more examples of rules (not made clickable because their
equivalents are already in the basic Yacas library): <ul> <li>``10 #
_x + 0 &lt;-- x;``</li> <li>``20 # _x - _x &lt;-- 0;``</li>
<li>``ArcSin(Sin(_x)) &lt;-- x;``</li> </ul> The last rule has no
explicit precedence specified in it (the precedence zero will be
assigned automatically by the system).

Yacas will first try to match the pattern as a template. Names
preceded or followed by an underscore can match any one object: a
number, a function, a list, etc. Yacas will assign the relevant
variables as local variables within the rule, and try the predicates
as stated in the pattern. The post-predicate (defined after the
pattern) is tried after all these matched. As an example, the
simplification rule ``_x - _x &lt;--0`` specifies that the two objects
at left and at right of the minus sign should be the same for this
transformation rule to apply.

==========================
Local simplification rules
==========================

Sometimes you have an expression, and you want to use specific
simplification rules on it that should not be universally applied.
This can be done with the ``/:`` and the ``/::`` operators.  Suppose
we have the expression containing things such as ``Ln(a*b)``, and we
want to change these into ``Ln(a)+Ln(b)``. The easiest way to do this
is using the ``/:`` operator as follows:

* ``Sin(x)*Ln(a*b)`` (example expression without simplification)
* ``Sin(x)*Ln(a*b) /: {Ln(_x*_y) <- Ln(x)+Ln(y) }`` (with instruction
  to simplify the expression)

A whole list of simplification rules can be built up in the list, and
they will be applied to the expression on the left hand side of
``/:``.

Note that for these local rules, ``&lt;-`` should be used instead of
``&lt;--``.  Using latter would result in a global definition of a new
transformation rule on evaluation, which is not the intention.

The ``/:`` operator traverses an expression from the top down, trying
to apply the rules from the beginning of the list of rules to the end
of the list of rules. If no rules can be applied to the whole
expression, it will try the sub-expressions of the expression being
analyzed.

It might be sometimes necessary to use the ``/::`` operator, which
repeatedly applies the ``/:`` operator until the result does not
change any more. Caution is required, since rules can contradict each
other, and that could result in an infinite loop. To detect this
situation, just use ``/:`` repeatedly on the expression. The
repetitive nature should become apparent.

======================
Programming essentials
======================

An important feature of Yacas is its programming language which
allows you to create your own programs for doing calculations.  This
section describes some constructs and functions for control flow.

Looping can be done with the function ``ForEach``. There are more
options, but ForEach is the simplest to use for now and will suffice
for this turorial.  The statement form ``ForEach(x, list) body``
executes its body for each element of the list and assigns the
variable x to that element each time. The statement form
``While(predicate) body`` repeats execution of the expression
represented by ``body`` until evaluation of the expression represented
by ``predicate`` returns ``False``.

This example loops over the integers from one to three, and writes out
a line for each, multiplying the integer by 3 and displaying the
result with the function ``Echo``: ``ForEach(x,1 .. 5) Echo(x," times
3 equals ",3*x);``

Compound statements
-------------------

Multiple statements can be grouped together using the ``[`` and ``]``
brackets. The compound ``[a; Echo("In the middle"); 1+2;];`` evaluates
``a``, then the ``Echo`` command, and finally evaluates ``1+2``, and
returns the result of evaluating the last statement ``1+2``.

A variable can be declared local to a compound statement block by the
function ``Local(var1, var2, ...)``. For example, if you execute
``[Local(v);v:=1+2;v;];`` the result will be ``3``. The program body
created a variable called ``v``, assigned the value of evaluating
``1+2`` to it, and made sure the contents of the variable ``v`` were
returned.  If you now evaluate ``v`` afterwards you will notice that
the variable ``v`` is not bound to a value any more. The variable
``v`` was defined locally in the program body between the two square
brackets ``[`` and ``]``.

Conditional execution is implemented by the ``If(predicate, body1,
body2)`` function call. If the expression ``predicate`` evaluates to
``True``, the expression represented by ``body1`` is evaluated,
otherwise ``body2`` is evaluated, and the corresponding value is
returned. For example, the absolute value of a number can be computed
with: ``f(x) := If(x < 0,-x,x);`` (note that there already is a
standard library function that calculates the absolute value of a
number).

Variables can also be made to be local to a small set of functions,
with ``LocalSymbols(variables) body``. For example, the following code
snippet: ``LocalSymbols(a,b) [a:=0;b:=0;
inc():=[a:=a+1;b:=b-1;show();]; show():=Echo("a = ",a," b = ",b); ];``
defines two functions, ``inc`` and ``show``. Calling ``inc()``
repeatedly increments ``a`` and decrements ``b``, and calling
``show()`` then shows the result (the function "inc" also calls the
function "show", but the purpose of this example is to show how two
functions can share the same variable while the outside world cannot
get at that variable). The variables are local to these two functions,
as you can see by evaluating ``a`` and ``b`` outside the scope of
these two functions. This feature is very important when writing a
larger body of code, where you want to be able to guarantee that there
are no unintended side-effects due to two bits of code defined in
different files accidentally using the same global variable.

To illustrate these features, let us create a list of all even
integers from 2 to 20 and compute the product of all those integers
except those divisible by 3 ::

  [
      Local(L,i,answer);
      L:={}; i:=2;
      /*Make a list of all even integers from 2 to 20 */
      While (i <= 20) [ L := Append(L, i); i := i + 2; ];
      /* Now calculate the product of all of these numbers that are not divisible by 3 */
      answer := 1;
      ForEach(i,L) If (Mod(i, 3) != 0, answer := answer * i);
      /* And return the answer */
      answer;
  ];

(Note that it is not necessarily the most economical way to do it in
Yacas.)

We used a shorter form of ``If(predicate, body)`` with only one body
which is executed when the condition holds. If the condition does not
hold, this function call returns ``False``. We also introduced
comments, which can be placed between ``/*`` and ``*/``. Yacas will
ignore anything between those two. When putting a program in a file
you can also use ``//``. Everything after ``//`` up until the end of
the line will be a comment.  Also shown is the use of the ``While``
function. Its form is ``While (predicate) body``.  While the
expression represented by ``predicate`` evaluates to ``True``, the
expression represented by ``body`` will keep on being evaluated.

The above example is not the shortest possible way to write out the
algorithm. It is written out in a procedural way, where the program
explains step by step what the computer should do. There is nothing
fundamentally wrong with the approach of writing down a program in a
procedural way, but the symbolic nature of Yacas also allows you to
write it in a more concise, elegant, compact way, by combining
function calls.

There is nothing wrong with procedural style, but there is amore
'functional' approach to the same problem would go as follows
below. The advantage of the functional approach is that it is shorter
and more concise (the difference is cosmetic mostly).

Before we show how to do the same calculation in a functional style,
we need to explain what a *pure function* is, as you will need it a
lot when programming in a functional style. We will jump in with an
example that should be self-explanatory. Consider the expression
``Lambda({x,y},x+y)``.  This has two arguments, the first listing x
and y, and the second an expression. We can use this construct with
the function Apply as follows: ``Apply(Lambda({x,y},x+y),{2,3})``. The
result should be ``5``, the result of adding ``2`` and ``3``. The
expression starting with ``Lambda`` is essentially a prescription for
a specific operation, where it is stated that it accepts 2 arguments,
and returns the arguments added together.  In this case, since the
operation was so simple, we could also have used the name of a
function to apply the arguments to, the addition operator in this case
``Apply("+",{2,3})``. When the operations become more complex however,
the Lambda construct becomes more useful.

Now we are ready to do the same example using a functional
approach. First, let us construct a list with all even numbers from 2
to 20. For this we use the ``..`` operator to set up all numbers from
one to ten, and then multiply that with two: ``2 * (1 .. 10)``.

Now we want an expression that returns all the even numbers up to 20
which are not divisible by 3. For this we can use ``Select``, which
takes as first argument a predicate that should return ``True`` if the
list item is to be accepted, and ``False`` otherwise, and as second
argument the list in question:
``Select(Lambda({n},Mod(n,3)!=0),2*(1 .. 10))``.  The numbers 6, 12
and 18 have been correctly filtered out. Here you see one example of a
pure function where the operation is a little bit more complex.

All that remains is to factor the items in this list. For this we can
use ``UnFlatten``.  Two examples of the use of ``UnFlatten`` are

* ``UnFlatten({a,b,c},"*",1)``
* ``UnFlatten({a,b,c},"+",0)``

The 0 and 1 are a base element to start with when grouping the
arguments in to an expression (they should be the respective `identity
elements <http://en.wikipedia.org/wiki/Identity_element>`_, hence it
is zero for addition and 1 for multiplication).

Now we have all the ingredients to finally do the same calculation we
did above in a procedural way, but this time we can do it in a
functional style, and thus captured in one concise single line: ::

  UnFlatten(Select(Lambda({n},Mod(n,3)!=0),2*(1 .. 10)),"*",1)

As was mentioned before, the choice between the two is mostly a matter
of style.

======
Macros
======

One of the powerful constructs in Yacas is the construct of a
macro. In its essence, a macro is a prescription to create another
program before executing the program. An example perhaps explains it
best. Evaluate the following expression ``Macro(for,{st,pr,in,bd})
[(@st);While(@pr)[(@bd);(@in);];];``. This expression defines a macro
that allows for looping.  Yacas has a ``For`` function already, but
this is how it could be defined in one line (In Yacas the ``For``
function is bodied, we left that out here for clarity, as the example
is about macros).

To see it work just type ``for(i:=0,i&lt;3,i:=i+1,Echo(i))``. You will see
the count from one to three.

The construct works as follows; The expression defining the macro sets
up a macro named ``for`` with four
arguments. On the right is the body of the macro. This body contains
expressions of the form ``@var``. These are replaced by the values
passed in on calling the macro.  After all the variables have been
replaced, the resulting expression is evaluated. In effect a new
program has been created. Such macro constructs come from LISP, and
are famous for allowing you to almost design your own programming
language constructs just for your own problem at hand. When used
right, macros can greatly simplify the task of writing a program.

You can also use the back-quote ````` to expand a macro in-place. It
takes on the form ```(expression)``, where the expression can again
contain sub-expressions of the form ``@variable``. These instances
will be replaced with the values of these variables.

====================================
The practice of programming in Yacas
====================================

When you become more proficient in working with Yacas you will be
doing more and more sophisticated calculations. For such calculations
it is generally necessary to write little programs. In real life you
will usually write these programs in a text editor, and then start
Yacas, load the text file you just wrote, and try out the
calculation. Generally this is an iterative process, where you go back
to the text editor to modify something, and then go back to Yacas,
type ``restart`` and then reload the file.

On this site you can run Yacas in a little window called a Yacas
calculation center (the same as the one below this tutorial). On page
there is tab that contains a Yacas calculation center. If you click on
that tab you will be directed to a larger calculation center than the
one below this tutorial. In this page you can easily switch between
doing a calculation and editing a program to load at startup. We tried
to make the experience match the general use of Yacas on a desktop as
much as possible. The Yacas journal (which you see when you go to the
Yacas web site) contains examples of calculations done before by
others.

===========================
Defining your own operators
===========================

Large part of the Yacas system is defined in the scripting language
itself. This includes the definitions of the operators it accepts, and
their precedences. This means that you too can define your own
operators. This section shows you how to do that.

Suppose we wanted to define a function ``F(x,y)=x/y+y/x``. We could
use the standard syntax ``F(a,b) := a/b + b/a;``.  ``F(1,2);``. For
the purpose of this demonstration, lets assume that we want to define
an infix operator ``xx`` for this operation. We can teach Yacas about
this infix operator with ``Infix("xx", OpPrecedence("/"));``. Here we
told Yacas that the operator ``xx`` is to have the same precedence as
the division operator.  We can now proceed to tell Yacas how to
evaluate expressions involving the operator ``xx`` by defining it as
we would with a function, ``a xx b := a/b + b/a;``.

You can verify for yourself ``3 xx 2 + 1;`` and ``1 + 3 xx 2;`` return
the same value, and that they follow the precedence rules (eg. ``xx``
binds stronger than ``+``).

We have chosen the name ``xx`` just to show that we don't need to use
the special characters in the infix operator's name. However we must
define this operator as infix before using it in expressions,
otherwise Yacas will raise a syntax error.

Finally, we might decide to be completely flexible with this important
function and also define it as a mathematical operator ``##`` . First
we define ``##`` as a <i>bodied</i> function and then proceed as
before. First we can tell Yacas that ``##`` is a bodied operator with
``Bodied("##", OpPrecedence("/"));``. Then we define the function
itself: ``##(a) b := a xx b;``. And now we can use the function,
``##(1) 3 + 2;``.

We have used the name ``##`` but we could have used any other name
such as ``xx`` or ``F`` or even ``_-+@+-_``.  Apart from possibly
confusing yourself, it doesn't matter what you call the functions you
define.

There is currently one limitation in Yacas: once a function name is
declared as infix (prefix, postfix) or bodied, it will always be
interpreted that way. If we declare a function ``f`` to be bodied, we
may later define different functions named ``f`` with different
numbers of arguments, however all of these functions must be bodied.

When you use infix operators and either a prefix of postfix operator
next to it you can run in to a situation where Yacas can not quite
figure out what you typed. This happens when the operators are right
next to each other and all consist of symbols (and could thus in
principle form a single operator). Yacas will raise an error in that
case. This can be avoided by inserting spaces.

================================
Some assorted programming topics
================================

One use of lists is the associative list, sometimes called a
dictionary in other programming languages, which is implemented in
Yacas simply as a list of key-value pairs. Keys must be strings and
values may be any objects. Associative lists can also work as
mini-databases, where a name is associated to an object.  As an
example, first enter ``record:={};`` to set up
an empty record. After that, we can fill arbitrary fields in this
record: ::

  record["name"]:="Isaia";
  record["occupation"]:="prophet";
  record["is alive"]:=False;

Now, evaluating ``record["name"]`` should result in the answer
``"Isaia"``. The record is now a list that contains three sublists, as
you can see by evaluating ``record``.

Assignment of multiple variables is also possible using lists. For
instance, evaluating ``{x,y}:={2!,3!}`` will result in 2 being
assigned to ``x`` and 6 to ``y``.

When assigning variables, the right hand side is evaluated before it
is assigned. Thus ``a:=2*2`` will set a to 4. This is however
<i>not</i> the case for functions. When entering ``f(x):=x+x`` the
right hand side, ``x+x``, is not evaluated before being assigned. This
can be forced by using ``Eval()``.  Defining ``f(x)`` with
``f(x):=Eval(x+x)`` will tell the system to first evaluate ``x+x``
(which results in ``2*x``) before assigning it to the user function
``f``. This specific example is not a very useful one but it will come
in handy when the operation being performed on the right hand side is
expensive. For example, if we evaluate a Taylor series expansion
before assigning it to the user-defined function, the engine doesn't
need to create the Taylor series expansion each time that user-defined
function is called.

The imaginary unit i is denoted ``I`` and complex numbers can be
entered as either expressions involving ``I``, as for example
``1+I*2``, or explicitly as ``Complex(a,b)`` for a+ib. The form
``Complex(re,im)`` is the way Yacas deals with complex numbers
internally.

==============
Linear Algebra
==============

Vectors of fixed dimension are represented as lists of their
components. The list ``{1, 2+x, 3*Sin(p)}`` would be a
three-dimensional vector with components ``1``, ``2+x`` and
``3*Sin(p)``. Matrices are represented as a lists of lists.

Vector components can be assigned values just like list items, since
they are in fact list items. If we first set up a variable called
"vector" to contain a three-dimensional vector with the command
``vector:=ZeroVector(3);`` (you can verify that it is indeed a vector
with all components set to zero by evaluating ``vector``), you can
change elements of the vector just like you would the elements of a
list (seeing as it is represented as a list). For example, to set the
second element to two, just evaluate ``vector[2] := 2;``. This results
in a new value for ``vector``.

Yacas can perform multiplication of matrices, vectors and numbers as
usual in linear algebra.  The standard Yacas script library also
includes taking the determinant and inverse of a matrix, finding
eigenvectors and eigenvalues (in simple cases) and solving linear sets
of equations, such as A * x = b where A is a matrix, and x and b are
vectors.  As a little example to wetten your appetite, we define a
Hilbert matrix: ``hilbert:=HilbertMatrix(3)``. We can then calculate
the determinant with ``Determinant(hilbert)``, or the inverse with
``Inverse(hilbert)``.  There are several more matrix operations
supported. See the reference manual for more details.

"Threading" of functions
------------------------

Some functions in Yacas can be "threaded". This means that calling the
function with a list as argument will result in a list with that
function being called on each item in the list. E.g. ``Sin({a,b,c});``
will result in ``{Sin(a),Sin(b),Sin(c)}``. This functionality is
implemented for most normal analytic functions and arithmetic
operators.

Functions as lists
------------------

For some work it pays to understand how things work under the
hood. Internally, Yacas represents all atomic expressions (numbers and
variables) as strings and all compound expressions as lists, like
Lisp. Try ``FullForm(a+b*c);`` and you will see the text ``(+ a (* b c
))`` appear on the screen. This function is occasionally useful, for
example when trying to figure out why a specific transformation rule
does not work on a specific expression.

If you try ``FullForm(1+2)`` you will see that the result is not quite
what we intended. The system first adds up one and two, and then shows
the tree structure of the end result, which is a simple number
``3``. To stop Yacas from evaluating something, you can use the
function ``Hold``, as ``FullForm(Hold(1+2))``. The function ``Eval``
is the opposite, it instructs Yacas to re-evaluate its argument
(effectively evaluating it twice). This undoes the effect of ``Hold``,
as for example ``Eval(Hold(1+2))``.

Also, any expression can be converted to a list by the function
``Listify`` or back to an expression by the function ``UnList``:

* ``Listify(a+b*(c+d));``
* ``UnList({Atom("+"),x,1});``

Note that the first element of the list is the name of the function
``+`` which is equivalently represented as ``Atom("+")`` and that the
subexpression ``b*(c+d)`` was not converted to list form. Listify just
took the top node of the expression.
