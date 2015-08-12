---
layout: page
title: A demonstration of the amazingly fast convergence of Newton iteration
author: ayalpinkus
---

You probably played the game where you have to guess a number below
one hundred, say, and where you get an indication "higher" or "lower"
for each guess you make. The trick with this game is to always guess
in the middle of the range known to contain the solution, so the
chance that the number you are guessing at is higher is equal to the
chance that the number is lower, thus halving the range with each
answer. This ensures that you arrive at the correct answer in the
least possible steps, on average, if this is all the information you
are given.

The method of searching by guessing in the middle of the possible
solutions is called a binary search (or sometimes the bisection
method), and is often the best approach when trying to find
something. The speed with which you gather information with a binary
search grows linearly with the number of questions you ask. Each time
you get an answer, you have another bit of information, while
maximizing the information content of the answer to each question. The
answer you get to a question where the odds for a certain answer is 99
percent gives much less information, because you already knew what the
answer was likely to be.

There are situations where binary search is not the best approach, and
finding the zero of a smooth continuous function is one such
example. For finding a zero, Newton iteration will usually outshine
binary search, and it is not even a close call. The reason finding a
zero of an analytic function is better done through Newton iteration
is that with a smooth continuous function one *does* have more
information, namely the derivative of the function! The function,
together with its derivative, can be used in an ingenious way to not
just determine the direction one has to consider for the zero, but
also to determine an estimate of the *distance* to the final
solution.

The speed of convergence of Newton iteration can sometimes be nothing
short of astounding. It will often happen, for suitably neatly defined
problems, that the amount of information you gather doubles with each
question you ask! Or even triples! Imagine having an encyclopedia,
where the number of volumes you have doubles each time you put in some
effort to get new information (by finding the answer to a
question). Each time you get an answer to a question, you double the
number of words of information you have. You can see this happen in
the demonstration at the top of this article, where the digits in red
indicate correct digits, and black are as of yet incorrect
digits. With the typical example, the number of digits precision
doubles or triples with each iteration. This is very far off from the
application of binary search, where you get one extra bit of
information in a linear fashion, each time you ask a question. With
binary search, you would have ten bits of information after ten
inquiries. With a Newton iteration you are bound to have around a
thousand (two to the power of ten). And the speed of growth of
information keeps speeding up exponentially.

{% include alert alert='Please note that the remaining part of the original note refers to functionality which is no longer present. Sorry for the inconvenience.' %}

At the top of this article is a little utility written in Javascript
that demonstrates the fast convergence of the Newton iteration for a
few examples. You can also enter your own. The digits in red are the
digits where the value is the same as the digit in the line below it
(the assumption is made that that digit has the final and correct
value). As you can see, the speed at which the number of red digits
increases per iteration grows exponentially for many examples.


The following <a
href="journaldescr.html?newtonconvergence">article</a> delves deeper
in to the math of Newton iteration, and presents the Yacas script code
that was used for this demonstration.
