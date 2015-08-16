---
layout: page
title: Frequently Asked Questions
permalink: /documentation/faq/
breadcrumb: true
---

What is a computer algebra system?
: A Computer Algebra System (CAS) is a program that can manipulate
  mathematical expressions, potentially reducing the time it takes to
  perform cumbersome but trivial calculations. It does this
  symbolically, so a CAS can return a mathematical expression as a
  resulting answer.

Why cannot Yacas solve my homework/integrodifferential matrix operator constraint equations/ [insert another problem]?
: Yacas is developed by a small group of volunteers and does not yet
  perform many of the sophisticated tasks that a modern CAS can
  theoretically handle. Ask the users' [mailing
  list](https://groups.google.com/forum/#!forum/yacas) if you have a
  specific problem that is covered in the manual and should be
  solvable by yacas. Yacas consists of a small kernel and a library of
  interpreted scripts in the easy-to-use yacas language; the scripts
  contain all CAS-related functionality. You are encouraged to
  contribute library code for solving a specific problem.

I want to use Yacas from inside my own application. What should I do?
: Yacas can be used in several ways: as a command-line application
  that takes text from standard input and prints text to standard
  output; as a server listening on a port (also on remote hosts);
  These options allow you to use the functionality of an installed
  Yacas application directly from another application without having
  to link to any Yacas code.

What platforms are supported?
: Yacas can be run directly from a web site as a Java applet. In
  addition, the C++ version of Yacas is very portable and runs on many
  platforms and OSes, including Unix flavors (including GNU/Linux and
  derivatives), Mac OS X, EPOC32, Ipaq and probably other devices
  running embedded Linux, and 32-bit Microsoft Windows (TM).

Yacas gives an error message, a wrong answer, etc. What's wrong?
: Most probably it is a bug in yacas, especially if you expected a
  correct answer after reading the manuals. Please report it to our
  [bugtracker](https://github.com/grzegorzmazur/yacas/issues). If not
  sure whether this is actually a bug, consider discussing in on our
  [mailing list](https://groups.google.com/forum/#!forum/yacas).

Is there a mailing list?
: [You bet!](https://groups.google.com/forum/#!forum/yacas)

