=============
Miscellaneous
=============

.. function:: Time(expr)

   measure the time taken by a function

   :param expr: any expression

   The function :func:`Time` evaluates the expression ``expr`` and prints the
   time in seconds needed for the evaluation. The time is printed to the current
   output stream. The built-in function :func:`GetTime` is used for timing.

   The result is the "user time" as reported by the OS, not the real ("wall
   clock") time. Therefore, any CPU-intensive processes running alongside yacas
   will not significantly affect the result of :func:`Time`.

   :Example:

   ::

      In> Time(N(MathLog(1000),40))
      0.34 seconds taken
      Out> 6.9077552789821370520539743640530926228033;


   .. seealso:: :func:`GetTime`


.. function:: SystemCall(str)

   pass a command to the shell

   The command contained in the string ``str`` is executed by the underlying
   operating system. The return value of :func:`SystemCall` is :const:`True` or
   :const:`False` according to the exit code of the command.

   The :func:`SystemCall` function is not allowed in the body of the
   :func:`Secure` command.

   In a UNIX environment, the command ``SystemCall("ls")`` would print
   the contents of the current directory::

      In> SystemCall("ls")
      AUTHORS
      COPYING
      ChangeLog
      ... (truncated to save space)
      Out> True;

   The standard UNIX command ``test`` returns success or failure
   depending on conditions.  For example, the following command will
   check if a directory exists::

      In> SystemCall("test -d scripts/")
      Out> True;

   Check that a file exists::

      In> SystemCall("test -f COPYING")
      Out> True;
      In> SystemCall("test -f nosuchfile.txt")
      Out> False;

   .. seealso:: :func:`Secure`
