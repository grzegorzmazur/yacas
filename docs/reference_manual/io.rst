=========================
Input/output and plotting
=========================

This chapter contains commands to use for input and output and
plotting. All output commands write to the same destination stream,
called the "current output". This is initially the screen, but may be
redirected by some commands. Similarly, most input commands read from
the "current input" stream, which can also be redirected. The
exception to this rule are the commands for reading script files,
which simply read a specified file.

.. function:: FullForm(expr)

   print an expression in LISP-format

   :param expr: expression to be printed in LISP-format

   Evaluates "expr", and prints it in LISP-format on the current
   output. It is followed by a newline. The evaluated expression is
   also  returned.    This can be useful if you want to study the
   internal representation of  a certain expression.

   :Example:

   ::

      In> FullForm(a+b+c);
      (+ (+ a b )c )
      Out> a+b+c;
      In> FullForm(2*I*b^2);
      (* (Complex 0 2 )(^ b 2 ))
      Out> Complex(0,2)*b^2;
      The first example shows how the expression {a+b+c} is
      internally represented. In the second example, {2*I} is
      first evaluated to {Complex(0,2)} before the expression
      is printed.
      

   .. seealso:: :func:`LispRead`, :func:`Listify`, :func:`Unlist`

.. function:: Echo(item)

   high-level printing routine

   :param item: the item to be printed
   :param list: a list of items to be printed

   If passed a single item, {Echo} will evaluate it and print it to
   the  current output, followed by a newline. If {item} is a string,
   it is  printed without quotation marks.    If there is one
   argument, and it is a list, {Echo} will print all the  entries in
   the list subsequently to the current output, followed by a
   newline. Any strings in the list are printed without quotation
   marks. All other entries are followed by a space.    {Echo} can be
   called with a variable number of arguments, they will all  be
   printed, followed by a newline.    {Echo} always returns :data:`True`.

   :Example:

   ::

      In> Echo(5+3);
      8
      Out> True;
      In> Echo({"The square of two is ", 2*2});
      The square of two is 4
      Out> True;
      In> Echo("The square of two is ", 2*2);
      The square of two is 4
      Out> True;
      Note that one must use the second calling format if one wishes to
      print a list:
      In> Echo({a,b,c});
      a b c
      Out> True;
      In> Echo({{a,b,c}});
      {a,b,c}
      Out> True;
      

   .. seealso:: :func:`PrettyForm`, :func:`Write`, :func:`WriteString`, :func:`RuleBaseListed`

.. function:: PrettyForm(expr)

   print an expression nicely with ASCII art

   :param expr: an expression

   {PrettyForm} renders an expression in a nicer way, using ascii art.
   This is generally useful when the result of a calculation is more
   complex than a simple number.

   :Example:

   ::

      In> Taylor(x,0,9)Sin(x)
      Out> x-x^3/6+x^5/120-x^7/5040+x^9/362880;
      In> PrettyForm(%)
      3    5      7       9
      x    x      x       x
      x - -- + --- - ---- + ------
      6    120   5040   362880
      Out> True;
      

   .. seealso:: :func:`EvalFormula`, :func:`PrettyPrinter'Set`

.. function:: EvalFormula(expr)

   print an evaluation nicely with ASCII art

   :param expr: an expression

   Show an evaluation in a nice way, using {PrettyPrinter'Set}  to
   show 'input = output'.

   :Example:

   ::

      In> EvalFormula(Taylor(x,0,7)Sin(x))
      3    5
      x    x
      Taylor( x , 0 , 5 , Sin( x ) ) = x - -- + ---
      6    120
      

   .. seealso:: :func:`PrettyForm`

.. function:: TeXForm(expr)

   export expressions to $LaTeX$

   :param expr: an expression to be exported

   {TeXForm} returns a string containing a $LaTeX$ representation of
   the Yacas expression {expr}. Currently the exporter handles most
   expression types but not all.

.. function:: CForm(expr)

   export expression to C++ code

   :param expr: expression to be exported

   {CForm} returns a string containing C++ code that attempts to
   implement the Yacas expression {expr}. Currently the exporter
   handles most expression types but not all.

.. function:: IsCFormable(expr)

   check possibility to export expression to C++ code

   :param expr: expression to be exported (this argument is not evaluated)
   :param funclist: list of "allowed" function atoms

   {IsCFormable} returns :data:`True` if the Yacas expression {expr} can be
   exported  into C++ code. This is a check whether the C++ exporter
   {CForm} can be safely  used on the expression.    A Yacas
   expression is considered exportable if it contains only functions
   that can be translated into C++ (e.g. {UnList} cannot be exported).
   All variables and constants are considered exportable.    The
   verbose option prints names of functions that are not exportable.
   The second calling format of {IsCFormable} can be used to "allow"
   certain function names that will be available in the C++ code.

   :Example:

   ::

      In> IsCFormable(Sin(a1)+2*Cos(b1))
      Out> True;
      In> V(IsCFormable(1+func123(b1)))
      IsCFormable: Info: unexportable function(s):
      func123
      Out> False;
      This returned :data:`False` because the function {func123} is not available in C++. We can
      explicitly allow this function and then the expression will be considered
      exportable:
      In> IsCFormable(1+func123(b1), {func123})
      Out> True;
      

   .. seealso:: :func:`CForm`, :func:`V`

.. function:: Write(expr, ...)

   low-level printing routine

   :param expr: expression to be printed

   The expression "expr" is evaluated and written to the current
   output. Note that Write accept an arbitrary number of arguments,
   all  of which are written to the current output (see second
   example). {Write} always returns :data:`True`.

   :Example:

   ::

      In> Write(1);
      1Out> True;
      In> Write(1,2);
      1 2Out> True;
      Write does not write a newline, so the {Out>} prompt
      immediately follows the output of {Write}.
      

   .. seealso:: :func:`Echo`, :func:`WriteString`

.. function:: WriteString(string)

   low-level printing routine for strings

   :param string: the string to be printed

   The expression "string" is evaluated and written to the current
   output without quotation marks. The argument should be a  string.
   WriteString always returns True.

   :Example:

   ::

      In> Write("Hello, world!");
      "Hello, world!"Out> True;
      In> WriteString("Hello, world!");
      Hello, world!Out> True;
      This example clearly shows the difference between Write and
      WriteString. Note that Write and WriteString do not write a newline,
      so the {Out>} prompt immediately follows the output.
      

   .. seealso:: :func:`Echo`, :func:`Write`

.. function:: Space()

   print one or more spaces

   :param nr: the number of spaces to print

   The command {Space()} prints one space on the  current output. The
   second form prints {nr} spaces on the current  output. The result
   is always True.

   :Example:

   ::

      In> Space(5);
      Out> True;
      

   .. seealso:: :func:`Echo`, :func:`Write`, :func:`NewLine`

.. function:: NewLine()

   print one or more newline characters

   :param nr: the number of newline characters to print

   The command {NewLine()} prints one newline character  on the
   current output. The second form prints "nr" newlines on the
   current output. The result is always True.

   :Example:

   ::

      In> NewLine();
      Out> True;
      

   .. seealso:: :func:`Echo`, :func:`Write`, :func:`Space`

.. function:: FromFile(name) body

   connect current input to a file

   :param name: string, the name of the file to read
   :param body: expression to be evaluated

   The current input is connected to the file "name". Then the
   expression  "body" is evaluated. If some functions in "body" try to
   read  from current input, they will now read from the file "name".
   Finally, the  file is closed and the result of evaluating "body" is
   returned.

   :Example:

   ::

      Suppose that the file {foo} contains
      2 + 5;
      Then we can have the following dialogue:
      In> FromFile("foo") res := Read();
      Out> 2+5;
      In> FromFile("foo") res := ReadToken();
      Out> 2;
      

   .. seealso:: :func:`ToFile`, :func:`FromString`, :func:`Read`, :func:`ReadToken`

.. function:: FromString(str) body;

   connect current input to a string

   :param str: a string containing the text to parse
   :param body: expression to be evaluated

   The commands in "body" are executed, but everything that is read
   from the current input is now read from the string "str". The
   result of "body" is returned.

   :Example:

   ::

      In> FromString("2+5; this is never read") \
      res := Read();
      Out> 2+5;
      In> FromString("2+5; this is never read") \
      res := Eval(Read());
      Out> 7;
      

   .. seealso:: :func:`ToString`, :func:`FromFile`, :func:`Read`, :func:`ReadToken`

.. function:: ToFile(name) body

   connect current output to a file

   :param name: string, the name of the file to write the result to
   :param body: expression to be evaluated

   The current output is connected to the file "name". Then the
   expression  "body" is evaluated. Everything that the commands in
   "body" print  to the current output, ends up in the file "name".
   Finally, the  file is closed and the result of evaluating "body" is
   returned.    If the file is opened again, the old contents will be
   overwritten.  This is a limitation of {ToFile}: one cannot append
   to a file that has already been created.

   :Example:

   ::

      Here is how one can create a file with C code to evaluate an expression:
      In> ToFile("expr1.c") WriteString(
      CForm(Sqrt(x-y)*Sin(x)) );
      Out> True;
      The file {expr1.c} was created in the current working directory and it
      contains the line
      sqrt(x-y)*sin(x)
      As another example, take a look at the following command:
      In> [ Echo("Result:");  \
      PrettyForm(Taylor(x,0,9) Sin(x)); ];
      Result:
      3    5      7       9
      x    x      x       x
      x - -- + --- - ---- + ------
      6    120   5040   362880
      Out> True;
      Now suppose one wants to send the output of this command to a
      file. This can be achieved as follows:
      In> ToFile("out") [ Echo("Result:");  \
      PrettyForm(Taylor(x,0,9) Sin(x)); ];
      Out> True;
      After this command the file {out} contains:
      Result:
      3    5      7       9
      x    x      x       x
      x - -- + --- - ---- + ------
      6    120   5040   362880
      

   .. seealso:: :func:`FromFile`, :func:`ToString`, :func:`Echo`, :func:`Write`, :func:`WriteString`, :func:`PrettyForm`, :func:`Taylor`

.. function:: ToString() body

   connect current output to a string

   :param body: expression to be evaluated

   The commands in "body" are executed. Everything that is printed on
   the current output, by {Echo} for instance, is  collected in a
   string and this string is returned.

   :Example:

   ::

      In> str := ToString() [ WriteString(  \
      "The square of 8 is "); Write(8^2); ];
      Out> "The square of 8 is  64";
      

   .. seealso:: :func:`FromFile`, :func:`ToString`, :func:`Echo`, :func:`Write`, :func:`WriteString`

.. function:: Read()

   read an expression from current input


   Read an expression from the current input, and return it
   unevaluated. When  the end of an input file is encountered, the
   token atom {EndOfFile} is returned.

   :Example:

   ::

      In> FromString("2+5;") Read();
      Out> 2+5;
      In> FromString("") Read();
      Out> EndOfFile;
      

   .. seealso:: :func:`FromFile`, :func:`FromString`, :func:`LispRead`, :func:`ReadToken`, :func:`Write`

.. function:: ToStdout() body

   select initial output stream for output

   :param body: expression to be evaluated

   When using {ToString} or {ToFile}, it might happen that something
   needs to be  written to the standard default initial output
   (typically the screen). {ToStdout} can be used to select this
   stream.

.. function:: ReadCmdLineString(prompt)

   read an expression from command line and return in string

   :param prompt: string representing the prompt shown on screen

   This function allows for interactive input similar to the command
   line.  When using this function, the history from the command line
   is also available.    The result is returned in a string, so it
   still needs to be parsed.    This function will typically be used
   in situations where one wants a custom   read-eval-print loop.

   :Example:

   ::

      The following defines a function that when invoked keeps asking
      for an expression (the <i>read</i> step), and then takes
      the derivative of it (the <i>eval</i> step) and then
      uses PrettyForm to display the result (the <i>print</i> step).
      In> ReEvPr() := \
      In>   While(True) [ \
      In>     PrettyForm(Deriv(x) \
      In>      FromString(ReadCmdLineString("Deriv> "):";")Read()); \
      In> ];
      Out> True;
      Then one can invoke the command, from which the following interaction
      might follow:
      In> ReEvPr()
      Deriv> Sin(a^2*x/b)
      /  2     \
      | a  * x |    2
      Cos| ------ | * a  * b
      \   b    /
      ----------------------
      2
      b
      Deriv> Sin(x)
      Cos( x )
      Deriv>
      

   .. seealso:: :func:`Read`, :func:`LispRead`, :func:`LispReadListed`

.. function:: LispRead()

   read expressions in LISP syntax

   The function {LispRead} reads an expression in the LISP syntax from
   the current input, and returns  it unevaluated. When the end of an
   input file is encountered, the  special token atom {EndOfFile} is
   returned.    The Yacas expression {a+b} is written in the LISP
   syntax as {(+ a b)}. The advantage of this syntax is that it is
   less ambiguous than the infix operator grammar that Yacas uses by
   default. 

   :Example:

   ::

      In> FromString("(+ a b)") LispRead();
      Out> a+b;
      In> FromString("(List (Sin x) (- (Cos x)))") \
      LispRead();
      Out> {Sin(x),-Cos(x)};
      In> FromString("(+ a b)")LispRead()
      Out> a+b;

   .. seealso:: :func:`FromFile`, :func:`FromString`, :func:`Read`, :func:`ReadToken`, :func:`FullForm`, :func:`LispReadListed`

.. function:: LispReadListed()

   read expressions in LISP syntax

   The function {LispReadListed} reads a LISP expression
   and returns  it in a list, instead of the form usual to Yacas
   (expressions).  The result can be thought of as applying {Listify}
   to {LispRead}.  The function {LispReadListed} is more useful for
   reading arbitrary LISP expressions, because the   first object in a
   list can be itself a list (this is never the case for Yacas
   expressions where the first object in a list is always a function
   atom).

   :Example:

   ::

      In> FromString("(+ a b)")LispReadListed()
      Out> {+,a,b};
      
   .. seealso:: :func:`FromFile`, :func:`FromString`, :func:`Read`, :func:`ReadToken`, :func:`FullForm`, :func:`LispRead`

.. function:: ReadToken()

   read a token from current input


   Read a token from the current input, and return it unevaluated.
   The returned object is a Yacas atom (not a string).  When  the end
   of an input file is encountered, the token atom {EndOfFile} is
   returned.    A token is for computer languages what a word is for
   human languages:  it is the smallest unit in which a command can be
   divided, so that the  semantics (that is the meaning) of the
   command is in some sense a  combination of the semantics of the
   tokens. Hence {a := foo} consists of three tokens, namely {a},
   {:=}, and {foo}.    The parsing of the string depends on the syntax
   of the language.  The part of the kernel that does the parsing is
   the "tokenizer".  Yacas can parse its own syntax (the default
   tokenizer) or it can be instructed to parse XML or C++ syntax using
   the directives {DefaultTokenizer} or {XmlTokenizer}.  Setting a
   tokenizer is a global action that affects all {ReadToken} calls.

   :Example:

   ::

      In> FromString("a := Sin(x)") While \
      ((tok := ReadToken()) != EndOfFile) \
      Echo(tok);
      a
      :=
      Sin
      (
      x
      )
      Out> True;
      We can read some junk too:
      In> FromString("-$3")ReadToken();
      Out> -$;
      The result is an atom with the string representation {-$}.
      Yacas assumes that {-$} is an operator symbol yet to be defined.
      The "{3}" will be in the next token.
      (The results will be different if a non-default tokenizer is selected.)
      
   .. seealso:: :func:`FromFile`, :func:`FromString`, :func:`Read`, :func:`LispRead`, :func:`DefaultTokenizer`

.. function:: Load(name)

   evaluate all expressions in a file

   :param name: string, name of the file to load

   The file "name" is opened. All expressions in the file are read and
   evaluated. {Load} always returns {true}.

   .. seealso:: :func:`Use`, :func:`DefLoad`, :func:`DefaultDirectory`, :func:`FindFile`

.. function:: Use(name)

   load a file, but not twice

   :param name: string, name of the file to load

   If the file "name" has been loaded before, either by an earlier
   call  to {Use} or via the {DefLoad}  mechanism, nothing happens.
   Otherwise all expressions in the file are  read and evaluated.
   {Use} always returns {true}.    The purpose of this function is to
   make sure that the file will at  least have been loaded, but is not
   loaded twice.

   .. seealso:: :func:`Load`, :func:`DefLoad`, :func:`DefaultDirectory`

.. function:: DefLoad(name)

   load a {.def} file

   :param name: string, name of the file (without {.def} suffix)

   The suffix {.def} is appended to "name" and the  file with this
   name is loaded. It should contain a list of functions,  terminated
   by a closing brace \} (the end-of-list delimiter). This  tells the
   system to load the file "name" as soon as the user calls  one of
   the functions named in the file (if not done so already). This
   allows for faster startup times, since not all of the rules
   databases  need to be loaded, just the descriptions on which files
   to load for  which functions.

   .. seealso:: :func:`Load`, :func:`Use`, :func:`DefaultDirectory`

.. function:: FindFile(name)

   find a file in the current path

   :param name: string, name of the file or directory to find

   The result of this command is the full path to the file that would
   be  opened when the command {Load(name)} would be  invoked. This
   means that the input directories are subsequently  searched for a
   file called "name". If such a file is not found, {FindFile} returns
   an empty string.    {FindFile("")} returns the name of the default
   directory (the first one on the search path).

   .. seealso:: :func:`Load`, :func:`DefaultDirectory`

.. function:: PatchLoad(name)

   execute commands between {<?} and {?>} in file

   :param name: string, name of the file to "patch"

   {PatchLoad} loads in a file and outputs the contents to the current
   output. The file can contain blocks delimited by {<?} and {?>}
   (meaning "Yacas Begin" and "Yacas End"). The piece of text between
   such delimiters is treated as a separate file with Yacas
   instructions,  which is then loaded and executed. All output of
   write statements  in that block will be written to the same current
   output.    This is similar to the way PHP works. You can have a
   static text file  with dynamic content generated by Yacas.

   .. seealso:: :func:`PatchString`, :func:`Load`

.. function:: Nl()

   the newline character


   This function returns a string with one element in it, namely a
   newline  character. This may be useful for building strings to send
   to some  output in the end.    Note that the second letter in the
   name of this command is a lower  case {L} (from "line").

   :Example:

   ::

      In> WriteString("First line" : Nl() : "Second line" : Nl());
      First line
      Second line
      Out> True;
      

   .. seealso:: :func:`NewLine`

.. function:: V(expression)

   set verbose output mode

   :param expression: expression to be evaluated in verbose mode

   The function {V(expression)} will evaluate the expression in
   verbose mode. Various parts of Yacas can show extra information
   about the work done while doing a calculation when using {V}.    In
   verbose mode, {InVerboseMode()} will return :data:`True`, otherwise  it
   will return :data:`False`.

   :Example:

   ::

      In> OldSolve({x+2==0},{x})
      Out> {{-2}};
      In> V(OldSolve({x+2==0},{x}))
      Entering OldSolve
      From  x+2==0  it follows that  x  = -2
      x+2==0  simplifies to  True
      Leaving OldSolve
      Out> {{-2}};
      In> InVerboseMode()
      Out> False
      In> V(InVerboseMode())
      Out> True
      

   .. seealso:: :func:`Echo`, :func:`N`, :func:`OldSolve`, :func:`InVerboseMode`

.. function:: InVerboseMode()

   check for verbose output mode

   In verbose mode, {InVerboseMode()} will return :data:`True`, otherwise it
   will return :data:`False`.

   :Example:

   ::

      In> InVerboseMode()
      Out> False
      In> V(InVerboseMode())
      Out> True
      

   .. seealso:: :func:`Echo`, :func:`N`, :func:`OldSolve`, :func:`V`


.. function:: Plot2D(f(x))

   adaptive two-dimensional plotting

   :param f(x): unevaluated expression containing one variables (function to be plotted)
   :param list: list of functions to plot
   :param a}, {b: numbers, plotting range in the $x$ coordinate
   :param option: atom, option name
   :param value: atom, number or string (value of option)

   The routine {Plot2D} performs adaptive plotting of one or several
   functions  of one variable in the specified range.  The result is
   presented as a line given by the equation $y=f(x)$.  Several
   functions can be plotted at once.  Various plotting options can be
   specified.  Output can be directed to a plotting program (the
   default is to use  {data}) to a list of values.    The function
   parameter {f(x)} must evaluate to a Yacas expression containing  at
   most one variable. (The variable does not have to be called {x}.)
   Also, {N(f(x))} must evaluate to a real (not complex) numerical
   value when given a numerical value of the argument {x}.  If the
   function {f(x)} does not satisfy these requirements, an error is
   raised.    Several functions may be specified as a list and they do
   not have to depend on the same variable, for example, {{f(x),
   g(y)}}.  The functions will be plotted on the same graph using the
   same coordinate ranges.    If you have defined a function which
   accepts a number but does not  accept an undefined variable,
   {Plot2D} will fail to plot it.  Use {NFunction} to overcome this
   difficulty.    Data files are created in a temporary directory
   {/tmp/plot.tmp/} unless otherwise requested.  File names  and other
   information is printed if {InVerboseMode()} returns :data:`True` on using
   {V()}.    The current algorithm uses Newton-Cotes quadratures and
   some heuristics for error estimation (see
   <*yacasdoc://Algo/3/1/*>).  The initial grid of {points+1} points
   is refined between any grid points $a$, $b$ if the integral
   $Integrate(x,a,b)f(x)$ is not approximated to the given precision
   by  the existing grid.    Default plotting range is {-5:5}. Range
   can also be specified as {x= -5:5} (note the mandatory space
   separating "{=}" and "{-}");  currently the variable name {x} is
   ignored in this case.    Options are of the form {option=value}.
   Currently supported option names  are: "points", "precision",
   "depth", "output", "filename", "yrange". Option values  are either
   numbers or special unevaluated atoms such as {data}.  If you need
   to use the names of these atoms  in your script, strings can be
   used. Several option/value pairs may be specified (the function
   {Plot2D} has a variable number of arguments).

   * {yrange}: the range of ordinates to use for plotting, e.g.
     {yrange=0:20}. If no range is specified, the default is usually
     to leave the choice to the plotting backend.
   * {points}: initial number of points (default 23) -- at least that
     many points will be plotted. The initial grid of this many points
     will be adaptively refined.
   * {precision}: graphing precision (default $10^(-6)$). This is
     interpreted as the relative precision of computing the integral
     of $f(x)-Min(f(x))$ using the grid points. For a smooth,
     non-oscillating function this value should be roughly 1/(number
     of screen pixels in the plot).
   * {depth}: max. refinement depth, logarithmic (default 5) -- means
     there will be at most $2^depth$ extra points per initial grid
     point.
   * {output}: name of the plotting backend. Supported names: {data}
     (default).  The {data} backend will return the data as a list of
     pairs such as {{{x1,y1}, {x2,y2}, ...}}.
   * {filename}: specify name of the created data file. For example:
     {filename="data1.txt"}.  The default is the name {"output.data"}.
     Note that if several functions are plotted, the data files will
     have a number appended to the given name, for example
     {data.txt1}, {data.txt2}.

   Other options may be supported in the future.

   The current implementation can deal with a singularity within the
   plotting range only if the function {f(x)} returns {Infinity},
   {-Infinity} or {Undefined} at the singularity.  If the function
   {f(x)} generates a numerical error and fails at a singularity,
   {Plot2D} will fail if one of the grid points falls on the
   singularity.  (All grid points are generated by bisection so in
   principle the endpoints and the {points} parameter could be chosen
   to avoid numerical singularities.)

.. seealso:: :func:`V`, :func:`NFunction`, :func:`Plot3DS`

.. function:: Plot3DS(f(x,y))

   three-dimensional (surface) plotting

   :param f(x,y): unevaluated expression containing two variables (function to be plotted)
   :param list: list of functions to plot
   :param a}, {b}, {c}, {d: numbers, plotting ranges in the $x$ and $y$ coordinates
   :param option: atom, option name
   :param value: atom, number or string (value of option)

   The routine {Plot3DS} performs adaptive plotting of a function  of
   two variables in the specified ranges.  The result is presented as
   a surface given by the equation $z=f(x,y)$.  Several functions can
   be plotted at once, by giving a list of functions.  Various
   plotting options can be specified.  Output can be directed to a
   plotting program (the default is to use  {data}), to a list of
   values.    The function parameter {f(x,y)} must evaluate to a Yacas
   expression containing  at most two variables. (The variables do not
   have to be called {x} and {y}.)  Also, {N(f(x,y))} must evaluate to
   a real (not complex) numerical value when given numerical values of
   the arguments {x}, {y}.  If the function {f(x,y)} does not satisfy
   these requirements, an error is raised.    Several functions may be
   specified as a list but they have to depend on the same symbolic
   variables, for example, {{f(x,y), g(y,x)}}, but not {{f(x,y),
   g(a,b)}}.  The functions will be plotted on the same graph using
   the same coordinate ranges.    If you have defined a function which
   accepts a number but does not  accept an undefined variable,
   {Plot3DS} will fail to plot it.  Use {NFunction} to overcome this
   difficulty.    Data files are created in a temporary directory
   {/tmp/plot.tmp/} unless otherwise requested.  File names  and other
   information is printed if {InVerboseMode()} returns :data:`True` on using
   {V()}.    The current algorithm uses Newton-Cotes cubatures and
   some heuristics for error estimation (see
   <*yacasdoc://Algo/3/1/*>).  The initial rectangular grid of
   {xpoints+1}*{ypoints+1} points is refined within any rectangle
   where the integral  of $f(x,y)$ is not approximated to the given
   precision by  the existing grid.    Default plotting range is
   {-5:5} in both coordinates.  A range can also be specified with a
   variable name, e.g. {x= -5:5} (note the mandatory space separating
   "{=}" and "{-}").  The variable name {x} should be the same as that
   used in the function {f(x,y)}.  If ranges are not given with
   variable names, the first variable encountered in the function
   {f(x,y)} is associated with the first of the two ranges.    Options
   are of the form {option=value}. Currently supported option names
   are "points", "xpoints", "ypoints", "precision", "depth", "output",
   "filename", "xrange", "yrange", "zrange". Option values  are either
   numbers or special unevaluated atoms such as {data}.  If you need
   to use the names of these atoms  in your script, strings can be
   used (e.g. {output="data"}). Several option/value pairs may be
   specified (the function {Plot3DS} has a variable number of
   arguments).

   * {xrange}, {yrange}: optionally override coordinate ranges. Note
     that {xrange} is always the first variable and {yrange} the
     second variable, regardless of the actual variable names.
   * {zrange}: the range of the $z$ axis to use for plotting, e.g.
     {zrange=0:20}. If no range is specified, the default is usually
     to leave the choice to the plotting backend. Automatic choice
     based on actual values may give visually inadequate plots if the
     function has a singularity.
   * {points}, {xpoints}, {ypoints}: initial number of points (default
     10 each) -- at least that many points will be plotted in each
     coordinate.  The initial grid of this many points will be
     adaptively refined.  If {points} is specified, it serves as a
     default for both {xpoints} and {ypoints}; this value may be
     overridden by {xpoints} and {ypoints} values.
   * {precision}: graphing precision (default $0.01$). This is
     interpreted as the relative precision of computing the integral
     of $f(x,y)-Min(f(x,y))$ using the grid points. For a smooth,
     non-oscillating function this value should be roughly 1/(number
     of screen pixels in the plot).
   * {depth}: max. refinement depth, logarithmic (default 3) -- means
     there will be at most $2^depth$ extra points per initial grid
     point (in each coordinate).
   * {output}: name of the plotting backend. Supported names: {data}
     (default). The {data} backend will return the data as a list of
     triples such as {{{x1, y1, z1}, {x2, y2, z2}, ...}}.

   Other options may be supported in the future.

   The current implementation can deal with a singularity within the
   plotting range only if the function {f(x,y)} returns {Infinity},
   {-Infinity} or {Undefined} at the singularity.  If the function
   {f(x,y)} generates a numerical error and fails at a singularity,
   {Plot3DS} will fail only if one of the grid points falls on the
   singularity.  (All grid points are generated by bisection so in
   principle the endpoints and the {xpoints}, {ypoints} parameters
   could be chosen to avoid numerical singularities.)

   The {filename} option is optional if using graphical backends, but
   can be used to specify the location of the created data file.

   :Example:

   ::

      In> Plot3DS(a*b^2)
      Out> True;
      In> V(Plot3DS(Sin(x)*Cos(y),x=0:20, y=0:20,depth=3))
      CachedConstant: Info: constant Pi is being 
      recalculated at precision 10
      CachedConstant: Info: constant Pi is being
      recalculated at precision 11
      Plot3DS: using 1699  points for function Sin(x)*Cos(y)
      Plot3DS: max. used 8 subdivisions for Sin(x)*Cos(y)
      Plot3DS'datafile: created file '/tmp/plot.tmp/data1'
      Out> True;

.. seealso:: :func:`V`, :func:`NFunction`, :func:`Plot2D`

.. function:: XmlExplodeTag(xmltext)

   convert XML strings to tag objects

   :param xmltext: string containing some XML tokens

   {XmlExplodeTag} parses the first XML token in {xmltext}  and
   returns a Yacas expression.    The following subset of XML syntax
   is supported currently:

   *   {<TAG [options]>} -- an opening tag
   *   {</TAG [options]>} -- a closing tag
   *   {<TAG [options] />} -- an open/close tag
   *   plain (non-tag) text

   The tag options take the form {paramname="value"}.

   If given an XML tag, {XmlExplodeTag} returns a structure of the
   form {XmlTag(name,params,type)}.  In the returned object, {name} is
   the (capitalized) tag name, {params} is an assoc list with the
   options (key fields capitalized), and type can be either "Open",
   "Close" or "OpenClose".

   If given a plain text string, the same string is returned.

   :Example:

   ::

      In> XmlExplodeTag("some plain text")
      Out> "some plain text";
      In> XmlExplodeTag("<a name=\"blah blah\"
      align=\"left\">")
      Out> XmlTag("A",{{"ALIGN","left"},
      {"NAME","blah blah"}},"Open");
      In> XmlExplodeTag("</p>")
      Out> XmlTag("P",{},"Close");
      In> XmlExplodeTag("<br/>")
      Out> XmlTag("BR",{},"OpenClose");

.. seealso:: :func:`XmlTokenizer`

.. function:: XmlTokenizer()

   select the default syntax tokenizer for parsing the input

   A "tokenizer" is an internal routine in the kernel that parses the
   input into Yacas expressions.  This affects all input typed in by a
   user at the prompt and also the input redirected from files or
   strings using {FromFile} and {FromString} and read using {Read} or
   {ReadToken}.    The Yacas environment currently supports some
   experimental tokenizers for   various syntaxes. {DefaultTokenizer}
   switches to the tokenizer used for  default Yacas syntax.
   {XmlTokenizer} switches to an XML syntax.  Note that setting the
   tokenizer is a global side effect.  One typically needs  to switch
   back to the default tokenizer when finished reading the special
   syntax.    Care needs to be taken when kernel errors are raised
   during a non-default tokenizer operation (as with any global change
   in the environment).  Errors need to be  caught with the
   {TrapError} function. The error handler code should re-instate  the
   default tokenizer,  or else the user will be unable to continue the
   session  (everything a user types will be parsed using a
   non-default tokenizer).    When reading XML syntax, the supported
   formats are the same as those of {XmlExplodeTag}.  The parser does
   not validate anything in the XML input.  After an XML token has
   been read in, it can be converted into an  Yacas expression with
   {XmlExplodeTag}.  Note that when reading XML, any plain text
   between tags is returned as one token.  Any malformed XML will be
   treated as plain text.

   :Example:

   ::

      In> [XmlTokenizer(); q:=ReadToken(); \
      DefaultTokenizer();q;]
      <a>
      Out> <a>;

   Note that:

   * after switching to {XmlTokenizer} the {In>} prompt disappeared;
     the user typed {<a>} and the {Out>} prompt with the resulting
     expression appeared.
   * The resulting expression is an atom with the string
     representation {<a>}; it is <i>not</i> a string.

.. seealso:: :func:`OMRead`, :func:`TrapError`, :func:`XmlExplodeTag`,
             :func:`ReadToken`, :func:`FromFile`, :func:`FromString`

.. function:: DefaultTokenizer()

   select the default syntax tokenizer for parsing the input

   A "tokenizer" is an internal routine in the kernel that parses the
   input into Yacas expressions.  This affects all input typed in by a
   user at the prompt and also the input redirected from files or
   strings using {FromFile} and {FromString} and read using {Read} or
   {ReadToken}.    The Yacas environment currently supports some
   experimental tokenizers for   various syntaxes. {DefaultTokenizer}
   switches to the tokenizer used for  default Yacas syntax.
   {XmlTokenizer} switches to an XML syntax.  Note that setting the
   tokenizer is a global side effect.  One typically needs  to switch
   back to the default tokenizer when finished reading the special
   syntax.    Care needs to be taken when kernel errors are raised
   during a non-default tokenizer operation (as with any global change
   in the environment).  Errors need to be  caught with the
   {TrapError} function. The error handler code should re-instate  the
   default tokenizer,  or else the user will be unable to continue the
   session  (everything a user types will be parsed using a
   non-default tokenizer).    When reading XML syntax, the supported
   formats are the same as those of {XmlExplodeTag}.  The parser does
   not validate anything in the XML input.  After an XML token has
   been read in, it can be converted into an  Yacas expression with
   {XmlExplodeTag}.  Note that when reading XML, any plain text
   between tags is returned as one token.  Any malformed XML will be
   treated as plain text.

.. seealso:: :func:`OMRead`, :func:`TrapError`, :func:`XmlExplodeTag`,
             :func:`ReadToken`, :func:`FromFile`, :func:`FromString`

.. function:: OMForm(expression)

   convert Yacas expression to OpenMath

   :param expression: expression to convert

   {OMForm} prints an OpenMath representation of the input parameter
   {expression} to standard output. If a Yacas symbol does not have a
   mapping defined by {OMDef}, it is translated to and from OpenMath
   as the OpenMath symbol in the CD "yacas" with the same name as it
   has in Yacas.

   :Example:

   ::

      In> str:=ToString()OMForm(2+Sin(a*3))
      Out> "<OMOBJ>
        <OMA>
          <OMS cd="arith1" name="plus"/>
          <OMI>2</OMI>
          <OMA>
            <OMS cd="transc1" name="sin"/>
            <OMA>
              <OMS cd="arith1" name="times"/>
              <OMV name="a"/>
              <OMI>3</OMI>
            </OMA>
          </OMA>
        </OMA>
      </OMOBJ>
      ";
      In> FromString(str)OMRead()
      Out> 2+Sin(a*3);

      In> OMForm(NotDefinedInOpenMath(2+3))
      <OMOBJ>
        <OMA>
          <OMS cd="yacas" name="NotDefinedInOpenMath"/>
          <OMA>
            <OMS cd="arith1" name="plus"/>
            <OMI>2</OMI>
            <OMI>3</OMI>
          </OMA>
        </OMA>
      </OMOBJ>
      Out> True

.. seealso:: :func:`XmlTokenizer`, :func:`XmlExplodeTag`, :func:`OMDef`

.. function:: OMRead()

   read OpenMath expression and convert to Yacas

   :param expression: expression to convert

   {OMRead} reads an OpenMath expression from standard input and
   returns a normal Yacas expression that matches the input OpenMath
   expression. If a Yacas symbol does not have a mapping defined by
   {OMDef}, it is translated to and from OpenMath as the OpenMath
   symbol in the CD "yacas" with the same name as it has in Yacas.

   :Example:

   ::

      In> str:=ToString()OMForm(2+Sin(a*3))
      Out> "<OMOBJ>
        <OMA>
          <OMS cd="arith1" name="plus"/>
          <OMI>2</OMI>
          <OMA>
            <OMS cd="transc1" name="sin"/>
            <OMA>
              <OMS cd="arith1" name="times"/>
              <OMV name="a"/>
              <OMI>3</OMI>
            </OMA>
          </OMA>
        </OMA>
      </OMOBJ>
      ";
      In> FromString(str)OMRead()
      Out> 2+Sin(a*3);

.. seealso:: :func:`XmlTokenizer`, :func:`XmlExplodeTag`, :func:`OMDef`

.. function:: OMDef(yacasForm, cd, name)

   define translations from Yacas to OpenMath and vice-versa.

   :param yacasForm: string with the name of a Yacas symbol, or a Yacas expression
   :param cd: OpenMath Content Dictionary for the symbol
   :param name: OpenMath name for the symbol
   :param yacasToOM: rule for translating an application of that symbol in Yacas into an OpenMath expression
   :param omToYacas: rule for translating an OpenMath expression into an application of this symbol in Yacas

   {OMDef} defines the translation rules for symbols between the Yacas
   representation and {OpenMath}.  The first parameter, {yacasForm},
   can be a string or an expression. The  difference is that when
   giving an expression only the {omToYacas} translation  is defined,
   and it uses the exact expression given. This is used for {OpenMath}
   symbols that must be translated into a whole subexpression in
   Yacas, such  as {set1:emptyset} which gets translated to an empty
   list as follows:      In> OMDef( {}, "set1","emptyset" )      Out>
   True      In> FromString("<OMOBJ><OMS cd=\"set1\"
   name=\"emptyset\"/></OMOBJ> ")OMRead()      Out> {}      In>
   IsList(%)      Out> True  Otherwise, a symbol that is not inside an
   application (OMA) gets translated to  the Yacas atom with the given
   name:      In> OMDef( "EmptySet", "set1","emptyset" )      Warning:
   the mapping for set1:emptyset was already defined as {} , but is
   redefined now as EmptySet      Out> True      In>
   FromString("<OMOBJ><OMS cd=\"set1\" name=\"emptyset\"/></OMOBJ>
   ")OMRead()      Out> EmptySet    The definitions for the symbols in
   the Yacas  library are in the ``*.rep`` script subdirectories. In
   those modules for which  the mappings are defined, there is a file
   called {om.ys} that contains the  {OMDef} calls. Those files are
   loaded in {openmath.rep/om.ys}, so any new  file must be added to
   the list there, at the end of the file.    A rule is represented as
   a list of expressions. Since both OM and  Yacas expressions are
   actually lists, the syntax is the same in both  directions. There
   are two template forms that are expanded before the  translation:

   * {$}: this symbol stands for the translation of the symbol applied
     in the original expression.

   * {_path}: a path into the original expression (list) to extract an
     element, written as an underscore applied to an integer or a list
     of integers.  Those integers are indexes into expressions, and
     integers in a list are applied recursively starting at the
     original expression.  For example, {_2} means the second
     parameter of the expression, while {_{3,2,1}} means the first
     parameter of the second parameter of the third parameter of the
     original expression.

   They can appear anywhere in the rule as expressions or subexpressions.

   Finally, several alternative rules can be specified by joining them
   with the {|} symbol, and each of them can be annotated with a
   post-predicate applied with the underscore {_} symbol, in the style
   of Yacas' simplification rules. Only the first alternative rule
   that matches is applied, so the more specific rules must be written
   first.

   There are special symbols recognized by {OMForm} to output
   {OpenMath} constructs that have no specific parallel in Yacas, such
   as an OpenMath symbol having a {CD} and {name}: Yacas symbols have
   only a name.  Those special symbols are:

   *   {OMS(cd, name)}: {<OMS cd="cd" name="name">}
   *   {OMA(f x y ...)}: {<OMA>f x y ...</OMA>}
   *   {OMBIND(binderSymbol, bvars, expression)}: {<OMBIND>binderSymbol bvars expression</OMBIND>}, where {bvars} must be produced by using {OMBVAR(...)}.
   *   {OMBVAR(x y ...)}: {<OMBVAR>x y ...</OMBVAR>}
   *   {OME(...)}: {<OME>...</OME>}

   When translating from OpenMath to Yacas, we just store unknown
   symbols as {OMS("cd", "name")}. This way we don't have to bother
   defining bogus symbols for concepts that Yacas does not handle, and
   we can evaluate expressions that contain them.

   :Example:

   ::
  
      In> OMDef( "Sqrt" ,  "arith1", "root", { $, _1, 2 }, $(_1)_(_2=2) | (_1^(1/_2)) );
      Out> True
      In> OMForm(Sqrt(3))
      <OMOBJ>
        <OMA>
          <OMS cd="arith1" name="root"/>
          <OMI>3</OMI>
          <OMI>2</OMI>
        </OMA>
      </OMOBJ>
      Out> True
      In> FromString("<OMOBJ><OMA><OMS cd=\"arith1\" name=\"root\"/><OMI>16</OMI><OMI>2</OMI></OMA></OMOBJ> ")OMRead()
      Out> Sqrt(16)
      In> FromString("<OMOBJ><OMA><OMS cd=\"arith1\" name=\"root\"/><OMI>16</OMI><OMI>3</OMI></OMA></OMOBJ> ")OMRead()
      Out> 16^(1/3)
  
      In> OMDef("Limit", "limit1", "limit", \
            {  $, _2, OMS("limit1", "under"), OMBIND(OMS("fns1", "lambda"), OMBVAR(_1), _4) }_(_3=Left)  \
            |{ $, _2, OMS("limit1", "above"), OMBIND(OMS("fns1", "lambda"), OMBVAR(_1), _4) }_(_3=Right) \
            |{ $, _2, OMS("limit1", "both_sides"), OMBIND(OMS("fns1", "lambda"), OMBVAR(_1), _3) },      \
            { $, _{3,2,1}, _1, Left,  _{3,3}}_(_2=OMS("limit1", "below")) \
            |{$, _{3,2,1}, _1, Right, _{3,3}}_(_2=OMS("limit1", "above")) \
            |{$, _{3,2,1}, _1, _{3,3}}                                    \
           );
      In> OMForm(Limit(x,0) Sin(x)/x)
      <OMOBJ>
        <OMA>
          <OMS cd="limit1" name="limit"/>
          <OMI>0</OMI>
          <OMS cd="limit1" name="both_sides"/>
          <OMBIND>
            <OMS cd="fns1" name="lambda"/>
            <OMBVAR>
              <OMV name="x"/>
            </OMBVAR>
            <OMA>
              <OMS cd="arith1" name="divide"/>
              <OMA>
                <OMS cd="transc1" name="sin"/>
                <OMV name="x"/>
              </OMA>
              <OMV name="x"/>
            </OMA>
          </OMBIND>
        </OMA>
      </OMOBJ>
      Out> True
      In> OMForm(Limit(x,0,Right) 1/x)
      <OMOBJ>
        <OMA>
          <OMS cd="limit1" name="limit"/>
          <OMI>0</OMI>
          <OMS cd="limit1" name="above"/>
          <OMBIND>
            <OMS cd="fns1" name="lambda"/>
            <OMBVAR>
              <OMV name="x"/>
            </OMBVAR>
            <OMA>
              <OMS cd="arith1" name="divide"/>
              <OMI>1</OMI>
              <OMV name="x"/>
            </OMA>
          </OMBIND>
        </OMA>
      </OMOBJ>
      Out> True
      In> FromString(ToString()OMForm(Limit(x,0,Right) 1/x))OMRead()
      Out> Limit(x,0,Right)1/x
      In> %
      Out> Infinity
  
   .. seealso:: :func:`OMRead`, :func:`OMForm`
  
