===================
String manipulation
===================


.. function:: StringMid'Set(index,substring,string)

   change a substring

   :param index: index of substring to get
   :param substring: substring to store
   :param string: string to store substring in

   Set (change) a part of a string. It leaves the original alone,
   returning a new changed copy.

   :Example:

   ::

      In> StringMid'Set(3,"XY","abcdef")
      Out> "abXYef";

   .. seealso:: :func:`StringMid'Get`, :func:`Length`


.. function:: StringMid'Get(index,length,string)

   retrieve a substring

   :param index: index of substring to get
   :param length: length of substring to get
   :param string: string to get substring from

   {StringMid'Get} returns a part of a string. Substrings can also be
   accessed using the {[]} operator.

   :Example:

   ::

      In> StringMid'Get(3,2,"abcdef")
      Out> "cd";
      In> "abcdefg"[2 .. 4]
      Out> "bcd";

   .. seealso:: :func:`StringMid'Set`, :func:`Length`


.. function:: Atom("string")

   convert string to atom

   :param "string": a string

   Returns an atom with the string representation given as the
   evaluated argument. Example: {Atom("foo");} returns {foo}.

   :Example:

   ::

      In> Atom("a")
      Out> a;

   .. seealso:: :func:`String`


.. function:: String(atom)

   convert atom to string

   :param atom: an atom


   {String} is the inverse of {Atom}: turns {atom} into {"atom"}.

   :Example:

   ::

      In> String(a)
      Out> "a";

   .. seealso:: :func:`Atom`


.. function:: ConcatStrings(strings)

   concatenate strings

   :param strings: one or more strings

   Concatenates strings.

   :Example:

   ::

      In> ConcatStrings("a","b","c")
      Out> "abc";

   .. seealso:: :func:`Concat`


.. function:: PatchString(string)

   execute commands between {<?} and {?>} in strings

   :param string: a string to patch

   This function does the same as PatchLoad, but it works on a string
   instead of on the contents of a text file. See PatchLoad for more
   details.

   :Example:

   ::

      In> PatchString("Two plus three is <? Write(2+3); ?> ");
      Out> "Two plus three is 5 ";

   .. seealso:: :func:`PatchLoad`

