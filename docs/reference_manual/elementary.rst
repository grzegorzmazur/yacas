====================
Elementary functions
====================

.. function:: Sin(x)

   trigonometric sine function

   :Example:

   ::

      In> Sin(1)
      Out> Sin(1);
      In> N(Sin(1),20)
      Out> 0.84147098480789650665;
      In> Sin(Pi/4)
      Out> Sqrt(2)/2;


   .. seealso:: :func:`Cos`, :func:`Tan`, :func:`ArcSin`, :func:`ArcCos`, :func:`ArcTan`, :const:`Pi`

.. function:: Cos(x)

   trigonometric cosine function

   :Example:

   ::

      In> Cos(1)
      Out> Cos(1);
      In> N(Cos(1),20)
      Out> 0.5403023058681397174;
      In> Cos(Pi/4)
      Out> Sqrt(1/2);


   .. seealso:: :func:`Sin`, :func:`Tan`, :func:`ArcSin`, :func:`ArcCos`, :func:`ArcTan`, :const:`Pi`

.. function:: Tan(x)

   trigonometric tangent function

   :Example:

   ::

      In> Tan(1)
      Out> Tan(1);
      In> N(Tan(1),20)
      Out> 1.5574077246549022305;
      In> Tan(Pi/4)
      Out> 1;


   .. seealso:: :func:`Sin`, :func:`Cos`, :func:`ArcSin`, :func:`ArcCos`, :func:`ArcTan`, :const:`Pi`

.. function:: ArcSin(x)

   inverse trigonometric function arc-sine

   :Example:

   ::

      In> ArcSin(1)
      Out> Pi/2;
      In> ArcSin(1/3)
      Out> ArcSin(1/3);
      In> Sin(ArcSin(1/3))
      Out> 1/3;
      In> x:=N(ArcSin(0.75))
      Out> 0.848062;
      In> N(Sin(x))
      Out> 0.7499999477;


   .. seealso:: :func:`Sin`, :func:`Cos`, :func:`Tan`, :const:`Pi`, :func:`Ln`, :func:`ArcCos`, :func:`ArcTan`

.. function:: ArcCos(x)

   inverse trigonometric function arc-cosine

   :Example:

   ::

      In> ArcCos(0)
      Out> Pi/2
      In> ArcCos(1/3)
      Out> ArcCos(1/3)
      In> Cos(ArcCos(1/3))
      Out> 1/3
      In> x:=N(ArcCos(0.75))
      Out> 0.7227342478
      In> N(Cos(x))
      Out> 0.75


   .. seealso:: :func:`Sin`, :func:`Cos`, :func:`Tan`, :const:`Pi`, :func:`Ln`, :func:`ArcSin`, :func:`ArcTan`

.. function:: ArcTan(x)

   inverse trigonometric function arc-tangent

   :Example:

   ::

      In> ArcTan(1)
      Out> Pi/4
      In> ArcTan(1/3)
      Out> ArcTan(1/3)
      In> Tan(ArcTan(1/3))
      Out> 1/3
      In> x:=N(ArcTan(0.75))
      Out> 0.643501108793285592213351264945231378078460693359375
      In> N(Tan(x))
      Out> 0.75


   .. seealso:: :func:`Sin`, :func:`Cos`, :func:`Tan`, :const:`Pi`, :func:`Ln`, :func:`ArcSin`, :func:`ArcCos`

.. function:: Exp(x)

   exponential function

   :Example:

   ::

      In> Exp(0)
      Out> 1;
      In> Exp(I*Pi)
      Out> -1;
      In> N(Exp(1))
      Out> 2.7182818284;


   .. seealso:: :func:`Ln`, :func:`Sin`, :func:`Cos`, :func:`Tan`

.. function:: Ln(x)

   natural logarithm

   :Example:

   ::

      In> Ln(1)
      Out> 0;
      In> Ln(Exp(x))
      Out> x;
      In> D(x) Ln(x)
      Out> 1/x;


   .. seealso:: :func:`Exp`, :func:`Arg`

.. function:: Sqrt(x)

   square root

   :Example:

   ::

      In> Sqrt(16)
      Out> 4;
      In> Sqrt(15)
      Out> Sqrt(15);
      In> N(Sqrt(15))
      Out> 3.8729833462;
      In> Sqrt(4/9)
      Out> 2/3;
      In> Sqrt(-1)
      Out> Complex(0,1);


   .. seealso:: :func:`Exp`, :func:`^`

.. function:: Abs(x)

   absolute value or modulus of complex number

   :Example:

   ::

      In> Abs(2);
      Out> 2;
      In> Abs(-1/2);
      Out> 1/2;
      In> Abs(3+4*I);
      Out> 5;


   .. seealso:: :func:`Sign`, :func:`Arg`

.. function:: Sign(x)

   sign of a number

   :Example:

   ::

      In> Sign(2)
      Out> 1;
      In> Sign(-3)
      Out> -1;
      In> Sign(0)
      Out> 1;
      In> Sign(-3) * Abs(-3)
      Out> -3;


   .. seealso:: :func:`Arg`, :func:`Abs`
