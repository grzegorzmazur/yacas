==============
Linear Algebra
==============

This chapter describes the commands for doing linear algebra. They can
be used to manipulate vectors, represented as lists, and matrices,
represented as lists of lists.

.. function:: Dot(t1,t2)

   get dot product of tensors

   :param t1,t2: tensor lists (currently only vectors and matrices are supported)

   {Dot} returns the dot (aka inner) product of two tensors t1 and t2.
   The last  index of t1 and the first index of t2 are contracted.
   Currently {Dot} works  only for vectors and matrices.
   {Dot}-multiplication of two vectors, a matrix  with a vector (and
   vice versa) or two matrices yields either a scalar, a  vector or a
   matrix.

   :Example:

   ::

      In> Dot({1,2},{3,4})
      Out> 11;
      In> Dot({{1,2},{3,4}},{5,6})
      Out> {17,39};
      In> Dot({5,6},{{1,2},{3,4}})
      Out> {23,34};
      In> Dot({{1,2},{3,4}},{{5,6},{7,8}})
      Out> {{19,22},{43,50}};
      Or, using the "."-Operator:
      In> {1,2} . {3,4}
      Out> 11;
      In> {{1,2},{3,4}} . {5,6}
      Out> {17,39};
      In> {5,6} . {{1,2},{3,4}}
      Out> {23,34};
      In> {{1,2},{3,4}} . {{5,6},{7,8}}
      Out> {{19,22},{43,50}};
      

   .. seealso:: :func:`Outer`, :func:`Cross`, :func:`IsScalar`, :func:`IsVector`, :func:`IsMatrix`

.. function:: InProduct(a,b)

   inner product of vectors (deprecated)

   :param a}, {b: vectors of equal length

   The inner product of the two vectors "a" and "b" is returned. The
   vectors need to have the same size.    This function is superceded
   by the {.} operator.

   :Example:

   ::

      In> {a,b,c} . {d,e,f};
      Out> a*d+b*e+c*f;
      

   .. seealso:: :func:`Dot`, :func:`CrossProduct`

.. function:: CrossProduct(a,b)

   outer product of vectors

   :param a}, {b: three-dimensional vectors

   The cross product of the vectors "a"  and "b" is returned. The
   result is perpendicular to both "a" and  "b" and its length is the
   product of the lengths of the vectors.  Both "a" and "b" have to be
   three-dimensional.

   :Example:

   ::

      In> {a,b,c} X {d,e,f};
      Out> {b*f-c*e,c*d-a*f,a*e-b*d};
      

   .. seealso:: :func:`InProduct`

.. function:: Outer(t1,t2)

   get outer tensor product

   :param t1,t2: tensor lists (currently only vectors are supported)

   {Outer} returns the outer product of two tensors t1 and t2.
   Currently  {Outer} work works only for vectors, i.e. tensors of
   rank 1. The outer  product of two vectors yields a matrix.

   :Example:

   ::

      In> Outer({1,2},{3,4,5})
      Out> {{3,4,5},{6,8,10}};
      In> Outer({a,b},{c,d})
      Out> {{a*c,a*d},{b*c,b*d}};
      Or, using the "o"-Operator:
      In> {1,2} o {3,4,5}
      Out> {{3,4,5},{6,8,10}};
      In> {a,b} o {c,d}
      Out> {{a*c,a*d},{b*c,b*d}};
      

   .. seealso:: :func:`Dot`, :func:`Cross`

.. function:: ZeroVector(n)

   create a vector with all zeroes

   :param n: length of the vector to return

   This command returns a vector of length "n", filled with zeroes.

   :Example:

   ::

      In> ZeroVector(4)
      Out> {0,0,0,0};
      

   .. seealso:: :func:`BaseVector`, :func:`ZeroMatrix`, :func:`IsZeroVector`

.. function:: BaseVector(k, n)

   base vector

   :param k: index of the base vector to construct
   :param n: dimension of the vector

   This command returns the "k"-th base vector of dimension "n". This
   is a vector of length "n" with all zeroes except for the "k"-th
   entry, which contains a 1.

   :Example:

   ::

      In> BaseVector(2,4)
      Out> {0,1,0,0};
      

   .. seealso:: :func:`ZeroVector`, :func:`Identity`

.. function:: Identity(n)

   make identity matrix

   :param n: size of the matrix

   This commands returns the identity matrix of size "n" by "n". This
   matrix has ones on the diagonal while the other entries are zero.

   :Example:

   ::

      In> Identity(3)
      Out> {{1,0,0},{0,1,0},{0,0,1}};
      

   .. seealso:: :func:`BaseVector`, :func:`ZeroMatrix`, :func:`DiagonalMatrix`

.. function:: ZeroMatrix(n)

   make a zero matrix

   :param n: number of rows
   :param m: number of columns

   This command returns a matrix with {n} rows and {m} columns,
   completely filled with zeroes. If only given one parameter,  it
   returns the square {n} by {n} zero matrix.

   :Example:

   ::

      In> ZeroMatrix(3,4)
      Out> {{0,0,0,0},{0,0,0,0},{0,0,0,0}};
      In> ZeroMatrix(3)
      Out> {{0,0,0},{0,0,0},{0,0,0}};
      

   .. seealso:: :func:`ZeroVector`, :func:`Identity`

.. function:: Diagonal(A)

   extract the diagonal from a matrix

   :param A: matrix

   This command returns a vector of the diagonal components  of the
   matrix {A}.

   :Example:

   ::

      In> Diagonal(5*Identity(4))
      Out> {5,5,5,5};
      In> Diagonal(HilbertMatrix(3))
      Out> {1,1/3,1/5};
      

   .. seealso:: :func:`DiagonalMatrix`, :func:`IsDiagonal`

.. function:: DiagonalMatrix(d)

   construct a diagonal matrix

   :param d: list of values to put on the diagonal

   This command constructs a diagonal matrix, that is a square matrix
   whose off-diagonal entries are all zero. The elements of the vector
   "d" are put on the diagonal.

   :Example:

   ::

      In> DiagonalMatrix(1 .. 4)
      Out> {{1,0,0,0},{0,2,0,0},{0,0,3,0},{0,0,0,4}};
      

   .. seealso:: :func:`Identity`, :func:`ZeroMatrix`

.. function:: OrthogonalBasis(W)

   create an orthogonal basis

   :param W: A linearly independent set of row vectors (aka a matrix)

   Given a linearly independent set {W} (constructed of rows vectors),
   this command returns an orthogonal basis {V} for {W}, which means
   that span(V) = span(W) and {InProduct(V[i],V[j]) = 0} when {i !=
   j}.  This function uses the Gram-Schmidt orthogonalization process.

   :Example:

   ::

      In> OrthogonalBasis({{1,1,0},{2,0,1},{2,2,1}})
      Out> {{1,1,0},{1,-1,1},{-1/3,1/3,2/3}};
      

   .. seealso:: :func:`OrthonormalBasis`, :func:`InProduct`

.. function:: OrthonormalBasis(W)

   create an orthonormal basis

   :param W: A linearly independent set of row vectors (aka a matrix)

   Given a linearly independent set {W} (constructed of rows vectors),
   this command returns an orthonormal basis {V} for {W}. This is done
   by first using {OrthogonalBasis(W)}, then dividing each vector by
   its  magnitude, so as the give them unit length.

   :Example:

   ::

      In> OrthonormalBasis({{1,1,0},{2,0,1},{2,2,1}})
      Out> {{Sqrt(1/2),Sqrt(1/2),0},{Sqrt(1/3),-Sqrt(1/3),Sqrt(1/3)},
      {-Sqrt(1/6),Sqrt(1/6),Sqrt(2/3)}};
      

   .. seealso:: :func:`OrthogonalBasis`, :func:`InProduct`, :func:`Normalize`

.. function:: Normalize(v)

   normalize a vector

   :param v: a vector

   Return the normalized (unit) vector parallel to {v}: a vector
   having the same  direction but with length 1.

   :Example:

   ::

      In> v:=Normalize({3,4})
      Out> {3/5,4/5};
      In> v . v
      Out> 1;
      

   .. seealso:: :func:`InProduct`, :func:`CrossProduct`

.. function:: Transpose(M)

   get transpose of a matrix

   :param M: a matrix

   {Transpose} returns the transpose of a matrix $M$. Because matrices
   are  just lists of lists, this is a useful operation too for lists.

   :Example:

   ::

      In> Transpose({{a,b}})
      Out> {{a},{b}};
      

.. function:: Determinant(M)

   determinant of a matrix

   :param M: a matrix

   Returns the determinant of a matrix M.

   :Example:

   ::

      In> A:=DiagonalMatrix(1 .. 4)
      Out> {{1,0,0,0},{0,2,0,0},{0,0,3,0},{0,0,0,4}};
      In> Determinant(A)
      Out> 24;
      

.. function:: Trace(M)

   trace of a matrix

   :param M: a matrix

   {Trace} returns the trace of a matrix $M$ (defined as the sum of
   the  elements on the diagonal of the matrix).

   :Example:

   ::

      In> A:=DiagonalMatrix(1 .. 4)
      Out> {{1,0,0,0},{0,2,0,0},{0,0,3,0},{0,0,0,4}};
      In> Trace(A)
      Out> 10;
      

.. function:: Inverse(M)

   get inverse of a matrix

   :param M: a matrix

   Inverse returns the inverse of matrix $M$. The determinant of $M$
   should  be non-zero. Because this function uses {Determinant} for
   calculating  the inverse of a matrix, you can supply matrices with
   non-numeric (symbolic)  matrix elements.

   :Example:

   ::

      In> A:=DiagonalMatrix({a,b,c})
      Out> {{a,0,0},{0,b,0},{0,0,c}};
      In> B:=Inverse(A)
      Out> {{(b*c)/(a*b*c),0,0},{0,(a*c)/(a*b*c),0},
      {0,0,(a*b)/(a*b*c)}};
      In> Simplify(B)
      Out> {{1/a,0,0},{0,1/b,0},{0,0,1/c}};
      

   .. seealso:: :func:`Determinant`

.. function:: Minor(M,i,j)

   get principal minor of a matrix

   :param M: a matrix
   :param i}, {j: positive integers

   Minor returns the minor of a matrix around  the element ($i$, $j$).
   The minor is the determinant of the matrix obtained from $M$ by
   deleting the $i$-th row and the $j$-th column.

   :Example:

   ::

      In> A := {{1,2,3}, {4,5,6}, {7,8,9}};
      Out> {{1,2,3},{4,5,6},{7,8,9}};
      In> PrettyForm(A);
      /                    \
      | ( 1 ) ( 2 ) ( 3 )  |
      |                    |
      | ( 4 ) ( 5 ) ( 6 )  |
      |                    |
      | ( 7 ) ( 8 ) ( 9 )  |
      \                    /
      Out> True;
      In> Minor(A,1,2);
      Out> -6;
      In> Determinant({{2,3}, {8,9}});
      Out> -6;
      

   .. seealso:: :func:`CoFactor`, :func:`Determinant`, :func:`Inverse`

.. function:: CoFactor(M,i,j)

   cofactor of a matrix

   :param M: a matrix
   :param i}, {j: positive integers

   {CoFactor} returns the cofactor of a matrix around  the element
   ($i$, $j$). The cofactor is the minor times  $(-1)^(i+j)$.

   :Example:

   ::

      In> A := {{1,2,3}, {4,5,6}, {7,8,9}};
      Out> {{1,2,3},{4,5,6},{7,8,9}};
      In> PrettyForm(A);
      /                    \
      | ( 1 ) ( 2 ) ( 3 )  |
      |                    |
      | ( 4 ) ( 5 ) ( 6 )  |
      |                    |
      | ( 7 ) ( 8 ) ( 9 )  |
      \                    /
      Out> True;
      In> CoFactor(A,1,2);
      Out> 6;
      In> Minor(A,1,2);
      Out> -6;
      In> Minor(A,1,2) * (-1)^(1+2);
      Out> 6;
      

   .. seealso:: :func:`Minor`, :func:`Determinant`, :func:`Inverse`

.. function:: MatrixPower(mat,n)

   get nth power of a square matrix

   :param mat: a square matrix
   :param n: an integer

   {MatrixPower(mat,n)} returns the {n}th power of a square matrix
   {mat}. For  positive {n} it evaluates dot products of {mat} with
   itself. For negative  {n} the nth power of the inverse of {mat} is
   returned. For {n}=0 the identity  matrix is returned.

.. function:: SolveMatrix(M,v)

   solve a linear system

   :param M: a matrix
   :param v: a vector

   {SolveMatrix} returns the vector $x$ that satisfies  the equation
   $M*x = v$. The determinant of $M$ should be non-zero.

   :Example:

   ::

      In> A := {{1,2}, {3,4}};
      Out> {{1,2},{3,4}};
      In> v := {5,6};
      Out> {5,6};
      In> x := SolveMatrix(A, v);
      Out> {-4,9/2};
      In> A * x;
      Out> {5,6};
      

   .. seealso:: :func:`Inverse`, :func:`Solve`, :func:`PSolve`, :func:`Determinant`

.. function:: CharacteristicEquation(matrix,var)

   get characteristic polynomial of a matrix

   :param matrix: a matrix
   :param var: a free variable

   CharacteristicEquation  returns the characteristic equation of
   "matrix", using  "var". The zeros of this equation are the
   eigenvalues  of the matrix, Det(matrix-I*var);

   :Example:

   ::

      In> A:=DiagonalMatrix({a,b,c})
      Out> {{a,0,0},{0,b,0},{0,0,c}};
      In> B:=CharacteristicEquation(A,x)
      Out> (a-x)*(b-x)*(c-x);
      In> Expand(B,x)
      Out> (b+a+c)*x^2-x^3-((b+a)*c+a*b)*x+a*b*c;
      

   .. seealso:: :func:`EigenValues`, :func:`EigenVectors`

.. function:: EigenValues(matrix)

   get eigenvalues of a matrix

   :param matrix: a square matrix

   EigenValues returns the eigenvalues of a matrix.  The eigenvalues x
   of a matrix M are the numbers such that  $M*v=x*v$ for some vector.
   It first determines the characteristic equation, and then
   factorizes this  equation, returning the roots of the
   characteristic equation  Det(matrix-x*identity).

   :Example:

   ::

      In> M:={{1,2},{2,1}}
      Out> {{1,2},{2,1}};
      In> EigenValues(M)
      Out> {3,-1};
      

   .. seealso:: :func:`EigenVectors`, :func:`CharacteristicEquation`

.. function:: EigenVectors(A,eigenvalues)

   get eigenvectors of a matrix

   :param matrix: a square matrix
   :param eigenvalues: list of eigenvalues as returned by {EigenValues}

   {EigenVectors} returns a list of the eigenvectors of a matrix.  It
   uses the eigenvalues and the matrix to set up n equations with  n
   unknowns for each eigenvalue, and then calls {Solve} to determine
   the values of each vector.

   :Example:

   ::

      In> M:={{1,2},{2,1}}
      Out> {{1,2},{2,1}};
      In> e:=EigenValues(M)
      Out> {3,-1};
      In> EigenVectors(M,e)
      Out> {{-ki2/ -1,ki2},{-ki2,ki2}};
      

   .. seealso:: :func:`EigenValues`, :func:`CharacteristicEquation`

.. function:: Sparsity(matrix)

   get the sparsity of a matrix

   :param matrix: a matrix

   The function {Sparsity} returns a number between {0} and {1} which
   represents the percentage of zero entries in the matrix. Although
   there is no definite critical value, a sparsity of {0.75}  or more
   is almost universally considered a "sparse" matrix. These type of
   matrices can be handled in a different manner than "full" matrices
   which speedup many calculations by orders of magnitude.

   :Example:

   ::

      In> Sparsity(Identity(2))
      Out> 0.5;
      In> Sparsity(Identity(10))
      Out> 0.9;
      In> Sparsity(HankelMatrix(10))
      Out> 0.45;
      In> Sparsity(HankelMatrix(100))
      Out> 0.495;
      In> Sparsity(HilbertMatrix(10))
      Out> 0;
      In> Sparsity(ZeroMatrix(10,10))
      Out> 1;
      

.. function:: Cholesky(A)

   find the Cholesky Decomposition

   :param A: a square positive definite matrix

   {Cholesky} returns a upper triangular matrix {R} such that
   {Transpose(R)*R = A}.  The matrix {A} must be positive definite,
   {Cholesky} will notify the user if the matrix  is not. Some
   families of positive definite matrices are all symmetric matrices,
   diagonal  matrices with positive elements and Hilbert matrices.

   :Example:

   ::

      In> A:={{4,-2,4,2},{-2,10,-2,-7},{4,-2,8,4},{2,-7,4,7}}
      Out> {{4,-2,4,2},{-2,10,-2,-7},{4,-2,8,4},{2,-7,4,7}};
      In> R:=Cholesky(A);
      Out> {{2,-1,2,1},{0,3,0,-2},{0,0,2,1},{0,0,0,1}};
      In> Transpose(R)*R = A
      Out> True;
      In> Cholesky(4*Identity(5))
      Out> {{2,0,0,0,0},{0,2,0,0,0},{0,0,2,0,0},{0,0,0,2,0},{0,0,0,0,2}};
      In> Cholesky(HilbertMatrix(3))
      Out> {{1,1/2,1/3},{0,Sqrt(1/12),Sqrt(1/12)},{0,0,Sqrt(1/180)}};
      In> Cholesky(ToeplitzMatrix({1,2,3}))
      In function "Check" :
      CommandLine(1) : "Cholesky: Matrix is not positive definite"
      

   .. seealso:: :func:`IsSymmetric`, :func:`IsDiagonal`, :func:`Diagonal`

.. function:: IsScalar(expr)

   test for a scalar

   :param expr: a mathematical object

   {IsScalar} returns :data:`True` if {expr} is a scalar, :data:`False` otherwise.
   Something is considered to be a scalar if it's not a list.

   :Example:

   ::

      In> IsScalar(7)
      Out> True;
      In> IsScalar(Sin(x)+x)
      Out> True;
      In> IsScalar({x,y})
      Out> False;
      

   .. seealso:: :func:`IsList`, :func:`IsVector`, :func:`IsMatrix`

.. function:: IsVector([pred,]expr)

   test for a vector

   :param expr: expression to test
   :param pred: predicate test (e.g. IsNumber, IsInteger, ...)

   {IsVector(expr)} returns :data:`True` if {expr} is a vector, :data:`False`
   otherwise.  Something is considered to be a vector if it's a list
   of scalars.  {IsVector(pred,expr)} returns :data:`True` if {expr} is a
   vector and if the  predicate test {pred} returns :data:`True` when
   applied to every element of  the vector {expr}, :data:`False` otherwise.

   :Example:

   ::

      In> IsVector({a,b,c})
      Out> True;
      In> IsVector({a,{b},c})
      Out> False;
      In> IsVector(IsInteger,{1,2,3})
      Out> True;
      In> IsVector(IsInteger,{1,2.5,3})
      Out> False;
      

   .. seealso:: :func:`IsList`, :func:`IsScalar`, :func:`IsMatrix`

.. function:: IsMatrix([pred,]expr)

   test for a matrix

   :param expr: expression to test
   :param pred: predicate test (e.g. IsNumber, IsInteger, ...)

   {IsMatrix(expr)} returns :data:`True` if {expr} is a matrix, :data:`False`
   otherwise.  Something is considered to be a matrix if it's a list
   of vectors of equal  length.  {IsMatrix(pred,expr)} returns :data:`True`
   if {expr} is a matrix and if the  predicate test {pred} returns
   :data:`True` when applied to every element of  the matrix {expr}, :data:`False`
   otherwise.

   :Example:

   ::

      In> IsMatrix(1)
      Out> False;
      In> IsMatrix({1,2})
      Out> False;
      In> IsMatrix({{1,2},{3,4}})
      Out> True;
      In> IsMatrix(IsRational,{{1,2},{3,4}})
      Out> False;
      In> IsMatrix(IsRational,{{1/2,2/3},{3/4,4/5}})
      Out> True;
      

   .. seealso:: :func:`IsList`, :func:`IsVector`

.. function:: IsSquareMatrix([pred,]expr)

   test for a square matrix

   :param expr: expression to test
   :param pred: predicate test (e.g. IsNumber, IsInteger, ...)

   {IsSquareMatrix(expr)} returns :data:`True` if {expr} is a square matrix,
   :data:`False` otherwise. Something is considered to be a square matrix if
   it's a matrix having the same number of rows and columns.
   {IsMatrix(pred,expr)} returns :data:`True` if {expr} is a square matrix
   and  if the predicate test {pred} returns :data:`True` when applied to
   every  element of the matrix {expr}, :data:`False` otherwise.

   :Example:

   ::

      In> IsSquareMatrix({{1,2},{3,4}});
      Out> True;
      In> IsSquareMatrix({{1,2,3},{4,5,6}});
      Out> False;
      In> IsSquareMatrix(IsBoolean,{{1,2},{3,4}});
      Out> False;
      In> IsSquareMatrix(IsBoolean,{{True,False},{False,True}});
      Out> True;
      

   .. seealso:: :func:`IsMatrix`

.. function:: IsHermitian(A)

   test for a Hermitian matrix

   :param A: a square matrix

   IsHermitian(A) returns :data:`True` if {A} is Hermitian and :data:`False`
   otherwise. $A$ is a Hermitian matrix iff Conjugate( Transpose $A$
   )=$A$.  If $A$ is a real matrix, it must be symmetric to be
   Hermitian.

   :Example:

   ::

      In> IsHermitian({{0,I},{-I,0}})
      Out> True;
      In> IsHermitian({{0,I},{2,0}})
      Out> False;
      

   .. seealso:: :func:`IsUnitary`

.. function:: IsOrthogonal(A)

   test for an orthogonal matrix

   :param A: square matrix

   {IsOrthogonal(A)} returns :data:`True` if {A} is orthogonal and :data:`False`
   otherwise. $A$ is orthogonal iff $A$*Transpose($A$) = Identity, or
   equivalently Inverse($A$) = Transpose($A$).

   :Example:

   ::

      In> A := {{1,2,2},{2,1,-2},{-2,2,-1}};
      Out> {{1,2,2},{2,1,-2},{-2,2,-1}};
      In> PrettyForm(A/3)
      /                      \
      | / 1 \  / 2 \ / 2 \   |
      | | - |  | - | | - |   |
      | \ 3 /  \ 3 / \ 3 /   |
      |                      |
      | / 2 \  / 1 \ / -2 \  |
      | | - |  | - | | -- |  |
      | \ 3 /  \ 3 / \ 3  /  |
      |                      |
      | / -2 \ / 2 \ / -1 \  |
      | | -- | | - | | -- |  |
      | \ 3  / \ 3 / \ 3  /  |
      \                      /
      Out> True;
      In> IsOrthogonal(A/3)
      Out> True;
      

.. function:: IsDiagonal(A)

   test for a diagonal matrix

   :param A: a matrix

   {IsDiagonal(A)} returns :data:`True` if {A} is a diagonal square matrix
   and :data:`False` otherwise.

   :Example:

   ::

      In> IsDiagonal(Identity(5))
      Out> True;
      In> IsDiagonal(HilbertMatrix(5))
      Out> False;
      

.. function:: IsLowerTriangular(A)

   test for a lower triangular matrix

   :param A: a matrix

   A lower/upper triangular matrix is a square matrix which has all
   zero entries above/below the diagonal.    {IsLowerTriangular(A)}
   returns :data:`True` if {A} is a lower triangular matrix and :data:`False`
   otherwise.  {IsUpperTriangular(A)} returns :data:`True` if {A} is an
   upper triangular matrix and :data:`False` otherwise.

   :Example:

   ::

      In> IsUpperTriangular(Identity(5))
      Out> True;
      In> IsLowerTriangular(Identity(5))
      Out> True;
      In> IsLowerTriangular({{1,2},{0,1}})
      Out> False;
      In> IsUpperTriangular({{1,2},{0,1}})
      Out> True;
      A non-square matrix cannot be triangular:
      In> IsUpperTriangular({{1,2,3},{0,1,2}})
      Out> False;
      

   .. seealso:: :func:`IsDiagonal`

.. function:: IsSymmetric(A)

   test for a symmetric matrix

   :param A: a matrix

   {IsSymmetric(A)} returns :data:`True` if {A} is symmetric and :data:`False`
   otherwise.  $A$ is symmetric iff Transpose ($A$) =$A$.

   :Example:

   ::

      In> A := {{1,0,0,0,1},{0,2,0,0,0},{0,0,3,0,0},
      {0,0,0,4,0},{1,0,0,0,5}};
      In> PrettyForm(A)
      /                                \
      | ( 1 ) ( 0 ) ( 0 ) ( 0 ) ( 1 )  |
      |                                |
      | ( 0 ) ( 2 ) ( 0 ) ( 0 ) ( 0 )  |
      |                                |
      | ( 0 ) ( 0 ) ( 3 ) ( 0 ) ( 0 )  |
      |                                |
      | ( 0 ) ( 0 ) ( 0 ) ( 4 ) ( 0 )  |
      |                                |
      | ( 1 ) ( 0 ) ( 0 ) ( 0 ) ( 5 )  |
      \                                /
      Out> True;
      In> IsSymmetric(A)
      Out> True;
      

   .. seealso:: :func:`IsHermitian`, :func:`IsSkewSymmetric`

.. function:: IsSkewSymmetric(A)

   test for a skew-symmetric matrix

   :param A: a square matrix

   {IsSkewSymmetric(A)} returns :data:`True` if {A} is skew symmetric and
   :data:`False` otherwise.  $A$ is skew symmetric iff $Transpose(A)$ =$-A$.

   :Example:

   ::

      In> A := {{0,-1},{1,0}}
      Out> {{0,-1},{1,0}};
      In> PrettyForm(%)
      /               \
      | ( 0 ) ( -1 )  |
      |               |
      | ( 1 ) ( 0 )   |
      \               /
      Out> True;
      In> IsSkewSymmetric(A);
      Out> True;
      

   .. seealso:: :func:`IsSymmetric`, :func:`IsHermitian`

.. function:: IsUnitary(A)

   test for a unitary matrix

   :param A: a square matrix

   This function tries to find out if A is unitary.    A matrix $A$ is
   orthogonal iff $A^(-1)$ = Transpose( Conjugate($A$) ). This is
   equivalent to the fact that the columns of $A$ build an orthonormal
   system  (with respect to the scalar product defined by
   {InProduct}).

   :Example:

   ::

      In> IsUnitary({{0,I},{-I,0}})
      Out> True;
      In> IsUnitary({{0,I},{2,0}})
      Out> False;
      

   .. seealso:: :func:`IsHermitian`, :func:`IsSymmetric`

.. function:: IsIdempotent(A)

   test for an idempotent matrix

   :param A: a square matrix

   {IsIdempotent(A)} returns :data:`True` if {A} is idempotent and :data:`False`
   otherwise.  $A$ is idempotent iff $A^2=A$. Note that this also
   implies that $A$ raised  to any power is also equal to $A$.

   :Example:

   ::

      In> IsIdempotent(ZeroMatrix(10,10));
      Out> True;
      In> IsIdempotent(Identity(20))
      Out> True;
      Special matrices
      

.. function:: JacobianMatrix(functions,variables)

   calculate the Jacobian matrix of $n$ functions in $n$ variables

   :param functions: an $n$-dimensional vector of functions
   :param variables: an $n$-dimensional vector of variables

   The function {JacobianMatrix} calculates the Jacobian matrix  of n
   functions in n variables.    The ($i$,$j$)-th element of the
   Jacobian matrix is defined as the derivative  of $i$-th function
   with respect to the $j$-th variable.

   :Example:

   ::

      In> JacobianMatrix( {Sin(x),Cos(y)}, {x,y} );
      Out> {{Cos(x),0},{0,-Sin(y)}};
      In> PrettyForm(%)
      /                                 \
      | ( Cos( x ) ) ( 0 )              |
      |                                 |
      | ( 0 )        ( -( Sin( y ) ) )  |
      \                                 /
      

.. function:: VandermondeMatrix(vector)

   create the Vandermonde matrix

   :param vector: an $n$-dimensional vector

   The function {VandermondeMatrix} calculates the Vandermonde matrix
   of a vector.    The ($i$,$j$)-th element of the Vandermonde matrix
   is defined as $i^(j-1)$.

   :Example:

   ::

      In> VandermondeMatrix({1,2,3,4})
      Out> {{1,1,1,1},{1,2,3,4},{1,4,9,16},{1,8,27,64}};
      In>PrettyForm(%)
      /                            \
      | ( 1 ) ( 1 ) ( 1 )  ( 1 )   |
      |                            |
      | ( 1 ) ( 2 ) ( 3 )  ( 4 )   |
      |                            |
      | ( 1 ) ( 4 ) ( 9 )  ( 16 )  |
      |                            |
      | ( 1 ) ( 8 ) ( 27 ) ( 64 )  |
      \                            /
      

.. function:: HessianMatrix(function,var)

   create the Hessian matrix

   :param function: a function in $n$ variables
   :param var: an $n$-dimensional vector of variables

   The function {HessianMatrix} calculates the Hessian matrix  of a
   vector.    If $f(x)$ is a function of an $n$-dimensional vector
   $x$, then the ($i$,$j$)-th element of the Hessian matrix of the
   function $f(x)$ is defined as   $ Deriv(x[i]) Deriv(x[j]) f(x) $.
   If the third  order mixed partials are continuous, then the Hessian
   matrix is symmetric (a standard theorem of calculus).    The
   Hessian matrix is used in the second derivative test   to discern
   if a critical point is a local maximum, a local  minimum or a
   saddle point.

   :Example:

   ::

      In> HessianMatrix(3*x^2-2*x*y+y^2-8*y, {x,y} )
      Out> {{6,-2},{-2,2}};
      In> PrettyForm(%)
      /                \
      | ( 6 )  ( -2 )  |
      |                |
      | ( -2 ) ( 2 )   |
      \                /
      

.. function:: HilbertMatrix(n)

   create a Hilbert matrix

   :param n,m: positive integers

   The function {HilbertMatrix} returns the {n} by {m} Hilbert matrix
   if given two arguments, and the square {n} by {n} Hilbert matrix
   if given only one. The Hilbert matrix is defined as {A(i,j) =
   1/(i+j-1)}.  The Hilbert matrix is extremely sensitive to
   manipulate and invert numerically.

   :Example:

   ::

      In> PrettyForm(HilbertMatrix(4))
      /                          \
      | ( 1 ) / 1 \ / 1 \ / 1 \  |
      |       | - | | - | | - |  |
      |       \ 2 / \ 3 / \ 4 /  |
      |                          |
      | / 1 \ / 1 \ / 1 \ / 1 \  |
      | | - | | - | | - | | - |  |
      | \ 2 / \ 3 / \ 4 / \ 5 /  |
      |                          |
      | / 1 \ / 1 \ / 1 \ / 1 \  |
      | | - | | - | | - | | - |  |
      | \ 3 / \ 4 / \ 5 / \ 6 /  |
      |                          |
      | / 1 \ / 1 \ / 1 \ / 1 \  |
      | | - | | - | | - | | - |  |
      | \ 4 / \ 5 / \ 6 / \ 7 /  |
      \                          /
      

   .. seealso:: :func:`HilbertInverseMatrix`

.. function:: HilbertInverseMatrix(n)

   create a Hilbert inverse matrix

   :param n: positive integer

   The function {HilbertInverseMatrix} returns the {n} by {n} inverse
   of the  corresponding Hilbert matrix. All Hilbert inverse matrices
   have integer  entries that grow in magnitude rapidly.

   :Example:

   ::

      In> PrettyForm(HilbertInverseMatrix(4))
      /                                         \
      | ( 16 )   ( -120 )  ( 240 )   ( -140 )   |
      |                                         |
      | ( -120 ) ( 1200 )  ( -2700 ) ( 1680 )   |
      |                                         |
      | ( 240 )  ( -2700 ) ( 6480 )  ( -4200 )  |
      |                                         |
      | ( -140 ) ( 1680 )  ( -4200 ) ( 2800 )   |
      \                                         /
      

   .. seealso:: :func:`HilbertMatrix`

.. function:: ToeplitzMatrix(N)

   create a Toeplitz matrix

   :param N: an $n$-dimensional row vector

   The function {ToeplitzMatrix} calculates the Toeplitz matrix given
   an $n$-dimensional row vector. This matrix has the same entries in
   all diagonal columns, from upper left to lower right.

   :Example:

   ::

      In> PrettyForm(ToeplitzMatrix({1,2,3,4,5}))
      /                                \
      | ( 1 ) ( 2 ) ( 3 ) ( 4 ) ( 5 )  |
      |                                |
      | ( 2 ) ( 1 ) ( 2 ) ( 3 ) ( 4 )  |
      |                                |
      | ( 3 ) ( 2 ) ( 1 ) ( 2 ) ( 3 )  |
      |                                |
      | ( 4 ) ( 3 ) ( 2 ) ( 1 ) ( 2 )  |
      |                                |
      | ( 5 ) ( 4 ) ( 3 ) ( 2 ) ( 1 )  |
      \                                /
      

.. function:: WronskianMatrix(func,var)

   create the Wronskian matrix

   :param func: an $n$-dimensional vector of functions
   :param var: a variable to differentiate with respect to

   The function {WronskianMatrix} calculates the Wronskian matrix  of
   $n$ functions.    The Wronskian matrix is created by putting each
   function as the  first element of each column, and filling in the
   rest of each  column by the ($i-1$)-th derivative, where $i$ is the
   current row.    The Wronskian matrix is used to verify that the $n$
   functions are linearly  independent, usually solutions to a
   differential equation.  If the determinant of the Wronskian matrix
   is zero, then the functions  are dependent, otherwise they are
   independent.

   :Example:

   ::

      In> WronskianMatrix({Sin(x),Cos(x),x^4},x);
      Out> {{Sin(x),Cos(x),x^4},{Cos(x),-Sin(x),4*x^3},
      {-Sin(x),-Cos(x),12*x^2}};
      In> PrettyForm(%)
      /                                                 \
      | ( Sin( x ) )      ( Cos( x ) )      /  4 \      |
      |                                     \ x  /      |
      |                                                 |
      | ( Cos( x ) )      ( -( Sin( x ) ) ) /      3 \  |
      |                                     \ 4 * x  /  |
      |                                                 |
      | ( -( Sin( x ) ) ) ( -( Cos( x ) ) ) /       2 \ |
      |                                     \ 12 * x  / |
      \                                                 /
      The last element is a linear combination of the first two, so the determinant is zero:
      In> A:=Determinant( WronskianMatrix( {x^4,x^3,2*x^4
      + 3*x^3},x ) )
      Out> x^4*3*x^2*(24*x^2+18*x)-x^4*(8*x^3+9*x^2)*6*x
      +(2*x^4+3*x^3)*4*x^3*6*x-4*x^6*(24*x^2+18*x)+x^3
      *(8*x^3+9*x^2)*12*x^2-(2*x^4+3*x^3)*3*x^2*12*x^2;
      In> Simplify(A)
      Out> 0;
      

.. function:: SylvesterMatrix(poly1,poly2,variable)

   calculate the Sylvester matrix of two polynomials

   :param poly1: polynomial
   :param poly2: polynomial
   :param variable: variable to express the matrix for

   The function {SylvesterMatrix} calculates the Sylvester matrix  for
   a pair of polynomials.    The Sylvester matrix is closely related
   to the resultant, which  is defined as the determinant of the
   Sylvester matrix. Two polynomials  share common roots only if the
   resultant is zero.

   :Example:

   ::

      In> ex1:= x^2+2*x-a
      Out> x^2+2*x-a;
      In> ex2:= x^2+a*x-4
      Out> x^2+a*x-4;
      In> A:=SylvesterMatrix(ex1,ex2,x)
      Out> {{1,2,-a,0},{0,1,2,-a},
      {1,a,-4,0},{0,1,a,-4}};
      In> B:=Determinant(A)
      Out> 16-a^2*a- -8*a-4*a+a^2- -2*a^2-16-4*a;
      In> Simplify(B)
      Out> 3*a^2-a^3;
      The above example shows that the two polynomials have common
      zeros if $ a = 3 $.
      

   .. seealso:: :func:`Determinant`, :func:`Simplify`, :func:`Solve`, :func:`PSolve`

