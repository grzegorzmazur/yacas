(provide 'yacas-functions)

(defun yacas-fn-isinfinity ()
  (interactive)
  (insert "IsInfinity()")
  (forward-char -1)
  (message "Args: n"))

(defun yacas-fn-isnonzerointeger ()
  (interactive)
  (insert "IsNonZeroInteger()")
  (forward-char -1)
  (message "Args: n"))

(defun yacas-fn-isnotzero ()
  (interactive)
  (insert "IsNotZero()")
  (forward-char -1)
  (message "Args: n"))

(defun yacas-fn-ispositiveinteger ()
  (interactive)
  (insert "IsPositiveInteger()")
  (forward-char -1)
  (message "Args: n"))

(defun yacas-fn-ispositivenumber ()
  (interactive)
  (insert "IsPositiveNumber()")
  (forward-char -1)
  (message "Args: n"))

(defun yacas-fn-isnegativeinteger ()
  (interactive)
  (insert "IsNegativeInteger()")
  (forward-char -1)
  (message "Args: n"))

(defun yacas-fn-isnegativenumber ()
  (interactive)
  (insert "IsNegativeNumber()")
  (forward-char -1)
  (message "Args: n"))

(defun yacas-fn-isinteger ()
  (interactive)
  (insert "IsInteger()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-isnumber ()
  (interactive)
  (insert "IsNumber()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-isodd ()
  (interactive)
  (insert "IsOdd()")
  (forward-char -1)
  (message "Args: integer"))

(defun yacas-fn-iseven ()
  (interactive)
  (insert "IsEven()")
  (forward-char -1)
  (message "Args: integer"))

(defun yacas-fn-fibonacci ()
  (interactive)
  (insert "Fibonacci()")
  (forward-char -1)
  (message "Args: n"))

(defun yacas-fn-pi ()
  (interactive)
  (insert "Pi()"))

(defun yacas-fn-conjugate ()
  (interactive)
  (insert "Conjugate()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-denom ()
  (interactive)
  (insert "Denom()")
  (forward-char -1)
  (message "Args: r"))

(defun yacas-fn-numer ()
  (interactive)
  (insert "Numer()")
  (forward-char -1)
  (message "Args: r"))

(defun yacas-fn-isrational ()
  (interactive)
  (insert "IsRational()")
  (forward-char -1)
  (message "Args: r"))

(defun yacas-fn-iszero ()
  (interactive)
  (insert "IsZero()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-max ()
  (interactive)
  (insert "Max()")
  (forward-char -1)
  (message "Args: x, y or a list of numbers"))

(defun yacas-fn-min ()
  (interactive)
  (insert "Min()")
  (forward-char -1)
  (message "Args: x,y or a list of numbers"))

(defun yacas-fn-factorize ()
  (interactive)
  (insert "Factorize()")
  (forward-char -1)
  (let ((var (read-string "Variable: ")))
    (if (string= var "")
	(message "Args: list")
      (insert var ", ")
      (setq var (read-string "From: "))
      (insert var ", ")
      (setq var (read-string "To: "))
      (insert var ", "))))

(defun yacas-fn-average ()
  (interactive)
  (insert "Average()")
  (forward-char -1)
  (message "Args: list of numbers"))

(defun yacas-fn-bin ()
  (interactive)
  (insert "Bin()")
  (forward-char -1)
  (message "Args: n, m"))

(defun yacas-fn-im ()
  (interactive)
  (insert "Im()")
  (forward-char -1)
  (message "Args: z"))

(defun yacas-fn-re ()
  (interactive)
  (insert "Re()")
  (forward-char -1)
  (message "Args: z"))

(defun yacas-fn-complex ()
  (interactive)
  (insert "Complex()")
  (forward-char -1)
  (message "Args: x, y"))

(defun yacas-fn-round ()
  (interactive)
  (insert "Round()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-ceil ()
  (interactive)
  (insert "Ceil()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-floor ()
  (interactive)
  (insert "Floor()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-decimal ()
  (interactive)
  (insert "Decimal()")
  (forward-char -1)
  (message "Args: fraction"))

(defun yacas-fn-contfrac ()
  (interactive)
  (insert "ContFrac()")
  (forward-char -1)
  (message "Args: x ;Opt args: maximum depth"))

(defun yacas-fn-padicexpand ()
  (interactive)
  (insert "PAdicExpand()")
  (forward-char -1)
  (message "Args: number, p"))

(defun yacas-fn-factors ()
  (interactive)
  (insert "Factors()")
  (forward-char -1)
  (message "Args: n (integer or polynomial)"))

(defun yacas-fn-factor ()
  (interactive)
  (insert "Factor()")
  (forward-char -1)
  (message "Args: n (integer or polynomial)"))

(defun yacas-fn-isprimepower ()
  (interactive)
  (insert "IsPrimePower()")
  (forward-char -1)
  (message "Args: n"))

(defun yacas-fn-isprime ()
  (interactive)
  (insert "IsPrime()")
  (forward-char -1)
  (message "Args: n"))

(defun yacas-fn-tobase ()
  (interactive)
  (insert "ToBase()")
  (forward-char -1)
  (message "Args: base, number"))

(defun yacas-fn-frombase ()
  (interactive)
  (insert "FromBase()")
  (forward-char -1)
  (message "Args: base, number"))

(defun yacas-fn-lcm ()
  (interactive)
  (insert "Lcm()")
  (forward-char -1)
  (message "Args: m, n (integers or polynomials)"))

(defun yacas-fn-gcd ()
  (interactive)
  (insert "Gcd()")
  (forward-char -1)
  (message "Args: m, n (or a list of numbers or polynomials)"))

(defun yacas-fn-mod ()
  (interactive)
  (insert "Mod()")
  (forward-char -1)
  (message "Args: m, n (integers or polynomials)"))

(defun yacas-fn-div ()
  (interactive)
  (insert "Div()")
  (forward-char -1)
  (message "Args: m, n (integers or polynomials)"))


(defun yacas-fn-limit ()
  (interactive)
  (insert "Limit()")
  (forward-char -1)
  (let ((var (read-string "Variable: ")))
    (insert var ", ")
    (setq var (read-string "Value: "))
    (insert var)
    (forward-char 1)))

(defun yacas-fn-antideriv ()
  (interactive)
  (insert "Integrate()")
  (forward-char -1)
  (message "Args: expression, variable"))

(defun yacas-fn-integrate ()
  (interactive)
  (insert "Integrate()")
  (forward-char -1)
  (let ((var (read-string "Variable: ")))
    (insert var ", ")
    (setq var (read-string "From: "))
    (insert var ", ")
    (setq var (read-string "To: "))
    (insert var)
    (forward-char 1)))

(defun yacas-fn-curl ()
  (interactive)
  (insert "Curl()")
  (forward-char -1)
  (message "Args: vector, basis"))

(defun yacas-fn-diverge ()
  (interactive)
  (insert "Diverge()")
  (forward-char -1)
  (message "Args: vector, basis"))

(defun yacas-fn-d ()
  (interactive)
  (insert "D()")
  (forward-char -1)
  (let ((var (read-string "Variable: ")))
    (insert var)
    (forward-char 1)))

(defun yacas-fn-newton ()
  (interactive)
  (insert "Newton()")
  (forward-char -1)
  (let ((var (read-string "Function: ")))
    (insert var ", ")
    (setq var (read-string "Variable: "))
    (insert var ", ")
    (setq var (read-string "Initial value: "))
    (insert var ", ")
    (setq var (read-string "Accuracy: "))
    (insert var)
    (forward-char 1)))

(defun yacas-fn-reversepoly ()
  (interactive)
  (insert "ReversePoly()")
  (forward-char -1)
  (let ((var (read-string "Polynomial 1: ")))
    (insert var ", ")
    (setq var (read-string "Polynomial 2: "))
    (insert var ", ")
    (setq var (read-string "New Variable: "))
    (insert var)
    (setq var (read-string "Degree: "))
    (insert var)
    (forward-char 1)))

(defun yacas-fn-inversetaylor ()
  (interactive)
  (insert "InverseTaylor()")
  (forward-char -1)
  (let ((var (read-string "Variable: ")))
    (insert var ", ")
    (setq var (read-string "Centered at: "))
    (insert var ", ")
    (setq var (read-string "Degree: "))
    (insert var)
    (forward-char 1)))

(defun yacas-fn-taylor ()
  (interactive)
  (insert "Taylor()")
  (forward-char -1)
  (let ((var (read-string "Variable: ")))
    (insert var ", ")
    (setq var (read-string "Centered at: "))
    (insert var ", ")
    (setq var (read-string "Order: "))
    (insert var)
    (forward-char 1)))

(defun yacas-fn-secure ()
  (interactive)
  (insert "Secure()")
  (forward-char -1)
  (message "Args: body"))

(defun yacas-fn-prog ()
  (interactive)
  (insert "Prog()")
  (forward-char -1)
  (message "Args: expressions ..."))

(defun yacas-fn-tracerule ()
  (interactive)
  (insert "TraceRule()")
  (forward-char -1)
  (message "Args: template"))

(defun yacas-fn-traceexp ()
  (interactive)
  (insert "TraceExp()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-tracestack ()
  (interactive)
  (insert "TraceStack()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-localsymbols ()
  (interactive)
  (insert "LocalSymbols()")
  (forward-char -1)
  (message "Args: symbols"))

(defun yacas-fn-apply ()
  (interactive)
  (insert "Apply()")
  (forward-char -1)
  (message "Args: operator, list"))

(defun yacas-fn-eval ()
  (interactive)
  (insert "Eval()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-hold ()
  (interactive)
  (insert "Hold()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-maxevaldepth ()
  (interactive)
  (insert "MaxEvalDepth()")
  (forward-char -1)
  (message "Args: n"))


(defun yacas-fn-fastarcsin ()
  (interactive)
  (insert "FastArcSin()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-fastarccos ()
  (interactive)
  (insert "FastArcCos()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-fastarctan ()
  (interactive)
  (insert "FastArcTan()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-fasttan ()
  (interactive)
  (insert "FastTan()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-fastcos ()
  (interactive)
  (insert "FastCos()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-fastsin ()
  (interactive)
  (insert "FastSin()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-fastpower ()
  (interactive)
  (insert "FastPower()")
  (forward-char -1)
  (message "Args: x, y"))

(defun yacas-fn-fastlog ()
  (interactive)
  (insert "FastLog()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-fastexp ()
  (interactive)
  (insert "FastExp()")
  (forward-char -1)
  (message "Args: x"))


(defun yacas-fn-patchload ()
  (interactive)
  (insert "PatchLoad()")
  (forward-char -1)
  (message "Args: filename"))

(defun yacas-fn-findfile ()
  (interactive)
  (insert "FindFile()")
  (forward-char -1)
  (message "Args: name"))

(defun yacas-fn-defload ()
  (interactive)
  (insert "DefLoad()")
  (forward-char -1)
  (message "Args: filename"))

(defun yacas-fn-load ()
  (interactive)
  (insert "Load()")
  (forward-char -1)
  (message "Args: filename"))

(defun yacas-fn-tofile ()
  (interactive)
  (insert "ToFile()")
  (forward-char -1)
  (message "Args: file"))

(defun yacas-fn-readtoken ()
  (interactive)
  (insert "ReadToken()"))

(defun yacas-fn-lispread ()
  (interactive)
  (insert "LispRead()"))

(defun yacas-fn-read ()
  (interactive)
  (insert "Read()"))

(defun yacas-fn-tostring ()
  (interactive)
  (insert "ToString()"))

(defun yacas-fn-fromstring ()
  (interactive)
  (insert "FromString()")
  (forward-char -1)
  (message "Args: string"))

(defun yacas-fn-fromfile ()
  (interactive)
  (insert "FromFile()")
  (forward-char -1)
  (message "Args: file"))

(defun yacas-fn-newline ()
  (interactive)
  (insert "NewLine()")
  (forward-char -1)
  (message "Args: number (optional)"))

(defun yacas-fn-space ()
  (interactive)
  (insert "Space()")
  (forward-char -1)
  (message "Args: number (optional)"))

(defun yacas-fn-writestring ()
  (interactive)
  (insert "WriteString()")
  (forward-char -1)
  (message "Args: string"))

(defun yacas-fn-write ()
  (interactive)
  (insert "Write()")
  (forward-char -1)
  (message "Args: expressions"))

(defun yacas-fn-prettyform ()
  (interactive)
  (insert "PrettyForm()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-echo ()
  (interactive)
  (insert "Echo()")
  (forward-char -1)
  (message "Args: list"))

(defun yacas-fn-fullform ()
  (interactive)
  (insert "FullForm()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-use ()
  (interactive)
  (insert "Use()")
  (forward-char -1)
  (message "Args: file"))


(defun yacas-fn-iszerovector ()
  (interactive)
  (insert "IsZeroVector()")
  (forward-char -1)
  (message "Args: vector"))

(defun yacas-fn-isunitary ()
  (interactive)
  (insert "IsUnitary()")
  (forward-char -1)
  (message "Args: square matrix"))

(defun yacas-fn-ishermitian ()
  (interactive)
  (insert "IsHermitian()")
  (forward-char -1)
  (message "Args: square matrix"))

(defun yacas-fn-eigenvectors ()
  (interactive)
  (insert "EigenVectors()")
  (forward-char -1)
  (message "Args: matrix, eigenvalues"))

(defun yacas-fn-eigenvalues ()
  (interactive)
  (insert "EigenValues()")
  (forward-char -1)
  (message "Args: matrix"))

(defun yacas-fn-characteristicequation ()
  (interactive)
  (insert "CharacteristicEquation()")
  (forward-char -1)
  (message "Args: matrix, variable"))

(defun yacas-fn-solvematrix ()
  (interactive)
  (insert "SolveMatrix()")
  (forward-char -1)
  (message "Args: matrix, vector"))

(defun yacas-fn-minor ()
  (interactive)
  (insert "Minor()")
  (forward-char -1)
  (message "Args: matrix, i, j"))

(defun yacas-fn-cofactor ()
  (interactive)
  (insert "CoFactor()")
  (forward-char -1)
  (message "Args: matrix, i, j"))

(defun yacas-fn-inverse ()
  (interactive)
  (insert "Inverse()")
  (forward-char -1)
  (message "Args: matrix"))

(defun yacas-fn-trace ()
  (interactive)
  (insert "Trace()")
  (forward-char -1)
  (message "Args: matrix"))

(defun yacas-fn-diagonalmatrix ()
  (interactive)
  (insert "DiagonalMatrix()")
  (forward-char -1)
  (message "Args: vector"))

(defun yacas-fn-determinant ()
  (interactive)
  (insert "Determinant()")
  (forward-char -1)
  (message "Args: matrix"))

(defun yacas-fn-tranpose ()
  (interactive)
  (insert "Tranpose()")
  (forward-char -1)
  (message "Args: matrix"))

(defun yacas-fn-zeromatrix ()
  (interactive)
  (insert "ZeroMatrix()")
  (forward-char -1)
  (message "Args: n, m"))

(defun yacas-fn-normalize ()
  (interactive)
  (insert "Normalize()")
  (forward-char -1)
  (message "Args: vector"))

(defun yacas-fn-ismatrix ()
  (interactive)
  (insert "IsMatrix()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-identity ()
  (interactive)
  (insert "Identity()")
  (forward-char -1)
  (message "Args: n"))

(defun yacas-fn-basevector ()
  (interactive)
  (insert "BaseVector()")
  (forward-char -1)
  (message "Args: row, n"))

(defun yacas-fn-zerovector ()
  (interactive)
  (insert "ZeroVector()")
  (forward-char -1)
  (message "Args: n"))

(defun yacas-fn-crossproduct ()
  (interactive)
  (insert "CrossProduct()")
  (forward-char -1)
  (message "Args: vector1, vector2"))

(defun yacas-fn-inproduct ()
  (interactive)
  (insert "InProduct()")
  (forward-char -1)
  (message "Args: vector1, vector2"))

(defun yacas-fn-commutator ()
  (interactive)
  (insert "Commutator()")
  (forward-char -1)
  (message "Args: a, b"))


(defun yacas-fn-islist ()
  (interactive)
  (insert "IsList()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-select ()
  (interactive)
  (insert "Select()")
  (forward-char -1)
  (message "Args: predicate, list"))

(defun yacas-fn-tableform ()
  (interactive)
  (insert "TableForm()")
  (forward-char -1)
  (message "Args: list"))

(defun yacas-fn-table ()
  (interactive)
  (insert "Table()")
  (forward-char -1)
  (message "Args: body, variable, from, to, step"))

(defun yacas-fn-bubblesort ()
  (interactive)
  (insert "BubbleSort()")
  (forward-char -1)
  (message "Args: list, \"compare\""))

(defun yacas-fn-unflatten ()
  (interactive)
  (insert "Unflatten()")
  (forward-char -1)
  (message "Args: list, operator, identity"))

(defun yacas-fn-flatten ()
  (interactive)
  (insert "Flatten()")
  (forward-char -1)
  (message "Args: expression, operator"))

(defun yacas-fn-associndices ()
  (interactive)
  (insert "AssocIndices()")
  (forward-char -1)
  (message "Args: list"))

(defun yacas-fn-assoc ()
  (interactive)
  (insert "Assoc()")
  (forward-char -1)
  (message "Args: key, list"))

(defun yacas-fn-partition ()
  (interactive)
  (insert "Partition()")
  (forward-char -1)
  (message "Args: list, n"))

(defun yacas-fn-take ()
  (interactive)
  (insert "Take()")
  (forward-char -1)
  (message "Args: list, integer of pair of integers"))

(defun yacas-fn-drop ()
  (interactive)
  (insert "Drop()")
  (forward-char -1)
  (message "Args: list, integer or pair of integers"))

(defun yacas-fn-filllist ()
  (interactive)
  (insert "FillList()")
  (forward-char -1)
  (message "Args: item, length"))

(defun yacas-fn-difference ()
  (interactive)
  (insert "Difference()")
  (forward-char -1)
  (message "Args: list1, list2"))

(defun yacas-fn-union ()
  (interactive)
  (insert "Union()")
  (forward-char -1)
  (message "Args: list1, list2"))

(defun yacas-fn-intersection ()
  (interactive)
  (insert "Intersection()")
  (forward-char -1)
  (message "Args: list1, list2"))

(defun yacas-fn-count ()
  (interactive)
  (insert "Count()")
  (forward-char -1)
  (message "Args: list, element"))

(defun yacas-fn-swap ()
  (interactive)
  (insert "Swap()")
  (forward-char -1)
  (message "Args: list, index1, index2"))

(defun yacas-fn-popback ()
  (interactive)
  (insert "PopBack()")
  (forward-char -1)
  (message "Args: stack"))

(defun yacas-fn-popfront ()
  (interactive)
  (insert "PopFront()")
  (forward-char -1)
  (message "Args: stack"))

(defun yacas-fn-pop ()
  (interactive)
  (insert "Pop()")
  (forward-char -1)
  (message "Args: stack, index"))

(defun yacas-fn-push ()
  (interactive)
  (insert "Push()")
  (forward-char -1)
  (message "Args: stack, element"))

(defun yacas-fn-removeduplicates ()
  (interactive)
  (insert "RemoveDuplicates()")
  (forward-char -1)
  (message "Args: list"))

(defun yacas-fn-destructiveappend ()
  (interactive)
  (insert "DestructiveAppend()")
  (forward-char -1)
  (message "Args: list, element"))

(defun yacas-fn-append ()
  (interactive)
  (insert "Append()")
  (forward-char -1)
  (message "Args: list, element"))

(defun yacas-fn-find ()
  (interactive)
  (insert "Find()")
  (forward-char -1)
  (message "Args: list, item"))

(defun yacas-fn-contains ()
  (interactive)
  (insert "Contains()")
  (forward-char -1)
  (message "Args: list, element"))

(defun yacas-fn-flatcopy ()
  (interactive)
  (insert "FlatCopy()")
  (forward-char -1)
  (message "Args: list"))

(defun yacas-fn-destructivereplace ()
  (interactive)
  (insert "DestructiveReplace()")
  (forward-char -1)
  (message "Args: list, index, element"))

(defun yacas-fn-replace ()
  (interactive)
  (insert "Replace()")
  (forward-char -1)
  (message "Args: list, index, element"))

(defun yacas-fn-destructivedelete ()
  (interactive)
  (insert "DestructiveDelete()")
  (forward-char -1)
  (message "Args: list, index"))

(defun yacas-fn-destructiveinsert ()
  (interactive)
  (insert "DestructiveInsert()")
  (forward-char -1)
  (message "Args: list, index, element"))

(defun yacas-fn-insert ()
  (interactive)
  (insert "Insert()")
  (forward-char -1)
  (message "Args: list, index, element"))

(defun yacas-fn-delete ()
  (interactive)
  (insert "Delete()")
  (forward-char -1)
  (message "Args: list, index"))

(defun yacas-fn-concat ()
  (interactive)
  (insert "Concat()")
  (forward-char -1)
  (message "Args: list1, list2, ..."))

(defun yacas-fn-listify ()
  (interactive)
  (insert "Listify()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-unlist ()
  (interactive)
  (insert "UnList()")
  (forward-char -1)
  (message "Args: list"))

(defun yacas-fn-list ()
  (interactive)
  (insert "List()")
  (forward-char -1)
  (message "Args: elements of list"))

(defun yacas-fn-destructivereverse ()
  (interactive)
  (insert "DestructiveReverse()")
  (forward-char -1)
  (message "Args: list"))

(defun yacas-fn-nth ()
  (interactive)
  (insert "Nth()")
  (forward-char -1)
  (message "Args: list, index"))

(defun yacas-fn-length ()
  (interactive)
  (insert "Length()")
  (forward-char -1)
  (message "Args: object (list, array or string)"))

(defun yacas-fn-tail ()
  (interactive)
  (insert "Tail()")
  (forward-char -1)
  (message "Args: list"))

(defun yacas-fn-head ()
  (interactive)
  (insert "Head()")
  (forward-char -1)
  (message "Args: list"))


(defun yacas-fn-mathdiv ()
  (interactive)
  (insert "MathDiv()")
  (forward-char -1)
  (message "Args: x, y"))

(defun yacas-fn-matharccos ()
  (interactive)
  (insert "MathArcCos()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-matharcsin ()
  (interactive)
  (insert "MathArcSin()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-matharctan ()
  (interactive)
  (insert "MathArcTan()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-mathtan ()
  (interactive)
  (insert "MathTan()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-mathcos ()
  (interactive)
  (insert "MathCos()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-mathsin ()
  (interactive)
  (insert "MathSin()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-mathpower ()
  (interactive)
  (insert "MathPower()")
  (forward-char -1)
  (message "Args: x, y"))

(defun yacas-fn-mathlog ()
  (interactive)
  (insert "MathLog()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-mathexp ()
  (interactive)
  (insert "MathExp()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-mathmod ()
  (interactive)
  (insert "MathMod()")
  (forward-char -1)
  (message "Args: x, y"))

(defun yacas-fn-mathabs ()
  (interactive)
  (insert "MathAbs()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-mathceil ()
  (interactive)
  (insert "MathCeil()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-mathfloor ()
  (interactive)
  (insert "MathFloor()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-mathsqrt ()
  (interactive)
  (insert "MathSqrt()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-mathdivide ()
  (interactive)
  (insert "MathDivide()")
  (forward-char -1)
  (message "Args: x, y"))

(defun yacas-fn-mathmultiply ()
  (interactive)
  (insert "MathMultiply()")
  (forward-char -1)
  (message "Args: x, y"))

(defun yacas-fn-mathsubtract ()
  (interactive)
  (insert "MathSubtract()")
  (forward-char -1)
  (message "Args: x, y"))

(defun yacas-fn-mathadd ()
  (interactive)
  (insert "MathAdd()")
  (forward-char -1)
  (message "Args: x, y"))

(defun yacas-fn-mathgcd ()
  (interactive)
  (insert "MathGcd()")
  (forward-char -1)
  (message "Args: m, n"))

(defun yacas-fn-mathor ()
  (interactive)
  (insert "MathOr()")
  (forward-char -1)
  (message "Args: booleans"))

(defun yacas-fn-mathnot ()
  (interactive)
  (insert "MathNot()")
  (forward-char -1)
  (message "Args: booleans"))

(defun yacas-fn-mathand ()
  (interactive)
  (insert "MathAnd()")
  (forward-char -1)
  (message "Args: booleans"))


(defun yacas-fn-listfromarray ()
  (interactive)
  (insert "ListFromArray()")
  (forward-char -1)
  (message "Args: array"))

(defun yacas-fn-arraycreatefromlist ()
  (interactive)
  (insert "ArrayCreateFromList()")
  (forward-char -1)
  (message "Args: list"))

(defun yacas-fn-arrayset ()
  (interactive)
  (insert "ArraySet()")
  (forward-char -1)
  (message "Args: array, index, element"))

(defun yacas-fn-arrayget ()
  (interactive)
  (insert "ArrayGet()")
  (forward-char -1)
  (message "Args: array, index"))

(defun yacas-fn-arraysize ()
  (interactive)
  (insert "ArraySize()")
  (forward-char -1)
  (message "Args: array"))

(defun yacas-fn-arraycreate ()
  (interactive)
  (insert "ArrayCreate()")
  (forward-char -1)
  (message "Args: size, initial value"))

(defun yacas-fn-generictypename ()
  (interactive)
  (insert "GenericTypeName()")
  (forward-char -1)
  (message "Args: object"))

(defun yacas-fn-isgeneric ()
  (interactive)
  (insert "IsGeneric()")
  (forward-char -1)
  (message "Args: object"))

(defun yacas-fn-shiftright ()
  (interactive)
  (insert "ShiftRight()")
  (forward-char -1)
  (message "Args: number, bits"))

(defun yacas-fn-shiftleft ()
  (interactive)
  (insert "ShiftLeft()")
  (forward-char -1)
  (message "Args: number, bits"))

(defun yacas-fn-greaterthan ()
  (interactive)
  (insert "GreaterThan()")
  (forward-char -1)
  (message "Args: number1, number2"))

(defun yacas-fn-lessthan ()
  (interactive)
  (insert "LessThan()")
  (forward-char -1)
  (message "Args: number1, number2"))

(defun yacas-fn-equals ()
  (interactive)
  (insert "Equals()")
  (forward-char -1)
  (message "Args: a, b"))

(defun yacas-fn-bitxor ()
  (interactive)
  (insert "BitXor()")
  (forward-char -1)
  (message "Args: m, n"))

(defun yacas-fn-bitor ()
  (interactive)
  (insert "BitOr()")
  (forward-char -1)
  (message "Args: m, n"))

(defun yacas-fn-bitand ()
  (interactive)
  (insert "BitAnd()")
  (forward-char -1)
  (message "Args: m, n"))

(defun yacas-fn-macrolocal ()
  (interactive)
  (insert "MacroLocal()")
  (forward-char -1)
  (message "Args: variables"))

(defun yacas-fn-local ()
  (interactive)
  (insert "Local()")
  (forward-char -1)
  (message "Args: variables"))

(defun yacas-fn-unfence ()
  (interactive)
  (insert "UnFence()")
  (forward-char -1)
  (message "Args: \"operator\", arity"))

(defun yacas-fn-tryretract ()
  (interactive)
  (insert "Retract()")
  (forward-char -1)
  (message "Args: \"operator\", arity"))

(defun yacas-fn-holdarg ()
  (interactive)
  (insert "HoldArg()")
  (forward-char -1)
  (message "Args: \"operator\", parameter"))

(defun yacas-fn-rulebase ()
  (interactive)
  (insert "RuleBase()")
  (forward-char -1)
  (message "Args: \"operator\", parameter list"))

(defun yacas-fn-macrorulebase ()
  (interactive)
  (insert "MacroRuleBase()")
  (forward-char -1)
  (message "Args: \"operator\", parameter list"))

(defun yacas-fn-rightprecedence ()
  (interactive)
  (insert "RightPrecedence()")
  (forward-char -1)
  (message "Args: \"operator\", precendence"))

(defun yacas-fn-leftprecedence ()
  (interactive)
  (insert "LeftPrecedence()")
  (forward-char -1)
  (message "Args: \"operator\", precendence"))

(defun yacas-fn-rightassociative ()
  (interactive)
  (insert "RightAssociative()")
  (forward-char -1)
  (message "Args: \"operator\""))

(defun yacas-fn-opprecedence ()
  (interactive)
  (insert "OpPrecedence()")
  (forward-char -1)
  (message "Args: string"))

(defun yacas-fn-ispostfix ()
  (interactive)
  (insert "IsPostfix()")
  (forward-char -1)
  (message "Args: string"))

(defun yacas-fn-isprefix ()
  (interactive)
  (insert "IsPrefix()")
  (forward-char -1)
  (message "Args: string"))

(defun yacas-fn-isinfix ()
  (interactive)
  (insert "IsInfix()")
  (forward-char -1)
  (message "Args: string"))

(defun yacas-fn-infix ()
  (interactive)
  (insert "Infix()")
  (forward-char -1)
  (message "Args: \"operator\", precedence"))

(defun yacas-fn-bodied ()
  (interactive)
  (insert "Bodied()")
  (forward-char -1)
  (message "Args: \"operator\", precedence"))

(defun yacas-fn-postfix ()
  (interactive)
  (insert "Postfix()")
  (forward-char -1)
  (message "Args: \"operator\""))

(defun yacas-fn-prefix ()
  (interactive)
  (insert "Prefix()")
  (forward-char -1)
  (message "Args: \"operator\""))

(defun yacas-fn-check ()
  (interactive)
  (insert "Check()")
  (forward-char -1)
  (message "Args: predicate, error"))

(defun yacas-fn-concatstrings ()
  (interactive)
  (insert "ConcatStrings()")
  (forward-char -1)
  (message "Args: string1, string2, ..."))

(defun yacas-fn-string ()
  (interactive)
  (insert "String()")
  (forward-char -1)
  (message "Args: atom"))

(defun yacas-fn-atom ()
  (interactive)
  (insert "Atom()")
  (forward-char -1)
  (message "Args: atom"))

(defun yacas-fn-lazyglobal ()
  (interactive)
  (insert "SetGlobalLazyVariable()")
  (forward-char -1)
  (message "Args: variable"))

(defun yacas-fn-object ()
  (interactive)
  (insert "Object()")
  (forward-char -1)
  (message "Args: predicate, object"))

(defun yacas-fn-clear ()
  (interactive)
  (insert "Clear()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-macroclear ()
  (interactive)
  (insert "MacroClear()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-set ()
  (interactive)
  (insert "Set()")
  (forward-char -1)
  (message "Args: variable, value"))

(defun yacas-fn-macroset ()
  (interactive)
  (insert "MacroSet()")
  (forward-char -1)
  (message "Args: variable, value"))

(defun yacas-fn-canprove ()
  (interactive)
  (insert "CanProve()")
  (forward-char -1)
  (message "Args: proposition"))

(defun yacas-fn-isconstant ()
  (interactive)
  (insert "IsConstant()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-isboolean ()
  (interactive)
  (insert "IsBoolean()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-isbound ()
  (interactive)
  (insert "IsBound()")
  (forward-char -1)
  (message "Args: variable"))

(defun yacas-fn-isstring ()
  (interactive)
  (insert "IsString()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-isatom ()
  (interactive)
  (insert "IsAtom()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-isfunction ()
  (interactive)
  (insert "IsFunction()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-isnonobject ()
  (interactive)
  (insert "IsNonObject()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-isfreeof ()
  (interactive)
  (insert "IsFreeOf()")
  (forward-char -1)
  (message "Args: expression, variable or list of variables"))

(defun yacas-fn-withvalue ()
  (interactive)
  (insert "WithValue()")
  (forward-char -1)
  (message "Args: variable, value, expression"))

(defun yacas-fn-subst ()
  (interactive)
  (insert "Subst()")
  (forward-char -1)
  (message "Args: from, to"))

(defun yacas-fn-patchstring ()
  (interactive)
  (insert "PatchString()")
  (forward-char -1)
  (message "Args: string"))

(defun yacas-fn-systemcall ()
  (interactive)
  (insert "SystemCall()")
  (forward-char -1)
  (message "Args: string"))

(defun yacas-fn-makevector ()
  (interactive)
  (insert "MakeVector()")
  (forward-char -1)
  (message "Args: variable, n"))

(defun yacas-fn-randomintegervector ()
  (interactive)
  (insert "RandomIntegerVector()")
  (forward-char -1)
  (message "Args: nr, from, to"))

(defun yacas-fn-map ()
  (interactive)
  (insert "Map()")
  (forward-char -1)
  (message "Args: \"operator\", list of lists"))

(defun yacas-fn-mapsingle ()
  (interactive)
  (insert "MapSingle()")
  (forward-char -1)
  (message "Args: \"operator\", list"))

(defun yacas-fn-nrargs ()
  (interactive)
  (insert "NrArgs()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-type ()
  (interactive)
  (insert "Type()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-permutations ()
  (interactive)
  (insert "Permutations()")
  (forward-char -1)
  (message "Args: list"))

(defun yacas-fn-levicivita ()
  (interactive)
  (insert "LeviCivita()")
  (forward-char -1)
  (message "Args: list"))

(defun yacas-fn-varlist ()
  (interactive)
  (insert "VarList()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-random ()
  (interactive)
  (insert "Random()"))

(defun yacas-fn-rationalize ()
  (interactive)
  (insert "Rationalize()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-sum ()
  (interactive)
  (insert "Sum()")
  (forward-char -1)
  (let ((var (read-string "Variable: ")))
    (if (string= var "")
	(message "Args: list")
      (insert var ", ")
      (setq var (read-string "From: "))
      (insert var ", ")
      (setq var (read-string "To: "))
      (insert var ", "))))

(defun yacas-fn-pslq ()
  (interactive)
  (insert "Pslq()")
  (forward-char -1)
  (message "Args: xlist, precision"))

(defun yacas-fn-help ()
  (interactive)
  (insert "Help()")
  (forward-char -1)
  (message "Opt arg: String containing the name of a function"))

(defun yacas-fn-prettyprinter ()
  (interactive)
  (insert "PrettyPrinter()")
  (forward-char -1)
  (message "Opt args: \"PrettyForm\" turns on pretty printing."))

(defun yacas-fn-defaultdirectory ()
  (interactive)
  (insert "DefaultDirectory()")
  (forward-char -1)
  (message "Args: Path to where yacas script files reside"))

(defun yacas-fn-n ()
  (interactive)
  (insert "N()")
  (forward-char -1)
  (message "Args: expression ;Opt args: precision"))


(defun yacas-fn-bigoh ()
  (interactive)
  (insert "BigOh()")
  (forward-char -1)
  (message "Args: polynomial, variable, degree"))

(defun yacas-fn-monic ()
  (interactive)
  (insert "Monic()")
  (forward-char -1)
  (message "Args: polynomial"))

(defun yacas-fn-leadingcoef ()
  (interactive)
  (insert "LeadingCoef()")
  (forward-char -1)
  (message "Args: polynomial"))

(defun yacas-fn-randompoly ()
  (interactive)
  (insert "RandomPoly()")
  (forward-char -1)
  (message "Args: variable, degree, coefmin, coefmax"))

(defun yacas-fn-primitivepart ()
  (interactive)
  (insert "PrimitivePart()")
  (forward-char -1)
  (message "Args: polynomial"))

(defun yacas-fn-content ()
  (interactive)
  (insert "Content()")
  (forward-char -1)
  (message "Args: polynomial"))

(defun yacas-fn-coef ()
  (interactive)
  (insert "Coef()")
  (forward-char -1)
  (message "Args: expression, variable, order"))

(defun yacas-fn-degree ()
  (interactive)
  (insert "Degree()")
  (forward-char -1)
  (message "Args: expression ;Opt args: variable"))

(defun yacas-fn-expand ()
  (interactive)
  (insert "Expand()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-lagrangeinterpolant ()
  (interactive)
  (insert "LagrangeInterpolant()")
  (forward-char -1)
  (message "Args: xlist, ylist, variable"))


(defun yacas-fn-getprecision ()
  (interactive)
  (insert "GetPrecision()"))

(defun yacas-fn-precision ()
  (interactive)
  (insert "Precision()")
  (forward-char -1)
  (message "Args: n"))

(defun yacas-fn-historysize ()
  (interactive)
  (insert "HistorySize()")
  (forward-char -1)
  (message "Args: The number of lines to store in history file"))


(defun yacas-fn-trigsimpcombine ()
  (interactive)
  (insert "TrigSimpCombine()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-eliminate ()
  (interactive)
  (insert "Eliminate()")
  (forward-char -1)
  (message "Args: var, replace, function"))

(defun yacas-fn-radsimp ()
  (interactive)
  (insert "RadSimp()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-simplify ()
  (interactive)
  (insert "Simplify()")
  (forward-char -1)
  (message "Args: expression"))

(defun yacas-fn-suchthat ()
  (interactive)
  (insert "SuchThat()")
  (forward-char -1)
  (message "Args: expression, variable"))

(defun yacas-fn-psolve ()
  (interactive)
  (insert "PSolve()")
  (forward-char -1)
  (message "Args: expression, variable"))

(defun yacas-fn-solve ()
  (interactive)
  (insert "Solve()")
  (forward-char -1)
  (message "Args: equation, variable"))


(defun yacas-fn-sethelpbrowser ()
  (interactive)
  (insert "SetHelpBrowser()")
  (forward-char -1)
  (message "Args: helpbrowser"))


(defun yacas-fn-sign ()
  (interactive)
  (insert "Sign()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-abs ()
  (interactive)
  (insert "Abs()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-sqrt ()
  (interactive)
  (insert "Sqrt()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-ln ()
  (interactive)
  (insert "Ln()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-exp ()
  (interactive)
  (insert "Exp()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-arctan ()
  (interactive)
  (insert "ArcTan()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-arccos ()
  (interactive)
  (insert "ArcCos()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-arcsin ()
  (interactive)
  (insert "ArcSin()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-tan ()
  (interactive)
  (insert "Tan()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-cos ()
  (interactive)
  (insert "Cos()")
  (forward-char -1)
  (message "Args: x"))

(defun yacas-fn-sin ()
  (interactive)
  (insert "Sin()")
  (forward-char -1)
  (message "Args: x"))


(defconst inferior-yacas-menu-arithmetic
  (list
   "Arith"
   ["Average" yacas-fn-average]
   ["Bin" yacas-fn-bin]
   ["Ceil" yacas-fn-ceil]
   ["Complex" yacas-fn-complex]
   ["Conjugate" yacas-fn-conjugate]
   ["ContFrac" yacas-fn-contfrac]
   ["Decimal" yacas-fn-decimal]
   ["Denom" yacas-fn-denom]
   ["Div" yacas-fn-div]
   ["Factor" yacas-fn-factor]
   ["Factorize" yacas-fn-factorize]
   ["Factors" yacas-fn-factors]
   ["Fibonacci" yacas-fn-fibonacci]
   ["Floor" yacas-fn-floor]
   ["FromBase" yacas-fn-frombase]
   ["Gcd" yacas-fn-gcd]
   ["Im" yacas-fn-im]
   ["Lcm" yacas-fn-lcm]
   ["Max" yacas-fn-max]
   ["Min" yacas-fn-min]
   ["Mod" yacas-fn-mod]
   ["Numer" yacas-fn-numer]
   ["PAdicExpand" yacas-fn-padicexpand]
   ["Pi" yacas-fn-pi]
   ["Re" yacas-fn-re]
   ["Round" yacas-fn-round]
   ["Sum" yacas-fn-sum]
   ["ToBase" yacas-fn-tobase]
   (list
    "Predicates"
    ["IsEven" yacas-fn-iseven]
    ["IsInfinity" yacas-fn-isinfinity]
    ["IsInteger" yacas-fn-isinteger]
    ["IsNegativeInteger" yacas-fn-isnegativeinteger]
    ["IsNegativeNumber" yacas-fn-isnegativenumber]
    ["IsNonZeroInteger" yacas-fn-isnonzerointeger]
    ["IsNotZero" yacas-fn-isnotzero]
    ["IsNumber" yacas-fn-isnumber]
    ["IsOdd" yacas-fn-isodd]
    ["IsPositiveInteger" yacas-fn-ispositiveinteger]
    ["IsPositiveNumber" yacas-fn-ispositivenumber]
    ["IsPrime" yacas-fn-isprime]
    ["IsPrimePower" yacas-fn-isprimepower]
    ["IsRational" yacas-fn-isrational]
    ["IsZero" yacas-fn-iszero])))

(defconst inferior-yacas-menu-variables
  (list
   "Variables"
   ["Clear" yacas-fn-clear]
   ["SetGlobalLazyVariable" yacas-fn-lazyglobal]
   ["Local" yacas-fn-local]
   ["MacroClear" yacas-fn-macroclear]
   ["MacroLocal" yacas-fn-macrolocal]
   ["MacroSet" yacas-fn-macroset]
   ["Set" yacas-fn-set]))

(defconst inferior-yacas-menu-debug
  (list
   "Debug"
   ["TraceExp" yacas-fn-traceexp]
   ["TraceRule" yacas-fn-tracerule]
   ["TraceStack" yacas-fn-tracestack]))

(defconst inferior-yacas-menu-calculus
  (list
   "Calc"
   ["AntiDeriv" yacas-fn-antideriv]
   ["Curl" yacas-fn-curl]
   ["D" yacas-fn-d]
   ["Diverge" yacas-fn-diverge]
   ["Integrate" yacas-fn-integrate]
   ["InverseTaylor" yacas-fn-inversetaylor]
   ["Limit" yacas-fn-limit]
   ["Newton" yacas-fn-newton]
   ["ReversePoly" yacas-fn-reversepoly]
   ["Taylor" yacas-fn-taylor]))

(defconst inferior-yacas-menu-control
  (list
   "Control"
   ["Apply" yacas-fn-apply]
   ["Else"  yacas-else]
   ["Eval" yacas-fn-eval]
   ["For"  yacas-for]
   ["ForEach"  yacas-foreach]
   ["Function" yacas-function]
   ["Hold" yacas-fn-hold]
   ["If"  yacas-if]
   ["LocalSymbols" yacas-fn-localsymbols]
   ["New local variable"  yacas-local]
   ["Procedure"  yacas-proc]
   ["Prog" yacas-fn-prog]
   ["Secure" yacas-fn-secure]
   ["Until"  yacas-until]
   ["While"  yacas-while]))

(defconst inferior-yacas-menu-fast
  (list
   "Fast..."
   ["FastArcCos" yacas-fn-fastarccos]
   ["FastArcSin" yacas-fn-fastarcsin]
   ["FastArcTan" yacas-fn-fastarctan]
   ["FastCos" yacas-fn-fastcos]
   ["FastExp" yacas-fn-fastexp]
   ["FastLog" yacas-fn-fastlog]
   ["FastPower" yacas-fn-fastpower]
   ["FastSin" yacas-fn-fastsin]
   ["FastTan" yacas-fn-fasttan]))

(defconst inferior-yacas-menu-io
  (list
   "I/O"
   ["DefLoad" yacas-fn-defload]
   ["Echo" yacas-fn-echo]
   ["FindFile" yacas-fn-findfile]
   ["FromFile" yacas-fn-fromfile]
   ["FromString" yacas-fn-fromstring]
   ["FullForm" yacas-fn-fullform]
   ["LispRead" yacas-fn-lispread]
   ["Load" yacas-fn-load]
   ["NewLine" yacas-fn-newline]
   ["PatchLoad" yacas-fn-patchload]
   ["PrettyForm" yacas-fn-prettyform]
   ["Read" yacas-fn-read]
   ["ReadToken" yacas-fn-readtoken]
   ["Space" yacas-fn-space]
   ["ToFile" yacas-fn-tofile]
   ["ToString" yacas-fn-tostring]
   ["Use" yacas-fn-use]
   ["Write" yacas-fn-write]
   ["WriteString" yacas-fn-writestring]))

(defconst inferior-yacas-menu-linalg
  (list
   "LinAlg"
   ["BaseVector" yacas-fn-basevector]
   ["CharacteristicEquation" yacas-fn-characteristicequation]
   ["CoFactor" yacas-fn-cofactor]
   ["Commutator" yacas-fn-commutator]
   ["CrossProduct" yacas-fn-crossproduct]
   ["Determinant" yacas-fn-determinant]
   ["DiagonalMatrix" yacas-fn-diagonalmatrix]
   ["EigenValues" yacas-fn-eigenvalues]
   ["EigenVectors" yacas-fn-eigenvectors]
   ["Identity" yacas-fn-identity]
   ["InProduct" yacas-fn-inproduct]
   ["Inverse" yacas-fn-inverse]
   ["Minor" yacas-fn-minor]
   ["Normalize" yacas-fn-normalize]
   ["SolveMatrix" yacas-fn-solvematrix]
   ["Trace" yacas-fn-trace]
   ["Tranpose" yacas-fn-tranpose]
   ["ZeroMatrix" yacas-fn-zeromatrix]
   ["ZeroVector" yacas-fn-zerovector]
   (list
    "Predicates"
    ["IsHermitian" yacas-fn-ishermitian]
    ["IsMatrix" yacas-fn-ismatrix]
    ["IsUnitary" yacas-fn-isunitary]
    ["IsZeroVector" yacas-fn-iszerovector])))

(defconst inferior-yacas-menu-arrays
  (list
   "Arrays"
   ["ArrayCreate" yacas-fn-arraycreate]
   ["ArrayCreateFromList" yacas-fn-arraycreatefromlist]
   ["ArrayGet" yacas-fn-arrayget]
   ["ArraySet" yacas-fn-arrayset]
   ["ArraySize" yacas-fn-arraysize]))

(defconst inferior-yacas-menu-sets
  (list
   "Sets"
   ["Difference" yacas-fn-difference]
   ["Intersection" yacas-fn-intersection]   
   ["Union" yacas-fn-union]))


(defconst inferior-yacas-menu-lists
  (list
   "Lists"
   inferior-yacas-menu-arrays
   inferior-yacas-menu-sets
   ["Append" yacas-fn-append]
   ["Assoc" yacas-fn-assoc]
   ["AssocIndices" yacas-fn-associndices]
   ["BubbleSort" yacas-fn-bubblesort]
   ["Concat" yacas-fn-concat]
   ["Contains" yacas-fn-contains]
   ["Count" yacas-fn-count]
   ["Delete" yacas-fn-delete]
   ["DestructiveAppend" yacas-fn-destructiveappend]
   ["DestructiveDelete" yacas-fn-destructivedelete]
   ["DestructiveInsert" yacas-fn-destructiveinsert]
   ["DestructiveReplace" yacas-fn-destructivereplace]
   ["DestructiveReverse" yacas-fn-destructivereverse]
   ["Drop" yacas-fn-drop]
   ["FillList" yacas-fn-filllist]
   ["Find" yacas-fn-find]
   ["FlatCopy" yacas-fn-flatcopy]
   ["Flatten" yacas-fn-flatten]
   ["Head" yacas-fn-head]
   ["Insert" yacas-fn-insert]
   ["Length" yacas-fn-length]
   ["List" yacas-fn-list]
   ["ListFromArray" yacas-fn-listfromarray]
   ["Listify" yacas-fn-listify]
   ["Nth" yacas-fn-nth]
   ["Partition" yacas-fn-partition]
   ["Pop" yacas-fn-pop]
   ["PopBack" yacas-fn-popback]
   ["PopFront" yacas-fn-popfront]
   ["Push" yacas-fn-push]
   ["RemoveDuplicates" yacas-fn-removeduplicates]
   ["Replace" yacas-fn-replace]
   ["Select" yacas-fn-select]
   ["Swap" yacas-fn-swap]
   ["Table" yacas-fn-table]
   ["TableForm" yacas-fn-tableform]
   ["Tail" yacas-fn-tail]
   ["Take" yacas-fn-take]
   ["UnList" yacas-fn-unlist]
   ["Unflatten" yacas-fn-unflatten]
   (list
    "Predicates"
    ["IsList" yacas-fn-islist])))

(defconst inferior-yacas-menu-math
  (list
   "Math..."
   ["MathAbs" yacas-fn-mathabs]
   ["MathAdd" yacas-fn-mathadd]
   ["MathAnd" yacas-fn-mathand]
   ["MathArcCos" yacas-fn-matharccos]
   ["MathArcSin" yacas-fn-matharcsin]
   ["MathArcTan" yacas-fn-matharctan]
   ["MathCeil" yacas-fn-mathceil]
   ["MathCos" yacas-fn-mathcos]
   ["MathDiv" yacas-fn-mathdiv]
   ["MathDivide" yacas-fn-mathdivide]
   ["MathExp" yacas-fn-mathexp]
   ["MathFloor" yacas-fn-mathfloor]
   ["MathGcd" yacas-fn-mathgcd]
   ["MathLog" yacas-fn-mathlog]
   ["MathMod" yacas-fn-mathmod]
   ["MathMultiply" yacas-fn-mathmultiply]
   ["MathNot" yacas-fn-mathnot]
   ["MathOr" yacas-fn-mathor]
   ["MathPower" yacas-fn-mathpower]
   ["MathSin" yacas-fn-mathsin]
   ["MathSqrt" yacas-fn-mathsqrt]
   ["MathSubtract" yacas-fn-mathsubtract]
   ["MathTan" yacas-fn-mathtan]))

(defconst inferior-yacas-menu-misc
  (list
   "Misc"
   ["Atom" yacas-fn-atom]
   ["BitAnd" yacas-fn-bitand]
   ["BitOr" yacas-fn-bitor]
   ["BitXor" yacas-fn-bitxor]
   ["Bodied" yacas-fn-bodied]
   ["Check" yacas-fn-check]
   ["ConcatStrings" yacas-fn-concatstrings]
   ["Equals" yacas-fn-equals]
   ["GenericTypeName" yacas-fn-generictypename]
   ["GreaterThan" yacas-fn-greaterthan]
   ["Help" yacas-fn-help]
   ["HoldArg" yacas-fn-holdarg]
   ["Infix" yacas-fn-infix]
   ["LeftPrecedence" yacas-fn-leftprecedence]
   ["LessThan" yacas-fn-lessthan]
   ["LeviCivita" yacas-fn-levicivita]
   ["MacroRuleBase" yacas-fn-Macrorulebase]
   ["MakeVector" yacas-fn-makevector]
   ["Map" yacas-fn-map]
   ["MapSingle" yacas-fn-mapsingle]
   ["N" yacas-fn-n]
   ["NrArgs" yacas-fn-nrargs]
   ["Object" yacas-fn-object]
   ["OpPrecedence" yacas-fn-opprecedence]
   ["PatchString" yacas-fn-patchstring]
   ["Permutations" yacas-fn-permutations]
   ["Postfix" yacas-fn-postfix]
   ["Prefix" yacas-fn-prefix]
   ["Pslq" yacas-fn-pslq]
   ["Random" yacas-fn-random]
   ["RandomIntegerVector" yacas-fn-randomintegervector]
   ["RightAssociative" yacas-fn-rightassociative]
   ["RightPrecedence" yacas-fn-rightprecedence]
   ["RuleBase" yacas-fn-rulebase]
   ["ShiftLeft" yacas-fn-shiftleft]
   ["ShiftRight" yacas-fn-shiftright]
   ["String" yacas-fn-string]
   ["Subst" yacas-fn-subst]
   ["SystemCall" yacas-fn-systemcall]
   ["Retract" yacas-fn-tryretract]
   ["Type" yacas-fn-type]
   ["UnFence" yacas-fn-unfence]
   ["VarList" yacas-fn-varlist]
   ["WithValue" yacas-fn-withvalue]
   (list
    "Predicates"
    ["CanProve" yacas-fn-canprove]
    ["IsAtom" yacas-fn-isatom]
    ["IsBoolean" yacas-fn-isboolean]
    ["IsBound" yacas-fn-isbound]
    ["IsConstant" yacas-fn-isconstant]
    ["IsFreeOf" yacas-fn-isfreeof]
    ["IsFunction" yacas-fn-isfunction]
    ["IsGeneric" yacas-fn-isgeneric]
    ["IsInfix" yacas-fn-isinfix]
    ["IsNonObject" yacas-fn-isnonobject]
    ["IsPostfix" yacas-fn-ispostfix]
    ["IsPrefix" yacas-fn-isprefix]
    ["IsString" yacas-fn-isstring])))

(defconst inferior-yacas-menu-poly
  (list
   "Poly"
   ["BigOh" yacas-fn-bigoh]
   ["Coef" yacas-fn-coef]
   ["Content" yacas-fn-content]
   ["Degree" yacas-fn-degree]
   ["Div" yacas-fn-div]
   ["Expand" yacas-fn-expand]
   ["Factor" yacas-fn-factor]
   ["Factors" yacas-fn-factors]
   ["Gcd" yacas-fn-gcd]
   ["LagrangeInterpolant" yacas-fn-lagrangeinterpolant]
   ["Lcm" yacas-fn-lcm]
   ["LeadingCoef" yacas-fn-leadingcoef]
   ["Mod" yacas-fn-mod]
   ["Monic" yacas-fn-monic]
   ["PrimitivePart" yacas-fn-primitivepart]
   ["RandomPoly" yacas-fn-randompoly]))
   
(defconst inferior-yacas-menu-config
  (list
   "Config"
   ["DefaultDirectory" yacas-fn-defaultdirectory]
   ["GetPrecision" yacas-fn-getprecision]
   ["HistorySize" yacas-fn-historysize]
   ["MaxEvalDepth" yacas-fn-maxevaldepth]
   ["Precision" yacas-fn-precision]
   ["PrettyPrinter" yacas-fn-prettyprinter]
   ["SetHelpBrowser" yacas-fn-sethelpbrowser]))

(defconst inferior-yacas-menu-simplify
  (list
   "Simp"
   ["Eliminate" yacas-fn-eliminate]
   ["RadSimp" yacas-fn-radsimp]
   ["Rationalize" yacas-fn-rationalize]
   ["Simplify" yacas-fn-simplify]
   ["SuchThat" yacas-fn-suchthat]
   ["TrigSimpCombine" yacas-fn-trigsimpcombine]))

(defconst inferior-yacas-menu-solve
  (list
   "Solve"
   ["PSolve" yacas-fn-psolve]
   ["Solve" yacas-fn-solve]))

(defconst inferior-yacas-menu-yacas
  (list
   "Yacas>>"
   ["Quit Yacas" inferior-yacas-quit]
   ["Toggle Yacas/Emacs menus" inferior-yacas-menu-toggle-menubar]))

(defconst inferior-yacas-menu-quit
  (list
   "quit"
   ["Quit Yacas" inferior-yacas-quit]))

(defconst inferior-yacas-menu-help
  (list
   "Help"
   ["Yacas help" yacas-help]))

(defconst inferior-yacas-menu-functions
  (list
   "Functions"
   ["Abs" yacas-fn-abs]
   ["ArcCos" yacas-fn-arccos]
   ["ArcSin" yacas-fn-arcsin]
   ["ArcTan" yacas-fn-arctan]
   ["Cos" yacas-fn-cos]
   ["Exp" yacas-fn-exp]
   ["Ln" yacas-fn-ln]
   ["Sign" yacas-fn-sign]
   ["Sin" yacas-fn-sin]
   ["Sqrt" yacas-fn-sqrt]
   ["Tan" yacas-fn-tan]))

(defconst inferior-yacas-menu-others
  (list
   "Others"
   inferior-yacas-menu-fast
   inferior-yacas-menu-math
   inferior-yacas-menu-io
   inferior-yacas-menu-misc))

(defconst inferior-yacas-menu-prog
  (list
   "Prog"
   inferior-yacas-menu-control
   inferior-yacas-menu-variables
   inferior-yacas-menu-debug))
