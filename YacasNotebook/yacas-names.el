;;; Lists of names for font-locking and completion.

(provide 'yacas-names)

;;Predicates  DONE

(setq yacas-symbol-predicates
(list
"<"
">"
"<="
">="
"!="
"="))


(setq yacas-predicates
(list
"And"
"Or"
"Not"
"GreaterThan"
"LessThan"
"Equals"
"IsPostfix"
"IsPrefix"
"IsInfix"
"CanProve"
"IsConstant"
"IsBoolean"
"IsBound"
"IsString"
"IsAtom"
"IsFunction"
"IsNonObject"
"IsFreeOf"
"IsGeneric"
"IsList"
"IsMatrix"
"IsInfinity"
"IsNonZeroInteger"
"IsNotZero"
"IsPositiveInteger"
"IsPositiveNumber"
"IsNegativeInteger"
"IsNegativeNumber"
"IsInteger"
"IsNumber"
"IsOdd"
"IsEven"
"IsRational"
"IsZero"
"IsPrimePower"
"IsPrime"
"IsZeroVector"
"IsUnitary"
"IsHermitian"
"MathOr"
"MathNot"
"MathAnd"
))



;;Functions DONE
(setq yacas-functions
(list
"Example"
"TrigSimpCombine"
"Eliminate"
"RadSimp"
"Simplify"
"SuchThat"
"PSolve"
"Solve"
"SetHelpBrowser"
"Sign"
"Abs"
"Sqrt"
"Ln"
"Exp"
"ArcTan"
"ArcCos"
"ArcSin"
"Tan"
"Cos"
"Sin"
"StubApiCStruct"
"StubApiCFile"
"StubApiCSetEnv"
"StubApiCRemark"
"StubApiCFunction"
"StubApiCInclude"
"StubApiCShortIntegerConstant"
"StubApiCStart"
"DllLoad"
"Fibonacci"
"Conjugate"
"Denom"
"Numer"
"Max"
"Min"
"Factorize"
"Average"
"Bin"
"Im"
"Re"
"Complex"
"Round"
"Ceil"
"Floor"
"TruncRadian"
"Decimal"
"ContFrac"
"PAdicExpand"
"Factors"
"Factor"
"ToBase"
"FromBase"
"Lcm"
"Gcd"
"Mod"
"Div"
"Limit"
"Integrate"
"Curl"
"Diverge"
"D"
"Newton"
"ReversePoly"
"InverseTaylor"
"Taylor"
"Secure"
"Prog"
"TraceRule"
"TraceExp"
"TraceStack"
"LocalSymbols"
"Apply"
"Eval"
"Hold"
"MaxEvalDepth"
"FastArcSin"
"FastArcCos"
"FastArcTan"
"FastTan"
"FastCos"
"FastSin"
"FastPower"
"FastLog"
"FastExp"
"PatchLoad"
"FindFile"
"DefLoad"
"Load"
"ToFile"
"ReadToken"
"LispRead"
"Read"
"ToString"
"FromString"
"FromFile"
"NewLine"
"Space"
"WriteString"
"Write"
"PrettyForm"
"Echo"
"FullForm"
"Use"
"EigenVectors"
"EigenValues"
"CharacteristicEquation"
"SolveMatrix"
"Minor"
"CoFactor"
"Inverse"
"Trace"
"DiagonalMatrix"
"Determinant"
"Tranpose"
"ZeroMatrix"
"Normalize"
"Identity"
"BaseVector"
"ZeroVector"
"CrossProduct"
"InProduct"
"Commutator"
"Select"
"TableForm"
"Table"
"BubbleSort"
"Unflatten"
"Flatten"
"AssocIndices"
"Assoc"
"Partition"
"Take"
"Drop"
"FillList"
"Difference"
"Union"
"Intersection"
"Count"
"Swap"
"PopBack"
"PopFront"
"Pop"
"Push"
"RemoveDuplicates"
"DestructiveAppend"
"Append"
"Find"
"Contains"
"FlatCopy"
"DestructiveReplace"
"Replace"
"DestructiveDelete"
"DestructiveInsert"
"Insert"
"Delete"
"Concat"
"Listify"
"UnList"
"List"
"DestructiveReverse"
"Nth"
"Length"
"Tail"
"Head"
"MathDiv"
"MathArcCos"
"MathArcSin"
"MathArcTan"
"MathTan"
"MathCos"
"MathSin"
"MathPower"
"MathLog"
"MathExp"
"MathMod"
"MathAbs"
"MathCeil"
"MathFloor"
"MathSqrt"
"MathDivide"
"MathMultiply"
"MathSubtract"
"MathAdd"
"MathGcd"
"ListFromArray"
"ArrayCreateFromList"
"ArraySet"
"ArrayGet"
"ArraySize"
"ArrayCreate"
"GenericTypeName"
"ShiftRight"
"ShiftLeft"
"BitXor"
"BitOr"
"BitAnd"
"UnFence"
"Retract"
"HoldArg"
"RuleBase"
"MacroRuleBase"
"RightPrecedence"
"LeftPrecedence"
"RightAssociative"
"OpPrecedence"
"Infix"
"Bodied"
"Postfix"
"Prefix"
"Check"
"ConcatStrings"
"String"
"Atom"
"SetGlobalLazyVariable"
"Object"
"Clear"
"MacroClear"
"Set"
"MacroSet"
"WithValue"
"Subst"
"PatchString"
"SystemCall"
"MakeVector"
"RandomIntegerVector"
"Map"
"MapSingle"
"NrArgs"
"Type"
"Permutations"
"LeviCivita"
"VarList"
"Random"
"Rationalize"
"Sum"
"Pslq"
"Help"
"PrettyPrinter"
"DefaultDirectory"
"N"
"BigOh"
"Monic"
"LeadingCoef"
"RandomPoly"
"PrimitivePart"
"Content"
"Coef"
"Degree"
"Expand"
"LagrangeInterpolant"
"GetPrecision"
"Precision"
"HistorySize"
"TrigSimpCombine"
"Eliminate"
"RadSimp"
"Simplify"
"SuchThat"
))


;;Constants DONE
(setq yacas-constants
(list
"Pi"
"I"
"True"
"False"
"EndOfFile"
"Infinity"
))


;;Control DONE
(setq yacas-control
(list
"Local"
"MacroLocal"
"While" 
"Until"
"If"
"Function"
"For"
"ForEach"
"Prog"
))


;;Operators  DONE
(setq yacas-symbol-operators
(list
";"
"("
")"
"{"
"}"
"]"
"["
"+"
"-"
"*"
"/"
"^"
"<<"
">>"
"!"
":"
"@"
"/@"
".."
"%"
"++"
"--"
":="
))


