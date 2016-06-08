package net.sf.yacas;

// new-style evaluator, passing arguments onto the stack in LispEnvironment
class YacasEvaluator extends EvalFuncBase {
    // FunctionFlags can be orred when passed to the constructor of this function

    static int Function = 0;    // Function: evaluate arguments
    static int Macro = 1;       // Function: don't evaluate arguments
    static int Fixed = 0;     // fixed number of arguments
    static int Variable = 2;  // variable number of arguments

    public YacasEvaluator(YacasEvalCaller aCaller, int aNrArgs, int aFlags) {
        iCaller = aCaller;
        iNrArgs = aNrArgs;
        iFlags = aFlags;
    }

    @Override
    public void Evaluate(LispPtr aResult, LispEnvironment aEnvironment, LispPtr aArguments) throws Exception {
        if ((iFlags & Variable) == 0)
            LispError.CheckNrArgs(iNrArgs + 1, aArguments, aEnvironment);

        int stacktop = aEnvironment.iStack.size();

        // Push a place holder for the result: push full expression so it is available for error reporting
        aEnvironment.iStack.add(new LispPtr(aArguments.Get()));

        LispIterator iter = new LispIterator(aArguments);
        iter.GoNext();

        int i;
        int nr = iNrArgs;

        if ((iFlags & Variable) != 0) {
            nr--;
        }

        // Walk over all arguments, evaluating them as necessary
        if ((iFlags & Macro) != 0) {
            for (i = 0; i < nr; i++) {
                LispError.Check(iter.GetObject() != null, LispError.KLispErrWrongNumberOfArgs);
                aEnvironment.iStack.add(new LispPtr(iter.GetObject().Copy(false)));
                iter.GoNext();
            }
            if ((iFlags & Variable) != 0) {
                LispPtr head = new LispPtr();
                head.Set(aEnvironment.iList.Copy(false));
                head.Get().Next().Set(iter.GetObject());
                aEnvironment.iStack.add(new LispPtr(LispSubList.New(head.Get())));
            }
        } else {
            LispPtr arg = new LispPtr();
            for (i = 0; i < nr; i++) {
                LispError.Check(iter.GetObject() != null, LispError.KLispErrWrongNumberOfArgs);
                LispError.Check(iter.Ptr() != null, LispError.KLispErrWrongNumberOfArgs);
                aEnvironment.iEvaluator.Eval(aEnvironment, arg, iter.Ptr());
                aEnvironment.iStack.add(new LispPtr(arg.Get()));
                iter.GoNext();
            }
            if ((iFlags & Variable) != 0) {

                LispPtr head = new LispPtr();
                head.Set(aEnvironment.iList.Copy(false));
                head.Get().Next().Set(iter.GetObject());
                LispPtr list = new LispPtr();
                list.Set(LispSubList.New(head.Get()));

                aEnvironment.iEvaluator.Eval(aEnvironment, arg, list);

                aEnvironment.iStack.add(new LispPtr(arg.Get()));
            }
        }

        iCaller.Eval(aEnvironment, stacktop);
        aResult.Set(aEnvironment.iStack.get(stacktop).Get());
        aEnvironment.iStack.subList(stacktop, aEnvironment.iStack.size()).clear();
    }
    YacasEvalCaller iCaller;
    int iNrArgs;
    int iFlags;
}
