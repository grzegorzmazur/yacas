
#ifndef __mathuserfunc_h__
#define __mathuserfunc_h__

#include "lispuserfunc.h"
#include "grower.h"

class PatternClass;

class BranchingUserFunction : public LispArityUserFunction
{
public:
    class BranchParameter
    {
    public:
        BranchParameter(LispStringPtr aParameter,
                        LispInt aHold=LispFalse)
            : iParameter(aParameter), iHold(aHold) {}
        LispStringPtr iParameter;
        LispInt       iHold;
    };
    class BranchRuleBase
    {
    public:
        virtual ~BranchRuleBase();
        virtual LispBoolean Matches(LispEnvironment& aEnvironment, LispPtr* aArguments) = 0;
        virtual LispInt Precedence() const = 0;
        virtual LispPtr& Body() = 0;
    };
    class BranchRule : public BranchRuleBase
    {
    public:
        virtual ~BranchRule();
        BranchRule(LispInt aPrecedence,LispPtr& aPredicate,LispPtr& aBody)
        {
            iPrecedence = aPrecedence;
            iPredicate.Set(aPredicate.Get());
            iBody.Set(aBody.Get());
        }
        virtual LispBoolean Matches(LispEnvironment& aEnvironment, LispPtr* aArguments);
        virtual LispInt Precedence() const;
        virtual LispPtr& Body();
    private:
        LispInt iPrecedence;
        LispPtr iBody;
        LispPtr iPredicate;
    };
    
    class BranchPattern : public BranchRuleBase
    {
    public:
        virtual ~BranchPattern();
        BranchPattern(LispInt aPrecedence,LispPtr& aPredicate,LispPtr& aBody)
        {
            iPatternClass = NULL;
            iPrecedence = aPrecedence;
            iPredicate.Set(aPredicate.Get());

            GenericClass *gen = aPredicate.Get()->Generic();
            Check(gen != NULL,KLispErrInvalidArg);
            Check(StrEqual(gen->TypeName(),"\"Pattern\""),KLispErrInvalidArg);

            iPatternClass = (PatternClass*)gen;
            iBody.Set(aBody.Get());
        }
        virtual LispBoolean Matches(LispEnvironment& aEnvironment, LispPtr* aArguments);
        virtual LispInt Precedence() const;
        virtual LispPtr& Body();
    private:
        LispInt iPrecedence;
        LispPtr iBody;
        LispPtr iPredicate;
        PatternClass *iPatternClass;
    };

    BranchingUserFunction(LispPtr& aParameters);
    virtual ~BranchingUserFunction();
    virtual void Evaluate(LispPtr& aResult,LispEnvironment& aEnvironment,
                  LispPtr& aArguments);
    virtual void HoldArgument(LispStringPtr aVariable);
    virtual LispInt Arity() const;
    virtual void DeclareRule(LispInt aPrecedence, LispPtr& aPredicate,
                             LispPtr& aBody);
    void DeclarePattern(LispInt aPrecedence, LispPtr& aPredicate,
                        LispPtr& aBody);
    void InsertRule(LispInt aPrecedence,BranchRuleBase* newRule);
    virtual LispPtr& ArgList();
    
private:
    CArrayGrower<BranchParameter> iParameters;
    CDeletingArrayGrower<BranchRuleBase*>     iRules;
    LispPtr iParamList;
};


#endif

