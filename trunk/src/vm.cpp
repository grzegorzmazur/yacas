
#include "lisptype.h"
#include "lispenvironment.h"
#include "lispatom.h"
#include "standard.h"
//#include "arggetter.h"
//#include "lispplugin.h"
//#include "platmath.h"
#include "stubs.h"
//#include 'genericstructs.h"
//#include "mathcommands.h"
#include "vm.h"


#define ATOM(_x) LispAtom::New(aEnvironment,_x)
#define RESULT _this_stack.GetElement(aStackTop)
#define ARGUMENT(i) _this_stack.GetElement(aStackTop+i)

#define PUSH(_x)   _this_stack.PushArgOnStack(_x)
#define POP(_i)    _this_stack.PopTo(_this_stack.GetStackTop() - _i)
#define STACKTOP() _this_stack.GetStackTop()
#define STACK(_i)  _this_stack.GetElement(_i)

#define ISTRUE(_x) IsTrue(aEnvironment, _x)
#define ISFALSE(_x) IsFalse(aEnvironment, _x)

void RunFunction(LispEnvironment& aEnvironment,LispInt aStackTop,
            const unsigned char* code, LispObject* aConstants[])
{
//printf("1...\n");
  register LispEnvironment::YacasArgStack& _this_stack = aEnvironment.iStack;
//printf("2...\n");

  const unsigned char* ptr = code;
//printf("3...\n");
  int offset,index,constant;
//printf("4...\n");
  for (;;)
  {
//printf("5...\n");
//printf("6... %d\n",(int)*ptr);
    switch (*ptr++)
    {
      case CodeVmJumpIfTrue  :
//printf("CodeVmJumpIfTrue\n");
        if (ISTRUE (STACK(STACKTOP()-1)))
        {
          offset = *ptr++; offset <<= 8; offset |= *ptr++;
//printf("    jump to %d\n",offset);
          ptr = code+offset;
        }
        else ptr+=2;
        break;
      case CodeVmJumpIfFalse :
//printf("CodeVmJumpIfFalse\n");
        if (ISFALSE (STACK(STACKTOP()-1)))
        {
          offset = *ptr++; offset <<= 8; offset |= *ptr++;
//printf("    jump to %d\n",offset);
          ptr = code+offset;
        }
        else ptr+=2;
        break;
      case CodeVmJump        :
//printf("CodeVmJump\n");
        offset = *ptr++; offset <<= 8; offset |= *ptr++;
//printf("    jump to %d\n",offset);
        ptr = code+offset;
        break;
      case CodeVmPushNulls   :
//printf("CodeVmPushNulls\n");
        _this_stack.PushNulls(*ptr++);
        break;
      case CodeVmPush        :
//printf("CodeVmPush\n");
        PUSH(ARGUMENT(*ptr++));
        break;
      case CodeVmPushConstant:
//printf("CodeVmPushConstant\n");
        constant = *ptr++; constant <<= 8; constant |= *ptr++;
        PUSH(aConstants[constant]);
        break;
      case CodeVmInitRegister:
//printf("CodeVmInitRegister\n");
        {
          index = *ptr++;
          constant = *ptr++; constant <<= 8; constant |= *ptr++;
          ARGUMENT(index) = (aConstants[constant]);
        }
        break;
      case CodeVmSetRegister :
//printf("CodeVmSetRegister\n");
        ARGUMENT(*ptr++) = (STACK(STACKTOP()-1));
        break;
      case CodeVmPop         :
//printf("CodeVmPop\n");
        _this_stack.PopTo(_this_stack.GetStackTop() - *ptr++);
        break;
      case CodeVmCall        :
//printf("CodeVmCall\n");
        {
          int nrargs;
          index = *ptr++;
          index <<= 8; index |= *ptr++;
          index <<= 8; index |= *ptr++;
          index <<= 8; index |= *ptr++;
#if (BITS_PER_LONG==64)
          index <<= 8; index |= *ptr++;
          index <<= 8; index |= *ptr++;
          index <<= 8; index |= *ptr++;
          index <<= 8; index |= *ptr++;
#endif

          nrargs   = *ptr++;
          void (*function)(LispEnvironment& aEnvironment,LispInt aStackTop);
          function = (void (*)(LispEnvironment& aEnvironment,LispInt aStackTop))index;
          function(aEnvironment,STACKTOP()-(nrargs+1) );
        }
        break;
      case CodeVmConsList    :
//printf("CodeVmConsList\n");
        {
          LispInt _n = *ptr++;
          LispInt i,stacktop = STACKTOP();
          for (i=0;i<_n;i++)
          {
            STACK(stacktop-2) = (STACK(stacktop-2)->Copy());
            STACK(stacktop-2)->Nixed() = (STACK(stacktop-1));
            POP(1);  stacktop--;
          }
          STACK(stacktop-1) = (LispSubList::New(STACK(stacktop-1)));
        }
        break;
      case CodeVmReturn      :
//printf("CodeVmReturn\n");
        return;
      default:
//printf("ERROR at position %d: %d\n",(int)((ptr-1)-code),(int)(ptr[-1]));
        RaiseError("Invalid op found when executing byte code");
        break;
    }
  }
}
