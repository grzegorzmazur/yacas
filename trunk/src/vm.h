
#ifndef __vm_h__
#define __vm_h__

#define BitsPerLong BITS_PER_LONG
  typedef void (*CallFuncType)(LispEnvironment& aEnvironment,LispInt aStackTop);
  extern void RunFunction(LispEnvironment& aEnvironment,LispInt aStackTop,
            const unsigned char* code, LispObject** aConstants);

  #define CodeVmJumpIfTrue          0x01  // (_label)   //   if (ISTRUE (STACK(STACKTOP()-1))) goto _label
  #define CodeVmJumpIfFalse         0x02  // (_label)  // if (ISFALSE(STACK(STACKTOP()-1))) goto _label
  #define CodeVmJump                0x03  // (_label)         // goto _label
  #define CodeVmPushNulls           0x04 // (_nr)       //     _this_stack.PushNulls(_nr)
  #define CodeVmPush                0x05 // (_index)          //PUSH(_index.Get())
  #define CodeVmPushConstant        0x06 // (_constnt) //PUSH(_constnt)
  #define CodeVmInitRegister        0x07 // (_index,_constant) // (_index).Set(_constant)
  #define CodeVmSetRegister         0x08 // (_index)            // (_index).Set(STACK(STACKTOP()-1).Get())
  #define CodeVmPop                 0x09 // (_i)                        // _this_stack.PopTo(_this_stack.GetStackTop() - _i)
  #define CodeVmCall                0x0A // (fname,nargs)              // fname(aEnvironment,STACKTOP()-(nargs+1) )
  #define CodeVmConsList            0x0B // (_n)
  #define CodeVmReturn              0xFF


#endif // __vm_h__
