


//#include "yacasprivate.h"
#include "numbers.h"
#include <stdio.h>
#include <stdarg.h>
#include <math.h>
//#include <iostream>

int verbose_debug=0; //linkage

#define ENABLE_TESTS 1

// whether to print detailed information about passed tests
bool show_passed = false;

// whether to print detailed information about failed tests
bool show_failed = false;

unsigned failed = 0;
unsigned passed = 0;

#define Check(a,b) CheckL(__LINE__,a,b)
#define CheckValues(a,b,c) CheckValuesL(__LINE__,a,b,c)
#define CheckStringEquals(a,b,c) CheckL(__LINE__,a,b,c)
#define CheckStringValue(a,b,c,d,e) CheckStringValueL(__LINE__,a,b,c,d,e)
#define CheckEquals(a,b,c,d,e) CheckEqualsL(__LINE__,a,b,c,d,e)


char* Message(char* str,...)
{
  static char buf[1024];
  va_list arg;
  va_start (arg, str);
#ifdef HAVE_VSNPRINTF
  vsnprintf (buf, 1020, str, arg);
#else
  /* Just cross fingers and hope the buffer is large enough */
  vsprintf (buf, str, arg);
#endif
  va_end (arg);
  return buf;
}

// compare a given LispString value to a given character string and print diagnostic
void CheckL(int line, LispString& str, const char* s, const char* test_description)
{
  if (strcmp(str.c_str(),s))
  {
    if (show_failed)
      printf("@@@@@@@ (line %d) %s: failed: %s != %s\n",line, test_description, str.c_str(), s);
    ++failed;
  }
  else
  {
    if (show_passed)
      printf("\t%s: passed\n", test_description);
    ++passed;
  }
  fflush(stdout);
}

// check that the condition is true and print diagnostic
void CheckL(int line, bool test_condition, const char* test_description)
{
  if (!test_condition)
  {
    if (show_failed)
      printf("@@@@@@@ (line %d) %s: failed\n",line, test_description);
    ++failed;
  }
  else
  {
    if (show_passed)
      printf("\t%s: passed\n", test_description);
    ++passed;
  }
  fflush(stdout);
}

// check that the condition is true and print diagnostic
void CheckValuesL(int line, double x, double y, const char* test_description)
{
  if (x!=y)
  {
    if (show_failed)
     printf("@@@@@@@ (line %d) %s: failed, %f!=%f\n",line, test_description, x, y);
    ++failed;
  }
  else
  {
    if (show_passed)
      printf("\t%s: passed\n", test_description);
    ++passed;
  }
  fflush(stdout);
}

// check that the given BigNumber gives a correct string value, print diagnostic
void CheckStringValueL(int line,
                      const BigNumber& x, const char* value,
                      LispInt precision,
                      LispInt base, const char* test_description)
{
  LispString str;
  x.ToString(str, precision, base);
  CheckL(line,str, value, test_description);
}

// check that the two numbers are equal by their string representation in given base to given number of base digits
void CheckEqualsL(int line,const BigNumber& x1, const BigNumber& x2, LispInt precision, LispInt base, const char* test_description)
{
  LispString str1, str2;
  x1.ToString(str1, precision, base);
  x2.ToString(str2, precision, base);
  CheckL(line, str1, str2.c_str(), test_description);
}

// print a progress message
void Next(const char* description)
{
  static int i=0;
  i++;
  if (show_failed || show_passed)
  {
    printf("Test group %d: %s\n",i, description);
  }
  fflush(stdout);
}

void Finish()
{
  printf("\nPassed %d, failed %d tests (pass --show-failed to see which tests failed).\n", passed, failed);
  fflush(stdout);
}

#if ENABLE_TESTS
// test small numbers
void TestTypes1(double value)
{
  int sign = (value>0)?1:((value<0)?-1:0);
  int int_value = int(value);
  int int_sign = (int_value>0)?1:((int_value<0)?-1:0);
// test integers
  BigNumber x;
  x.SetTo(int_value);
  Check(x.IsInt(), "set to integer type");
  CheckValues(x.Double(),int_value, "value is correct");
  Check(x.IsIntValue(), "is an integer value");
  CheckValues(x.Sign(),int_sign, "sign is correct");
  x.BecomeInt();
  Check(x.IsInt(), "still integer type");
  x.BecomeFloat();
  Check(!x.IsInt(), "converted to double type");
  CheckValues(x.Double(),int_value, "value is still correct");
  CheckValues(x.Sign(),int_sign, "sign is correct");
  Check(x.IsSmall(), "value is small");
  x.BecomeInt();
  Check(x.IsInt(), "converted to integer type");
  CheckValues(x.Double(),int_value, "value is still correct");
  x.Negate(x);  // negate an integer
  CheckValues(x.Double(),-int_value, "value is negated");
  CheckValues(x.Sign(),-int_sign, "sign is correct");

// test floats
  x.SetTo(value);
  BigNumber y;
  y.SetTo(value);  // test constructor from double
  Check(y.IsSmall(), "value is small");
  Check(y.Equals(x), "y=x");
  Check(x.Equals(y), "x=y");
  CheckValues(x.Double(), y.Double(), "x=y by double value");
  CheckValues(x.Sign(), y.Sign(), "x=y by sign");
  CheckValues(x.BitCount(), y.BitCount(), "x=y by bit count");
  Check(!x.IsInt(), "set to float type");
  double returned_value =x.Double();
  CheckValues(returned_value,value, "value is correct");
// value doesn't have to be float
  //  Check(!x.IsIntValue(), "is not an integer value");
  // x should be unchanged after this:
  x.BecomeFloat(x.GetPrecision());
  Check(!x.IsInt(), "still float type");
  returned_value = x.Double();
  CheckValues(returned_value,value, "value is still correct");
  Check(x.IsSmall(), "value is small");
  CheckValues(x.Sign(),sign, "sign is correct");

  Check(y.Equals(x), "y=x");
  Check(x.Equals(y), "x=y");
  y.SetTo(int_value);  // constructor from integers
  CheckValues(y.Double(),int_value, "value of y is correct");
  Check(y.IsInt() && y.IsIntValue(), "y is integer");

  x.Negate(x);  // negate a float
  returned_value = x.Double();
  CheckValues(returned_value,-value, "value is negated");
  CheckValues(x.Sign(),-sign, "sign is correct");
  x.BecomeInt();
  Check(x.IsInt(), "converted to integer type");
  returned_value = x.Double();
  CheckValues(returned_value,-int_value, "value is correct");
  x.BecomeFloat();
  Check(!x.IsInt(), "convert to float type");
  CheckValues(x.Double(),-int_value, "value is still correct");
  Check(x.IsIntValue(), "is an integer value");
  Check(x.IsSmall(), "value is small");

 
}

// test big numbers: assignment, types, comparison
void TestTypes2(const char* float_string, const char* float_printed, const char* int_string, double double_value)
{
  const LispInt base = 10;
  const LispInt precision = 4*strlen(float_string);
  int sign = (double_value>0)?1:((double_value<0)?-1:0);
  LispString str; // temporary space
 
// test constructors from strings
  BigNumber x(float_string, precision, base);
  BigNumber y;
  y.SetTo(int_string, precision, base);

  Check(!x.IsInt(), "x is a float type as read");
  Check(y.IsInt(), "y is an integer type as read");
  CheckValues(x.Double(),double_value, "double value is correct");
  if (x.Double()!=double_value)
  {
    if (show_failed)
      printf("mismatch: %24e vs %24e\n", x.Double(), double_value);
  }
  x.ToString(str, precision, base);
  CheckStringEquals(str, float_printed, "float value is printed back correctly");
  Check(!x.IsIntValue(), "x has a non-integer value as read");
  Check(y.IsIntValue(), "y has an integer value as read");
  y.ToString(str, precision, base);
  CheckStringEquals(str, int_string, "int value is printed back correctly");
  CheckValues(x.Sign(),sign, "sign of x is correct");
  CheckValues(y.Sign(),sign, "sign of y is correct");

  y.ToString(str,precision, base);
  CheckStringEquals(str,int_string,"read integer value correctly");
  BigNumber z(x);
  Check(!z.IsInt(), "z has float type");
  Check(!z.IsIntValue(), "z has a float value");
  z.BecomeInt();
  CheckValues(z.Sign(),sign, "sign of z is correct");
  z.ToString(str,precision,base);
  CheckStringEquals(str,int_string,"convert to integer value correctly");
  Check(z.IsInt(), "z is an integer now");
  Check(z.IsIntValue(), "z has an integer value now");
  // now x is a float, y is an integer, and z is =Round(x).
  Check(z.Equals(y), "z=y");
  Check(y.Equals(z), "y=z");
  Check(x.Equals(x), "x=x");
  Check(y.Equals(y), "y=y");
  Check(z.Equals(z), "z=z");
  Check(!x.Equals(z), "x!=z");
  Check(!z.Equals(x), "z!=x");
  Check(!x.Equals(y), "x!=y");
 
  z.BecomeFloat(x.GetPrecision());
  Check(!z.IsInt(), "z is not an integer now");
  CheckValues(z.Sign(),sign, "sign of z is correct");
  Check(z.IsIntValue(), "z still has an integer value");
  Check(!x.Equals(z), "x!=z");
  Check(!z.Equals(x), "z!=x");
  if (x.Equals(z))
  {
    if (show_failed)
      printf("problematic value: x=%f, precision %d, %d bits\n", x.Double(), x.GetPrecision(), z.GetPrecision());
  }
 
  BigNumber t;
  t.SetTo(x);
  Check(t.Equals(x), "t=x");
  Check(t.Equals(t), "t=t");
  t.Negate(t);
  CheckValues(t.Sign(),-sign, "sign of t is correct");
  Check(!t.Equals(x), "t!=x now");
  Check(!x.LessThan(x), "x<x is false");
  Check(!t.LessThan(t), "t<t is false");
  Check(double_value > 0 && t.LessThan(x) || !(double_value>0) && !t.LessThan(x), "comparison t <> x is correct");
  Check(double_value < 0 && x.LessThan(t) || !(double_value<0) && !x.LessThan(t), "comparison x <> t is correct");

}

// test some integer and float arithmetic
void TestArith1(const char* str_value, int base, int val1, double val2)
{
  const long prec=long((strlen(str_value)+20)*3.322)+10;  // many guard digits
  const long print_prec = strlen(str_value)+20;  // at least 20 more digits must be correct
  BigNumber x(str_value, prec, base);
  BigNumber x1(x), x2(x), y;
 
  y.SetTo(val1);
// compute x:= -(-x+y)+y
  x.Add(x,y, prec);
  x.Negate(x);
  x.Add(x,y, prec);
  x.Negate(x);
//  Check(x.Equals(x1), "add and subtract an integer");
  if (!x.Equals(x1))
  {
    if (show_failed || show_passed)
    {
      printf("WARNING: this test may fail due to roundoff error, please check:\n");
      printf(   "x:= -(-x+y)+y for x=%s and y=%d\n",str_value,val1);
    }
  }
  CheckEquals(x, x1, print_prec, base, "result of add and subtract an integer");
  x1.SetTo(x);
  x1.Negate(x1);
  x1.Add(x1,x, prec);
  x.SetTo(0.);
  if (x1.IsInt()) x.BecomeInt();
  CheckEquals(x1,x,prec,base, "x-x=0");
 
  x.SetTo(x2);
  x1.SetTo(x2);
  BigNumber z;
  z.SetTo(val2);
  z.Precision(prec);  // consider z to have the same precision as x,x1,x2
  for (int i=0; i<100; ++i)
  {
    x.Add(x,z, prec);
  }
  Check(x.GetPrecision()> prec-3, Message("x has precision %d which must be at least %d\n ---- in calculation with x=%f, z=%f", x.GetPrecision(), prec-3, x.Double(), z.Double()));
  z.Negate(z);
  CheckValues(z.GetPrecision(), prec, "z has the same precision");
  BigNumber t;
  t.SetTo(100);
  Check(t.IsInt(), "t is integer");
  z.Multiply(z,t,prec);
  CheckValues(z.GetPrecision(), prec, "z has the same precision");
  x.Add(x,z, prec);
//  Check(x.Equals(x1), "add and subtract a double 100 times");
  x1.BecomeFloat(prec);
  if (!x.Equals(x1))
  {
    if (show_failed || show_passed)
      printf("WARNING: this test may fail due to roundoff error, please check:\n-------- precision of x is %d, precision of x1 is %d bits\n", x.GetPrecision(), x1.GetPrecision());
  }
  CheckEquals(x, x1, print_prec, base, "result of add and subtract a double 100 times");
 
  x.SetTo(x2);
  x1.SetTo(x2);
  for (int i=0; i<100; ++i) x.Add(x,x, prec);
  y.SetTo(1);
  y.ShiftLeft(y, 100);
  Check(y.IsInt(), "y is integer");
  x1.Multiply(x1,y, prec);
  Check(x.Equals(x1), "add to itself 100 times");
  CheckEquals(x, x1, print_prec, base, "result of add to itself 100 times");
 
}

// both values should be nonzero. We check some basic arithmetic on these double values at normal precision and at high precision
void TestArith2(double a, double b)
{
  BigNumber x;
  const LispInt prec = 200;
  x.SetTo(a);
  BigNumber y;
  y.SetTo(b);
  x.Precision(prec);
  y.Precision(prec);
  // compute 1/a/b*b*a and compare with 1
  BigNumber z, t;
  t.SetTo(1.);
  z.SetTo(1);  // this is integer but no matter
  z.Divide(z,x,prec);
  z.Divide(z,y,prec);
/*
  z.Add(z,t,prec);
  t.Negate(t);
  z.Add(z,t,prec);
  t.Negate(t);
*/
  z.Multiply(z,x,prec);
  z.Multiply(z,y,prec);
//  z.Precision(prec-2);  // round off a little
  CheckEquals(z,t,50,10,"correct arithmetic at double precision");
 
  // compute 2^(-400) * a* b * 2^400 / a/ b and compare with 1
  t.SetTo(1.);  // this must not be integer
  t.ShiftRight(t, 4*prec);
  z.SetTo(x);
  z.Multiply(z,t,prec);
  z.Multiply(z,y,prec);
/*
  z.Add(z,t,prec);
  t.Negate(t);
  z.Add(z,t,prec);
  t.Negate(t);
*/
  z.ShiftLeft(z,4*prec);
  t.ShiftLeft(t,4*prec);
  z.Divide(z,x,prec);
  z.Divide(z,y,prec);
//  z.Precision(prec-2);  // round off a little
  CheckEquals(z,t,50,10,"correct arithmetic at high precision");
}

// test only reading from string and comparing to the correct value
void test_string2float(double value, const char* test_string, LispInt precision, LispInt base)
{
  BigNumber x;
  x.SetTo(value);
  Check(!x.IsInt(), "x has a float type");
  BigNumber y(test_string, precision, base);
  Check(!y.IsInt(), "y has a float type");
//  if (x.Equals(y)) printf("y has precision %d and value %f\n", y.GetPrecision(), y.Double());  // debug
  CheckEquals(x,y,precision,base,"value read from string matches");
}

// test only reading from string and comparing to the correct value
void test_string2string(const char* test_string, LispInt precision=0, LispInt base=10)
{
  BigNumber x(test_string,precision,base); // Set string with low precision
  LispString str;

  //  Check(x.GetPrecision()>3*strlen(test_string), Message("for string %s, precision of x is %d and must be at least %d", test_string, x.GetPrecision(), 3*strlen(test_string)));  // this is actually incorrect but helps debugging
 
  x.ToString(str,200,base); // Request it back with high precision
  Check(!strcmp(test_string,str.c_str()),Message("String rep. %s != %s",test_string,str.c_str()));
}



void test_float2string(double value, const char* test_string, LispInt precision, LispInt base)
{
  BigNumber x;
  x.SetTo(value);
  Check(!x.IsInt(), "x has a float type");
  LispString str;
  x.ToString(str, precision, base);
  CheckStringEquals(str, test_string, "printed string value matches");
}
void test_string2int(LispInt value, const char* test_string, LispInt precision, LispInt base)
{
  BigNumber x;
  x.SetTo(value);
  Check(x.IsInt(), "x has int type");
  BigNumber y(test_string, precision, base);
  Check(y.IsInt(), "y has int type");
 
  CheckEquals(x,y,precision,base,"value read from string matches");
}

void test_int2string(LispInt value, const char* test_string, LispInt precision, LispInt base)
{
  BigNumber x;
  x.SetTo(value);
  Check(x.IsInt(), "x has int type");
  LispString str;
  x.ToString(str, precision, base);
  CheckStringEquals(str, test_string, "printed string value matches");
}

// test printing to string and reading from string
void test_float_eq_string(double value, const char* test_string, LispInt precision, LispInt base)
{
  test_float2string(value, test_string, precision, base);
  test_string2float(value, test_string, precision, base);
}

void test_int_eq_string(LispInt value, const char* test_string, LispInt precision, LispInt base)
{
  test_int2string(value, test_string, precision, base);
  test_string2int(value, test_string, precision, base);
}

// test multiple-precision integer arithmetic
void test_mul_div_1(const char* str1, const char* str2, const char* str3, int base)
{
  // assuming that str3 = str1 * str2, we check this arithmetic
  BigNumber x(str1, 0, base), y(str2, 0, base), z(str3, 0, base), result;
  result.Multiply(x,y,0);
  CheckEquals(result, z, 0, base, "multiplication x*y is correct");
  result.Divide(result, x, 0);
  CheckEquals(result, y, 0, base, "division z/x is correct");
 
  result.Multiply(result,x,0);
  CheckEquals(result, z, 0, base, "multiplication y*x is correct");
  result.Divide(result, y, 0);
  CheckEquals(result, x, 0, base, "division z/y is correct");
 
}

void test_less_than(const char* str1, const char* str2, int base)
{
  // str1 < str2 must be true
  BigNumber x, y;
  x.SetTo(str1, 0, base);
  y.SetTo(str2, 0, base);
  Check(!x.Equals(y), Message("%s (%d bits) != %s (%d bits)", str1, x.GetPrecision(), str2, y.GetPrecision()));
  Check(!y.Equals(x), Message("%s (%d bits) != %s (%d bits)", str2, y.GetPrecision(), str1, x.GetPrecision()));
  Check(x.LessThan(y), Message("%s (%d bits) < %s (%d bits)", str1, x.GetPrecision(), str2, y.GetPrecision()));
  Check(!y.LessThan(x), Message("! %s (%d bits) < %s (%d bits)", str2, y.GetPrecision(), str1, x.GetPrecision()));
}


#endif // whether to test numbers

int main(int argc, char** argv)
{

  {
    int i;
    for (i=1;i<argc;i++)
    {
      if (!strcmp(argv[i],"--show-failed"))
        show_failed = true;
      if (!strcmp(argv[i],"--show-passed"))
        show_passed = true;
    }
  }

    LispString  str;

#if ENABLE_TESTS
#if 1
{

    Next("first examples");
    // Calculate z=x+y where x=10 and y=15
    BigNumber x("10",100,10);
    BigNumber y("15",100,10);
    BigNumber z;
    z.Add(x,y,10);
    // cast the result to a string
    LispString  str;
    z.ToString(str,10);
    CheckStringEquals(str,"25", "adding 10 and 15");

}
{
    BigNumber n1("65535",100,10);
    n1.ToString(str,20,10);
    CheckStringEquals(str,"65535", "reading 65535 from string");
    n1.ToString(str,20,10);
    CheckStringEquals(str,"65535", "reading 65535 from string again");
    n1.ToString(str,30,2);
    CheckStringEquals(str,"1111111111111111", "printing in binary");
    n1.Negate(n1);
    n1.ToString(str,20,10);
    CheckStringEquals(str,"-65535", "negate 65535");

    BigNumber res1;
    res1.Add(n1,n1,10);
    res1.ToString(str,20, 10);
    CheckStringEquals(str,"-131070", "add -65535 to itself");
}
#endif
{
//////////////////////////////////////////////////
///// BigNumber comprehensive test suite
//////////////////////////////////////////////////
  printf("SizeOf(BigNumber)=%ld\n", (long)sizeof(BigNumber));

  Next("constructor");
  BigNumber x;  // default constructor
  Next("call some functions on objects with undefined values");
  x.Sign();
  x.IsInt();
  Next("1");
  x.Double();  // value is undefined
  Next("2");
  x.Negate(x);
  Next("3");
  x.Multiply(x,x,100);
  Next("4");
  Next("4.a");
  try
  {
    BigNumber y;
    y.Divide(y,y,100);  // This works
  }
  catch (LispInt err)
  {
    if (show_passed)
      printf("Correctly caught division by zero exception.\n");
  }
  Next("4.b");
  try
  {
    x.Divide(x,x,100);  // possibly zero division requested since x is undefined
  }
  catch (LispInt err)
  {
    if (show_passed)
      printf("Correctly caught division by zero exception.\n");
  }
  Next("5");
  x.Add(x,x,100);
  Next("6");
  x.MultiplyAdd(x,x,100);
  Next("7");
  x.IsIntValue();
  Next("8");
  x.IsSmall();
  Next("9");
  x.ShiftLeft(x, 2);
  Next("10");
  x.BecomeFloat();
  Next("11");
  //RaiseError("testing error messages");
  x.Precision(100);
  CheckValues(x.GetPrecision(),100, "set precision successfully");
  x.BecomeInt();
  Next("12");
  x.BitOr(x,x);
  Next("13");
  x.SetTo(double(x.BitCount()));
 
  Next("construct 0 from string");
  BigNumber y("0", 10);  // construct
  CheckValues(y.Double(),0, "value of 0 is correct");
  Check(y.IsInt(), "0 is of integer type");
  Check(y.IsIntValue(), "value of 0 is integer");

  y.SetTo("0.", 10);  // construct
  CheckValues(y.Double(),0, "value of 0. is correct");
  Check(!y.IsInt(), "0. is of float type");
  Check(y.IsIntValue(), "value of 0. is integer");

  Next("read binary string");
  y.SetTo("-101010", 50, 2);  // construct with given precision
  CheckValues(y.Double(),-42, "value is correct");
  Check(y.IsInt(), "has int type");
  Check(y.IsIntValue(), "value is integer");
  Check(y.IsSmall(), "value is small");
  y.BecomeInt();
  Check(y.IsInt(), "has integer type");
  Check(y.IsSmall(), "value is small");
 
  Next("testing big integers");
  x.SetTo("010203040506070809101112131415161718192021222324252627282930", 0, 10);  // the precision argument should be ignored when reading integers
  Check(x.IsInt(), "has integer type");
  CheckValues(x.Sign(),1, "has positive value");
  Check(x.IsIntValue(), "has integer value");
  Check(!x.IsSmall(), "big integer is not small");
  x.Multiply(x,x,0);
  Check(x.IsInt(), "has integer type");
  Check(x.IsIntValue(), "has integer value");
  Check(!x.IsSmall(), "big integer is not small");
  y.SetTo(2);
  y.Mod(x,y);
  CheckValues(y.Double(),0, "value is even");
  x.BecomeFloat();
  Check(!x.IsInt(), "has float type");
  Check(x.IsIntValue(), "still has integer value type");
 

  Next("test small integers and floats");
  TestTypes1(0);
  TestTypes1(1);
  TestTypes1(-1);
  TestTypes1(2);
  TestTypes1(-2);
  TestTypes1(1.11111111);
  TestTypes1(-0.0000044);
  TestTypes1(-1.99999e6);
  TestTypes1(1.88800001e-20);
  TestTypes1(-100000.1);
  TestTypes1(15416.563);

  Next("test big integers and floats 1");
  // values must be out of range for platform numbers; float values must be non-integer. Usage:
  // TestTypes2("float value", "printed float value", "equivalent integer value", double_value, base);
  const char* num1_float = "3.00000000000000000000000000000000000000099999999999992";
  const char* num1_printed = num1_float;
  const char* num1_int = "3";
  const double num1_double = 3;
  const char* num2_float =     "-100000000000000.233333333331";
  const char* num2_printed = "-0.100000000000000233333333331e15";
  const char* num2_int = "-100000000000000";
  const double num2_double = -1.0000000000000023333333333e14;
  const char* num3_float =     "10000000000000000.233333333331";
  const char* num3_printed = "0.10000000000000000233333333331e17";
  const char* num3_int = "10000000000000000";
  const double num3_double = 1.e16;
  const char* num4_float = "123.33233233233233233232333333111111111111199999999999993";
  const char* num4_printed = num4_float;
  const char* num4_int = "123";
  const double num4_double = 123.332332332332332332323333333333;

  TestTypes2(num1_float, num1_printed, num1_int, num1_double);
  Next("test big integers and floats 2");
  TestTypes2(num2_float, num2_printed, num2_int, num2_double);
  Next("test big integers and floats 3");
  TestTypes2(num3_float, num3_printed, num3_int, num3_double);
  Next("test big integers and floats 4");
  TestTypes2(num4_float, num4_printed, num4_int, num4_double);
 
  Next("test copy constructor");
  y.SetTo(-15416.0);
  Check(!y.IsInt(), "y is of float type");
  Check(y.IsIntValue(), "y has integer value");
  BigNumber z(y);  // copy constructor
  Check(!z.IsInt(), "z is of float type");
  CheckValues(z.Double(),-15416, "value is still -15416");
  Check(z.IsSmall(), "value is small");
 
  Next("test integer conversion");
  z.BecomeInt();
  x.SetTo(z);  // copy assignment
  Check(x.IsInt(), "is of integer type");
  CheckValues(x.Double(),-15416, "value is still -15416");
  Check(x.IsSmall(), "value is small");

  Next("equality");

  Check(x.Equals(x), "x=x");
  Check(y.Equals(y), "y=y");
  Check(x.Equals(y), "x=y");
  Check(y.Equals(x), "y=x");
  Check(z.Equals(x), "z=x");
  Check(z.Equals(y), "z=y");
  Check(x.Equals(z), "x=z");
  x.Negate(x);  // negate an integer
  Check(x.IsInt(), "x still of integer type");
  CheckValues(x.Double(),15416, "value is now 15416");
  Check(!z.Equals(x), "z!=-x");
  Check(!y.Equals(x), "y!=-x");
 
  Next("comparisons");
  Check(!x.LessThan(x), "x<x is false");
  Check(!x.LessThan(y), "y<y is false");
  Check(y.LessThan(x), "y<x is true");
  Check(!z.LessThan(y), "z<y is false");
  Check(z.LessThan(x), "z<x is true");
 
  Next("simple arithmetic 1");
  TestArith1(num1_int, 10, 15, -25);
  TestArith1(num1_float, 10, -15, 0.19e-8);
  Next("simple arithmetic 2");
  TestArith1(num2_int, 10, -15, -2500000);
  TestArith1(num2_float, 10, -15, 0.19e8);
  Next("simple arithmetic 3");
  TestArith1(num3_int, 10, 1500, 29);
  TestArith1(num3_float, 10,  -15, 0.19);
  Next("simple arithmetic 4");
  TestArith1(num4_int, 10, 1500, 29);
  TestArith1(num4_float, 10,  -50000, 99999.99999);
 
  Next("try to leak memory");
  for (int i=0; i<=10000; ++i) x.SetTo(y);
  Check(x.Equals(y), "x=y after 10000 times");

  Next("use big numbers in a loop");
  y.SetTo(100);
  z.SetTo(1);
  BigNumber t;
  t.SetTo(0);
  for (x.SetTo(0); x.LessThan(y); x.Add(x,z,10))
  {
    t.Add(t,x,10);
  }
  CheckValues(t.Double(),4950, "correct sum of n from 0 to 99");
 
  Next("bit counts");
  x.SetTo(65537);
  x.SetTo(double(x.BitCount()));
  CheckValues(x.Double(),17, "bit count of 65537 is 17");
  Check(x.Double()!=16, "bit count of 65537 is not 16");

  x.SetTo(1./1050000.);
  Check(!x.IsInt(), "x is a floating-point value");
  CheckValues(x.Sign(),1, "x is positive");
  x.SetTo(double(x.BitCount()));
  Check(x.IsIntValue(), "bit count has integer value");
  CheckValues(x.Double(),-20, "bit count of 1./1050000. is (-20)");
  Check(x.Double()!=-21, "bit count is not -21");
  Check(x.Double()!=-19, "bit count is not -19");
  x.SetTo(1./1048576.);
  x.SetTo(double(x.BitCount()));
  CheckValues(x.Double(),-19, "bit count of 1./1048576. is (-19)");
  Check(x.Double()!=-20, "bit count is not -20");
  Check(x.Double()!=-18, "bit count is not -18");

  x.SetTo(0.);
  Check(!x.IsInt(), "x has float type");
  CheckValues(x.BitCount(), 0, "bit count of 0. is 0");
  x.BecomeInt();
  CheckValues(x.BitCount(), 1, "bit count of 0 is 1");
 
 
  long B_x;
#define test_bit_count(a) \
  x.SetTo(a); \
  B_x = x.BitCount(); \
  x.BecomeFloat(); \
  CheckValues(x.BitCount(), B_x, "bit count is the same after BecomeFloat()");

  test_bit_count(2)
  test_bit_count(-1)
  test_bit_count(1)
  test_bit_count(12345)
  test_bit_count(255)

 
 
  Next("arithmetic with large powers");
  // compute 15^(large number) and check that it divides both 3^(that number) and 5^(that number)
  x.SetTo(15);
  for(int i=0; i<10; ++i) x.Multiply(x,x,10);
  Check(x.IsInt(), "15^N is integer");
  y.SetTo(3);


  for(int i=0; i<10; ++i) y.Multiply(y,y,10);

  z.Mod(x,y);
  Check(y.LessThan(x), "3^N<15^N");
  CheckValues(z.Sign(),0, "15^N divides 3^N");
  z.SetTo(5);
  for(int i=0; i<10; ++i) z.Multiply(z,z,10);
  t.Mod(x,z);
  CheckValues(t.Sign(),0, "15^N divides 5^N");
  y.Multiply(z,y,10);
  Check(x.Equals(y), "15^N = 3^N*5^N");
 
  Next("string input/output");
  // test_float_eq_string tests that the string representation and the numerical representation are equivalent;
  // test_int_eq_string does this for integers.
  // test_float2string, test_string2int etc. are for testing the conversions only in one direction (used  e.g.to test capital E exponents and other non-canonical representations).
  // For example: to test that "10" is read as integer 10 and printed back as "10", use test_int_and_strnig(10, "10", precision, base). Here precision is the number of "base" digits that we need to represent the number. If fewer digits are given, then fewer digits will be printed.
  // To test that "0.0001E4" is read as floating 1., use test_string2float(1., "0.0001E4, precision, base). ("1." will of course not be printed like that.)
  // To test that "0.14e-100" is read as equal to floating 140e-103 and that floating 140e-103 is printed back as 0.14e-100, use test_float_eq_string(140e-103, "0.14e-100", precision, base)
 
  test_int_eq_string(0, "0", 10, 10);
  test_float_eq_string(0., "0.", 10, 10);
  test_string2float(0., "-0.", 10, 10);
  test_int_eq_string(1, "1", 10, 10);
  test_float_eq_string(1., "1.", 10, 10);
  test_int_eq_string(-1, "-1", 10, 10);
  test_float_eq_string(-1., "-1.", 10, 2);
  test_int_eq_string(-1, "-1", 10, 3);
  test_int_eq_string(-13, "-111", 10, 3);
  test_float_eq_string(-13., "-111.", 10, 3);
  test_int_eq_string(-13, "-1101", 10, 2);
  test_int_eq_string(13, "13", 10, 10);
  test_int_eq_string(1000000000, "1000000000", 10, 10);
  test_float_eq_string(1000000000., "0.1e10", 10, 10);
  test_int_eq_string(-13, "-d", 10, 16);
  test_float_eq_string(-13.0, "-d.", 10, 16);
  test_float_eq_string(13.1245e-15, "0.131245e-13", 10, 10);
  test_float2string(10.1245e-15, "0.101e-13", 3, 10);  // print fewer digits than we have
  test_float2string(13.1e-15, "0.131e-13", 10, 10);  // do not print more digits than we have
  test_string2float(13.1245e-15, "0.131245e-13", 3, 10);  // do not read fewer digits
  test_float_eq_string(13.1245e-15, "0.131245e-13", 6, 10);
  test_float2string(-10.1245e-15, "-0.101e-13", 3, 10);  // print fewer digits than we have
  test_float2string(-13.1e-15, "-0.131e-13", 10, 10);  // do not print more digits than we have
  test_string2float(-13.1245e-15, "-0.131245e-13", 3, 10);  // do not read fewer digits

  // test reading of strings in different variations
// lowercase e
  test_string2float(0.13e17, "13.0e15", 10, 10);
  test_string2float(0.13e17, "13.e15", 10, 10);
  test_string2float(0.13e17, "13e15", 10, 10);
  test_string2float(-0.13e17, "-13.0e15", 10, 10);
  test_string2float(-0.13e17, "-13.e15", 10, 10);
  test_string2float(-0.13e17, "-13e15", 10, 10);
  test_string2float(0.13e-13, "13.0e-15", 10, 10);
  test_string2float(0.13e-13, "13.e-15", 10, 10);
  test_string2float(0.13e-13, "13e-15", 10, 10);
  test_string2float(-0.13e-13, "-13.0e-15", 10, 10);
  test_string2float(-0.13e-13, "-13.e-15", 10, 10);
  test_string2float(-0.13e-13, "-13e-15", 10, 10);

// uppercase e
  test_string2float(0.13e17, "13.0E15", 10, 10);
  test_string2float(0.13e17, "13.E15", 10, 10);
  test_string2float(0.13e17, "13E15", 10, 10);
  test_string2float(-0.13e17, "-13.0E15", 10, 10);
  test_string2float(-0.13e17, "-13.E15", 10, 10);
  test_string2float(-0.13e17, "-13E15", 10, 10);
  test_string2float(0.13e-13, "13.0E-15", 10, 10);
  test_string2float(0.13e-13, "13.E-15", 10, 10);
  test_string2float(0.13e-13, "13E-15", 10, 10);
  test_string2float(-0.13e-13, "-13.0E-15", 10, 10);
  test_string2float(-0.13e-13, "-13.E-15", 10, 10);
  test_string2float(-0.13e-13, "-13E-15", 10, 10);

  test_float_eq_string(0.0011, "0.0011", 10, 10);
  test_float_eq_string(0.01, "0.01", 10, 10);
  test_float_eq_string(0.001, "0.001", 10, 10);
  test_float_eq_string(0.1, "0.1", 10, 10);
  test_float_eq_string(-0.001, "-0.001", 10, 10);
  test_int_eq_string(1234, "1234", 10, 10);
  test_int_eq_string(12345, "12345", 10, 10);
  test_int_eq_string(123456, "123456", 10, 10);
 
  Next("determine types from string");
  x.SetTo("1234",0,10);
  Check(x.IsInt(), "1234 is integer");
  Check(x.IsIntValue(), "1234 has int value");
  x.SetTo("1234.5",50,10);
  Check(!x.IsInt(), "1234.5 is float type");
  Check(!x.IsIntValue(), "1234.5 has float value");
  x.SetTo("1234.5e2",50,10);
  Check(!x.IsInt(), "1234.5e2 is float type");
  Check(x.IsIntValue(), "1234.5e2 has int value");

  Next("bit operations");
  // precision is ignored when reading integers
  x.SetTo("4020402040204020402040204020402040204020", 0, 16);
  y.SetTo("6123612361236123612361236123612361236123", 0, 16);
  Check(x.IsInt() && y.IsInt(), "x and y have integer values");
  z.BitXor(x,y);
  t.SetTo("2103210321032103210321032103210321032103", 0, 16);
  Check(z.Equals(t), "bit Xor operation correct");
  z.BitAnd(x,y);
  t.SetTo("4020402040204020402040204020402040204020", 0, 16);
  Check(z.Equals(t), "bit And operation correct");
  z.BitOr(x,y);
  t.SetTo("6123612361236123612361236123612361236123", 0, 16);
  Check(z.Equals(t), "bit Or operation correct");
  z.ShiftRight(x,4);
  t.SetTo("402040204020402040204020402040204020402", 0, 16);
  Check(z.Equals(t), "ShiftRight operation correct");
  z.ShiftLeft(y,4);
  t.SetTo("61236123612361236123612361236123612361230", 0, 16);
  Check(z.Equals(t), "ShiftLeft operation correct");
  t.SetTo(4);
  z.ShiftRight(x,4);
  t.SetTo("402040204020402040204020402040204020402", 0, 16);
  Check(z.Equals(t), "ShiftRight operation correct");
  t.SetTo(4);
  z.ShiftLeft(y,4);
  t.SetTo("61236123612361236123612361236123612361230", 0, 16);
  Check(z.Equals(t), "ShiftLeft operation correct");
  z.BitNot(z);
  t.SetTo("9EDC9EDC9EDC9EDC9EDC9EDC9EDC9EDC9EDC9EDCF", 0, 16);
  z.BitAnd(z,t);  // result of BitNot is negative
  Check(z.Equals(t), "bit Not operation correct");
  CheckEquals(z, t, 50, 16, "bit Not operation result printed correctly");
 
  // some old tests from Ayal

    z.SetTo("5a5a5a5a5a5a5a5a5a5a5a5a", 0, 16);
    z.ShiftRight(z,1);
  t.SetTo("2d2d2d2d2d2d2d2d2d2d2d2d", 0, 16);
    CheckEquals(z, t, 0, 16, "shift right is correct");
 
    z.ShiftRight(z,19);
  t.SetTo("5a5a5a5a5a5a5a5a5a5", 0, 16);
    CheckEquals(z, t, 0, 16, "another shift right is correct");

    z.ShiftLeft(z,1);
    t.SetTo("b4b4b4b4b4b4b4b4b4a", 0, 16);
    CheckEquals(z, t, 0, 16, "shift left is correct");
 
    z.ShiftLeft(z,19);
    t.SetTo("5a5a5a5a5a5a5a5a5a500000", 0, 16);
    CheckEquals(z, t, 0, 16, "another shift left is correct");


  Next("adding zero to numbers");  // add integer zero and also floating zero
  x.SetTo(0);
  y.SetTo(100.123);
  x.Add(x,y,40);
  Check(x.Equals(y) && y.Equals(x), "x=y and y=x");
 
  long prec1 = 20;
  x.SetTo(0.);
  x.Precision(prec1);
  x.Add(x,y, prec1*2);
  Check(x.Equals(y), "x = x + 0.");
  // the precision of x should be at most prec1+10, this is really implementation-dependent (GUARD_BITS and so on)
  Check(x.GetPrecision() < prec1+10, "x has correct precision");
  if (x.GetPrecision() >= prec1 + 10)
  {
    if (show_failed)
      printf ("x has precision %d\n", x.GetPrecision());
  }
  Next("simple precision test with 100/243");  // compute 100/243 to 9 bits, should be 0.411
  prec1 = 10;
  x.SetTo(100./243.);
  x.Precision(prec1);
  y.SetTo(1000.);
  y.Precision(prec1);
  Check(y.GetPrecision()==prec1, "y has precision 10");
  Check(!(x.IsInt() || y.IsInt()), "x and y are of float type");
  x.Multiply(x,y, prec1);  // now x should be 411.522... but only with 10 float bits of correct mantissa
  Check(!(x.IsInt()), "x is of float type");
  y.SetTo(411);
  Check(y.IsInt() && y.Double() == 411., "y has integer type and equal to 411");
  Check(x.Equals(y), "x is equal to integer 411");
  y.BecomeFloat(2*prec1);
  Check((!y.IsInt()) && y.Double() == 411., "y has float type and equal to 411");
  Check(x.Equals(y), "x is equal to float 411.");
  y.SetTo(412.);
  Check((!y.IsInt()) && y.Double() == 412., "y has float type and equal to 412");
  Check(x.Equals(y), "x is also equal to float 412.");
  CheckStringValue(x, "410.", 2*prec1, 10, "imprecise 100./243. is printed correctly to 9 bits");
  // since both initial numbers had 10 bits of mantissa, the product should have 9 bits
  CheckValues(x.GetPrecision(), prec1-1, "x has precision 9");
  CheckValues(x.BitCount(), 9,  "float x has correct BitCount");
 
  // promote to 10 bits
  x.Precision(prec1);
  CheckStringValue(x, "412.", 2*prec1, 10, "imprecise 100./243. is printed correctly to 10 bits");
 
 
  y.SetTo(1234.5678);
  y.Precision(10);
  Check(y.IsIntValue(), "y has integer value due to low precision");
  CheckStringValue(y, "1230.", 100, 10, "value of y is printed correctly");
  x.SetTo(1024);
  x.ShiftRight(x, 10);
  CheckStringValue(x, "1", 10, 10, "correct ShiftRight on integer 1024");
  x.ShiftRight(x, 1);
  CheckStringValue(x, "0", 10, 10, "correct ShiftRight on integer 1");
  x.ShiftRight(x, 1);
  CheckStringValue(x, "0", 10, 10, "correct ShiftRight on integer 0");
  x.SetTo(-1024);
  x.ShiftLeft(x, 2);
  CheckStringValue(x, "-4096", 10, 10, "correct ShiftRight on integer -1024");
  x.ShiftRight(x, 12);
  CheckStringValue(x, "-1", 10, 10, "correct ShiftRight on integer -4096");
  x.ShiftRight(x, 1);
  CheckStringValue(x, "0", 10, 10, "correct ShiftRight on integer -1");
  x.SetTo(-0.25);
  x.ShiftRight(x,2);
  Check(!x.IsInt(), "x has float type");
  Check(!x.IsIntValue(), "x has float value");
  CheckStringValue(x, "-0.0625", 10, 10, "correct ShiftRight on float -0.25");
  x.ShiftLeft(x, 5);
  Check(!x.IsInt(), "x still has float type");
  Check(x.IsIntValue(), "x has integer value");
  CheckStringValue(x, "-2.", 10, 10, "correct ShiftRight on float -0.0625");

  Next("Floor()");
  x.BecomeFloat();
  Check(!x.IsInt(), "x has float type");
  x.Precision(100);
  x.SetTo(1.7);
  y.Floor(x);
  Check(y.IsInt(), "Floor() returns int value");
  CheckStringValue(y, "1", 10, 10, "Floor(1.7) is correct");
  x.SetTo(-1.7);
  y.Floor(x);
  CheckStringValue(y, "-2", 10, 10, "Floor(-1.7) is correct");
  x.SetTo(-17.0);
  y.Floor(x);
  CheckStringValue(y, "-17", 10, 10, "Floor(-17.0) is correct");
  x.SetTo(1.0);
  y.Floor(x);
  CheckStringValue(y, "1", 10, 10, "Floor(1.0) is correct");
  x.SetTo(10);
  y.Floor(x);
  CheckStringValue(y, "10", 10, 10, "Floor(10) is correct");
 
  Next("modular arithmetic");
  x.SetTo(123);
  y.SetTo(9);
  y.Mod(x,y);
  CheckStringValue(y, "6", 10, 10, "Mod(123,9) is correct");
  x.SetTo(-123);
  y.SetTo(9);
  y.Mod(x,y);
  CheckStringValue(y, "3", 10, 10, "Mod(-123,9) is correct");
 
  Next("comparison");
  x.SetTo(123);
  y.SetTo(124);
  Check(x.LessThan(y), "123<124 is true");
  y.SetTo(123);
  Check(!x.LessThan(y), "123<123 is false");
  Check(!y.LessThan(x), "123<123 is false");
  y.SetTo(122);
  Check(!x.LessThan(y), "123<122 is false");
  Check(y.LessThan(x), "122<123 is true");
  y.SetTo(-1000.43);
  Check(!x.LessThan(y), "123<-1000.43 is false");
  Check(y.LessThan(x), "123>-1000.43 is true");
  y.SetTo(123.);
  Check(!x.LessThan(y), "123.0<123 is false");
  Check(!y.LessThan(x), "123.0<123 is false");
 
  Next("integer division");
  x.SetTo(15);
  y.SetTo(4);
  z.Divide(x,y,10);
  CheckStringValue(z, "3", 10, 10, "Div(15,4)==3");
  x.SetTo(-15);
  y.SetTo(4);
  z.Divide(x,y,10);
  CheckStringValue(z, "-3", 10, 10, "Div(-15,4)==-3");
 
  Next("floating-point division");
  x.SetTo(15.);
  y.SetTo(4.);

{
  LispString str;
  x.ToString(str,100,10);
  if (show_failed || show_passed)
    printf("x = %s (should be 15.)\n",str.c_str());
  y.ToString(str,100,10);
  if (show_failed || show_passed)
    printf("y = %s (should be 4.)\n",str.c_str());
}

  z.Divide(x,y,10);
  CheckStringValue(z, "3.75", 10, 10, "15/4==3.75");
  x.SetTo(1.);
  x.Precision(300);
  y.SetTo(7);
  x.Divide(x,y,300);
  CheckValues(x.GetPrecision(), 300, "x has precision 300");
  CheckStringValue(x,
  "0.142857142857142857142857142857142857142857"
    "142857142857142857142857142857142857142857142857",
  1000, 10, "high-precision division");

  Next("division by zero");
  x.SetTo(0);
  Check(x.IsInt(), "x=0 as integer");
  try
  {
  x.Divide(y,y,100);
  }
  catch (LispInt err)
  {
    if (show_passed)
      printf("Correctly caught integer division by zero.\n");
  }
  y.SetTo(0.);
  try
  {
  y.Divide(y,y,100);
  }
  catch (LispInt err)
  {
    if (show_passed)
      printf("Correctly caught floating division by zero.\n");
  }
    Next("conversions with large strings");
  x.SetTo("3.000000000000000000000000000000000000000050104", 0, 10);
  y.SetTo("3.000000000000000000000000000000000000000050204", 150, 10);
  Check(!x.Equals(y), "read a large number of digits from string");
  {
    LispString str;
    x.ToString(str, 50, 10);
    CheckStringEquals(str, "3.000000000000000000000000000000000000000050104","printed back a large number of digits from string");
  }
  y.SetTo(-3.);
  y.Precision(200);
  x.Add(x, y, 200);
  Check(x.Sign()>0, "read big float value correctly");
 
  {
    BigNumber a("3.000000000000000000000000000000000000000050104", 150, 10);
    LispString str;
    a.ToString(str, 50, 10);
    CheckStringEquals(str, "3.000000000000000000000000000000000000000050104","constructor from string");
 
  }
  {
    BigNumber a("3.000000000000000000000000000000000000000050104", 0, 10);
    BigNumber b(a);
    LispString str;
    b.ToString(str, 50, 10);
    CheckStringEquals(str, "3.000000000000000000000000000000000000000050104","copy constructor");
 
  }
  {
    BigNumber a;
    a.SetTo("3.000000000000000000000000000000000000000050104", 150, 10);
    LispString str;
    a.ToString(str, 50, 10);
    CheckStringEquals(str, "3.000000000000000000000000000000000000000050104","assignment from string using a fresh number");
 
  }
  {
    BigNumber a;
    a.SetTo("3.000000000000000000000000000000000000000050104", 150, 10);
    BigNumber b;
    b.SetTo(a);
    LispString str;
    b.ToString(str, 50, 10);
    CheckStringEquals(str, "3.000000000000000000000000000000000000000050104","assignment from string and a copy operation");
 
  }
 
  Next("multiply-add");
  x.SetTo(10);
  y.SetTo(2);
  z.SetTo(5.5);
  x.MultiplyAdd(y,z,10);
  Check(!x.IsInt(), "x is now float");
  Check(x.IsIntValue(), "x has integer value");
  CheckValues(x.Double(),21, "x==21");
  CheckStringValue(x, "21.", 10, 10, "10+2*5.5 = 21.");

  x.SetTo(10);
  y.SetTo(2);
  z.SetTo(5);
  x.MultiplyAdd(y,z,10);
  Check(x.IsInt(), "x is int");
  CheckValues(x.Double(),20, "x==20");
  CheckStringValue(x, "20", 10, 10, "10+2*5 = 20");
 
  Next("arithmetic 2");
  TestArith2(1, 2);
  TestArith2(-1, 2);
  TestArith2(-1, -2);
  TestArith2(10,10);
  TestArith2(0.4, 142);
  TestArith2(400.1881, -4);
  TestArith2(-1., 1000000);
  TestArith2(253,1./253.);
  TestArith2(1.0e-15, -2.0e-15);
 
  Next("rough precision control");
  {// compute ((1+2^(-150+s)) - 1) * 2^(150-s) with 150 bits and compare with 1
      BigNumber x;
      LispInt prec = 150,  // at most 180
        shift_amount = 8;  // currently at least 2
      x.SetTo(1.);
      x.Precision(prec);
      CheckStringValue(x, "1.", 10, 10, "x=1.");
      CheckValues(x.GetPrecision(),prec, "correct precision is set on x");
      BigNumber y;
      y.SetTo(1.);
      y.Precision(prec);
      y.ShiftRight(y,prec-shift_amount); // y = 2 ^ (-150+s)
      x.Add(x,y,prec);  // now x should be slightly different from 1
      y.SetTo(-1);
      x.Add(x,y,prec);  // now x should be positive, equal to 2^(-149)
      CheckValues(x.Sign(),1, "x is positive");
      x.ShiftLeft(x,prec-shift_amount); // x = 2^(150-s)* x
      CheckStringValue(x, "1.", 10, 10, "x=1. again");
      CheckValues(x.GetPrecision(), shift_amount, "x has correct precision");
  // compute  (1+10^(-60))-1, this should be 0 with 150 bits
      y.SetTo(1.e-60);  // at least 60
      y.Precision(prec);
      x.SetTo(1.);
      x.Precision(prec);
      x.Add(x,y,prec);  // x = 1+10^(-60) must be equal to 1 due to roundoff
      y.SetTo(-1);
      x.Add(x,y,prec);  // now should be 0
      CheckStringValue(x, "0.", 10, 10, "x=0 now due to roundoff");
  }

/*Heuh... this didn't work out as I expected... */
   Next("Precision of string representation higher than requested precision");

   test_string2string("6.23");

   test_string2string("1");
   test_string2string("1000000000000000000000");
   test_string2string("10000000.");
   test_string2string("0.1");
   test_string2string("-0.1");

   test_string2string("1.1");
   test_string2string("-1.1");

   test_string2string("0.01");
   test_string2string("-0.01");

   test_string2string("1.01");
   test_string2string("-1.01");

   test_string2string("2.0000000000000000000001");
   test_string2string("-2.0000000000000000000001");

   test_string2string("1.0000000000000000000001");
   test_string2string("-1.0000000000000000000001");

   test_string2string("0.10000000000000000000001e11");
   test_string2string("-0.10000000000000000000001e11");

   test_string2string("0.10000000000000000000001e-11");
   test_string2string("-0.10000000000000000000001e-11");

  Next("read strings with many extra zeros correctly");
  test_string2float(0., "000.0000", 0, 10);
  test_string2float(0., "000.", 0, 10);
  test_string2float(0., ".000", 0, 10);
  test_string2float(0.002, "000.002", 0, 10);
  test_string2float(0.003, ".003", 0, 10);
  test_string2float(40., "40.00", 0, 10);
  test_string2float(0.005, ".005000", 0, 10);
  test_string2float(60., "000000060.00", 0, 10);
  test_string2float(70., "000000070.", 0, 10);
  test_string2float(80., "0000000.8000e2", 0, 10);
  test_string2float(90., "9000e-2", 0, 10);
  test_string2float(1000., "1000.000", 0, 10);
  test_string2float(1.3e15, "0001.3000e15", 0, 10);
  test_string2float(1.3e15, "0001.3e15", 0, 10);
  test_string2float(1.3e15, "00013e14", 0, 10);
 
  { // set aPrecision to 0 and see what happens
    BigNumber x0("0.10000", 0, 10);
    BigNumber x("0.1", 10, 10);
    LispString str;
    x.ToString(str, 0, 10);
    CheckStringEquals(str, "0.1", "printed 0.1 correctly");
    CheckEquals(x0, x, 0, 10, "value is consistent");
 
    x.SetTo("0.1", 0, 10);
    x.ToString(str, 0, 10);
    CheckStringEquals(str, "0.1", "printed 0.1 correctly");
    CheckEquals(x0, x, 0, 10, "value is consistent");
 
  }
 

  prec1 = 34;  // this is really implementation-dependent, but the following numbers should be related to prec1 because of precision tracking

    Next("a calculation from arithmetic.yts");
  {// compute 10e3*1.2e-4 - 1.2, must get zero
    BigNumber x("10e3", 10), y("1.2e-4", 10), z("1.2", 10);
    z.Negate(z);
    CheckValues(z.GetPrecision(), prec1, "z has correct precision");
    //z.MultiplyAdd(x,y,10);
    x.Multiply(x,y,prec1);
    CheckValues(x.GetPrecision(), prec1-1, "x has correct precision");
    z.Add(z,x,prec1);
    CheckValues(z.GetPrecision(), prec1-2, "z has correct precision");
    CheckValues(z.Sign(),0,"z has correct sign");
    CheckStringValue(z, "0.", 10, 10, "z is equal to 0.");
  }
  {
    BigNumber z("0.000", 10, 10);
    CheckValues(z.GetPrecision(), prec1, "z has correct precision");
    BigNumber y("0.5", 10, 10);
    CheckValues(y.GetPrecision(), prec1, "y has correct precision");
    z.Add(z,y,10);
 
    CheckValues(z.GetPrecision(), 10, "z has correct precision");
    CheckValues(z.Sign(),1,"z has correct sign");
    CheckStringValue(z, "0.5", 10, 10, "z is equal to 0.");

  }

  Next("the same calculation imitating the Yacas kernel");
  {
    BigNumber x("10e3", 10), y("1.2e-4", 10), z0("1.2", 10);
    BigNumber t1(10);
    t1.Negate(z0);
    CheckValues(t1.GetPrecision(), prec1, "t1 has correct precision");
    BigNumber t2(10);
    t2.Multiply(x,y,prec1);
    CheckValues(t2.GetPrecision(), prec1-1, "t2 has correct precision");
    BigNumber z(10);
    z.Add(t2,t1, prec1);
    CheckValues(z.GetPrecision(), prec1-2, "z has correct precision");
    CheckValues(z.Sign(),0,"z has correct sign");
    CheckStringValue(z, "0.", 10, 10, "z is equal to 0.");
  }
  {
    BigNumber x("0.000", 10, 10);
    CheckValues(x.GetPrecision(), prec1, "x has correct precision");
    BigNumber y("0.5", 10, 10);
    CheckValues(y.GetPrecision(), prec1, "y has correct precision");
    BigNumber z(100);
    z.Add(x,y,10);
 
    CheckValues(z.GetPrecision(), 10, "z has correct precision");
    CheckValues(z.Sign(),1,"z has correct sign");
    CheckStringValue(z, "0.5", 10, 10, "z is equal to 0.");

  }
 
  Next("precision control for Add(), check optimization");
  {
    BigNumber x("0.10000011", 10, 10);
    BigNumber y("0.1", 10, 10);  // not necessary to give trailing zeros since we are requesting 10 base digits of precision
    CheckValues(x.GetPrecision(), prec1, Message("x=%10.10f has correct initial precision", x.Double()));
    y.Negate(y);
    y.Add(x,y,prec1);  // now y = 0.00000011 and has low precision
    CheckValues(y.GetPrecision(), 13, "y has precision 13");
    for (int i=0; i<100; i++)
    {
      x.Add(x,y,prec1);
    }
    CheckValues(x.GetPrecision(), prec1-2, Message("x=%10.10f has precision %d", x.Double(), prec1-3));
    y.Negate(y);
    for (int i=0; i<100; i++)
    {
      x.Add(x,y,prec1);
    }
    CheckValues(x.GetPrecision(), prec1-2, "x has precision prec1-3");
    CheckStringValue(x, "0.10000011", 10, 10, "x is equal to its old value");
 

  }
  Next("precision control: Precision(), SetTo(), Negate()");
 
  {
  LispInt prec = 34, prec1 = 10, prec2 = 120;
    BigNumber x("0.1",10,10);
    CheckValues(x.GetPrecision(), prec, "x has correct initial precision");
    prec = x.GetPrecision();
    x.Negate(x);
    CheckValues(x.GetPrecision(), prec, "correct precision after Negate()");
    BigNumber y;
    y.Negate(x);
    CheckValues(y.GetPrecision(), prec, "correct copying of precision after y:=x");
    x.Precision(prec1);
    x.Negate(x);
    CheckValues(x.GetPrecision(), prec1, "x has fewer bits now");
    Check(x.Equals(y), "x=y after decreasing precision");
    Check(y.Equals(x), "y=x after decreasing precision");
    CheckEquals(x, y, prec1, 2, "x=y by string comparison at initial precision");
    x.SetTo(y);
    CheckValues(x.GetPrecision(), prec, "correct copying of precision after y:=x");
    x.Precision(prec2);
    CheckValues(x.GetPrecision(), prec2, "x has more bits now");
    Check(x.Equals(y), "x=y after increasing precision");
    Check(y.Equals(x), "y=x after increasing precision");
    CheckEquals(x, y, prec, 2, "x=y by string comparison");
  }
  Next("precision control for Equals() and LessThan()");
  {
    BigNumber x("0.1110100010", 0, 2);
    BigNumber y("0.1110100000", 0, 2);
    CheckValues(x.GetPrecision(), 10, "x has precision 10");
    CheckValues(y.GetPrecision(), 10, "y has precision 10");
    CheckValues(x.BitCount(), 0, "x has bit count 0");
    BigNumber z1(x);
    z1.Negate(z1);
    z1.Add(z1,y,10);  // now z1 = -0.000000001 and only one digit is correct
    CheckValues(z1.Sign(),-1,"z1 has correct sign");
    CheckStringValue(z1, "-0.1e-1000",10,2,"z1 has correct value");
    CheckValues(z1.GetPrecision(), 1, "z1 has one correct digit");
    CheckValues(z1.BitCount(), -8, "z1 has bit count -8");
 
    Check(!x.Equals(y), "x!=y");
    Check(!y.Equals(x), "y!=x");
    Check(!x.LessThan(y), "!x<y");
    Check(y.LessThan(x), "y<x");
    x.Negate(x);
    y.Negate(y);
    Check(x.LessThan(y), "-x<-y");
    Check(!y.LessThan(x), "!-y<-x");
    CheckEquals(x, y, 7, 2, "x=y by string comparison to 7 bits");
 
    // reduce precision to make them equal
    y.Precision(8);
    Check(x.Equals(y), "x=y after decreasing precision");
    Check(y.Equals(x), "y=x after decreasing precision");
    Check(!x.LessThan(y), "!x<y after decreasing precision");
    Check(!y.LessThan(x), "!y<x after decreasing precision");

    // another number
    BigNumber z("-0.1110100001", 10, 2);
    Check(x.Equals(z), "x=z");  // x should have been the same number
    // any two zeros are equal
    z.Negate(z);
    z.Add(z,x, 20);  // should give 0
    CheckValues(z.Sign(), 0, "z=0. now");
    x.SetTo(0);  // integer zero
    Check(x.Equals(z), "x=z");
    Check(z.Equals(x), "z=x");
    x.SetTo(0.);  // floating zero
    Check(x.Equals(z), "x=z");
    Check(z.Equals(x), "z=x");
    x.Precision(5);
    Check(x.Equals(z), "x=z");
    Check(z.Equals(x), "z=x");
    x.Precision(-3);
    Check(x.Equals(z), "x=z");
    Check(z.Equals(x), "z=x");
    x.Precision(15);
    Check(x.Equals(z), "x=z");
 
    // zero is equal to a small number
    x.SetTo("-.00001", 10, 2);
    y.SetTo("0.", 5, 2);
    CheckValues(y.GetPrecision(), 5, "y is 0. with 5 bits");
  }
  Next("roundoff control for subtraction");
  {  // when subtracting these two numbers, there is not enough precision to distinguish the result from 0
    BigNumber x("0.1110100001", 0, 2);
    BigNumber y("0.1110100000", 0, 2);
    CheckValues(x.GetPrecision(), 10, "x has precision 10");
    CheckValues(y.GetPrecision(), 10, "y has precision 10");
    BigNumber z1(x);
    z1.Negate(z1);
    z1.Add(z1,y,10);  // now z1 = 0 because the one last digit difference was lost in roundoff error
    CheckValues(z1.Sign(),0,"z1 has correct sign");
    BigNumber z2(0);
    Check(z2.Equals(z1), "z1 is equal to zero");
    CheckStringValue(z1, "0.",30, 2,"z1 prints as '0.'");
    CheckValues(z1.GetPrecision(), 9, "z1 has correct order of error");
    CheckValues(z1.BitCount(), 0, "z1 has bit count 0");  // floating 0. has bit count 0
  }
  {
    Next("subtracting in exponential notation");
    BigNumber x("0.3e20", 20, 10), z("-0.3e20", 20, 10), y;
    y.Add(x, z, 20);
    CheckValues(y.Sign(), 0, "y == 0");
    CheckStringValue(y, "0.", 20, 10, "y prints correctly");
  }
  {  // when subtracting these two numbers, there is enough precision to distinguish the result from 0
    BigNumber x("0.1110100100", 0, 2);
    BigNumber y("0.1110100000", 0, 2);
    CheckValues(x.GetPrecision(), 10, "x has precision 10");
    CheckValues(y.GetPrecision(), 10, "y has precision 10");
    BigNumber z1(x);
    z1.Negate(z1);
    z1.Add(z1,y,10);
    CheckValues(z1.Sign(),-1,"z1 has correct sign");
    BigNumber z2;
    z2.SetTo(0);
    Check(!z2.Equals(z1), "z1 is not equal to zero");
    z2.SetTo(0.);
    z2.Precision(7);
    Check(!z2.Equals(z1), "z1 is equal to zero up to 7 digits");
    CheckValues(z1.GetPrecision(), 2, "z1 has correct precision");
    CheckValues(z1.BitCount(), -7, "z1 has bit count -7");
    CheckStringValue(z1, "-0.1e-111",30, 2,"z1 prints as '-0.1e-111'");
  }
  {
    Next("BecomeFloat preserves digits");
    // a number that is surely too large for a system type
    BigNumber x("1023101201074747474747474747200000000000000000000000000007878787878301288888888877777777777777777770312312", 0, 10);
    BigNumber y(x);
    Check(x.IsInt() && y.IsInt(), "x and y have integer type");
    y.BecomeFloat(0);
    Check(!y.IsInt(), "y is float");
    Check(y.IsIntValue(), "y has integer value");
    Check(y.Equals(x), "y = x");
//    cout << y.GetPrecision() << endl;
    y.BecomeInt();
    Check(y.Equals(x), "y = x after BecomeInt");
 
    // preserve precision
    y.SetTo(1.2434);
    x.SetTo(y);
    y.Precision(20);
    y.BecomeFloat();
    Check(y.Equals(x), "y did not change after BecomeFloat");
    CheckValues(y.GetPrecision(), 20, "y preserves precision after BecomeFloat");
  }


  {
    Next("integer multiplication and division");
    test_mul_div_1("110000000000", "200000000", "22000000000000000000", 10);
    test_mul_div_1("2", "3", "6", 8);
    test_mul_div_1("-2", "3", "-6", 8);
    test_mul_div_1("99999991231239948718238175", "-3888242333132323344455311", "-388824199218168293972222204499935156395306941697425", 10);
  }
  {
    Next("comparison of numbers in exponential notation");
    // -1e-5 < 0
    BigNumber x, y;
    x.SetTo("-1e-5", 34, 10);
    Check(!x.IsInt(), "x has float type");
    CheckValues(x.Double(), -1e-5, "x is -1e-5");
    CheckValues(x.Sign(), -1, "x has correct sign, -1");
    y.SetTo(0);  // integer 0
    Check(!x.Equals(y), "x is nonzero");
    Check(x.LessThan(y), "-1e-5 < 0");
 
    // compare zero with various numbers
    test_less_than("-2e-5", "0", 10);
    test_less_than("-2e-1", "0", 10);
    test_less_than("0", "2e-2", 10);
    test_less_than("0", "1e-5", 10);
    test_less_than("0", "1e-10", 10);
    test_less_than("0", "1e-60", 10);

    // compare floating zero with floating nonzero
    test_less_than("-2.3e-5", "0.00000000", 10);
    test_less_than("-2e-1", "0.000", 10);
    test_less_than("0.0000", "2e-2", 10);
    test_less_than("0.0000000000", "1e-5", 10);
    test_less_than("0.0000000000000000000000", "1.234e-10", 10);
    test_less_than("0.0000000000000000000000000000000000000000000000000000000000000000000000000000000000000", "1.234567e-60", 10);
 
    // compare floats with integers
    test_less_than("-1.0000000000000000000000000000001", "-1", 10);
    test_less_than("0.99999999999999999999999999999999992", "1", 10);
 
    test_less_than("-1.00000000000000000000000000000002e3", "-1000", 10);
 
    // compare nonzero numbers
    test_less_than("1e-5", "2e-5", 10);
    test_less_than("1e-5", "1e-3", 10);
    test_less_than("1", "1.0000000000000000000001", 10);
    test_less_than("-1.00000000000000000000000000001262177448353", "-1", 10);
    test_less_than("1", "1.00000000000000000000000000001262177448353", 10);
    test_less_than("1e-115", "2e-105", 10);
    test_less_than("2e-15", "2e-5", 10);
    test_less_than("2e-1", "1", 10);
    test_less_than("-2e-2", "-2e-5", 10);
    test_less_than("2e-5", "2e-2", 10);
    test_less_than("1e-10", "2e-2", 10);
    test_less_than("-1e-10", "2e-2", 10);
    test_less_than("-1e-10", "-1e-20", 10);
    test_less_than("-1", "-1e-10", 10);
    test_less_than("-2e-80", "-1e-100", 10);

    test_less_than("213.", "214", 10);
    test_less_than("213", "214.", 10);
    test_less_than("-213.", "214", 10);
    test_less_than("-213", "214.", 10);
    test_less_than("213", "214", 10);
    test_less_than("1", "1.0000000000000000000000000000000000000000000000000000003", 10);

  }
  {
    Next("precision control for IsIntValue()");
    BigNumber x, y;
    x.SetTo(145.0);
    Check(x.IsIntValue(), "145.0 has integer value");
    x.SetTo(195.23);
    y.SetTo(-14.23);
    CheckValues(x.GetPrecision(), 53, "x has 53 bits");
    CheckValues(y.GetPrecision(), 53, "y has 53 bits");
    y.Add(x, y, 60);
    CheckValues(y.GetPrecision(), 53, "y has 53 bits");
    Check(y.IsIntValue(), "subtracted to integer value");
    CheckValues(y.Double(), 181.0, "y has correct value");
    CheckStringValue(y, "181.", 20, 10, "y prints correctly");
 
    // imitating the Yacas calculation, FloatIsInt(5.1*10)
    x.SetTo("5.1", 34, 10);
    y.SetTo(10);
    Check(!x.IsIntValue(), "x has float value");
    x.Multiply(x,y,34);
    Check(x.IsIntValue(), "10*x has int value");
 
    x.SetTo("101.00010", 0, 2);
    CheckValues(x.GetPrecision(), 8, "x has 8 bits");
    Check(!x.IsIntValue(), "x has float value");
    x.Precision(7);
    Check(!x.IsIntValue(), "x has float value");
    x.Precision(6);
    Check(x.IsIntValue(), "x has int value");

    x.SetTo("101.11110", 0, 2);
    CheckValues(x.GetPrecision(), 8, "x has 8 bits");
    Check(!x.IsIntValue(), "x has float value");
    x.Precision(7);
    Check(!x.IsIntValue(), "x has float value");
    x.Precision(6);
    Check(x.IsIntValue(), "x has int value");
  }
  {
    Next("precision control for floating zero");
    BigNumber x;
    x.SetTo(0.);
    x.Precision(100);
    Check(x.Sign() == 0, "x = 0");
    CheckValues(x.BitCount(), 0, "x has 0 bit count");
    Check(x.GetPrecision() == 100, "x has 100 bits");
    x.Multiply(x,x,300);
    CheckValues(x.GetPrecision(), 200, "x has 200 bits");
    Check(x.Sign() == 0, "x = 0");
    x.Add(x,x,300);
    CheckValues(x.GetPrecision(), 199, "x has 199 bits");
    Check(x.Sign() == 0, "x = 0");
 
    // floating zero with explicit digits
    x.SetTo("-000000000000000.000000", 0, 2);
    CheckValues(x.Sign(), 0, "x is 0");
    CheckValues(x.GetPrecision(), 7, "x has 7 bits");
    x.SetTo("-0.00000e10", 0, 2);
    CheckValues(x.Sign(), 0, "x is 0");
    CheckValues(x.GetPrecision(), 6, "x has 6 bits");
  }
  {
    Next("precision control for addition");
#define bad_num  "1.00000000000000000000000000001262177448353"
    BigNumber x(bad_num, 0, 10);
    // this number is nearly 1 + 2^(-96) and this somehow triggers an error in the gmp version
    BigNumber y("1", 0, 10);  // integer 1
    Check(y.LessThan(x), "1<x");
    BigNumber z(y), t(y);
    Check(y.LessThan(x), "1<x");
    z.Negate(x);  // now z = - 1.000...0126...
    t.Negate(t);  // now t = -1
    Check(z.LessThan(t), "z<t");
    Check(!z.Equals(t), "z!=t");
    Check(!t.Equals(z), "t!=z");
    CheckStringValue(z, "-" bad_num, 80, 10, "z prints correctly");
    z.Add(z,y,x.GetPrecision());  // -1.000...0126... + 1 is just below 0
    Check(z.Sign() == -1, "-x+1 < 0");
    Check(z.GetPrecision()<x.GetPrecision()-90, "at least 90 bits of precision are lost on subtraction");
 
    // test how two numbers with equal absolute errors are added.
    // Note that the last digit may be rounded off, so put one more bit as guard bit.
    x.SetTo("1.0001000", 0, 2);
    y.SetTo("0.0001010", 0, 2);
    CheckValues(x.GetPrecision(), 8, "x has 8 bits");
    CheckValues(y.GetPrecision(), 4, "y has 4 bits");
    z.SetTo("0", 0, 2);  // set to integer, to see if it works correctly
    z.Add(x,y, 8);  // enough bits to add correctly
    CheckValues(z.GetPrecision(), 7, "x+y has 7 bits");
    CheckStringValue(z, "1.001001", 10, 2, "x+y prints correctly");
    z.SetTo("0", 0, 2);
    z.Add(x,y,5);  // not enough bits, need to lose precision here
    CheckValues(z.GetPrecision(), 5, "x+y has 5 bits");
    CheckStringValue(z, "1.001", 10, 2, "x+y prints correctly at lower precision");
 
    // one number is much more precise than the other, precision of the answer should be unchanged
    x.SetTo("1.0001000", 0, 2);
    y.SetTo("0.0001010000000", 0, 2);
    CheckValues(x.GetPrecision(), 8, "x has 8 bits");
    CheckValues(y.GetPrecision(), 10, "y has 10 bits");
    z.Add(x,y,20);  // enough bits to add correctly
    CheckValues(z.GetPrecision(), 8, "x+y has 8 bits");
    CheckStringValue(z, "1.001001", 10, 2, "x+y prints correctly");
 
    // one number is smaller than the absolute error of another, answer should be unchanged
    x.SetTo( "1.0001", 8, 2);
    y.SetTo("-0.0000000001", 4, 2);
    //answer: 1.0001000 with 8 bits
    CheckValues(x.GetPrecision(), 8, "x has 8 bits");
    CheckValues(y.GetPrecision(), 4, "y has 4 bits");
    CheckValues(x.BitCount(), 1, "x has bit count 1");
    CheckValues(y.BitCount(), -9, "y has bit count -9");
    z.Add(x,y,20);  // enough bits to add correctly
    CheckValues(z.GetPrecision(), 8, "x+y has 8 bits");
    CheckStringValue(z, "1.0001", 10, 2, "x+y prints correctly");
 
    // loss of precision due to subtraction only
    x.SetTo( "1.0001010", 0, 2);
    y.SetTo("-1.000000001000", 0, 2);
    //answer: 0.0001010
    CheckValues(x.GetPrecision(), 8, "x has 8 bits");
    CheckValues(y.GetPrecision(), 13, "y has 13 bits");
    z.Add(x,y,20);  // enough bits to add correctly
    CheckValues(z.GetPrecision(), 4, "x+y has 4 bits");
    CheckStringValue(z, "0.000101", 10, 2, "x+y prints correctly");

    // loss of precision due to subtraction and to roundoff error (one more bit should be lost than in the previous case)
    x.SetTo( "1.000101000", 0, 2);
    y.SetTo("-1.000001000", 0, 2);
    //answer: 0.00010000
    CheckValues(x.GetPrecision(), 10, "x has 10 bits");
    CheckValues(y.GetPrecision(), 10, "y has 10 bits");
    z.Add(x,y,20);  // enough bits to add correctly
    CheckValues(z.GetPrecision(), 5, "x+y has 5 bits");
    CheckStringValue(z, "0.0001", 10, 2, "x+y prints correctly");
 
    // one number is smaller than the other, but its error is larger
    x.SetTo( "1.0001010010", 0, 2);
    y.SetTo( "0.0000010", 0, 2);
    //answer: 1.0001100
    CheckValues(x.GetPrecision(), 11, "x has 10 bits");
    CheckValues(y.GetPrecision(), 2, "y has 2 bits");
    z.Add(x,y,20);  // enough bits to add correctly
    CheckValues(z.GetPrecision(), 8, "x+y has 8 bits");
    CheckStringValue(z, "1.00011", 100, 2, "x+y prints correctly");
  }
  {
    Next("precision control for multiplication");
    // one number is more precise than the other
    x.SetTo( "1.0001010010", 0, 2);
    y.SetTo( "1.00", 0, 2);
    //answer: 1.00
    CheckValues(x.GetPrecision(), 11, "x has 11 bits");
    CheckValues(y.GetPrecision(), 3, "y has 3 bits");
    z.Multiply(x,y,20);  // enough bits
    CheckValues(z.GetPrecision(), 3, "x*y has 3 bits");
    CheckStringValue(z, "1.", 100, 2, "x*y prints correctly");

    // both numbers are equally precise, losing 1 bit
    x.SetTo( "1.0001010010", 0, 2);
    y.SetTo( "1.0100010001", 0, 2);
    CheckValues(x.GetPrecision(), 11, "x has 11 bits");
    CheckValues(y.GetPrecision(), 11, "y has 11 bits");
    z.Multiply(x,y,20);  // enough bits
    CheckValues(z.GetPrecision(), 10, "x*y has 10 bits");
    CheckStringValue(z, "1.0101111", 100, 2, "x*y prints correctly");
    z.Multiply(x,y,5);  // not enough bits
    CheckValues(z.GetPrecision(), 5, "x*y has 5 bits");
    CheckStringValue(z, "1.011", 100, 2, "x*y prints correctly");
  }
  {
    Next("precision control for division");
    // one number is more precise than the other
    x.SetTo( "1.0001010010", 0, 2);
    y.SetTo( "1.00", 0, 2);
    //answer: 1.00
    CheckValues(x.GetPrecision(), 11, "x has 11 bits");
    CheckValues(y.GetPrecision(), 3, "y has 3 bits");
    z.Divide(x,y,20);  // enough bits
    CheckValues(z.GetPrecision(), 3, "x/y has 3 bits");
    CheckStringValue(z, "1.", 100, 2, "x/y prints correctly");

    // both numbers are equally precise, losing 1 bit
    x.SetTo( "1.1111101010", 0, 2);
    y.SetTo( "1.0001010010", 0, 2);
    CheckValues(x.GetPrecision(), 11, "x has 11 bits");
    CheckValues(y.GetPrecision(), 11, "y has 11 bits");
    z.Divide(x,y,20);  // enough bits
    CheckValues(z.GetPrecision(), 10, "x/y has 10 bits");
    CheckStringValue(z, "1.11010101", 100, 2, "x/y prints correctly");
    z.Divide(x,y,5);  // not enough bits
    CheckValues(z.GetPrecision(), 5, "x/y has 5 bits");
    CheckStringValue(z, "1.1101", 100, 2, "x/y prints correctly");
  }
  {
    Next("precision control for mixed integer/float arithmetic");
    BigNumber x(  "1.1010101", 0, 2);
    BigNumber y("100", 0, 2);
    CheckValues(x.GetPrecision(), 8, "x has 8 bits");
    // add integer to float, precision is determined by the absolute error of the float
    BigNumber z;
    z.Add(x,y,20);  // enough bits
    CheckValues(z.GetPrecision(), 10, "x+y has 10 bits");
    CheckStringValue(z, "101.1010101", 100, 2, "x+y prints correctly");

    z.Multiply(x,y,20);  // enough bits
    CheckValues(z.GetPrecision(), 8, "x*y has 8 bits");
    CheckStringValue(z, "110.10101", 100, 2, "x*y prints correctly");
  }
  {
    Next("precision control for floating-point shifts");
    BigNumber x("0.1010101", 0, 2), y;
    CheckValues(x.GetPrecision(), 7, "x has 7 bits");
    x.ShiftRight(x, 4);
    y.SetTo("0.00001010101", 0, 2);
    CheckEquals(x, y, 12, 2, "x is correct after shift");
    CheckValues(x.GetPrecision(), 7, "x has 7 bits");
    x.ShiftLeft(x, 10);
    CheckValues(x.GetPrecision(), 7, "x has 7 bits");
  }
  {
    Next("precision control for Floor()");
    BigNumber x("10000.110", 0, 2), y;
    CheckValues(x.GetPrecision(), 8, "x has 8 bits");

    y.Floor(x);
    Check(y.IsInt(), "y has int type");
    Check(y.IsIntValue(), "y has int value");
    CheckValues(y.Double(), 16, "y = 16");
 
    y.Negate(x);
    y.Floor(y);
    Check(y.IsInt(), "y has int type");
    Check(y.IsIntValue(), "y has int value");
    CheckValues(y.Double(), -17, "y = -17");
 
  }
}  // end of the comprehensive test suite


#endif  // if ENABLE_TESTS
    Finish();
    return 0;
}

