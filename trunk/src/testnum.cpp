


//#include "yacasprivate.h"
#include "numbers.h"
#include <stdio.h>
#include <math.h>

#define ENABLE_TESTS 1

// whether to print detailed information about passed tests
const bool show_passed = false;

unsigned failed = 0;
unsigned passed = 0;

// compare a given LispString value to a given character string and print diagnostic
void Check(LispString& str, const char* s, const char* test_description)
{
    if (strcmp(str.String(),s))
    {
        printf("@@@@@@@ %s: failed: %s != %s\n",test_description, str.String(),s);
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
void Check(bool test_condition, const char* test_description)
{
    if (!test_condition)
    {
        printf("@@@@@@@ %s: failed\n",test_description);
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
void CheckStringValue(const BigNumber& x, const char* value, LispInt precision, LispInt base, const char* test_description)
{
    LispString str;
    x.ToString(str, precision, base);
    Check(str, value, test_description);
}

// print a progress message
void Next(const char* description)
{
    static int i=0;
    i++;
    printf("Test group %d: %s\n",i, description);
    fflush(stdout);
}

void Finish()
{
    printf("Passed %d, failed %d tests.\n", passed, failed);
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
	Check(x.Double()==int_value, "value is correct");
	Check(x.IsIntValue(), "is an integer value");
	Check(x.Sign()==int_sign, "sign is correct");
	x.BecomeInt();
	Check(x.IsInt(), "still integer type");
	x.BecomeFloat();
	Check(!x.IsInt(), "converted to double type");
	Check(x.Double()==int_value, "value is still correct");
	Check(x.Sign()==int_sign, "sign is correct");
	Check(x.IsSmall(), "value is small");
	x.BecomeInt();
	Check(x.IsInt(), "converted to integer type");
	Check(x.Double()==int_value, "value is still correct");
	x.Negate(x);	// negate an integer
	Check(x.Double()==-int_value, "value is negated");
	Check(x.Sign()==-int_sign, "sign is correct");

// test floats
	x.SetTo(value);
	Check(!x.IsInt(), "set to float type");
	Check(x.Double()==value, "value is correct");
// value doesn't have to be float
	//	Check(!x.IsIntValue(), "is not an integer value");
	x.BecomeFloat();
	Check(!x.IsInt(), "still float type");
	Check(x.Double()==value, "value is still correct");
	Check(x.IsSmall(), "value is small");
	Check(x.Sign()==sign, "sign is correct");

	BigNumber y;
	y.SetTo(value);	// test constructor from double
	Check(y.IsSmall(), "value is small");
	Check(y.Equals(x), "y=x");
	Check(x.Equals(y), "x=y");
	y.SetTo(int_value);	// constructor from integers
	Check(y.Double()==int_value, "value of y is correct");
	Check(y.IsInt() && y.IsIntValue(), "y is integer");

	x.Negate(x);	// negate a float
	Check(x.Double()==-value, "value is negated");
	Check(x.Sign()==-sign, "sign is correct");
	x.BecomeInt();
	Check(x.IsInt(), "converted to integer type");
	Check(x.Double()==-int_value, "value is correct");
	x.BecomeFloat();
	Check(!x.IsInt(), "convert to float type");
	Check(x.Double()==-int_value, "value is still correct");
	Check(x.IsIntValue(), "is an integer value");
	Check(x.IsSmall(), "value is small");

	
}

// test big numbers: assignment, types, comparison
void TestTypes2(const char* float_string, const char* int_string, double double_value)
{
	const LispInt base = 10;
	int sign = (double_value>0)?1:((double_value<0)?-1:0);
	
// test constructors from strings
	BigNumber x(float_string, 3*strlen(float_string), base);
	BigNumber y;
	y.SetTo(int_string, 3*strlen(int_string), base);
//	y.BecomeInt();	// FIXME: this would be unnecessary if the type were automatically decided based on the string.

//	Check(!x.IsSmall(), "x is a big number");
//	Check(!y.IsSmall(), "y is a big number");
	
	Check(!x.IsInt(), "x is a float type as read");
	Check(y.IsInt(), "y is an integer type as read");
	Check(x.Double()==double_value, "value is correct");
	Check(!x.IsIntValue(), "x has a float value");
	Check(y.IsIntValue(), "y has an integer value");
	Check(x.Sign()==sign, "sign of x is correct");
	Check(y.Sign()==sign, "sign of y is correct");

	LispString str;
	y.ToString(str,base);
	Check(str,int_string,"read integer value correctly");
	BigNumber z(x);
	Check(!z.IsInt(), "z has int type");
	Check(!z.IsIntValue(), "z has a float value");
	z.BecomeInt();
	Check(z.Sign()==sign, "sign of z is correct");
	z.ToString(str,base);
	Check(str,int_string,"convert to integer value correctly");
	Check(z.IsInt(), "z is an integer now");
	Check(z.IsIntValue(), "z has an integer value now");
	
	Check(z.Equals(y), "z=y");
	Check(y.Equals(z), "y=z");
	Check(x.Equals(x), "x=x");
	Check(y.Equals(y), "y=y");
	Check(z.Equals(z), "z=z");
	Check(!x.Equals(z), "x!=z");
	Check(!z.Equals(x), "z!=x");
	Check(!x.Equals(y), "x!=y");
	
	z.BecomeFloat();
	Check(!z.IsInt(), "z is not an integer now");
	Check(z.Sign()==sign, "sign of z is correct");
	Check(z.IsIntValue(), "z still has an integer value");

	
	BigNumber t;
	t.SetTo(x);
	Check(t.Equals(x), "t=x");
	Check(t.Equals(t), "t=t");
	t.Negate(t);
	Check(t.Sign()==-sign, "sign of t is correct");
	Check(!t.Equals(x), "t!=x now");
	Check(!x.LessThan(x), "x<x is false");
	Check(!t.LessThan(t), "t<t is false");
	Check(double_value > 0 && t.LessThan(x) || !(double_value>0) && t.LessThan(x), "comparison t <> x is correct");
	Check(double_value < 0 && x.LessThan(t) || !(double_value<0) && !x.LessThan(t), "comparison x <> t is correct");

}

// test some integer and float arithmetic
void TestArith1(const char* str_value, int base, int val1, double val2)
{
	long prec=strlen(str_value)+5;
	BigNumber x(str_value, prec, base);
	BigNumber x1(x), x2(x), y;
	
	y.SetTo(val1);
	x.Add(x,y, prec);
	x.Negate(x);
	x.Add(x,y, prec);
	x.Negate(x);
	Check(x.Equals(x1), "add and subtract an integer");
	x1.Negate(x1);
	x1.Add(x1,x, prec);
	Check(x1.Sign()==0, "x-x=0");
	
	x.SetTo(x2);
	x1.SetTo(x2);
	BigNumber z;
	z.SetTo(val2);
	for (int i=0; i<100; ++i) x.Add(x,z, prec);
	z.Negate(z);
	BigNumber t;
	t.SetTo(100);
	z.Multiply(z,t,prec);
	x.Add(x,z, prec);
	Check(x.Equals(x1), "add and subtract a double 100 times");
	
	x.SetTo(x2);
	x1.SetTo(x2);
	y.SetTo(fabs(100));
	for (int i=0; i<100; ++i) x.Add(x,x, prec);
	x1.Multiply(x1,y, prec);
	Check(x.Equals(x1), "add to itself 100 times");
	
}


void TestStringIO(double value, const char* test_string, LispInt precision, LispInt base)
{	
	BigNumber x;
	x.SetTo(value);
	Check(!x.IsInt(), "have a float value");
	LispString str;
	x.ToString(str, precision, base);
	Check(str, test_string, "printed string value matches");
}
void TestStringIO(int value, const char* test_string, LispInt precision, LispInt base)
{	
	BigNumber x;
	x.SetTo(value);
	Check(x.IsInt(), "have an int value");
	LispString str;
	x.ToString(str, precision, base);
	Check(str, test_string, "printed string value matches");
}

#endif // test numbers

int main(void)
{
    LispString  str;
/*
    ANumber res1("10",100,10);
    ANumber res2("10",100,10);
    ANumber res3("10",100,10);
*/
//    printf("WordBits = %d\n",WordBits);
//    printf("WordMask = %ld\n",WordMask);
//    printf("Starting tests...\n");

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
    Check(str,"25", "adding 10 and 15");

}
{
    BigNumber n1("65535",100,10);
    n1.ToString(str,20,10);
    Check(str,"65535", "reading 65535 from string");
    n1.ToString(str,20,10);
    Check(str,"65535", "reading 65535 from string again");
    n1.ToString(str,30,2);
    Check(str,"1111111111111111", "printing in binary");
    n1.Negate(n1);
    n1.ToString(str,20,10);
    Check(str,"-65535", "negate 65535");

    BigNumber res1;
    res1.Add(n1,n1,10);    
    res1.ToString(str,20, 10);
    Check(str,"-131070", "add -65535 to itself");
}
#endif
{
//////////////////////////////////////////////////
///// BigNumber comprehensive test suite
//////////////////////////////////////////////////

	Next("library name");	
	printf("Testing numeric library: '%s'.\n", BigNumber::NumericLibraryName());
	
	Next("constructor");
	BigNumber x;	// default constructor
	Next("call some functions on objects with undefined values");
	x.IsInt();
	x.Double();	// value is undefined
	Next("construct 0 from string");
	BigNumber y("0", 10);	// construct
	Check(y.Double()==0, "value of 0");

	Next("read binary string");
	y.SetTo("-101010", 50, 2);	// construct with given precision
	Check(y.Double()==-42, "value is correct");
	Check(y.IsInt(), "has int type");
	Check(y.IsIntValue(), "value is integer");
	Check(y.IsSmall(), "value is small");
	y.BecomeInt();
	Check(y.IsInt(), "has integer type");
	Check(y.IsSmall(), "value is small");
	
	Next("testing big integers");
	x.SetTo("010203040506070809101112131415161718192021222324252627282930", 200, 10);
	Check(x.IsInt(), "has integer type");
	Check(x.Sign()==1, "has positive value");
	Check(x.IsIntValue(), "has integer value");
	Check(!x.IsSmall(), "big integer is not small");
	x.Multiply(x,x,0);
	Check(x.IsInt(), "has integer type");
	Check(x.IsIntValue(), "has integer value");
	Check(!x.IsSmall(), "big integer is not small");
	y.SetTo(2);
	y.Mod(x,y);
	Check(y.Double()==0, "value is even");
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
	// values must be out of range for platform numbers. Usage:
	// TestTypes2("float value", "equivalent integer value", double_value, base);
	const char* num1_float = "3.00000000000000000000000000000000000000099999999999992";
	const char* num1_int = "3";
	const double num1_double = 3;
	const char* num2_float = "-100000000000000.23333333333";
	const char* num2_int = "-100000000000000";
	const double num2_double = -1.e14;
	const char* num3_float = "10000000000000000.23333333333";
	const char* num3_int = "10000000000000000";
	const double num3_double = 1.e16;
	const char* num4_float = "123.33233233233233233232333333111111111111199797797973333";
	const char* num4_int = "123";
	const double num4_double = 123.332332332332332332323333333333;

	TestTypes2(num1_float, num1_int, num1_double);
	Next("test big integers and floats 2");
	TestTypes2(num2_float, num2_int, num2_double);
	Next("test big integers and floats 3");
	TestTypes2(num3_float, num3_int, num3_double);
	Next("test big integers and floats 4");
	TestTypes2(num4_float, num4_int, num4_double);
	
	Next("test copy constructor");
	y.SetTo(-15416.0);
	Check(!y.IsInt(), "y is of float type");
	Check(y.IsIntValue(), "y has integer value");
	BigNumber z(y);	// copy constructor
	Check(!z.IsInt(), "z is of float type");
	Check(z.Double()==-15416, "value is still -15416");
	Check(z.IsSmall(), "value is small");
	
	Next("test integer conversion");
	z.BecomeInt();
	x.SetTo(z);	// copy assignment
	Check(x.IsInt(), "is of integer type");
	Check(x.Double()==-15416, "value is still -15416");
	Check(x.IsSmall(), "value is small");

	Next("equality");

	Check(x.Equals(x), "x=x");
	Check(y.Equals(y), "y=y");
	Check(x.Equals(y), "x=y");
	Check(y.Equals(x), "y=x");
	Check(z.Equals(x), "z=x");
	Check(z.Equals(y), "z=y");
	Check(x.Equals(z), "x=z");
	x.Negate(x);	// negate an integer
	Check(x.IsInt(), "x still of integer type");
	Check(x.Double()==15416, "value is now 15416");
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
	Check(t.Double()==4950, "correct sum of n from 0 to 99");
	
	Next("bit counts");
	x.SetTo(65537);
	x.BitCount(x);
	Check(x.IsInt(), "bit count is integer");
	Check(x.Double()==17, "bit count of 65537 is 17");
	Check(x.Double()!=16, "bit count of 65537 is not 16");
	x.SetTo(1./1050000.);
	Check(!x.IsInt(), "x is a floating-point value");
	Check(x.Sign()==1, "x is positive");
	x.BitCount(x);
	Check(x.IsInt(), "bit count is integer");
	Check(x.Double()==-20, "bit count of 2^(-20)");
	Check(x.Double()!=-21, "bit count of 2^(-20) is not -21");
	Check(x.Double()!=-19, "bit count of 2^(-20) is not -19");
	
	Next("arithmetic with large powers");
	// compute 15^(large number) and check that it divides both 3^(that number) and 5^(that number)
	x.SetTo(15);
	for(int i=0; i<10; ++i) x.Multiply(x,x,10);
	Check(x.IsInt(), "15^N is integer");
	y.SetTo(3);
	for(int i=0; i<10; ++i) y.Multiply(y,y,10);
	z.Mod(x,y);
	Check(y.LessThan(x), "3^N<15^N");
	Check(z.Sign()==0, "15^N divides 3^N");
	z.SetTo(5);
	for(int i=0; i<10; ++i) z.Multiply(z,z,10);
	t.Mod(x,z);
	Check(t.Sign()==0, "15^N divides 5^N");
	y.Multiply(z,y,10);
	Check(x.Equals(y), "15^N = 3^N*5^N");
	
	Next("string input/output");
	TestStringIO(0, "0", 10, 10);
	TestStringIO(0., "0.", 10, 10);
	TestStringIO(1, "1", 10, 10);
	TestStringIO(1., "1.", 10, 10);
	TestStringIO(-1, "-1", 10, 10);
	TestStringIO(-1., "-1.", 10, 2);
	TestStringIO(-1, "-1", 10, 3);
	TestStringIO(-13, "-111", 10, 3);
	TestStringIO(-13., "-111.", 10, 3);
	TestStringIO(-13, "-1101", 10, 2);
	TestStringIO(13, "13", 10, 10);
	TestStringIO(1000000000, "1000000000", 10, 10);
	TestStringIO(1000000000., "0.1e10", 10, 10);
	TestStringIO(-13, "-d", 10, 16);
	TestStringIO(-13.0, "-d.", 10, 16);
	TestStringIO(-13.0e-15, "-0.13e-13", 10, 10);
	TestStringIO(13.1245e-15, "0.131245e-13", 10, 10);
	TestStringIO(13.1245e-15, "0.131e-13", 3, 10);
	TestStringIO(13.1245e-15, "0.131245e-13", 6, 10);
	TestStringIO(-13.1245e-15, "-0.131e-13", 3, 10);
	TestStringIO(-13.1245e-15, "-0.131245e-13", 6, 10);
	TestStringIO(13.0e15, "0.13e17", 10, 10);
	TestStringIO(13.0e15, "0.13e17", 10, 10);
	TestStringIO(0.0011, "0.0011", 10, 10);
	TestStringIO(0.01, "0.01", 10, 10);
	TestStringIO(0.001, "0.001", 10, 10);
	TestStringIO(0.1, "0.1", 10, 10);
	TestStringIO(-0.001, "-0.001", 10, 10);
	TestStringIO(1234, "1234", 10, 10);
	TestStringIO(12345, "12345", 10, 10);
	TestStringIO(123456, "123456", 10, 10);
	
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
	Check(z.Equals(t), "ShiftRight operation correct (small shift)");
	z.ShiftLeft(y,4);
	t.SetTo("61236123612361236123612361236123612361230", 0, 16);
	Check(z.Equals(t), "ShiftLeft operation correct (small shift)");
	t.SetTo(4);
	z.ShiftRight(x,t);
	t.SetTo("402040204020402040204020402040204020402", 0, 16);
	Check(z.Equals(t), "ShiftRight operation correct (big shift)");
	t.SetTo(4);
	z.ShiftLeft(y,t);
	t.SetTo("61236123612361236123612361236123612361230", 0, 16);
	Check(z.Equals(t), "ShiftLeft operation correct (big shift)");
	z.BitNot(z);
	t.SetTo("9EDC9EDC9EDC9EDC9EDC9EDC9EDC9EDC9EDC9EDCF", 0, 16);
	Check(z.Equals(t), "BitNot operation correct");
	
	Next("precision");
	x.BecomeFloat();
	x.Precision(10);
	x.SetTo(100./243.);
	x.Precision(10);
	y.SetTo(1000.);
	y.Precision(10);
	x.Multiply(x,y, 10);
	CheckStringValue(x, "411", 20, 10, "imprecise 100./243. is correct");
	Check(x.IsIntValue(), "have only 10 bits of precision");

	Next("Floor()");
	x.BecomeFloat();
	Check(!x.IsInt(), "have a float value");
	x.Precision(100);
	x.SetTo(1.7);
	y.Floor(x);
	Check(!y.IsInt(), "Floor() returns a float value");
	CheckStringValue(y, "1.", 10, 10, "Floor(1.7) is correct");
	x.SetTo(-1.7);
	y.Floor(x);
	CheckStringValue(y, "-2.", 10, 10, "Floor(-1.7) is correct");
	x.SetTo(-17.0);
	y.Floor(x);
	CheckStringValue(y, "-17.", 10, 10, "Floor(-17.0) is correct");
	x.SetTo(1.0);
	y.Floor(x);
	CheckStringValue(y, "1.", 10, 10, "Floor(1.0) is correct");
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
	z.Divide(x,y,10);
	CheckStringValue(z, "3.75", 10, 10, "15/4==3.75");
	
	
	Next("multiply-add");
	x.SetTo(10);
	y.SetTo(2);
	z.SetTo(5.5);
	x.MultiplyAdd(y,z,10);
	Check(!x.IsInt(), "x is now float");
	Check(x.IsIntValue(), "x has integer value");
	Check(x.Double()==21, "x==22");
	CheckStringValue(x, "21.", 10, 10, "10+2*5.5 = 21.");
	
}
	

/*
    ANumber n1("65535",100,10);
    ANumberToString(str, n1, 10);
    Check(str,"65535");
    ANumberToString(str, n1, 10);
    Check(str,"65535");
    ANumberToString(str, n1, 2);
    Check(str,"1111111111111111");

    Negate(n1);
    ANumberToString(str, n1, 2);
    Check(str,"-1111111111111111");
    Negate(n1);
    Add(res1, n1, n1);
    ANumberToString(str, res1, 16);
    Check(str,"1fffe");
*/
/*    
    n1.SetTo("ff0000",16);
    ANumberToString(str, n1, 16);
    Check(str,"ff0000");
    ANumber n2("f",100,16);
    ANumberToString(str, n2, 10);
    Check(str,"15");

    Subtract(res1, n1, n2);
    ANumberToString(str, res1, 16);
    {
        char ss[20];
        sprintf(ss,"%lx",0xff0000-15L);
        Check(str,ss);
    }
    Next(""); //2

    n1.SetTo("5a5a5a5a5a5a5a5a5a5a5a5a",16);
    BaseShiftRight(n1,1);
    ANumberToString(str, n1, 16);
    Check(str,"2d2d2d2d2d2d2d2d2d2d2d2d");
    
    BaseShiftRight(n1,19);
    ANumberToString(str, n1, 16);
    Check(str,"5a5a5a5a5a5a5a5a5a5");

    BaseShiftLeft(n1,1);
    ANumberToString(str, n1, 16);
    Check(str,"b4b4b4b4b4b4b4b4b4a");
    
    BaseShiftLeft(n1,19);
    ANumberToString(str, n1, 16);
    Check(str,"5a5a5a5a5a5a5a5a5a500000");

    Next("");  //3

    n1.SetTo("550000000");
    n2.SetTo("100000000");
    Subtract(res1,n1,n2);
    ANumberToString(str, res1, 10);
    Check(str,"450000000");
    BaseGcd(res1, n1, n2);
    ANumberToString(str, res1, 10);
    Check(str,  "50000000");

    n1.SetTo("ff0000",16);
    n2.SetTo("100000",16);
    Subtract(res1,n1,n2);
    ANumberToString(str, res1, 16);
    Check(str, "ef0000");

    Next("");     //4

    n1.SetTo("10000");
    n2.SetTo("1234");
    Multiply(res1, n1, n2);
    ANumberToString(str, res1, 10);
    Check(str, "12340000");

    Divide(res2, res3, res1, n2);
    ANumberToString(str, res2, 10);
    Check(str, "10000");
    ANumberToString(str, res3, 10);
    Check(str, "0");


//    {
//        char ss[100];
//    RED:
//        printf("numer = ");
//        fscanf(stdin,"%s",ss);
//        n1.SetTo(ss);
//        printf("denom = ");
//        fscanf(stdin,"%s",ss);
//        n2.SetTo(ss);
//        Divide(res2, res3, n1, n2);
//        ANumberToString(str, res2, 10);
//        printf("quotient : %s\n",str.String());
//        ANumberToString(str, res3, 10);
//        printf("remainder : %s\n",str.String());
//        goto RED;
//    }


    n1.SetTo("10");
    n2.SetTo("2");
    Divide(res2, res3, n1, n2);
    ANumberToString(str, res2, 10);
    Check(str, "5");
    ANumberToString(str, res3, 10);
    Check(str, "0");
    

    n1.SetTo("1000000000");
    n2.SetTo("200");
    Divide(res2, res3, n1, n2);
    ANumberToString(str, res2, 10);
    Check(str, "5000000");

    ANumber fac("1",100);
    Faculty(fac,5);
    ANumberToString(str, fac, 10);
    Check(str, "120");

    n1.SetTo("25");
    Sqrt(res1, n1);
    ANumberToString(str, res1, 10);
    Check(str, "5");

    n1.iPrecision = 10;
    n1.SetTo("1.50");

    ANumberToString(str, n1, 10);
    Check(str, "1.5");
*/    
/*
    n1.SetTo("");
    n2.SetTo("");
*/
#endif
    Finish();
    return 0;
}
         
