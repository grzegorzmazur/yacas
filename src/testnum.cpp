


//#include "yacasprivate.h"
#include "numbers.h"
#include <stdio.h>

#define ENABLE_TESTS 1

unsigned failed = 0;
unsigned passed = 0;

void Check(LispString& str, const char* s, const char* test_description)
{
    if (strcmp(str.String(),s))
    {
        printf("@@@@@@@ %s: failed: %s != %s\n",test_description, str.String(),s);
		++failed;
    }
	else
	{
		printf("\t%s: passed\n", test_description);
		++passed;
	}
}

void Check(bool test_condition, const char* test_description)
{
    if (!test_condition)
    {
        printf("@@@@@@@ %s: failed\n",test_description);
		++failed;
    }
	else
	{
		printf("\t%s: passed\n", test_description);
		++passed;
	}
}

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

// test big numbers
void TestTypes2(const char* float_string, const char* int_string, double double_value)
{
	const LispInt base = 10;
	int sign = (double_value>0)?1:((double_value<0)?-1:0);
	
// test constructors from strings
	BigNumber x(float_string, 3*strlen(float_string), base);
	BigNumber y;
	y.SetTo(int_string, 3*strlen(int_string), base);
	y.BecomeInt();	// FIXME: this would be unnecessary if the type were automatically decided based on the string.

	Check(!x.IsSmall(), "x is a big number");
	Check(!y.IsSmall(), "y is a big number");
	
	Check(!x.IsInt(), "x is a float type");
	Check(y.IsInt(), "y is an integer type");
	Check(x.Double()==double_value, "value is correct");
	Check(!x.IsIntValue(), "x has a float value");
	Check(y.IsIntValue(), "y has an integer value");
	Check(x.Sign()==sign, "sign of x is correct");
	Check(y.Sign()==sign, "sign of y is correct");

	LispString str;
	y.ToString(str,base);
	Check(str,int_string,"read integer value correctly");
	BigNumber z(x);
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
    n1.ToString(str,10);
    Check(str,"65535", "reading 65535 from string");
    n1.ToString(str,10);
    Check(str,"65535", "reading 65535 from string again");
    n1.ToString(str,2);
    Check(str,"1111111111111111", "printing in binary");
    n1.Negate(n1);
    n1.ToString(str,10);
    Check(str,"-65535", "negate 65535");

    BigNumber res1;
    res1.Add(n1,n1,10);    
    res1.ToString(str,10);
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
	Next("call some functions on object with undefined value");
	x.IsInt();
	x.Double();	// value is undefined
	Next("construct 0 from string");
	BigNumber y("0", 10);	// construct
	Check(y.Double()==0, "value of 0");

	Next("read binary string");
	y.SetTo("-101010", 50, 2);	// construct with given precision
	Check(y.Double()==-42, "value is correct");
	Check(!y.IsInt(), "has float type");
	Check(y.IsIntValue(), "value is integer");
	Check(y.IsSmall(), "value is small");
	y.BecomeInt();
	Check(y.IsInt(), "has integer type");
	Check(y.IsSmall(), "value is small");

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

	Next("test big integers and floats");
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
	TestTypes2(num2_float, num2_int, num2_double);
	TestTypes2(num3_float, num3_int, num3_double);
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
	
	Next("simple arithmetic");
	TestArith1(num1_int, 10, 15, -25);
	TestArith1(num1_float, 10, -15, 0.19e-8);
	TestArith1(num2_int, 10, -15, -2500000);
	TestArith1(num2_float, 10, -15, 0.19e8);
	TestArith1(num3_int, 10, 1500, 29);
	TestArith1(num3_float, 10,  -15, 0.19);
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
	
	
	
	
	
}
	
#endif

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
    Finish();
    return 0;
}
         
