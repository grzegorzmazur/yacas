


#include <stdio.h>
#include "anumber.h"


void Check(LispString& str,char* s)
{
    if (strcmp(str.String(),s))
    {
        printf("ERROR: %s != %s\n",str.String(),s);
    }
}

void Next()
{
    static int i=0;
    i++;
    printf("[%d]",i);
    fflush(stdout);
}

void Finish()
{
    printf("\n");
    fflush(stdout);
}


void Faculty(ANumber& fac,int n)
{
    fac.SetTo("1");
    int i;
    for (i=1;i<=n;i++)
    {
        char ss[100];
        sprintf(ss,"%d",i);
        ANumber f(ss,100);
        ANumber re("1",100);
        re.CopyFrom(fac);
        Multiply(fac,re,f);
    }
}

int main(void)
{
    LispString  str;
    ANumber res1("10",100,10);
    ANumber res2("10",100,10);
    ANumber res3("10",100,10);

    printf("WordBits = %d\n",WordBits);
    printf("WordMask = %ld\n",WordMask);
    printf("Starting tests...\n");

    Next(); //1
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

    Next(); //2

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

    Next();  //3

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

    Next();     //4

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

/*
    {
        char ss[100];
    RED:
        printf("numer = ");
        fscanf(stdin,"%s",ss);
        n1.SetTo(ss);
        printf("denom = ");
        fscanf(stdin,"%s",ss);
        n2.SetTo(ss);
        Divide(res2, res3, n1, n2);
        ANumberToString(str, res2, 10);
        printf("quotient : %s\n",str.String());
        ANumberToString(str, res3, 10);
        printf("remainder : %s\n",str.String());
        goto RED;
    }
    */

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
    
/*
    n1.SetTo("");
    n2.SetTo("");
*/
    Finish();
    return 0;
}
         
