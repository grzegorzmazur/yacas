
#include <stdio.h>
#include <math.h>

template<class T>
inline T MyFabs(const T v)
{
    if (v<0)
        return -v;
    return v;
}

double MyLog(double x,double epsilon)
{
    double result=0;
    double xminusone = -(x-1);
    double term=-1;
    int n=1;

//    printf("MyLog\n");
    // This part converges fast only iff 0 < x < 2
    while (MyFabs(term/n) > epsilon)
    {
        term*=xminusone;
        result+=term/n;
        n++;
    }

    return result;
}

double MyPow(double x, double y,double epsilon)
{
    int revert = 0;
    double result=0;

    if (x<=0)
        return 0;
    
    // x^-y = 1/(x^y)
    if (y<0)
    {
        revert=1;
        y=-y;
    }

    // This part converges fast only iff y>=1
    if (y>=1)
    {
        double xminusone=x-1;
        double term=1;
        int n=0;
        while(MyFabs(term)>epsilon && y-n>-1)
        {
            result=result+term;
            term*=(y-n);
            term*=xminusone;
            n++;
            term/=n;
            //                printf("%lg (%lg)\n",result,term);
        }

        // if this happens it didn't converge after all
        if (y-n <= -1)
        {
//            printf("Result can not be trusted\n");
            goto OTHER;
        }
    }
    else
    {
    OTHER:
        result=0;
        // x = xx*1.5^e
        if (x>1.5)
        {
            double xx=1;
//            printf("shuffle trick\n");
            int e=0;
            xx=x;
            while (xx>1.5)
            {
                xx/=1.5;
                e++;
             }
            result = MyPow(xx,y,epsilon) * MyPow(1.5,e*y,epsilon);
        }
        else // this one only converges iff x < 2, because of the mylog
        {
            double term=1;
            double ylnx = y*MyLog(x,epsilon);
            int n=0;
            while(MyFabs(term)>epsilon)
            {
                result = result + term;
                term*=ylnx;
                n++;
                term/=n;
            }
        }
    }
    
    if (revert)
    {
        result=1/result;
    }
    return result;
}


int main(void)
{
    double x,y,epsilon;

REDO:
    printf("Calculating x^y:\n");
    printf("Enter x: ");
    fscanf(stdin,"%lg",&x);
    printf("Enter y: ");
    fscanf(stdin,"%lg",&y);

    epsilon = 1e-15;
//    printf("Enter precision: ");
//    fscanf(stdin,"%lg",&epsilon);

    double result = MyPow(x,y,epsilon);
    printf("%lg^%lg = %lg\n",x,y,result);
    double other = pow(x,y);
    printf("standard math library returns %lg (difference %lg)\n",pow(x,y),other-result);
    goto REDO;
    return 0;
}
