
/* experimental new design of a big integer class */


// Headers from standard library (in this case standard C/C++ library
#include <stdio.h>
#include <assert.h>
#include <ctype.h>
#include <string>
#include <map>
#include <vector>

using namespace std;


// Class definition: BigInteger
#ifndef __bignumber_h__
#define __bignumber_h__

typedef unsigned short PlatWord;
typedef unsigned long  PlatDoubleWord;

const int PlatDigits = (8*sizeof(PlatWord));
const PlatDoubleWord WordBase = ((PlatDoubleWord)1)<<PlatDigits;

class BigInteger
{
public:
  inline BigInteger();
  inline bool operator==(const BigInteger& aOther) const;
  inline bool operator!=(const BigInteger& aOther) const;
  BigInteger(const string& anInteger); // Create an integer
  BigInteger(const BigInteger& aOther)
    : digits(aOther.digits), negative(aOther.negative) {}
  string ToString() const;
  inline void Add(const BigInteger& aSource);
  inline void MultiplyAdd(const BigInteger& aX, const BigInteger& aY);
  inline void Negate() {negative = !negative;};
protected:
  typedef PlatWord ElementType;      // two bytes
  typedef PlatDoubleWord DoubleElementType; // four bytes
  enum { WordBits = PlatDigits};
  inline void Normalize();
  inline void AddWord(PlatWord aTerm);
  inline void MultWord(PlatWord aFactor);
  inline void DivideWord(PlatDoubleWord aNumber, PlatDoubleWord& aCarry);
private:
  vector<ElementType> digits;
  bool negative;
};


inline BigInteger::BigInteger() : negative(false)
{
  digits.push_back(0);
}
inline bool BigInteger::operator==(const BigInteger& aOther) const
{
  return (negative == aOther.negative && digits == aOther.digits);
}
inline bool BigInteger::operator!=(const BigInteger& aOther) const
{
  return !(*this == aOther);
}

inline void BigInteger::Normalize()
{
  int nr=digits.size();
  while (nr > 1 && digits[nr-1] == 0)
  {
    digits.pop_back();
    nr--;
  }
}


inline void BigInteger::AddWord(PlatWord aTerm)
{
  if (digits.size() == 0)
    digits.push_back(0);
  digits.push_back(0);
  DoubleElementType carry = 0;

  {
    DoubleElementType accu;
    accu = digits[0];
    accu += aTerm;
    accu += carry;
    digits[0] = (ElementType)(accu);
    carry = (accu >> WordBits);
  }
  int i=1;
  while (carry)
  {
    DoubleElementType accu;
    accu = digits[i];
    accu += carry;
    digits[i] = (ElementType)(accu);
    carry = (accu >> WordBits);
    i++;
  }
  Normalize();
}

inline void BigInteger::MultWord(PlatWord aFactor)
{
  unsigned i;
  digits.push_back(0);
  DoubleElementType carry = 0;
  for (i=0;i<digits.size();i++)
  {
    DoubleElementType accu;
    accu = digits[i];
    accu *= aFactor;
    accu += carry;
    digits[i] = (ElementType)(accu);
    carry = (accu >> WordBits);
  }
  assert(carry == 0);
  Normalize();
}


inline void BigInteger::DivideWord(PlatDoubleWord aNumber, PlatDoubleWord& aCarry)
{
  PlatDoubleWord carry=0;
  int i;
  int nr=digits.size();
  for (i=nr-1;i>=0;i--)
  {
    PlatDoubleWord word = ((carry*WordBase)+((PlatDoubleWord)(digits[i])));
    PlatWord digit = (PlatWord)(word / aNumber);
    PlatWord newCarry= (PlatWord)(word % aNumber);
    digits[i] = digit;
    carry= newCarry;
  }
  //carry now is the remainder
  aCarry = carry;
  Normalize();
}


inline void BigInteger::Add(const BigInteger& aSource)
{
  while (digits.size() < aSource.digits.size())
    digits.push_back(0);
  digits.push_back(0);
  unsigned i;
  DoubleElementType carry = 0;
  for (i=0;i<aSource.digits.size();i++)
  {
    DoubleElementType accu;
    accu = digits[i];
    accu += aSource.digits[i];
    accu += carry;
    digits[i] = (ElementType)(accu);
    carry = (accu>>WordBits);
  }
  while (carry)
  {
    DoubleElementType accu;
    accu = digits[i];
    accu += carry;
    digits[i] = (ElementType)(accu);
    carry = (accu>>WordBits);
    i++;
  }
  Normalize();
}


inline void BigInteger::MultiplyAdd(const BigInteger& aX, const BigInteger& aY)
{
  unsigned i,j;
  for (i=aX.digits.size()+aY.digits.size()-digits.size();i>0;--i)
    digits.push_back(0);

  for (i=0;i<aX.digits.size();i++)
  {
    DoubleElementType carry = 0;
    DoubleElementType factor = aX.digits[i];
    for (j=0;j<aY.digits.size();j++)
    {
      DoubleElementType accu;
      accu = digits[i+j]
         + ((DoubleElementType)aY.digits[j])*factor
         + carry;
      digits[i+j] = (ElementType)(accu);
      carry = (accu>>WordBits);
    }

    while (carry)
    {
      DoubleElementType accu;
      accu = digits[i+j] + carry;
      digits[i+j] = (ElementType)(accu);
      carry = (accu>>WordBits);
      j++;
    }
    assert(carry == 0);
  }
  Normalize();
}


#endif // __bignumber_h__







BigInteger::BigInteger(const string& anInteger)
{
  unsigned i=0;
  negative = false;
  if (anInteger[0] == '-')
  {
    negative = true;
    i++;
  }
  for (;i<anInteger.size();++i)
  {
    int digit = anInteger[i]-'0';
    MultWord(10);
    AddWord(digit);
  }
}

string BigInteger::ToString() const
{
  BigInteger zero;
  if (*this == zero) return "0";
  string result;
  BigInteger number(*this);

  while (number != zero)
  {
    PlatDoubleWord digit;
    number.DivideWord(10, digit);
    result.push_back(digit);
  }
  int i,nr=result.size();
  for (i=(nr>>1)-1;i>=0;--i)
  {
    char swp = result[i];
    result[i] = '0' + result[result.size()-i-1];
    result[result.size()-i-1] = '0' + swp;
  }
  if (nr&1) result[(nr>>1)] += '0';
  if (negative) result.insert(0,"-",0,1);
  return result;
}


int main(int argc, char** argv)
{
#define X "123456789"
#define Y "23456789"
#define Z "456789"
  BigInteger x(X);
  BigInteger y(Y);
  BigInteger z(Z);
  BigInteger result;
  result.MultiplyAdd(x,y);
  result.Add(z);
  printf("%s * %s + %s = \n\t%s\n",X,Y,Z,result.ToString().c_str());

/*
In> 123456789 * 23456789 + 456789
Out> 2895899850647310

*/
  BigInteger yacasResult("2895899850647310");
  printf("Yacas says \n\t%s\n",yacasResult.ToString().c_str());

  return 0;
}


