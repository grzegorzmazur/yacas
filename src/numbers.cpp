#include <stdio.h>
#include <string.h>
//#include <stdlib.h>
#include "lisptype.h"
#include "numbers.h"
#include "standard.h"
#include "platmath.h"
#include "errors.h"

#ifdef USE_NEW_BIGNUM

//////////////////////////////////////////////////
///// BigNumber implementation through BigInt/BigFloat
///// (coded by Serge Winitzki)
//////////////////////////////////////////////////

/// The number class describes either integers or floats, depending on the type_ flag. However, both the float part (float_, exponent_) and the integer part (int_) are always initialized for the life of the BigNumber object.
/// Here int_ is a BigInt object and float_ is a BigFloat object.
/// A special mode is that of "exp-float" number. Then the type_ flag has the value KExpFloat. The number is equal to float_ * 2^exponent_ where exponent_ is a big integer and float_ is a normal big float value with a normal exponent. Otherwise the value of the exponent_ stays zero (actually it is ignored).
/// The exp-float mode is to be used only for really large or really small exponents. (not yet implemented)
/// A BigInt/BigFloat object contains valid, initialized number objects at all times. Only one of them is used at any time, depending on the current type of the number.

// TO DO:
// design and implement exp-float, or decide why it is unnecessary.




BigNumber::BigNumber(LispInt aPrecision) { init(aPrecision); }

void BigNumber::init(LispInt aPrecision)
{// this function should be called only from constructors because we don't want to re-initialize BigInt/BigFloat objects
	turn_int();	// by default all numbers are created integer
	iPrecision = aPrecision;	// default
	float_.Precision(aPrecision);
}

BigNumber::~BigNumber()
{
}

// construct from string
BigNumber::BigNumber(const LispCharPtr aString,LispInt aPrecision,LispInt aBase)
{
	// iPrecision is going to be set in SetTo
	init();
	SetTo(aString, aPrecision, aBase);
}


/// copy constructor
BigNumber::BigNumber(const BigNumber& aOther)
{
	init();
	SetTo(aOther);
}


// assign from another number
void BigNumber::SetTo(const BigNumber& aOther)
{
  if (this == &aOther)
	return;
  else
  if (aOther.IsInt())
  {
	turn_int();
	int_.SetTo(aOther.int_);
  }
  else
  {
	turn_float();
	iPrecision = aOther.GetPrecision();
	float_.SetTo(aOther.float_);
	if (aOther.IsExpFloat())
	  exponent_.SetTo(aOther.exponent_);
  }
  type_ = aOther.type_;
}

// assign from string, result is always a float type
void BigNumber::SetTo(const LispCharPtr aString,LispInt aPrecision,LispInt aBase)
{// FIXME: we cannot read expfloats, e.g. 1.3e123412341234123412341234, which should be possible.
  if (aBase<2 || aBase>32)
  {
  	RaiseError("BigNumber::SetTo(string): error: aBase should be between 2 and 32, not %d\n", aBase);
  	return;
  }
	// decide whether the string is an integer or a float
  if (strchr(aString, '.') || aBase<=10 && (strchr(aString, 'e') || strchr(aString,'E')) || strchr(aString, '@'))
  {	// converting to a float
	// estimate the number of bits we need to have
	  // find the first significant digit:
	  LispInt digit1 = strspn(aString, ".-0");	// initial zeros are not significant
	 // find the number of significant base digits (sig_digits)
	  LispInt sig_digits = strcspn(aString+digit1, (aBase<=10) ? "-eE@" : "-@"); // trailing zeros and . *are* significant, do not include them in the sets
	  if (sig_digits<=0)
	  {	// this is when we have "0." in various forms
		  sig_digits=1;
		  // precision of 0.: for now let's set it to 1 significant digits (in principle we might allow things like 0.00e10 to denote special floating zeros but it's probably not very useful, since our floating zeros have binary precision and not decimal)
	  }
	  else
	  {	// our number is nonzero
		  if (strchr(aString+digit1, '.'))
			  sig_digits--;	// this is when we have "1.000001" where "." is not a digit, so need to decrement
	  }
	  // ok, so we need to represent MAX(aPrecision,sig_digits) digits in base aBase
	  iPrecision=(LispInt) digits_to_bits(MAX(aPrecision,sig_digits), aBase);

	  turn_float();
	  float_.SetTo(aString, iPrecision+GUARD_BITS, aBase);
  }
  else
  {	// converting to an integer, aPrecision is ignored
    turn_int();
  	int_.SetTo(aString, aBase);
  }
}


// assign from a platform type
void BigNumber::SetTo(long value)
{
  turn_int();
  int_.SetTo(value);
}


void BigNumber::SetTo(double value)
{
  turn_float();
  float_.SetTo(value);
  iPrecision = 53;	// standard double has 53 bits
}


// Convert back to other types
/// ToString : return string representation of the number at given precision (base digits) in aResult

void BigNumber::ToString(LispString& aResult, LispInt aPrecision, LispInt aBase) const
{
  if (aBase<2 || aBase>32)
  {
  	RaiseError("BigNumber::ToString: error: aBase should be between 2 and 32, not %d\n", aBase);
  	return;
  }
  if (IsInt())
  {	// find how many chars we need
	LispInt size=(LispInt) bits_to_digits(int_.BitCount(), aBase);
	char* buffer=(char*)malloc(size+2);
	int_.ToString(buffer, size, aBase);
	// assign result
	aResult.SetStringCounted(buffer, strlen(buffer));
	free(buffer);
  }
  else
  {	// we have a floating-point number. Note that we cannot print exp-float numbers (except in base 2^n).
  // FIXME: maybe allow printing exp-float numbers in bases 2, 4, 8, 16?
    if (IsExpFloat())
    {
	    RaiseError("BigNumber::ToString: printing exp-floats is not implemented\n");
	    return;
    }
	// note: aPrecision means *base digits* here. Print not more than aPrecision digits.
	LISPASSERT(aPrecision>0);
	if (aPrecision <=0)
		aPrecision = 1;	// refuse to print with 0 or fewer digits
    unsigned long size=(unsigned long)aPrecision;
    // how many precise base digits we have. Print not more than print_prec digits.
    unsigned long print_prec = bits_to_digits(iPrecision, aBase);
    print_prec = MIN(print_prec, (unsigned long)aPrecision);
	
    // the size needed to print the exponent cannot be more than 200 chars since we refuse to print exp-floats
    size += 200;
    LispCharPtr buffer=(LispCharPtr)malloc(size);
    if (!buffer)
    {
	    RaiseError("BigNumber::ToString: out of memory, need %ld chars\n", size);
	    return;
    }
    LispCharPtr offset = buffer;
	LispCharPtr end_of_buffer = buffer+size-1;
    if (Sign()==0)
    {    // according to the latest revelations, the floating-point zero must be printed as "0."
	    strcpy(offset, "0."); // end of string is here too
    }
    else
    {
	// print a number using fixed point if the exponent is between -4 and +8
	const long lower_exp=-4, upper_exp=8;
	long exp_small;
	// get the exponent in given base and print the string at the same time
	// string is printed starting at an offset to allow for leading zeros/decimal point
	offset += 2-lower_exp;
	// print mantissa
	float_.GetMantissaExp(offset, end_of_buffer-offset, &exp_small, print_prec, aBase);
	if (lower_exp <= exp_small && exp_small <= upper_exp)
	{	// print in fixed point.
		if (exp_small>0)
		{	// Insert a point somewhere in the middle. Uses memmove() if needed
			// *offset contains something like "-123" and exp_small is 2, then we need to get "-12.3". Or perhaps we had "-123" and exp_small=5, then we need to get "-12300."
			// point position
			char* point_pos = offset + exp_small;
			if (Sign()<0) ++point_pos;
			// decide if the point is within the printed part of the string
			size=strlen(offset);
			if (point_pos-offset>=(signed)size) // ok here because point_pos-offset is always positive if exp_small>0
			{	// no moving required, but perhaps need to pad with zeros, and we need to add the point
				int i=size;
				for(; i<point_pos-offset; ++i)
				  offset[i] = '0';
				strcpy(offset+i, ".");	// end of string
			}
			else
			{	// need to insert the point and shift the rest of the string
				memmove(point_pos+1, point_pos, size-(point_pos-offset)+1);
				*point_pos = '.';
			}
		}
		else
		{// add leading zeros and decimal point, handle sign
		// we need to add (1-exp_small) zeros total
			if (Sign()<0)
			{	// *offset contains something like "-123..."
				// and instead we need -0.00123... (if small_exp=-2)
				// final offset:
				offset = offset-(2-exp_small);
				memcpy(offset, "-0.",3);	// 3 chars added
				// fill more zeros
				for (int i=3; i<3-exp_small; ++i)
				  offset[i] = '0';
				// done printing
			}
			else	// the number is surely nonzero.
			{	// *offset contains something like "123..."
				// and instead we need 0.00123... (if small_exp=-2)
				// final offset:
				offset = offset-(2-exp_small);
				memcpy(offset, "0.",2);	// 2 chars added
				// fill more zeros
				for (int i=2; i<2-exp_small; ++i)
				  offset[i] = '0';
				// done printing to buffer
			}
		}	// end of printing numbers < 1
	}	// end of printing in fixed point
	else
	{ // printing in floating point
		// if the number is negative, need to handle the - sign
		offset -= 2;	// final offset already known
		if (Sign()<0)
			memcpy(offset, "-0.", 3);
		else
			memcpy(offset, "0.", 2);
		
		// now print the exponent: either "e" or "@" and then the long integer
		LispInt mantissa_size = strlen(offset);
		offset[mantissa_size] = (aBase<=10) ? 'e' : '@';	// one character there, start exponent at offset size+1. Printing as per GMP instructions.
		// compute the total exponent
		BigInt exp_total;
		exp_total.SetTo((LispInt)exp_small);
		char* exp_start_pos = offset+mantissa_size+1;
		exp_total.ToString(exp_start_pos, end_of_buffer - exp_start_pos, aBase);
	}
	// assign result
     }	// finished printing a float
     aResult.SetStringCounted(offset, MIN(int(strlen(offset)), end_of_buffer - offset));
     free(buffer);
   } // finished printing a nonzero number
}

/// Give approximate representation as a double number
double BigNumber::Double() const
{
  if(IsInt())
	return int_.Double();
  else if (IsExpFloat())
  {
	  RaiseError("BigNumber::Double: error: cannot convert an ExpFloat to double\n");
	  return 0;
  }
  else
	return float_.Double();
}

/// get library name
const LispCharPtr BigNumber::NumericLibraryName()
{
	return BigInt::NumericLibraryName();
}



//basic object manipulation
LispBoolean BigNumber::Equals(const BigNumber& aOther) const
{
// bit counts and signs must not be different
  long B_x = this->BitCount(), B_y = aOther.BitCount();
  if (B_x != B_y || this->Sign() * aOther.Sign() == -1)
  	return LispFalse;
  // any two zeros are equal
  if (this->Sign() == 0 && aOther.Sign() == 0)
  	return LispTrue;
  // now check for zero compared with nonzero
  if (
		  // *this is zero and aOther is nonzero
		  this->Sign() == 0 && B_y < - this->GetPrecision()
		  ||
		  // *this is zero and aOther is nonzero
		  aOther.Sign() == 0 && B_x < - aOther.GetPrecision()
	) return LispTrue;
  // now we may assume that both are nonzero and that the bit counts are equal
  if (this->IsInt())
    if (aOther.IsInt())
    	return int_.Equals(aOther.int_);
    else	// comparing integers with floats: must convert both to floats
    {	// *this is integer, aOther is float
		// convert *this to a float and compare as floats
    	BigNumber x(*this);
		x.BecomeFloat(aOther.GetPrecision()+1);	// 1 guard digit here
		return x.Equals(aOther);
    }
  else
    if (aOther.IsInt())
    {	// *this is float, aOther is integer
	return aOther.Equals(*this);
    }
    else
    // two floats x, y are equal if |x-y| < max(Delta x, Delta y), where Delta x is the absolute error of x
    {
    	BigNumber x_minus_y(*this);
	// compute min(m,n)
	long min_prec = MIN(this->GetPrecision(), aOther.GetPrecision());
	// compute x-y
	x_minus_y.Negate(x_minus_y);
	x_minus_y.Add(x_minus_y, aOther, min_prec);
	// compute Abs(x-y)
	if (x_minus_y.Sign()<0) x_minus_y.Negate(x_minus_y);
	long B_x_minus_y = x_minus_y.BitCount();
	// perform quick checks on signs and bit counts
	if (x_minus_y.Sign()==0)	// got "exact" float 0, bit count will be wrong
		return LispTrue;
	if (B_x_minus_y <= B_y-min_prec-1)
		return LispTrue;
	if (B_x_minus_y > B_y-min_prec+1)
		return LispFalse;
	// quick checks didn't work, compute Abs(x)*2^(-m) and Abs(y)*2^(-n)
	BigNumber abs_x(*this);
	if (abs_x.Sign()<0) abs_x.Negate(abs_x);
	abs_x.ShiftRight(abs_x, this->GetPrecision());
	BigNumber abs_y(aOther);
	if (abs_y.Sign()<0) abs_y.Negate(abs_y);
	abs_y.ShiftRight(abs_y, aOther.GetPrecision());
	// return true if Abs(x-y) < Abs(x)*2^(-m) or if Abs(x-y) < Abs(y)*2^(-n)
	return x_minus_y.float_.LessThan(abs_x.float_) || x_minus_y.float_.LessThan( abs_y.float_);
    }
}


LispBoolean BigNumber::IsInt() const
{
	return (type_ & KInt)!=0;
}


LispBoolean BigNumber::IsExpFloat() const
{
	return (type_ & KExpFloat)==KExpFloat;
}

LispBoolean BigNumber::IsIntValue() const
{
	// *this has integer value if it's an integer or if it does not have enough digits to be non-integer, or if it's exactly equal to an integer
  if (IsInt() || GetPrecision() < BitCount() || float_.IsIntValue())
	  return LispTrue;
  // check if the number is integer within its precision
  // compute y=x-Floor(x)
  BigNumber y;
  y.Floor(*this);
  y.Negate(y);
  y.Add(y,*this,GetPrecision()+1);	// 1 guard digit here
  // return true if B(y)<-n
  return y.BitCount() < - this->GetPrecision();
}


LispBoolean BigNumber::IsSmall() const
{
  if (IsInt())
  	return int_.IsSmall();
  else
  // determine whether a BigFloat fits into double.
  {
	long exp_small = float_.GetBinaryExp();
	return
	(
		!IsExpFloat() 
		&& iPrecision <= 53	// standard float is 53 bits
		&& (exp_small<=1021 && exp_small >= -1023)
	);
	// standard range of double precision is about 53 bits of mantissa and binary exponent of about 1021
  }

}


void BigNumber::BecomeInt()
{
  if (!IsInt())
  {
	iPrecision = 1;	// arbitrary but nonnegative
	turn_int();
	int_.SetTo(float_);
  }
}


/// Transform integer to float, setting a given bit precision.
/// Note that aPrecision=0 means automatic setting (just enough digits to represent the integer).
void BigNumber::BecomeFloat(LispInt aPrecision)
{
  if (IsInt())
  {	// the precision of the resulting float is at least the number of bits in the int_, i.e. its bit count
	iPrecision = MAX((LispInt)BitCount(), aPrecision)+GUARD_BITS;
	float_.SetTo(int_, iPrecision);
  	turn_float();
	LISPASSERT(iPrecision >= 0);
  }
}


LispBoolean BigNumber::LessThan(const BigNumber& aOther) const
{ // first check whether the two numbers are equal up to precision
  if (this->Equals(aOther))
  	return LispFalse;
  // not equal, now we can simply compare the values
  if (IsInt())
    if (aOther.IsInt())
		return int_.LessThan(aOther.int_);
    else
    {	// need to temporarily convert *this to a float
    	BigNumber temp(*this);
		temp.BecomeFloat();
    	return temp.LessThan(aOther);
    }
  else
    if (aOther.IsInt())
    {	// need to temporarily convert aOther to a float
    	BigNumber temp(aOther);
		temp.BecomeFloat();
    	return this->LessThan(temp);
    }
    else
  	return float_.LessThan(aOther.float_);
	// FIXME: this does not work for KExpFloat
}


//arithmetic
/// Multiply two numbers at given precision and put result in *this
void BigNumber::Multiply(const BigNumber& aX, const BigNumber& aY, LispInt aPrecision)
{
  if (aX.IsInt())
    if (aY.IsInt())	// both integer
    {
      if (!IsInt()) turn_int(); // if we are float, then we are not aX or aY, can clear our values
      int_.Multiply(aX.int_, aY.int_);
    }
    else	// int + float, need to promote to float
    {
      if (aX.Sign()==0)
      {	// multiplying by integer 0, set result to integer 0
        SetTo(0);
      }
      else
      {	// multiplying by nonzero integer, precision is unmodified
          BigNumber temp(aX);
          temp.BecomeFloat(aY.GetPrecision()+GUARD_BITS);	// enough digits here
          Multiply(temp, aY, aPrecision);
        iPrecision = MIN(aPrecision, aY.GetPrecision());
      }
    }
  else
    if (aY.IsInt())	// float + int, need to promote to float
    {
      Multiply(aY, aX, aPrecision);
    }
    else	// float + float
    {
      if (IsInt()) turn_float(); // if we are int, then we are not aX or aY, can clear our values
	  if (aX.Sign()*aY.Sign()==0)
	  {	// one or both are zero
		// use a modified definition of BitCount now
		long B_x = (aX.Sign()==0) ? 1-aX.GetPrecision() : aX.BitCount();
		long B_y = (aY.Sign()==0) ? 1-aY.GetPrecision() : aY.BitCount();
		long xy_prec = 2-B_x-B_y;
		SetTo(0.);
		iPrecision = xy_prec;
	  }
	  else
	  {
		// both aX and aY are nonzero
		long xy_prec = MIN(aX.GetPrecision(), aY.GetPrecision()) - DIST(aX.GetPrecision(), aY.GetPrecision());
		// set our new precision
		long real_xy_prec = MIN((long)aPrecision, xy_prec)+GUARD_BITS;
		float_.Multiply(aX.float_, aY.float_, real_xy_prec);
		iPrecision = xy_prec;
	  }
      // FIXME: this does not work for KExpFloat
    }
}


/* Multiply two numbers, and add to *this (this is useful and generally efficient to implement).
*/
void BigNumber::MultiplyAdd(const BigNumber& aX, const BigNumber& aY, LispInt aPrecision)
{
  if (aX.IsInt() && aY.IsInt() && IsInt())
  {// all three are integers
	int_.MultiplyAdd(aX.int_, aY.int_);
  }
  else
  if (!aX.IsInt() && !aY.IsInt() && !IsInt())
  {// all three are floats
	  BigNumber temp;
	  temp.Multiply(aX, aY, aPrecision);
	  Add(*this, temp, aPrecision);
  }
  else
  {	// some are integers and some are floats. Need to promote all to floats and then call MultiplyAdd again
    if (IsInt())
    {
    	this->BecomeFloat(aPrecision+GUARD_BITS);
	MultiplyAdd(aX, aY, aPrecision);
    }
    else
    if (aX.IsInt())
    {
    	BigNumber temp(aX);
	temp.BecomeFloat(aPrecision+GUARD_BITS);
	MultiplyAdd(temp, aY, aPrecision);
    }
    else
    if (aY.IsInt())
    {	// need to promote both aX and aY to floats
      	BigNumber temp(aY);
	temp.BecomeFloat(aPrecision+GUARD_BITS);
	MultiplyAdd(aX, temp, aPrecision);
    }
  }
}


/// Add two numbers at given precision and return result in *this
void BigNumber::Add(const BigNumber& aX, const BigNumber& aY, LispInt aPrecision)
{
  if (aX.IsInt())
    if (aY.IsInt())	// both integer
    {
      if (!IsInt())
		  turn_int(); // if we are float, then we are not aX or aY
      int_.Add(aX.int_, aY.int_);
    }
    else	// int + float, need to promote to float
    {
      BigNumber temp(aX);
      temp.BecomeFloat(MAX((long)0, 1 + aY.GetPrecision()+aX.BitCount()-aY.BitCount()) +4);	// 4 guard bits here
      Add(temp, aY, aPrecision);
    }
  else
    if (aY.IsInt())	// float + int, need to promote to float
    {
    	Add(aY, aX, aPrecision);
    }
    else	// float + float
    {
      if (IsInt()) turn_float(); // if we are int, then we are not aX or aY, can clear our values
	  // bit counts of aX, aY, and of absolute errors DeltaX, DeltaY
	  long B_x = aX.BitCount();
	  long B_y = aY.BitCount();
	  long B_Dx = B_x-aX.GetPrecision();
	  long B_Dy = B_y-aY.GetPrecision();
	  // check for apriori underflow
	  if (B_x<=B_Dy-1)
	  {	// neglect x, assign z=y, set precision
		  if (this!= &aY)
		  {
			  float_.SetTo(aY.float_);
		  }
		  iPrecision = MIN((long)aPrecision, aY.GetPrecision()-DIST(B_x, B_Dy-1));
	  }
	  else if (B_y<=B_Dx-1)
	  {	// neglect y, assign z=x, set precision
		  Add(aY, aX, aPrecision);
	  }
	  else	 // no apriori underflow
	  {
		  // precision needed for computing x+y
		  long xy_prec = 1+MAX(B_x, B_y)-MAX(B_Dx, B_Dy);
		  // need to introduce a temporary here, because otherwise we might wrongly truncate float_
		  // precision with which we shall actually be performing the addition
		  long real_xy_prec = xy_prec+GUARD_BITS;
		  BigFloat result;
		  result.Precision(real_xy_prec);
    	  result.Add(aX.float_, aY.float_, real_xy_prec);
		  long B_z = result.GetBinaryExp();	// bit count of z=x+y
		  // compute the actual precision p of the result (z)
		  // not always optimal but a good first value:
		  long p = B_z - 1 - MAX(B_Dx, B_Dy) - DIST(B_Dx, B_Dy);
		  if (B_Dx > B_Dy && B_x > B_y || B_Dx < B_Dy && B_x < B_y)
		  {	// the error of x dominates and the value of x also dominates, or ditto for y
			  p++;
		  }
		  // check for minimum roundoff (when both arguments are of the same sign)
		  if (aX.Sign()*aY.Sign()==1)
		  {
			  p = MAX(p, (long)MIN(aX.GetPrecision(), aY.GetPrecision()));
		  }
		  // do not make the result more precise than asked
		  p = MIN((long)aPrecision, p);
		  // check for underflow again
		  if (p < 0 && result.Sign()!=0)
		  {	// underflow, set result to zero and reset precision
			  p = p-B_z;
			  result.SetTo(0);
		  }

		  float_.SetTo(result);
		  iPrecision = p;
	  }	// handled no apriori underflow
      // FIXME: this does not work for KExpFloat
    }	// handled float + float
}


/// Negate the given number, return result in *this
void BigNumber::Negate(const BigNumber& aX)
{
  if (aX.IsInt())
  {
    if (!IsInt()) turn_int();
    int_.Negate(aX.int_);
  }
  else
  {
    if (IsInt()) turn_float();	// we are not aX
    float_.Negate(aX.float_);
  }
  // precision is unchanged
  iPrecision = aX.GetPrecision();
}


/// Divide two numbers and return result in *this. Note: if the two arguments are integer, it should return an integer result!
void BigNumber::Divide(const BigNumber& aX, const BigNumber& aY, LispInt aPrecision)
{
  if (aY.Sign()==0)
  {	// zero division, report and do nothing
  	RaiseError("BigNumber::Divide: zero division request ignored\n");
	return;
  }
  if (aX.IsInt())
  {
	// check for zero
	if (aX.Sign()==0)
	{	// divide 0 by something, set result to integer 0
		SetTo(0);
	}
	else if (aY.IsInt())
    {
    	if (!IsInt()) turn_int();
		int_.Div(aX.int_, aY.int_);	// e.g. divide -5/3 = -1
    }
    else
	{	// divide nonzero integer by nonzero float, precision is unmodified
   	  BigNumber temp(aX);
   	  temp.BecomeFloat(aY.GetPrecision()+GUARD_BITS);	// enough digits here
	  long p = MIN(aPrecision, aY.GetPrecision());
   	  Divide(temp, aY, p);
	  iPrecision = p;
    }
  }
  else	// aX is a float, aY is nonzero
  {
	// check for a floating zero
	if (aX.Sign()==0)
	{
		// result is 0. with precision m-B(y)+1
		long p = aX.GetPrecision()-aY.BitCount()+1;
		SetTo(0.);
		iPrecision = p;
	}
    else if (aY.IsInt())
    {	// aY is integer, must be promoted to float
      BigNumber temp(aY);
      temp.BecomeFloat(MIN(aPrecision, aX.GetPrecision())+GUARD_BITS);
      Divide(aX, temp, aPrecision);
    }
    else
    {	// both aX and aY are nonzero floats
		if (IsInt()) turn_float();	// we are not aX or aY
		long p = MIN(aX.GetPrecision(), aY.GetPrecision()) - DIST(aX.GetPrecision(), aY.GetPrecision());
		p = MIN((long)aPrecision, p);
		float_.Precision(p+GUARD_BITS);
		float_.Divide(aX.float_, aY.float_, p+GUARD_BITS);
		iPrecision = p;
    }
  }
	// FIXME: this does not work for KExpFloat
}



/// integer operation: *this = y mod z
void BigNumber::Mod(const BigNumber& aY, const BigNumber& aZ)
{
  if (aY.IsInt() && aZ.IsInt())
  {
  	if (!IsInt()) turn_int();
  	int_.Mod(aY.int_, aZ.int_);
  }
}

void BigNumber::Floor(const BigNumber& aX)
{
// check that aX is a float and that it has enough digits to evaluate its integer part
  if (!aX.IsInt() && !aX.IsExpFloat())
  {	// now aX is a float for which we can evaluate Floor()
    if(aX.GetPrecision() >= aX.BitCount())
    {	// now aX is a float for which we can evaluate Floor()
      turn_float();	// just in case we are integer; we are not aX
      // we are float now
      float_.Floor(aX.float_);
      BecomeInt();	// we are integer now
    }
    else
    {	// do not have enough precision, raise error
      RaiseError("BigNumber::Floor: error: not enough precision of argument %e (%ld bits)", aX.Double(), aX.GetPrecision());
      SetTo(aX);
    }
  }
  else if (this != &aX) // no change for integers or for exp floats, or if we don't have enough digits; just assign the value
  {
	SetTo(aX);
  }

  LISPASSERT(iPrecision >= 0);
}

// round to a given precision (in bits) and set target precision. Does not change the number if the current precision is lower, or if the number is an integer.
void BigNumber::Precision(LispInt aPrecision)
{
  if (!IsInt())
  {	// set precision flags
	LISPASSERT(aPrecision >= 0);
	iPrecision = aPrecision;
	aPrecision = MAX(aPrecision + (LispInt)GUARD_BITS, 1);	// pretend that the requested precision is actually larger
  	float_.Precision(aPrecision);

	// determine the binary exponent
	long exp_small = float_.GetBinaryExp();
	// determine the shift amount
	long shift_amount = exp_small - aPrecision;
	// truncate at aPrecision bits
	float_.Multiply2exp(float_, - shift_amount);
	float_.Floor(float_);
	float_.Multiply2exp(float_, shift_amount);
  }	// do nothing if integer
}



/// Bitwise operations, return result in *this.
void BigNumber::ShiftLeft(const BigNumber& aX, LispInt aNrToShift)
{
  if (aNrToShift>=0)
  {
    if (aX.IsInt() && aNrToShift >= 0)
    {
  	if (!IsInt()) turn_int();
		int_.ShiftLeft(aX.int_, aNrToShift);
    }
    else
    {
  	if (IsInt()) turn_float();	// we are not aX
	float_.Multiply2exp(aX.float_, aNrToShift);
    }
  }	// do nothing if the shift amount is negative
}


void BigNumber::ShiftRight(const BigNumber& aX, LispInt aNrToShift)
{
  if (aNrToShift>=0)
  {
    if (aX.IsInt() && aNrToShift >= 0)
    {
  	if (!IsInt()) turn_int();
	int_.ShiftRight(aX.int_, aNrToShift);
    }
    else
    {
  	if (IsInt()) turn_float();	// we are not aX
	float_.Multiply2exp(aX.float_, - aNrToShift);
    }
  }	// do nothing if the shift amount is negative
}


void BigNumber::ShiftLeft(const BigNumber& aX, const BigNumber& aNrToShift)
{
// first, see if we can use short numbers
  if (aNrToShift.IsInt() && aNrToShift.Sign()>=0)
  {
    if (aNrToShift.IsSmall())
    {
  	long shift_amount=(long)aNrToShift.Double();
	ShiftLeft(aX, shift_amount);
    }
    else
    {// only floats can be shifted by a non-small number, so convert to float and use exponent_
    // FIXME: make this work for large shift amounts
    }
  }	// do nothing if shift amount is not integer or negative
}


void BigNumber::ShiftRight(const BigNumber& aX, const BigNumber& aNrToShift)
{
// first, see if we can use short numbers
  if (aNrToShift.IsInt() && aNrToShift.Sign()>=0)
  {
    if (aNrToShift.IsSmall())
    {
  	long shift_amount=(long)aNrToShift.Double();
	ShiftRight(aX, shift_amount);
    }
    else
    {// only floats can be shifted by a non-small number, so convert to float and use exponent_
    // FIXME: make this work for large shift amounts
    }
  }	// do nothing if shift amount is not integer or negative
}


void BigNumber::BitAnd(const BigNumber& aX, const BigNumber& aY)
{
  if (aX.IsInt() && aY.IsInt())
  {
  	if (!IsInt()) turn_int();
	int_.BitAnd(aX.int_, aY.int_);
  }	// do nothing if the arguments are not integer
}


void BigNumber::BitOr(const BigNumber& aX, const BigNumber& aY)
{
  if (aX.IsInt() && aY.IsInt())
  {
  	if (!IsInt()) turn_int();
	int_.BitOr(aX.int_, aY.int_);
  }
}


void BigNumber::BitXor(const BigNumber& aX, const BigNumber& aY)
{
  if (aX.IsInt() && aY.IsInt())
  {
  	if (!IsInt()) turn_int();
	int_.BitXor(aX.int_, aY.int_);
  }
}


void BigNumber::BitNot(const BigNumber& aX)
{
  if (aX.IsInt())
  {
  	if (!IsInt()) turn_int();
	int_.BitNot(aX.int_);
  }
}


/// return true if the bit count fits into signed long
LispBoolean BigNumber::BitCountIsSmall() const
{
	return !IsExpFloat();
}

// give BitCount as platform integer; if it is an expfloat, give the bit count of the float part
signed long BigNumber::BitCount() const
{
  if (Sign()==0)
	  return 1;	// BitCount(0)=1
  if (IsInt())
  {
	  return int_.BitCount();
  }
  else
  {
	  return float_.GetBinaryExp();
  }
}
/// Bit count operation: return the number of significant bits if integer, return the binary exponent if float (shortcut for binary logarithm)
void BigNumber::BitCount(const BigNumber& aX)
{
	SetTo(aX.BitCount());
	if (aX.IsExpFloat())
		int_.Add(int_, aX.exponent_);
}



/// Give sign (-1, 0, 1)
LispInt BigNumber::Sign() const
{
  if (IsInt())
  {
  	return int_.Sign();
  }
  else
  {
  	return float_.Sign();
  }
}

/// these functions do not change the number but merely prepare its type
void BigNumber::turn_float()
{
//  if (IsInt())
  {
    type_ = KFloat;
  }
}

void BigNumber::turn_int()
{
//  if (!IsInt())
  {
    type_ = KInt;
  }
}

// copy from low-level objects
void BigNumber::ImportBigInt(const BigInt& aX)
{
	turn_int();
	int_.SetTo(aX);
}
void BigNumber::ImportBigFloat(const BigFloat& aX)
{
	turn_float();
	float_.SetTo(aX);
}
// copy to BigInt/BigFloat objects
void BigNumber::ExportBigInt(BigInt& aX) const
{
	aX.SetTo(int_);
}
void BigNumber::ExportBigFloat(BigFloat& aX) const
{
	aX.SetTo(float_);
}


//////////////////////////////////////////////////
///// End of BigNumber implementation through BigInt/BigFloat
//////////////////////////////////////////////////

#endif	// USE_NEW_BIGNUM
