'''Interface yacas (Yet Another Computer Algebra System) to Python

This module is provides symbolic math capability similar to the Matlab
Symbolic Toolbox (http://www.mathworks.com/products/symbolic/). It is
not yet as capable but could get there (I think).

I am using yacas (http://www.xs4all.nl/~apinkus/yacas.html) compiled
as an extension for Python. It has a text-in, text-out interface that
Ayal Pinkus kindly provided. Yacas is very programmable and I expect
its capabilities to grow over time. It already provides most of what I
needed when I wrote this.

This Python code was written by Gary Bishop in July of 2002. It is
free for any use. If you or your lawyer are stupid enough to believe
that I have any liability for this code, then do not use
it. Otherwise, be my guest.

'''

import os, re, types

import yacasc
'''yacasc is the extension module that embeds yacas'''

from Numeric import *
'''import Numeric so that we can get the deep magic (how does it work?)
that converts sin(foo) into foo.sin() for classes'''

# initialize yacas, this should probably be a more pythonic object interface
yacasc.yacas_init()

class Yacas:
  '''Wraps the static c-style yacas interface in a class'''
  __shared_state = {}
  def __init__(self):
    self.__dict__ = self.__shared_state

  def eval_string(self, string):
    '''pass a string to Yacas and return the result string'''
    yacasc.yacas_eval(string)
    err = yacasc.yacas_error()
    if err:
      raise YacasError(err)
    res = yacasc.yacas_result()
    if not res:
      return ""
    if res[-1] == ';':
      res = res[:-1]
    if res[0] == '"':
      res = res[1:-1]
    # bash away spaces and formatting
    res = " ".join(res.split())
    #print 'eval_string returns', res
    return res

  def eval_lisp(self, arg):
    '''pass a string representing a Lisp expression to Yacas and return the output Lisp string'''
    arg = 'FullForm(Eval(FromString("%s")LispRead()))' % arg
    yacasc.yacas_eval(arg)
    err = yacasc.yacas_error()
    if err:
      raise YacasError(err)
    res = yacasc.yacas_output() # get the printed output from FullForm
    if not res:
      return ""
    res = " ".join(res.split())
    #print 'eval_lisp returns',res
    return res

  def eval_sym(self, in_tree):
    '''pass an expression tree to Yacas and return an expressio tree'''
    in_string = in_tree.toYacas()
    #print 'in_string=',in_string
    out_string = self.eval_lisp(in_string)
    #print 'out_string=', out_string
    out_tree = FromYacas(out_string).GetExpr()
    return out_tree

# Too many globals
yacas = Yacas()

class YacasError(Exception):
  '''Raise these to signal errors from Yacas'''
  def __init__(self, value):
    self.value = value
  def __str__(self):
    return `self.value`
  
class Sym:
  '''Symbolic objects for Python to use

  Ordinary Python entities like numbers and arrays are just
  represented by their value. Symbolic variables are represented by
  their name as a string. Expressions are represented by SymExpr
  objects.

  '''
  def __init__(self, value):
    '''convert an arbitrary thing into a Symbolic thing'''
    self.value = value

  def Simplify(self):
    '''Ask yacas to simplify the expression'''
    return yacas.eval_sym(SymExpr('Simplify', (self,)))

  def Deriv(self, var):
    '''Take the derivative with respect to the symbolic variable'''
    return yacas.eval_sym(SymExpr('Deriv', (var, self)))

  def Evaluate(self, **kwargs):
    '''Evaluate the expression with variables bound by the keyword arguments

    Example:
    a = Sym('a')
    b = Sym('b')
    c = 2 * a ** b
    c.Evaluate(a=2, b=3)

    should produce 16'''
    
    return eval(str(self), globals(), kwargs)

  # implement the arithemetic operators
  # I bet there is a fancy way to do this without all this repitition but it
  # might be really hard to grok...
  def __add__(self, other):
    return SymExpr('+', (self, other))

  def __sub__(self, other):
    return SymExpr('-', (self, other))

  def __mul__(self, other):
    return SymExpr('*', (self, other))

  def __div__(self, other):
    return SymExpr('/', (self, other))

  def __pow__(self, other):
    return SymExpr('**', (self, other))

  
  def __radd__(self, other):
    return SymExpr('+', (other, self))

  def __rsub__(self, other):
    return SymExpr('-', (other, self))

  def __rmul__(self, other):
    return SymExpr('*', (other, self))

  def __rdiv__(self, other):
    return SymExpr('/', (other, self))

  def __rpow__(self, other):
    return SymExpr('**', (other, self))
  
  def __neg__(self):
    return SymExpr('-', (self,))

  def __pos__(self):
    return self

  def __abs__(self):
    return SymExpr('abs', (self,))

  def sin(self):
    return SymExpr('sin', (self,))

  def cos(self):
    return SymExpr('cos', (self,))

  def tan(self):
    return SymExpr('tan', (self,))

  def arcsin(self):
    return SymExpr('arcsin', (self,))

  def arccos(self):
    return SymExpr('arccos', (self,))

  def arctan(self):
    return SymExpr('arctan', (self,))

  def sqrt(self):
    return SymExpr('sqrt', (self,))

  def exp(self):
    return SymExpr('exp', (self,))

  def log(self):
    return SymExpr('log', (self,))

  def conjugate(self):
    return SymExpr('conjugate', (self,))

  def __str__(self):
    return toPython(self.value)

  def toYacas(self):
    return toYacas(self.value)

  def toPython(self, prec=0):
    return toPython(self.value)
  
  def __coerce__(self, other):
    if type(self) == type(other):
      #print 'coerce same',self, other
      return (self, other)
    else:
      #print 'coerce diff', self, other
      return (self, Sym(other))

def toPython(obj, prec=0):
  try:
    return obj.toPython(prec)
  except AttributeError:
    return str(obj)

def toYacas(obj):
  try:
    return obj.toYacas()
  except AttributeError:
    if type(obj) == types.ComplexType:
      return '(Complex %s %s)' % (real(obj), imag(obj))
    else:
      return str(obj)

class SymExpr(Sym):
  _operator_precedence = { '+':1, '-':1, '*':2, '/':2, '**':3 }

  _to_yacas_map = { '**':'^', 'abs':'Abs', 'sin':'Sin', 'cos':'Cos', 'tan':'Tan',
                    'arcsin':'ArcSin', 'arccos':'ArcCos', 'arctan':'ArcTan',
                    'exp':'Exp', 'log':'Log', 'sqrt':'Sqrt', 'conjugate':'Conjugate' }
  
  def __init__(self, op, operands):
    self.op = op
    self.operands = operands

  def toPython(self, prec):
    try:
      myprec =  self._operator_precedence[self.op]
    except KeyError:
      # function call
      if len(self.operands) > 0:
        result = '%s(%s' % (self.op, toPython(self.operands[0]))
        for o in self.operands[1:]:
          result = result + ', %s' % toPython(o)
        result = result + ')'
      else:
        result = '%s()' % self.op
      return result
    else:
      if len(self.operands) == 2:
        left = toPython(self.operands[0], myprec)
        right = toPython(self.operands[1], myprec)
        if myprec < prec:
          return '(%s%s%s)' % (left, self.op, right)
        else:
          return '%s%s%s' % (left, self.op, right)
      elif len(operands) == 1:
        return '%s %s' % (self.op, toPython(self.operands[0], 5))

  def __str__(self):
    return self.toPython(0)
  
  def toYacas(self):
    l = len(self.operands)
    op = self._to_yacas_map.get(self.op, self.op)
    if l == 2:
      return '(%s %s %s)' % (op, toYacas(self.operands[0]), toYacas(self.operands[1]))
    elif l == 1:
      return '(%s %s)' % (op, toYacas(self.operands[0]))
    elif l == 0:
      return '(%s)' % op
    else:
      res = '(%s' % op
      for o in operands:
        res = res + ' %s' % toYacas(o)
      res = res + ')'
      return res

class FromYacas:
  '''handle translation back from yacas output in Lisp format to Sym objects.

  It is a simple recursive decent parser and lexer all rolled into one.'''
  
  fromYacasTable = {}
  '''The first time this is used the init function will fill in the
  entries in this table. I could not figure out how to refer to self
  otherwise'''
  
  def __init__(self, str):
    self.str = str.lstrip()
    if len(self.fromYacasTable) == 0:
      fyt = { '+':('+', SymExpr),
              '-':('-', SymExpr),
              '*':('*', SymExpr),
              '/':('/', SymExpr),
              '^':('**', SymExpr),
              'Abs':('abs', SymExpr),
              'Sin':('sin', SymExpr),
              'Cos':('cos', SymExpr),
              'Tan':('tan', SymExpr),
              'ArcSin':('arcsin', SymExpr),
              'ArcCos':('arccos', SymExpr),
              'ArcTan':('arctan', SymExpr),
              'Exp':('exp', SymExpr),
              'Log':('log', SymExpr),
              'Sqrt':('sqrt', SymExpr),
              'Complex':(None, self.Complex),
              'Conjugate':('conjugate', SymExpr),
              }
      for key,value in fyt.items():
        self.fromYacasTable[key] = value

  def Next(self):
    '''Get the next input character and advance the pointer'''
    res = self.str[0]
    self.str = self.str[1:].lstrip()
    return res

  def LookAhead(self):
    '''Get the next input character without advancing the pointer'''
    return self.str[0]
        
  def GetExpr(self):
    '''Get a Lisp expression object'''
    if self.LookAhead() == '(':
      self.Next()
      op = self.GetOperator()
      operands = []
      while self.LookAhead() != ')':
        operands.append(self.GetExpr())
      self.Next()
      try:
        py_op,func = self.fromYacasTable[op]
        #print op,py_op,operands
        if py_op:
          res = func(py_op, operands)
        else:
          res = func(operands)
        #print res
        return res
      except KeyError:
        raise YacasError('unknown operator "%s" in "%s"' % (op, self.str))
    else:
      term = self.GetTerm()
      return Sym(term)

  def GetOperator(self):
    '''Get an operator or function'''
    m = re.match('[-+*/^a-zA-Z0-9_]+', self.str)
    if m:
      self.str = self.str[m.end():].lstrip()
      return m.group(0)
    raise YacasError, 'syntax error "%s"' % self.str

  def GetTerm(self):
    '''Get a simple object like a number or name'''
    m = re.match('[-+0-9.]+', self.str)
    if m: # a number
      self.str = self.str[m.end():].lstrip()
      return eval(m.group(0))
    m = re.match('[a-zA-Z_][a-zA-Z0-9_]*', self.str)
    if m: # an id
      self.str = self.str[m.end():].lstrip()
      return m.group(0)
    # handle arrays here?
    
    raise YacasError, 'syntax error "%s"' % self.str

  def Complex(self, operands):
    '''Convert complex numbers directly'''
    return apply(complex, operands)

if __name__ == '__main__':
  x = Sym('x')
  z = x + 2
  w = 2 * z**2
  a = z*(w+x)
  print 'a=',a
  print 'simplified a=',a.Simplify()
  print 'deriv a=',a.Deriv(x).Simplify()


    
    

  
