#!/usr/bin/env python

# Simple Parser
# Copyright (C) 2009-2010 Christophe Delord
# http://www.cdsoft.fr/sp.html

# This file is part of Simple Parser.
# 
# Simple Parser is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# Simple Parser is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General Public License
# along with Simple Parser.  If not, see <http://www.gnu.org/licenses/>.

"""
+-------------------------------+-------------------------------------+
|            C A L C            | Powered by Python and Simple Parser |
+-------------------------------+-+-----------------------------------+
| Modes:                          | Numbers:                          |
|     num flt32 flt64 rat         |     binary: b... or ...b          |
|     int int8 int16 int32 int64  |     octal : o... or ...o          |
+---------------------------------+     hexa  : h... or ...h or 0x... |
| Variable and function:          |     float : 1.2e-3                |
|     variable = expression       +-----------------------------------+
|     function(x, y) = expression | Operators:                        |
+---------------------------------+     or xor and not                |
| Builtin functions:              |     < <= > >= == !=               |
|     fact, fib, rev, ...         |     cond?expr:expr                |
| type help to list all functions |     + - | ^ * / % & >> << ~ **    |
+---------------------------------+-----------------------------------+
| Commands:                       | Copyright (c) 2010 C. Delord      |
|     ? help bye                  | http://www.cdsoft.fr/sp.html      |
+---------------------------------+-----------------------------------+
"""

HELP = """
Functions from math (Python package):
-------------------------------------

    Number-theoretic and representation functions:
        ceil, sign, abs, floor, isinf, isnan, trunc

    Power and logarithmic functions:
        exp, log, log1p, log10, pow, sqr, sqrt

    Trigonometric functions:
        acos, asin, atan, atan2
        cos, sin, tan, hypot

    Angular conversion:
        degrees, radians

    Hyperbolic functions:
        acosh, asinh, atanh, cosh, sinh, tanh

Constants:

    pi, e

See http://docs.python.org/library/math.html

Other specific functions:
-------------------------

    rev : reverses bit order in int8, int16, int32 or int64 mode
    fact, fib : examples to show how to write recursive functions

The mode commands (num, int, rat, ...) can be used as variables in
expression to test the current mode.
"""

BUILTIN = r"""

    # Default numeric mode
    num

    # Recursive function examples:
    fact(n) = (n>0) ? n*fact(n-1) : 1
    fib(n) = (n<2) ? n : fib(n-1) + fib(n-2)

    # Reverse bit order
    _rev4(q) = q&b1000 >> 3 | q&b0100 >> 1 | q&b0010 << 1 | q&b0001 << 3
    _rev8(w) = _rev4(w&0xF0 >> 4) | _rev4(w&0x0F) << 4
    _rev16(w) = _rev8(w&0xFF00 >> 8) | _rev8(w&0x00FF) << 8
    _rev32(w) = _rev16(w&0xFFFF0000 >> 16) | _rev16(w&0x0000FFFF) << 16
    _rev64(w) = _rev32(w&0xFFFFFFFF00000000 >> 32) | _rev32(w&0x00000000FFFFFFFF) << 32
    rev(w) = int8 ? _rev8(w) : \
             int16 ? _rev16(w) : \
             int32 ? _rev32(w) : \
             int64 ? _rev64(w) : \
             undef

"""

import math, os, re, struct, sys
import sp

try:
    import readline
except ImportError:
    pass

def help(full=False):
    print(__doc__.strip())
    print("")
    if full:
        print("...")
        print("")

def error(msg=None):
    if msg:
        print(msg)
    else:
        help()
    sys.exit(-1)

class Quit:
    def eval(self, lenv):
        sys.exit()

class Help:
    def __init__(self, full=False):
        self.full = full
    def eval(self, lenv):
        help = "\n" + __doc__.strip()
        if self.full: help += "\n" + HELP
        return help
try:
    cmp
except NameError:
    def cmp(x, y):
        if x < y: return -1
        if x > y: return +1
        return 0

class Undef:
    name, descr = "undef", "Undefined"
    def __init__(self, val=None):
        self.val = None
        self.bool = False
    def __int__(self):      return Undef()
    def __float__(self):    return Undef()
    def __str__(self):      return Undef.descr
    def __lt__(x, y):       return Undef()
    def __gt__(x, y):       return Undef()
    def __le__(x, y):       return Undef()
    def __ge__(x, y):       return Undef()
    def __eq__(x, y):       return Undef()
    def __ne__(x, y):       return Undef()
    def __add__(x, y):      return Undef()
    def __sub__(x, y):      return Undef()
    def __or__(x, y):       return Undef()
    def __xor__(x, y):      return Undef()
    def __mul__(x, y):      return Undef()
    def __mod__(x, y):      return Undef()
    def __truediv__(x, y):  return Undef()
    def __and__(x, y):      return Undef()
    def __rshift__(x, y):   return Undef()
    def __lshift__(x, y):   return Undef()
    def __pos__(x):         return Undef()
    def __neg__(x):         return Undef()
    def __invert__(x):      return Undef()
    def __pow__(x, y):      return Undef()
    def eval(self, lenv):   return self
    def ceil(x):            return Undef()
    def trunc(x):           return Undef()
    def sign(x):            return Undef()
    def abs(x):             return Undef()
    def floor(x):           return Undef()
    def isinf(x):           return Undef()
    def isnan(x):           return Undef()
    def exp(x):             return Undef()
    def log(x, b=None):     return Undef()
    def log1p(x):           return Undef()
    def log10(x):           return Undef()
    def pow(x, y):          return Undef()
    def sqr(x):             return Undef()
    def sqrt(x):            return Undef()
    def acos(x):            return Undef()
    def asin(x):            return Undef()
    def atan(x):            return Undef()
    def atan2(x, y):        return Undef()
    def cos(x):             return Undef()
    def sin(x):             return Undef()
    def tan(x):             return Undef()
    def hypot(x, y):        return Undef()
    def degrees(x):         return Undef()
    def radians(x):         return Undef()
    def acosh(x):           return Undef()
    def asinh(x):           return Undef()
    def atanh(x):           return Undef()
    def cosh(x):            return Undef()
    def sinh(x):            return Undef()
    def tanh(x):            return Undef()

class Num:
    name, descr = "num", "Number"
    def __init__(self, val):
        if isinstance(val, Num): self.val = val.val
        else: self.val = val
        self.bool = bool(val)
    def __int__(self):      return int(self.val)
    def __float__(self):    return float(self.val)
    def __str__(self):      return str(self.val)
    def __lt__(x, y):       return x.val < y.val
    def __gt__(x, y):       return x.val > y.val
    def __le__(x, y):       return x.val <= y.val
    def __ge__(x, y):       return x.val >= y.val
    def __eq__(x, y):       return x.val == y.val
    def __ne__(x, y):       return x.val != y.val
    def __add__(x, y):      return x.__class__(x.val + y.val)
    def __sub__(x, y):      return x.__class__(x.val - y.val)
    def __or__(x, y):       return x.__class__(x.val | y.val)
    def __xor__(x, y):      return x.__class__(x.val ^ y.val)
    def __mul__(x, y):      return x.__class__(x.val * y.val)
    def __mod__(x, y):      return x.__class__(x.val % y.val)
    def __truediv__(x, y):  return x.__class__(x.val / y.val)
    def __and__(x, y):      return x.__class__(x.val & y.val)
    def __rshift__(x, y):   return x.__class__(x.val >> y.val)
    def __lshift__(x, y):   return x.__class__(x.val << y.val)
    def __pos__(x):         return x.__class__(+x.val)
    def __neg__(x):         return x.__class__(-x.val)
    def __invert__(x):      return x.__class__(~x.val)
    def __pow__(x, y):      return x.__class__(x.val ** y.val)
    def eval(self, lenv):   return self
    def ceil(x):            return x.__class__(math.ceil(x.val))
    def trunc(x):           return x.__class__(math.trunc(x.val))
    def sign(x):            return x.__class__(cmp(x.val, 0))
    def abs(x):             return x.__class__(abs(x.val))
    def floor(x):           return x.__class__(math.floor(x.val))
    def isinf(x):           return x.__class__(math.isinf(x.val))
    def isnan(x):           return x.__class__(math.isnan(x.val))
    def exp(x):             return x.__class__(math.exp(x.val))
    def log(x, b=None):     return x.__class__(math.log(float(x.val), float(getattr(b, 'val', math.e))))
    def log1p(x):           return x.__class__(math.log1p(float(x.val)))
    def log10(x):           return x.__class__(math.log10(float(x.val)))
    def pow(x, y):          return x.__class__(math.pow(x.val, y.val))
    def sqr(x):             return x.__class__(x.val**2)
    def sqrt(x):            return x.__class__(math.sqrt(x.val))
    def acos(x):            return x.__class__(math.acos(x.val))
    def asin(x):            return x.__class__(math.asin(x.val))
    def atan(x):            return x.__class__(math.atan(x.val))
    def atan2(x, y):        return x.__class__(math.atan2(x.val, y.val))
    def cos(x):             return x.__class__(math.cos(x.val))
    def sin(x):             return x.__class__(math.sin(x.val))
    def tan(x):             return x.__class__(math.tan(x.val))
    def hypot(x, y):        return x.__class__(math.hypot(x.val, y.val))
    def degrees(x):         return x.__class__(math.degrees(x.val))
    def radians(x):         return x.__class__(math.radians(x.val))
    def acosh(x):           return x.__class__(math.acosh(x.val))
    def asinh(x):           return x.__class__(math.asinh(x.val))
    def atanh(x):           return x.__class__(math.atanh(x.val))
    def cosh(x):            return x.__class__(math.cosh(x.val))
    def sinh(x):            return x.__class__(math.sinh(x.val))
    def tanh(x):            return x.__class__(math.tanh(x.val))

class Float(Num):
    name, descr = "flt32", "32 bit Float"
    def __init__(self, val): self.val = float(val)
    def __str__(self): return """%s
    ieee: 0x%08X"""%(   self.val,
                        ieee_int32(self.val),
    )

class Double(Num):
    name, descr = "flt64", "64 bit Float"
    def __init__(self, val): self.val = float(val)
    def __str__(self): return """%s
    ieee: 0x%016X"""%(  self.val,
                        ieee_int64(self.val),
    )

try:
    from fractions import Fraction

except ImportError:

    class Rat(Undef):
        descr = "Rational (not available, requires Python 2.6 or 3.1)"

else:

    class Rat(Num):
        name, descr = "rat", "Rational"
        def __init__(self, val):
            if isinstance(val, float):
                self.val = Fraction("%.53f"%(val)).limit_denominator(1000000000)
            elif isinstance(val, Num): self.val = val.val
            else: self.val = Fraction(val)
        def __int__(self): return self.val.numerator//self.val.denominator
        def __float__(self): return self.val.numerator/self.val.denominator
        def __str__(self): return str(self.val)

class Int(Num):
    name, descr = "int", "Integer"
    def __init__(self, val): self.val = int(val)
    def __truediv__(x, y): return x.__class__(x.val // y.val)
    def __str__(self): return base(self.val, radix=10, group=3, width=None)

class Int8(Num):
    name, descr = "int8", "8 bit Integer"
    width = 8
    def __init__(self, val): self.val = int(val) & (2**self.width-1)
    def __truediv__(x, y): return x.__class__(x.val // y.val)
    def __str__(self): return """%s
    hex: %s
    oct: %s
    bin: %s"""%(    base(self.val, radix=10, group=3, width=None),
                    base(self.val, radix=16, group=4, width=self.width),
                    base(self.val, radix=8,  group=3, width=self.width),
                    base(self.val, radix=2,  group=4, width=self.width),
    )

class Int16(Int8):
    name, descr = "int16", "16 bit Integer"
    width = 16
    def __str__(self): return """%s
    hex: %s
    bin: %s"""%(    base(self.val, radix=10, group=3, width=None),
                    base(self.val, radix=16, group=4, width=self.width),
                    base(self.val, radix=2,  group=4, width=self.width),
    )

class Int32(Int8):
    name, descr = "int32", "32 bit Integer"
    width = 32
    def __str__(self): return """%s
    hex: %s
    bin: %s
    flt: %s"""%(    base(self.val, radix=10, group=3, width=None),
                    base(self.val, radix=16, group=4, width=self.width),
                    base(self.val, radix=2,  group=4, width=self.width),
                    ieee_float(self.val),
    )

class Int64(Int32):
    name, descr = "int64", "64 bit Integer"
    width = 64
    def __str__(self): return """%s
    hex: %s
    bin: %s
    flt: %s"""%(    base(self.val, radix=10, group=3, width=None),
                    base(self.val, radix=16, group=4, width=self.width),
                    base(self.val, radix=2,  group=4, width=self.width),
                    ieee_double(self.val),
    )

def ieee_int32(x):
    return struct.unpack("I", struct.pack("f", x))[0]

def ieee_int64(x):
    return struct.unpack("Q", struct.pack("d", x))[0]

def ieee_float(n):
    return struct.unpack("f", struct.pack("I", n))[0]

def ieee_double(n):
    return struct.unpack("d", struct.pack("Q", n))[0]

def base(N, radix=10, group=3, width=None):
    if width:
        N %= 2**width
        bits_per_digit = {16:4, 8:3, 2:1}[radix]
        min_len = width//bits_per_digit
    if N < 0:
        N = -N
        sign = "-"
    else:
        sign = ""
    s = ""
    while N:
        N, d = divmod(N, radix)
        s = s + "0123456789ABCDEF"[d]
    s = s or "0"
    if width:
        s = s + "0"*(min_len-len(s))
    s = " ".join(s[i:i+group] for i in range(0, len(s), group))
    return sign + s[::-1]

class Env(dict):
    NUMBER = Undef

class Bin:
    def __init__(self, n):
        self.val = int(n.replace('_', ''), 2)
    def eval(self, lenv):
        return genv.NUMBER(self.val)

class Oct:
    def __init__(self, n):
        self.val = int(n.replace('_', ''), 8)
    def eval(self, lenv):
        return genv.NUMBER(self.val)

class Hex:
    def __init__(self, n):
        self.val = int(n.replace('_', ''), 16)
    def eval(self, lenv):
        return genv.NUMBER(self.val)

class Dec:
    def __init__(self, n):
        self.val = int(n.replace('_', ''), 10)
    def eval(self, lenv):
        return genv.NUMBER(self.val)

class Real:
    def __init__(self, n):
        self.val = float(n.replace('_', ''))
    def eval(self, lenv):
        return genv.NUMBER(self.val)

class Var:
    def __init__(self, n):
        self.name = n
    def eval(self, lenv):
        try:
            args, expr = lenv[self.name]
        except KeyError:
            try:
                args, expr = genv[self.name]
            except KeyError:
                return self.builtin(lenv)
        if len(args) != 0:
            raise TypeError("Wrong argument number in %s"%self.name)
        return expr.eval(lenv)
    def builtin(self, lenv):
        try:
            f = {
                'num':      (lambda: genv.NUMBER(genv.NUMBER == Num)),
                'int':      (lambda: genv.NUMBER(genv.NUMBER == Int)),
                'int8':     (lambda: genv.NUMBER(genv.NUMBER == Int8)),
                'int16':    (lambda: genv.NUMBER(genv.NUMBER == Int16)),
                'int32':    (lambda: genv.NUMBER(genv.NUMBER == Int32)),
                'int64':    (lambda: genv.NUMBER(genv.NUMBER == Int64)),
                'flt32':    (lambda: genv.NUMBER(genv.NUMBER == Float)),
                'flt64':    (lambda: genv.NUMBER(genv.NUMBER == Double)),
                'rat':      (lambda: genv.NUMBER(genv.NUMBER == Rat)),
                'undef':    Undef,
                'pi':       (lambda: genv.NUMBER(math.pi)),
                'e':        (lambda: genv.NUMBER(math.e)),
            }[self.name]
        except KeyError:
            raise NameError(self.name)
        return f()

class Mode:
    def __init__(self, mode):
        self.mode = mode
    def eval(self, lenv):
        genv.NUMBER = self.mode
        return "%s mode"%genv.NUMBER.descr

class Set:
    def __init__(self, fundef, expr):
        self.fundef, self.expr = fundef, expr
    def eval(self, lenv):
        genv[self.fundef.name] = (self.fundef.args, self.expr)
        if len(self.fundef.args) == 0:
            return self.expr.eval(lenv)
        else:
            return "%s(%s)"%(self.fundef.name, ",".join(self.fundef.args))

class FunDef:
    def __init__(self, name, args):
        self.name, self.args = name, args

class FunEval:
    def __init__(self, name, args):
        self.name, self.args = name, args
    def eval(self, lenv):
        try:
            args, expr = lenv[self.name]
        except KeyError:
            try:
                args, expr = genv[self.name]
            except KeyError:
                return self.builtin(lenv)
        if len(args) != len(self.args):
            raise TypeError("Wrong argument number in %s"%self.name)
        lenv = dict((arg_name, ([], arg_expr.eval(lenv))) for arg_name, arg_expr in zip(args, self.args))
        return expr.eval(lenv)
    def builtin(self, lenv):
        try:
            arity, fun = {
                'ceil':     (1, genv.NUMBER.ceil),
                'trunc':    (1, genv.NUMBER.trunc),
                'sign':     (1, genv.NUMBER.sign),
                'abs':      (1, genv.NUMBER.abs),
                'floor':    (1, genv.NUMBER.floor),
                'isinf':    (1, genv.NUMBER.isinf),
                'isnan':    (1, genv.NUMBER.isnan),
                'exp':      (1, genv.NUMBER.exp),
                'log':      ((1,2), genv.NUMBER.log),
                'log1p':    (1, genv.NUMBER.log1p),
                'log10':    (1, genv.NUMBER.log10),
                'pow':      (2, genv.NUMBER.pow),
                'sqr':      (1, genv.NUMBER.sqr),
                'sqrt':     (1, genv.NUMBER.sqrt),
                'acos':     (1, genv.NUMBER.acos),
                'asin':     (1, genv.NUMBER.asin),
                'atan':     (1, genv.NUMBER.atan),
                'atan2':    (2, genv.NUMBER.atan2),
                'cos':      (1, genv.NUMBER.cos),
                'sin':      (1, genv.NUMBER.sin),
                'tan':      (1, genv.NUMBER.tan),
                'hypot':    (2, genv.NUMBER.hypot),
                'degrees':  (1, genv.NUMBER.degrees),
                'radians':  (1, genv.NUMBER.radians),
                'acosh':    (1, genv.NUMBER.acosh),
                'asinh':    (1, genv.NUMBER.asinh),
                'atanh':    (1, genv.NUMBER.atanh),
                'cosh':     (1, genv.NUMBER.cosh),
                'sinh':     (1, genv.NUMBER.sinh),
                'tanh':     (1, genv.NUMBER.tanh),
            }[self.name]
        except KeyError:
            raise NameError(self.name)
        n_args = len(self.args)
        if (type(arity) == tuple and n_args not in arity) or \
           (type(arity) == int and n_args != arity):
            raise TypeError("Wrong argument number in %s"%self.name)
        args = [arg.eval(lenv) for arg in self.args]
        return fun(*args)

class LogicOr:
    def __init__(self, x, y):
        self.x, self.y = x, y
    def eval(self, lenv):
        x = self.x.eval(lenv)
        if x.bool:
            return x
        else:
            return self.y.eval(lenv)

class LogicXor:
    def __init__(self, x, y):
        self.x, self.y = x, y
    def eval(self, lenv):
        x = self.x.eval(lenv)
        y = self.y.eval(lenv)
        if x.bool and not y.bool:
            return x
        elif y.bool and not x.bool:
            return y
        else:
            return genv.NUMBER(False)

class LogicAnd:
    def __init__(self, x, y):
        self.x, self.y = x, y
    def eval(self, lenv):
        x = self.x.eval(lenv)
        if x.bool:
            return self.y.eval(lenv)
        else:
            return x

class LogicNot:
    def __init__(self, x):
        self.x = x
    def eval(self, lenv):
        return genv.NUMBER(not self.x.eval(lenv).bool)

class Lt:
    def __init__(self, x, y):
        self.x, self.y = x, y
    def eval(self, lenv):
        return genv.NUMBER(self.x.eval(lenv) < self.y.eval(lenv))

class Gt:
    def __init__(self, x, y):
        self.x, self.y = x, y
    def eval(self, lenv):
        return genv.NUMBER(self.x.eval(lenv) > self.y.eval(lenv))

class Le:
    def __init__(self, x, y):
        self.x, self.y = x, y
    def eval(self, lenv):
        return genv.NUMBER(self.x.eval(lenv) <= self.y.eval(lenv))

class Ge:
    def __init__(self, x, y):
        self.x, self.y = x, y
    def eval(self, lenv):
        return genv.NUMBER(self.x.eval(lenv) >= self.y.eval(lenv))

class Eq:
    def __init__(self, x, y):
        self.x, self.y = x, y
    def eval(self, lenv):
        return genv.NUMBER(self.x.eval(lenv) == self.y.eval(lenv))

class Ne:
    def __init__(self, x, y):
        self.x, self.y = x, y
    def eval(self, lenv):
        return genv.NUMBER(self.x.eval(lenv) != self.y.eval(lenv))

class Cond:
    def __init__(self, cond, true_expr, false_expr):
        self.cond, self.true_expr, self.false_expr = cond, true_expr, false_expr
    def eval(self, lenv):
        if bool(self.cond.eval(lenv).val):
            return self.true_expr.eval(lenv)
        else:
            return self.false_expr.eval(lenv)

class Add:
    def __init__(self, x, y):
        self.x, self.y = x, y
    def eval(self, lenv):
        return self.x.eval(lenv) + self.y.eval(lenv)

class Sub:
    def __init__(self, x, y):
        self.x, self.y = x, y
    def eval(self, lenv):
        return self.x.eval(lenv) - self.y.eval(lenv)

class Or:
    def __init__(self, x, y):
        self.x, self.y = x, y
    def eval(self, lenv):
        return self.x.eval(lenv) | self.y.eval(lenv)

class Xor:
    def __init__(self, x, y):
        self.x, self.y = x, y
    def eval(self, lenv):
        return self.x.eval(lenv) ^ self.y.eval(lenv)

class Mul:
    def __init__(self, x, y):
        self.x, self.y = x, y
    def eval(self, lenv):
        return self.x.eval(lenv) * self.y.eval(lenv)

class Div:
    def __init__(self, x, y):
        self.x, self.y = x, y
    def eval(self, lenv):
        return self.x.eval(lenv) / self.y.eval(lenv)

class Mod:
    def __init__(self, x, y):
        self.x, self.y = x, y
    def eval(self, lenv):
        return self.x.eval(lenv) % self.y.eval(lenv)

class And:
    def __init__(self, x, y):
        self.x, self.y = x, y
    def eval(self, lenv):
        return self.x.eval(lenv) & self.y.eval(lenv)

class Rshift:
    def __init__(self, x, y):
        self.x, self.y = x, y
    def eval(self, lenv):
        return self.x.eval(lenv) >> self.y.eval(lenv)

class Lshift:
    def __init__(self, x, y):
        self.x, self.y = x, y
    def eval(self, lenv):
        return self.x.eval(lenv) << self.y.eval(lenv)

class Pow:
    def __init__(self, x, y):
        self.x, self.y = x, y
    def eval(self, lenv):
        return self.x.eval(lenv) ** self.y.eval(lenv)

class Pos:
    def __init__(self, x):
        self.x = x
    def eval(self, lenv):
        return +self.x.eval(lenv)

class Neg:
    def __init__(self, x):
        self.x = x
    def eval(self, lenv):
        return -self.x.eval(lenv)

class Inv:
    def __init__(self, x):
        self.x = x
    def eval(self, lenv):
        return ~self.x.eval(lenv)

def _fy(f, y): return lambda x: f(x, y)

def fx(f, x): return f(x)

def reduce(x, fs):
    for f in fs: x = f(x)
    return x

parse = sp.compile(r"""

    ident = r'[a-zA-Z_]\w*' ;

    bin = r'b([_0-1]+)\b' : `Bin` ;
    bin = r'([_0-1]+)b\b' : `Bin` ;
    oct = r'o([_0-7]+)\b' : `Oct` ;
    oct = r'([_0-7]+)o\b' : `Oct` ;
    hex = r'h([_0-9a-fA-F]+)\b' : `Hex` ;
    hex = r'([_0-9a-fA-F]+)h\b' : `Hex` ;
    hex = r'0x([_0-9a-fA-F]+)\b' : `Hex` ;
    real = r'(?:\d+\.\d*|\d*\.\d+)(?:[eE][-+]?\d+)?|\d+[eE][-+]?\d+' : `Real` ;
    dec = r'\d+' : `Dec` ;
    var = ident : `Var`;

    logic_or_op = 'or' `LogicOr` ;
    logic_or_op = 'xor' `LogicXor` ;
    logic_and_op = 'and' `LogicAnd` ;
    logic_not_op = 'not' `LogicNot` ;

    rel_op = '<' `Lt` ;
    rel_op = '>' `Gt` ;
    rel_op = '<=' `Le` ;
    rel_op = '>=' `Ge` ;
    rel_op = '==' `Eq` ;
    rel_op = '!=' `Ne` ;

    add_op = '+' `Add` ;
    add_op = '-' `Sub` ;
    add_op = '|' `Or` ;
    add_op = '^' `Xor` ;

    mul_op = '*' `Mul` ;
    mul_op = '/' `Div` ;
    mul_op = '%' `Mod` ;
    mul_op = '&' `And` ;
    mul_op = '>>' `Rshift` ;
    mul_op = '<<' `Lshift` ;

    pow_op = '**' `Pow` ;

    un_op = '+' `Pos` ;
    un_op = '-' `Neg` ;
    un_op = '~' `Inv` ;
    
    separator: r'\s+';

    !S = 'bye' `Quit()`
       | '?'            `Help()`
       | 'help'         `Help(full=True)`
       | 'num'          `Mode(Num)`
       | 'int8'         `Mode(Int8)`
       | 'int16'        `Mode(Int16)`
       | 'int32'        `Mode(Int32)`
       | 'int64'        `Mode(Int64)`
       | 'int'          `Mode(Int)`
       | 'flt32'        `Mode(Float)`
       | 'flt64'        `Mode(Double)`
       | 'rat'          `Mode(Rat)`
       | fundef '=' expr :: `Set`
       | expr
       ;

    fundef = ident ('(' [ident/',']* ')' | `[]`) :: `FunDef` ;

    expr = logic_or ;

    logic_or = logic_and (logic_or_op logic_and :: `_fy`)* :: `reduce` ;
    logic_and = condition (logic_and_op condition :: `_fy`)* :: `reduce` ;
    condition = logic_not_op condition :: `fx` | relation ;

    relation = ternary (rel_op ternary :: `_fy`)* :: `reduce` ;

    ternary = arith '?' ternary ':' ternary :: `Cond` | arith ;
    arith = term (add_op term :: `_fy`)* :: `reduce` ;
    term = fact (mul_op fact :: `_fy`)* :: `reduce` ;
    fact = un_op fact :: `fx` | pow ;
    pow = atom (pow_op fact :: `_fy`)? :: `reduce` ;

    atom = '(' expr ')' ;
    atom = ident '(' [expr/',']* ')' :: `FunEval` ;
    atom = (bin | oct | hex | real | dec) ;
    atom = ident : `Var`;

""")

genv = Env()

def exc():
    e = getattr(sys, 'exc_value', None)
    if e is None:
        info = getattr(sys, 'exc_info', None)
        if info is not None: e = info()[1]
    return e

try:
    next
except NameError:
    def next(iterator):
        for item in iterator:
            return item

def run(src):
    lines = iter(src.splitlines())
    for line in lines:
        while line and line[-1] == '\\':
            line = line[:-1] + next(lines)
        if re.match(r"^\s*(#.*)?$", line):
            if VERBOSE:
                print(line)
            continue
        if VERBOSE and PROMPT:
            print("(%s) %s"%(genv.NUMBER.name, line))
        try:
            e = parse(line)
            val = e.eval(Env())
        except Exception:
            e = exc()
            print("%s: %s"%(e.__class__.__name__, e))
            print("")
        else:
            if VERBOSE:
                print("= %s"%val)
                print("")

VERBOSE = False
PROMPT = True
SHOW_HELP = True
INTERACTIVE = True

run(BUILTIN)

VERBOSE = True

try: raw_input
except NameError: raw_input = input

if __name__ == '__main__':
    args = iter(sys.argv[1:])
    for arg in args:
        if arg == '-h':
            help()
            INTERACTIVE = False
            SHOW_HELP = False
        elif arg == '-s':
            VERBOSE = False
            INTERACTIVE = False
            SHOW_HELP = False
        elif arg == '-v':
            VERBOSE = True
            INTERACTIVE = False
            SHOW_HELP = False
        elif arg == '-e':
            try:
                src = next(args)
            except StopIteration:
                error("-e requires a file name or an expression")
            if os.path.isfile(src):
                try:
                    src = open(src).read()
                except IOError:
                    error("can not read %s"%src)
            run(src)
            INTERACTIVE = False
            SHOW_HELP = False
        elif arg == '-i':
            INTERACTIVE = True
        elif arg == '-q': sys.exit()
        else:
            run(arg)
            INTERACTIVE = False
            SHOW_HELP = False
    if sys.stdin.isatty():
        if SHOW_HELP: help()
        PROMPT = False
        while True:
            run(raw_input("(%s) "%genv.NUMBER.name))
    elif INTERACTIVE:
        try:
            src = sys.stdin.read()
        except IOError:
            pass
        else:
            run(src)
