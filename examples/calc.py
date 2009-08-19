#!/usr/bin/env python3

# Simple Parser
# Copyright (C) 2009 Christophe Delord
# http://christophe.delord.free.fr/sp

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

""" Calc

num  : generic numerical calculus   | assignment: name = expression
int  : integral calculus            | binary    : b... or ...b
int8 : integral calculus on 8 bits  | octal     : o... or ...o
int16: integral calculus on 16 bits | hexa      : h... or ...h or 0x...
int32: integral calculus on 32 bits | operators : + - | ^ * % / & >> << ~ **
int64: integral calculus on 64 bits | functions : rev factor
flt32: 32 bit float calculus        |
flt64: 64 bit float calculus        |
rat  : rational calculus            | this help : ?
"""

import struct
import sys

from sp import *
from fractions import Fraction

try:
    import readline
except ImportError:
    pass

class Num:
    name, descr = "num", "Number"
    def __init__(self, val):
        if isinstance(val, Num): self.val = val.val
        else: self.val = val
    def __int__(self):      return int(self.val)
    def __float__(self):    return float(self.val)
    def __str__(self):      return str(self.val)
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
    def rev(self):
        raise TypeError("%s can not be bit reversed"%self.__class__.__name__)
    def factor(self):
        n = int(self.val)
        if n != self.val: raise TypeError("%s is not integer"%self.val)
        if n < 0: ds = [-1]; n = -n
        else: ds = []
        rn = n**0.5
        while n > 1 and n%2==0: ds.append(2); n //= 2
        d = 3
        while d <= rn:
            while n%d==0: ds.append(d); n //= d
            d += 2
        if n > 1: ds.append(n)
        return " ".join(map(str, ds))

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
    ieee: 0x%16X"""%(   self.val,
                        ieee_int64(self.val),
    )

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
    def rev(self):
        """ reverse bit order """
        return self.__class__(
            sum(    ((self.val>>i)&0x1)<<(self.width-1-i)
                    for i in range(self.width)
            )
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

class Calc:

    def __init__(self):

        self.number = Num

        def bin2int(n):
            n = n.replace('_', '')
            n = n.replace('b', '')
            return int(n, 2)

        def oct2int(n):
            n = n.replace('_', '')
            n = n.replace('o', '')
            return int(n, 8)

        def hex2int(n):
            n = n.replace('_', '')
            n = n.replace('h', '')
            if n.startswith('0x'): n = n[2:]
            return int(n, 16)

        def real2float(n):
            n = n.replace('_', '')
            return float(n)

        def dec2int(n):
            n = n.replace('_', '')
            return int(n)

        bin = (R(r'b[_0-1]+\b') | R(r'[_0-1]+b\b')) / bin2int
        oct = (R(r'o[_0-7]+\b') | R(r'[_0-7]+o\b')) / oct2int
        hex = ( R(r'h[_0-9a-fA-F]+\b') |
                R(r'[_0-9a-fA-F]+h\b') |
                R(r'0x[_0-9a-fA-F]+\b')) / hex2int
        real = R(r'(\d+\.\d*|\d*\.\d+)([eE][-+]?\d+)?|\d+[eE][-+]?\d+') / real2float
        dec = R(r'\d+') / dec2int
        var = R(r'[a-zA-Z_]\w*')

        from operator import pos, neg, invert, add, sub, or_, xor, \
                             mul, mod, truediv, floordiv, and_, \
                             rshift, lshift, pow as pow_
        op1 = lambda f,x: {'+':pos, '-':neg, '~':invert}[f](x)
        op = lambda f,y: lambda x: {'+':add, '-':sub,
                                    '*':mul, '%':mod, '/':truediv,
                                    '**':pow_,
                                    '&':and_, '|':or_, '^':xor,
                                    '>>':rshift, '<<':lshift,
                                   }[f](x,y)
        def red(x, fs):
            for f in fs: x = f(x)
            return x

        with Separator(r'\s+'):

            expr = Rule()

            calc = ( K('?') & C(__doc__.strip())
                   | K('num') / self.mode(Num)
                   | K('int8') / self.mode(Int8)
                   | K('int16') / self.mode(Int16)
                   | K('int32') / self.mode(Int32)
                   | K('int64') / self.mode(Int64)
                   | K('int') / self.mode(Int)
                   | K('flt32') / self.mode(Float)
                   | K('flt64') / self.mode(Double)
                   | K('rat') / self.mode(Rat)
                   | (((var & '=') | C('_')) & expr) * self.assign
                   )

            fact = Rule()
            atom = ( '(' & expr & ')'
                   | (K('rev') & '(' & expr & ')') / (lambda x: x.rev())
                   | (K('factor') & '(' & expr & ')') / (lambda x: x.factor())
                   | (bin | oct | hex | real | dec | var / self.val)
                     / self.convert
                   )
            pow = (atom & ((R(r'\*\*') & fact) * op)[:1]) * red
            fact |= (R(r'\+|-|~') & fact) * op1 | pow
            term = (fact & ((R(r'\*|%|/|&|>>|<<') & fact) * op)[:]) * red
            expr |= (term & ((R(r'\+|-|\||\^') & term) * op)[:]) * red

        self.calc = calc
        self.var = {}

    def mode(self, m):
        def setmode(_):
            self.number = m
            return "%s mode"%self.number.descr
        return setmode

    def convert(self, x):
        return self.number(x)

    def assign(self, var, val):
        self.var[var] = val
        return str(val)

    def val(self, var):
        return self.var[var]

    def __call__(self, s):
        return self.calc(s)

def base(N, radix=10, group=3, width=None):
    if width:
        N %= 2**width
        bits_per_digit = {16:4, 8:3, 2:1}[radix]
        min_len = width//bits_per_digit
    s = ""
    while N:
        N, d = divmod(N, radix)
        s = s + "0123456789ABCDEF"[d]
    s = s or "0"
    if width:
        s = s + "0"*(min_len-len(s))
    s = " ".join(s[i:i+group] for i in range(0, len(s), group))
    return s[::-1]

if __name__ == '__main__':
    print(__doc__.strip())
    print()
    calc = Calc()
    for cmd in sys.argv[1:]:
        try:
            calc(cmd)
        except Exception as e:
            print("%s: %s"%(e.__class__.__name__, e))
    while True:
        expr = input("(%s) "%calc.number.name)
        if not expr: continue
        try:
            val = calc(expr)
        except Exception as e:
            print("%s: %s"%(e.__class__.__name__, e))
        else:
            print("=", val)
        print()
