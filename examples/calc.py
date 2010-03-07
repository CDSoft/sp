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

""" Calc

num  : generic numerical calculus   | assignment: name = expression
int  : integral calculus            | binary    : b... or ...b
int8 : integral calculus on 8 bits  | octal     : o... or ...o
int16: integral calculus on 16 bits | hexa      : h... or ...h or 0x...
int32: integral calculus on 32 bits | operators : + - | ^ * % / & >> << ~ **
int64: integral calculus on 64 bits | functions : rev factor sqrt
flt32: 32 bit float calculus        |
flt64: 64 bit float calculus        |
rat  : rational calculus            | this help : ?
"""

import struct
import sys

import sp
from fractions import Fraction

try:
    import readline
except ImportError:
    pass

class Calc:
    def __init__(self):
        self.number = Num
        self.memory = {}
    def __getitem__(self, var): return self.memory[var.name]
    def __setitem__(self, var, val): self.memory[var.name] = val

class Help:
    def __init__(self):
        self.doc = __doc__.strip()
    def eval(self, calc=None):
        return self.doc

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
    def sqrt(self):
        return self.__class__(self.val ** (1/2))

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

class Bin:
    def __init__(self, n): self.val = int(n.replace('_', ''), 2)
    def eval(self, calc): return calc.number(self.val)

class Oct:
    def __init__(self, n): self.val = int(n.replace('_', ''), 8)
    def eval(self, calc): return calc.number(self.val)

class Hex:
    def __init__(self, n): self.val = int(n.replace('_', ''), 16)
    def eval(self, calc): return calc.number(self.val)

class Dec:
    def __init__(self, n): self.val = int(n.replace('_', ''), 10)
    def eval(self, calc): return calc.number(self.val)

class Real:
    def __init__(self, n): self.val = float(n.replace('_', ''))
    def eval(self, calc): return calc.number(self.val)

class Var:
    def __init__(self, n): self.name = n
    def eval(self, calc):
        try: val = calc[self]
        except KeyError: raise NameError(self.name)
        return val.eval(calc)

class Mode:
    def __init__(self, mode): self.mode = mode
    def eval(self, calc):
        calc.number = self.mode
        return "%s mode"%calc.number.descr

class Assign:
    def __init__(self, name, expr): self.name, self.expr = name, expr
    def eval(self, calc):
        calc[self.name] = self.expr
        return self.expr.eval(calc)

class Op2:
    def __init__(self, x, y): self.x, self.y = x, y

class Op1:
    def __init__(self, x): self.x = x

class Add(Op2):
    def eval(self, calc): return self.x.eval(calc) + self.y.eval(calc)

class Sub(Op2):
    def eval(self, calc): return self.x.eval(calc) - self.y.eval(calc)

class Or(Op2):
    def eval(self, calc): return self.x.eval(calc) | self.y.eval(calc)

class Xor(Op2):
    def eval(self, calc): return self.x.eval(calc) ^ self.y.eval(calc)

class Mul(Op2):
    def eval(self, calc): return self.x.eval(calc) * self.y.eval(calc)

class Mod(Op2):
    def eval(self, calc): return self.x.eval(calc) % self.y.eval(calc)

class Div(Op2):
    def eval(self, calc): return self.x.eval(calc) / self.y.eval(calc)

class And(Op2):
    def eval(self, calc): return self.x.eval(calc) & self.y.eval(calc)

class RShift(Op2):
    def eval(self, calc): return self.x.eval(calc) >> self.y.eval(calc)

class LShift(Op2):
    def eval(self, calc): return self.x.eval(calc) << self.y.eval(calc)

class Pow(Op2):
    def eval(self, calc): return self.x.eval(calc) ** self.y.eval(calc)

class Pos(Op1):
    def eval(self, calc): return +self.x.eval(calc)

class Neg(Op1):
    def eval(self, calc): return -self.x.eval(calc)

class Inv(Op1):
    def eval(self, calc): return ~self.x.eval(calc)

class Rev(Op1):
    def eval(self, calc): return self.x.eval(calc).rev()

class Factor(Op1):
    def eval(self, calc): return self.x.eval(calc).factor()

class Sqrt(Op1):
    def eval(self, calc): return self.x.eval(calc).sqrt()

def red(x, ys):
    for f, y in ys:
        x = f(x, y)
    return x

def red1(f, x):
    return f(x)

parser = sp.compile(
    r"""
        bin = r'b([_0-1]+)\b' : `Bin` ;
        bin = r'([_0-1]+)b\b' : `Bin` ;
        oct = r'o([_0-7]+)\b' : `Oct` ;
        oct = r'([_0-7]+)o\b' : `Oct` ;
        hex = r'h([_0-9a-fA-F]+)\b' : `Hex` ;
        hex = r'([_0-9a-fA-F]+)h\b' : `Hex` ;
        hex = r'0x([_0-9a-fA-F]+)\b' : `Hex` ;
        real = r'(?:\d+\.\d*|\d*\.\d+)(?:[eE][-+]?\d+)?|\d+[eE][-+]?\d+' : `Real` ;
        dec = r'\d+' : `Dec` ;
        var = r'[a-zA-Z_]\w*' : `Var`;

        addop = '+' `Add` ;
        addop = '-' `Sub` ;
        addop = '|' `Or` ;
        addop = '^' `Xor` ;

        mulop = '*' `Mul` ;
        mulop = '%' `Mod` ;
        mulop = '/' `Div` ;
        mulop = '&' `And` ;
        mulop = '>>' `RShift` ;
        mulop = '<<' `LShift` ;

        unop = '+' `Pos` ;
        unop = '-' `Neg` ;
        unop = '~' `Inv` ;

        powop = '**' `Pow` ;

        separator: r'\s+' ;

        !S = '?'        `Help()`;

        !S = 'num'      `Mode(Num)`;
        !S = 'int8'     `Mode(Int8)`;
        !S = 'int16'    `Mode(Int16)`;
        !S = 'int32'    `Mode(Int32)`;
        !S = 'int64'    `Mode(Int64)`;
        !S = 'int'      `Mode(Int)`;
        !S = 'flt32'    `Mode(Float)`;
        !S = 'flt64'    `Mode(Double)`;
        !S = 'rat'      `Mode(Rat)`;

        !S = var '=' expr :: `Assign` ;
        !S = expr ;

        expr = term (addop term)* :: `red` ;
        term = fact (mulop fact)* :: `red` ;
        fact = unop fact :: `red1` | pow ;
        pow = atom (powop fact)? :: `red` ;

        atom = '(' expr ')' ;
        atom = 'rev' '(' expr ')' : `Rev` ;
        atom = 'factor' '(' expr ')' : `Factor` ;
        atom = 'sqrt' '(' expr ')' : `Sqrt` ;
        atom = bin | oct | hex | real | dec | var ;
    """)

if __name__ == '__main__':
    print(Help().eval())
    print()
    calc = Calc()
    while True:
        expr = input("(%s) "%calc.number.name)
        if not expr: continue
        try:
            val = parser(expr).eval(calc)
        except Exception as e:
            print("%s: %s"%(e.__class__.__name__, e))
        else:
            print("=", val)
        print()
