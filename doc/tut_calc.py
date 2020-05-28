#!/usr/bin/env python3

__license__ = """
Simple Parser
Copyright (C) 2009-2010 Christophe Delord
http://cdelord.fr/sp

This file is part of Simple Parser.

Simple Parser is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Simple Parser is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with Simple Parser.  If not, see <http://www.gnu.org/licenses/>.
"""

import sp
from sp import *

def Calc():

    from operator import pos, neg, add, sub, mul, truediv as div

    op1 = lambda f,x: {'+':pos, '-':neg}[f](x)
    op2 = lambda f,y: lambda x: {'+': add, '-': sub, '*': mul, '/': div}[f](x,y)

    def red(x, fs):
        for f in fs: x = f(x)
        return x

    number = R('[0-9]+') / int
    addop = R('[+-]')
    mulop = R('[*/]')

    with Separator(r'\s+'):

        expr = Rule()
        fact = Rule()
        fact |= (addop & fact) * op1
        fact |= '(' & expr & ')'
        fact |= number
        term = (fact & ( (mulop & fact) * op2 )[:]) * red
        expr |= (term & ( (addop & term) * op2 )[:]) * red

    return expr, sp.compile("""
        number = r'[0-9]+' : `int` ;
        addop = r'[+-]' ;
        mulop = r'[*/]' ;

        separator = r'\s+' ;

        !expr = term (addop term :: `op2`)* :: `red` ;
        term = fact (mulop fact :: `op2`)* :: `red` ;
        fact = addop fact :: `op1` ;
        fact = '(' expr ')' ;
        fact = number ;
    """)

try: raw_input
except NameError: raw_input = input

print(__license__.strip())
print("*"*70)
calc1, calc2 = Calc()
while True:
    expr = raw_input('Enter an expression: ')
    try: print('calc1: %s = %s'%(expr, calc1(expr)))
    except Exception as e: print('calc1: %s:'%e.__class__.__name__, e)
    try: print('calc2: %s = %s'%(expr, calc2(expr)))
    except Exception as e: print('calc2: %s:'%e.__class__.__name__, e)
