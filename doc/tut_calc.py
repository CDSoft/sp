#!/usr/bin/env python3

from sp import *

def Calc():

    from operator import pos, neg, add, sub, mul, truediv as div

    op1 = lambda f,x: {'+':pos, '-':neg}[f](x)
    op2 = lambda f,y: lambda x: {'+': add, '-': sub, '*': mul, '/': div}[f](x,y)

    def red(x, fs):
        for f in fs: x = f(x)
        return x

    number = Token(r'[0-9]+') / int
    addop = Token('[+-]')
    mulop = Token('[*/]')

    with Separator(r'\s+'):

        expr = Rule()
        fact = Rule()
        fact |= (addop & fact) * op1
        fact |= Drop(r'\(') & expr & Drop(r'\)')
        fact |= number
        term = (fact & ( (mulop & fact) * op2 )[:]) * red
        expr |= (term & ( (addop & term) * op2 )[:]) * red

    return expr

calc = Calc()
while True:
    expr = input('Enter an expression: ')
    try: print(expr, '=', calc(expr))
    except Exception as e: print("%s:"%e.__class__.__name__, e)
