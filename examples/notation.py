#!/usr/bin/env python3

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

# Infix/prefix/postfix expression conversion

import sp

try:
    import readline
except ImportError:
    pass

class Op:
    """ Binary operator """
    precedence = {'+':1, '-':1, '*':2, '/':2, '^':3}
    def __init__(self, op, a, b):
        self.op = op                    # operator ("+", "-", "*", "/", "^")
        self.prec = Op.precedence[op]   # precedence of the operator
        self.a, self.b = a, b           # operands
    def infix(self):
        a = self.a.infix()
        if self.a.prec < self.prec: a = "(%s)"%a
        b = self.b.infix()
        if self.b.prec <= self.prec: b = "(%s)"%b
        return "%s %s %s"%(a, self.op, b)
    def prefix(self):
        a = self.a.prefix()
        b = self.b.prefix()
        return "%s %s %s"%(self.op, a, b)
    def postfix(self):
        a = self.a.postfix()
        b = self.b.postfix()
        return "%s %s %s"%(a, b, self.op)

class Atom:
    """ Atomic expression """
    def __init__(self, s):
        self.a = s
        self.prec = 99
    def infix(self): return self.a
    def prefix(self): return self.a
    def postfix(self): return self.a

class Func:
    """ Function expression """
    def __init__(self, name, *args):
        self.name = name
        self.args = args
        self.prec = 99
    def infix(self):
        args = [a.infix() for a in self.args]
        return "%s(%s)"%(self.name, ",".join(args))
    def prefix(self):
        args = [a.prefix() for a in self.args]
        return "%s %s"%(self.name, " ".join(args))
    def postfix(self):
        args = [a.postfix() for a in self.args]
        return "%s %s"%(" ".join(args), self.name)

# Grammar for arithmetic expressions

def red(x, fs):
    for f in fs: x = f(x)
    return x

parser = sp.compile(r"""

    ident = r'\w+' : `Atom` ;

    func1 = r'sin' | r'cos' | r'tan' ;
    func2 = r'min' | r'max' ;

    op = op_add | op_mul | op_pow ;
    op_add = r'[+-]' ;
    op_mul = r'[*/]' ;
    op_pow = r'\^' ;

    separator: r'\s+' ;

    !axiom = expr      `"infix"`
           | expr_pre  `"prefix"`
           | expr_post `"postfix"`
           ;

    # Infix expressions

    expr = term (op_add term :: `lambda op, y: lambda x: Op(op, x, y)`)* :: `red` ;
    term = fact (op_mul fact :: `lambda op, y: lambda x: Op(op, x, y)`)* :: `red` ;
    fact = atom (op_pow fact :: `lambda op, y: lambda x: Op(op, x, y)`)? :: `red` ;
    atom = ident ;
    atom = '(' expr ')' ;
    atom = func1 '(' expr ')' :: `Func` ;
    atom = func2 '(' expr ',' expr ')' :: `Func` ;

    # Prefix expressions

    expr_pre = ident ;
    expr_pre = op expr_pre expr_pre :: `Op` ;
    expr_pre = func1 expr_pre :: `Func` ;
    expr_pre = func2 expr_pre expr_pre :: `Func` ;

    # Postfix expressions

    expr_post = ident sexpr_post :: `lambda x, f: f(x)` ;

    sexpr_post = expr_post op :: `lambda y, op: lambda x: Op(op, x, y)` ;
    sexpr_post = expr_post func2 :: `lambda y, f: lambda x: Func(f, x, y)` ;
    sexpr_post = func1 :: `lambda f: lambda x: Func(f, y)` ;

""")

while 1:
    e = input(":")
    if e == "": break
    try:
        expr, t = parser(e)
    except Exception as e:
        print(e)
    else:
        print(e, "is a", t, "expression")
        print("\tinfix   :", expr.infix())
        print("\tprefix  :", expr.prefix())
        print("\tpostfix :", expr.postfix())
