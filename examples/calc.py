#!/usr/bin/env python

# Simple Parser
# Copyright (C) 2009-2010 Christophe Delord
# http://cdelord.fr/sp

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

from __future__ import division

import math
import sys
import sp

try:
    import readline
except ImportError:
    pass

class Calc(dict):

    def __init__(self):
        dict.__init__(self)
        _fy = lambda f, y: lambda x: f(x, y)
        _fg = lambda f, g: lambda x: g(f(x))
        _ = lambda x: x
        fx = lambda f, x: f(x)
        xf = lambda x, f: f(x)
        def reduce(x, fs):
            for f in fs: x = f(x)
            return x
        self.parser = sp.compile(r"""

            ident = r'[a-zA-Z_]\w*' ;

            real = r'(?:\d+\.\d*|\d*\.\d+)(?:[eE][-+]?\d+)?|\d+[eE][-+]?\d+' : `float` ;
            int = r'\d+' : `int` ;
            var = ident : `self.__getitem__`;

            add_op = '+'    `lambda x, y: x + y` ;
            add_op = '-'    `lambda x, y: x - y` ;
            add_op = '|'    `lambda x, y: x | y` ;
            add_op = '^'    `lambda x, y: x ^ y` ;

            mul_op = '*'    `lambda x, y: x * y` ;
            mul_op = '/'    `lambda x, y: x / y` ;
            mul_op = '%'    `lambda x, y: x % y` ;
            mul_op = '&'    `lambda x, y: x & y` ;
            mul_op = '>>'   `lambda x, y: x << y` ;
            mul_op = '<<'   `lambda x, y: x >> y` ;

            pow_op = '**'   `lambda x, y: x ** y` ;

            un_op = '+'     `lambda x: +x` ;
            un_op = '-'     `lambda x: -x` ;
            un_op = '~'     `lambda x: ~x` ;

            post_un_op = '!'    `math.factorial` ;

            separator: r'\s+';

            !S = ident '=' expr :: `self.__setitem__`
               | expr
               ;

            expr = term (add_op term :: `_fy`)* :: `reduce` ;
            term = fact (mul_op fact :: `_fy`)* :: `reduce` ;
            fact = un_op fact :: `fx` | pow ;
            pow = postfix (pow_op fact :: `_fy`)? :: `reduce` ;

            #postfix = atom post_un_op :: `xf` | atom ;
            postfix = atom _postfix :: `xf` ;
            _postfix = post_un_op _postfix :: `_fg` | `_` ;

            atom = '(' expr ')' ;
            atom = real | int ;
            atom = var ;

        """)

    def __call__(self, input):
        return self.parser(input)

def exc():
    e = getattr(sys, 'exc_value', None)
    if e is None:
        info = getattr(sys, 'exc_info', None)
        if info is not None: e = info()[1]
    return e

try: raw_input
except NameError: raw_input = input

calc = Calc()

while True:
    expr = raw_input(": ")
    sp.clean()
    try:
        val = calc(expr)
        if val is not None:
            print("= %s"%calc(expr))
    except:
        print("! %s"%exc())
    print("")
