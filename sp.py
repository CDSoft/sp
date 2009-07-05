#!/usr/bin/env python3

""" Simple Parser

Simple Parser is a simple parser generator for Python.

Parsers are:
    - recursive descendent: the rules are easy to read and understand
    - written in pure Python: the rules are simple Python expressions

Parsers are made of Python classes and operators. A parser is a
Python expression that produces a callable object.

Example: a simple 4 operation calculator
----------------------------------------

>>> def calc_parser():
...     def applyall(x, fs):
...         for f in fs: x = f(x)
...         return x
...     num = Token(r'\d+') / int
...     with Separator(r'\s+'):
...         expr = Rule()
...         atom = num | Drop(r'\(') & expr & Drop(r'\)')
...         fact = Rule()
...         fact |= atom
...         fact |= (r'\+' & fact) * (lambda _, x: +x)
...         fact |= (r'\-' & fact) * (lambda _, x: -x)
...         term = ( fact & ( (r'\*' & fact) * (lambda _, y: lambda x: x*y)
...                         | (r'\/' & fact) * (lambda _, y: lambda x: x/y)
...                         )[:]
...                ) * applyall
...         expr |= ( term & ( (r'\+' & term) * (lambda _, y: lambda x: x+y)
...                          | (r'\-' & term) * (lambda _, y: lambda x: x-y)
...                          )[:]
...                 ) * applyall
...     return expr
>>> calc = calc_parser()
>>> calc("1+2+3")
6
>>> calc("1+(2*3)")
7
>>> calc("1-(2*3)")
-5

"""

__license__ = """Mini Parser is available under the GNU Lesser General Public.

Mini Parser: A Python parser generator

Copyright (C) 2007 Christophe Delord

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
"""

import re

def _p(obj):
    """ converts 'obj' to a parser object

    A parser is not changed:
    >>> word = Token('Ham') | Token('Spam')
    >>> id(_p(word)) == id(word)
    True

    A string is translated into a single Token parser
    >>> num = _p(r'\d+')
    >>> num.parse("42 43", "")
    ('42', ' 43', '')
    >>> num.parse("spam 42", "")
    (None, 'spam 42', '')

    Other types raise an exception:
    >>> _p(None)
    Traceback (most recent call last):
        ...
    TypeError: None is not a valid parser
    """
    if isinstance(obj, Parser): return obj
    if isinstance(obj, str): return Token(obj)
    raise TypeError("%s is not a valid parser"%obj)

class Parser:
    """ A parser shall have a parse method that:
        - trims separator before and after parsing (to allow different
            separators in a single parser)
        - recursively calls the child parsers
        - returns a tuple (value, "rest to parse", "min rest to parse") in case of success
        - returns a tuple (None, "initial value", "min rest to parse") in case of failure
    """

    def __init__(self):
        global _separator
        self.separator = _separator

    def __call__(self, s):
        """ removes separators before and after parsing and returns the object parsed

        >>> with Separator(r'\s+'):
        ...     num = Token('\d+') / int
        ...     num[:]("   42  43     ")
        [42, 43]

        raises a SyntaxError exception when the string can not be parsed:
        >>> num("\\nSpam")
        Traceback (most recent call last):
            ...
        SyntaxError: Parse error at line 2

        or when their are remaining stuff not parseable:
        >>> num("\\n\\n42\\n...")
        Traceback (most recent call last):
            ...
        SyntaxError: Parse error at line 4
        """
        s1, m = self.skipsep(s, s)
        x, s1, m = self.parse(s1, m)
        s1, m = self.skipsep(s1, m)
        if x is None or s1 != "":
            line = s.count('\n')-m.count('\n')+1
            raise SyntaxError("Parse error at line %d"%(line))
        return x

    def skipsep(self, s, m):
        """ removes separators from a string

        >>> with Separator(r'\s'):
        ...     p = Parser()
        >>> p.skipsep("   spam   ", "")
        ('spam   ', '')
        """
        if self.separator is None: return s, m
        while True:
            sep, s, m = self.separator.parse(s, m)
            if sep is None: return s, m

    def __and__(self, other):
        """ returns a sequence parser

        >>> ab = Token("a") & "b"
        >>> ab.parse("abc", "")
        (('a', 'b'), 'c', '')
        >>> ab.parse("bac", "")
        (None, 'bac', '')
        """
        return And(self, other)

    def __rand__(self, other):
        """ returns a sequence parser

        >>> ab = "a" & Token("b")
        >>> ab.parse("abc", "")
        (('a', 'b'), 'c', '')
        >>> ab.parse("bac", "")
        (None, 'bac', '')
        """
        return And(other, self)

    def __or__(self, other):
        """ returns an alternative parser

        >>> ab = Token("a") | "b"
        >>> ab.parse("abc", "")
        ('a', 'bc', '')
        >>> ab.parse("bac", "")
        ('b', 'ac', '')
        >>> ab.parse("cab", "")
        (None, 'cab', '')
        """
        return Or(self, other)

    def __ror__(self, other):
        """ returns an alternative parser

        >>> ab = "a" | Token("b")
        >>> ab.parse("abc", "")
        ('a', 'bc', '')
        >>> ab.parse("bac", "")
        ('b', 'ac', '')
        >>> ab.parse("cab", "")
        (None, 'cab', '')
        """
        return Or(other, self)

    def __getitem__(self, slice):
        """ returns a repetition parser

        Repetition is a slice:
            - start is the minimal occurrence number (default is 0)
            - stop is the maximal occurrence number (default is infinite)
            - step is the separator between each object (default is no separator)

        >>> with Separator('\s'):
        ...     item = Token(r'\d+') / int
        >>> item[:].parse("1 2 3 4", "")
        ([1, 2, 3, 4], '', '')
        >>> item[:2].parse("1 2 3 4", "")
        ([1, 2], '3 4', '')
        >>> item[::","].parse("1, 2, 3, 4", "")
        ([1, 2, 3, 4], '', '')
        """
        return Rep(self, slice.start, slice.stop, slice.step)

    def __truediv__(self, func):
        """ returns a parser that applies a function to the result of another parser

        The argument of the function is a single object.

        >>> sum = lambda xy: xy[0]+xy[1]
        >>> num = Token(r'\d+') / int               # parses an integer
        >>> add = (num & Drop(r'\+') & num) / sum   # returns the sum of two integers
        >>> add.parse("1+2+4", "")
        (3, '+4', '')
        """
        return Apply(self, func)

    def __mul__(self, func):
        """ returns a parser that applies a function to the result of another parser

        The arguments of the function are in a list or a tuple,
        each item being given to the function as a separate argument.

        >>> sum = lambda x,y: x+y
        >>> num = Token(r'\d+') / int               # parses an integer
        >>> add = (num & Drop(r'\+') & num) * sum   # returns the sum of two integers
        >>> add.parse("1+2+4", "")
        (3, '+4', '')
        """
        return ApplyStar(self, func)

class Separator:
    """ defines the current separator

    A classical usage is to define tokens outside a separator block
    and rules inside a separator block (for instance to discard spaces and comments).
    The separator is defined by a parser or a single string containing a regular expression.

    It is designed to be used within a 'with' block:

    >>> num = Token(r'\d+')
    >>> nums = num[:]
    >>> nums.parse(" 42 43 ", "") # matches nothing at the very beginning of the input
    ([], ' 42 43 ', '')
    >>> with Separator(r'\s'):
    ...     nums = num[:]
    >>> nums.parse(" 42 43 ", "") # can match numbers after discarding spaces
    (['42', '43'], '', '')
    """

    def __init__(self, parser):
        self.parser = _p(parser)

    def __enter__(self):
        global _separator
        self.previous_parser = _separator
        _separator = self.parser

    def __exit__(self, type, value, traceback):
        global _separator
        _separator = self.previous_parser

_separator = None

class Token(Parser):
    """ is a single token parser

    The token is defined by a regular expression.
    The token returns the string matched by the regular expression.

    >>> t = Token(r'ham|spam')
    >>> with Separator(r'\s'): t[:]("ham ham spam")
    ['ham', 'ham', 'spam']
    """

    def __init__(self, pattern, flags=0):
        Parser.__init__(self)
        self.pattern = re.compile(pattern, flags)

    def parse(self, s, m):
        s1, m = self.skipsep(s, m)
        token = self.pattern.match(s1)
        if not token: return None, s, m
        token = token.group(0)
        rest, m = self.skipsep(s1[len(token):], m[len(token):])
        return token, rest, m

class Const(Parser):
    """ is a constant parser

    Const parses nothing, simply returns a constant.

    >>> c = Const('foo')
    >>> c.parse("spam", "")
    ('foo', 'spam', '')
    """

    def __init__(self, val):
        Parser.__init__(self)
        self.val = val

    def parse(self, s, m):
        s, m = self.skipsep(s, m)
        return self.val, s, m

class Drop(Parser):
    """ parses something and replaces the value by 'nil'

    Used to discard some items returned by a sequence parser.
    The sequence parser will ignore 'nil' items.

    >>> num = Token(r'\d+') / int
    >>> expr = r'\(' & num & r'\)'
    >>> expr("(42)")
    ('(', 42, ')')

    >>> expr = Drop(r'\(') & num & Drop(r'\)')
    >>> expr("(42)")
    42
    """

    def __init__(self, parser):
        Parser.__init__(self)
        self.parser = _p(parser)

    def parse(self, s, m):
        rest, m = self.skipsep(s, m)
        x, rest, m = self.parser.parse(rest, m)
        if x is None: return None, s, m
        rest, m = self.skipsep(rest, m)
        return nil, rest, m

class And(Parser):
    """ parses a sequence.

    Takes parsers as arguments.
    If these arguments are also sequences, the sequence is flatten.
    And(And(x,y),z) <=> And(x,And(y,z)) <=> And(x,y,z)
    or (x & y) & z <=> x & (y & z) <=> x & y & z

    The sequence returns a tuple containing the values returned by the subparsers.
    >>> s = Token('a') & 'b' & 'c'
    >>> s("abc")
    ('a', 'b', 'c')

    Arguments may return 'nil'. Their values will be ignored.
    >>> s = Token('a') & Drop('b') & 'c'
    >>> s("abc")
    ('a', 'c')

    If the sequence returns only one argument, it is not returned in a tuple.
    >>> s = Drop('a') & Drop('b') & 'c'
    >>> s("abc")
    'c'
    """

    def __init__(self, *parsers):
        Parser.__init__(self)
        self.items = []
        for parser in parsers:
            if isinstance(parser, And): self.items.extend(parser.items)
            else: self.items.append(_p(parser))

    def parse(self, s, m):
        tokens = []
        rest, m = self.skipsep(s, m)
        for item in self.items:
            token, rest, m = item.parse(rest, m)
            if token is None: return None, s, m
            if token is not nil: tokens.append(token)
            rest, m = self.skipsep(rest, m)
        if len(tokens) == 1: return tokens[0], rest, m
        return tuple(tokens), rest, m

class _Nil:
    pass
nil = _Nil()

class Or(Parser):
    """ parses an alternative.

    Takes parsers as arguments.
    If these arguments are also alternatives, the alternative is flatten.
    Or(Or(x,y),z) <=> Or(x,Or(y,z)) <=> Or(x,y,z)
    or (x | y) | z <=> x | (y | z) <=> x | y | z

    The alternative returns the value returns by the first matching subparser.
    >>> s = Token('a') | 'b' | 'c'
    >>> s("b")
    'b'
    """

    def __init__(self, *parsers):
        Parser.__init__(self)
        self.items = []
        for parser in parsers:
            if isinstance(parser, Or): self.items.extend(parser.items)
            else: self.items.append(_p(parser))

    def parse(self, s, m):
        s1, m = self.skipsep(s, m)
        for item in self.items:
            token, rest, m = item.parse(s1, m)
            if token is not None:
                rest, m = self.skipsep(rest, m)
                return token, rest, m
        return None, s, m

class Rule(Parser):
    """ returns an empty parser that can be later enriched.

    Used to define recursive rules.
    The symbol can be used even before the rule is defined.
    Alternatives are added by the |= operator.

    >>> As = Rule()
    >>> A = Token('A')
    >>> As |= A & As
    >>> As |= Const(())
    >>> As("AAA")
    ('A', ('A', ('A', ())))
    """

    def __init__(self):
        Parser.__init__(self)
        self.parser = None

    def __ior__(self, parser):
        if self.parser is None: self.parser = _p(parser)
        else: self.parser = Or(self.parser, parser)
        return self

    def parse(self, s, m):
        s1, m = self.skipsep(s, m)
        x, rest, m = self.parser.parse(s1, m)
        if x is None: return None, s, m
        rest, m = self.skipsep(rest, m)
        return x, rest, m

class Rep(Parser):
    """ parses repetitions.

    Arguments:
        min is the minimal number of iterations (default is 0)
        max is the maximal number of iterations (default is infinite)
        step is the separator parser (default is none)

    Classical repetition can be coded this way:
        A* <=> A[:] (zero or more A)
        A+ <=> A[1:] (one or more A)
        A? <=> A[:1] (zero or one A)

    >>> A = Token('A')
    >>> As = A[:]
    >>> As('')
    []
    >>> As('A')
    ['A']
    >>> As('AAA')
    ['A', 'A', 'A']

    >>> As = A[1:]
    >>> As('')
    Traceback (most recent call last):
        ...
    SyntaxError: Parse error at line 1
    >>> As('A')
    ['A']
    >>> As('AAA')
    ['A', 'A', 'A']

    >>> As = A[:1]
    >>> As('')
    []
    >>> As('A')
    ['A']
    >>> As('AAA')
    Traceback (most recent call last):
        ...
    SyntaxError: Parse error at line 1

    The separator provides a convenient way to parse comma separated lists for instance.
    >>> t = Token(r'\w')
    >>> lst = t[::',']
    >>> lst("a,b,c")
    ['a', 'b', 'c']
    """

    def __init__(self, parser, min, max, sep):
        Parser.__init__(self)
        self.parser = parser
        if min is None: min = 0
        if max is None: max = -1
        self.min = min
        self.max = max
        if sep is None: self.parse = self._parse_no_sep
        else: 
            self.parse = self._parse_with_sep
            self.sep = _p(sep)

    def _parse_no_sep(self, s, m):
        items = []
        i = 0
        rest, m = self.skipsep(s, m)
        while i != self.max:
            i += 1
            item, rest, m = self.parser.parse(rest, m)
            if item is None:
                if i <= self.min: return None, s, m
                return items, rest, m
            items.append(item)
            rest, m = self.skipsep(rest, m)
        return items, rest, m

    def _parse_with_sep(self, s, m):
        rest, m = self.skipsep(s, m)
        item, rest, m = self.parser.parse(rest, m)
        if item is None:
            if 1 <= self.min: return None, s, m
            rest, m = self.skipsep(rest, m)
            return [], rest, m
        items = [item]
        i = 1
        rest, m = self.skipsep(rest, m)
        while i != self.max:
            i += 1
            sep, rest, m = self.sep.parse(rest, m)
            if sep is None:
                if i <= self.min: return None, s, m
                return items, rest, m
            rest, m = self.skipsep(rest, m)
            item, rest, m = self.parser.parse(rest, m)
            if item is None:
                if i <= self.min: return None, s, m
                return items, rest, m
            items.append(item)
            rest, m = self.skipsep(rest, m)
        return items, rest, m

class Apply(Parser):
    """ applies a function to the result of a parser

    The function has one argument.

    >>> num = Apply(Token(r'\d+'), int)
    >>> a = Apply(num, int)
    >>> a("42")
    42

    >>> a = Apply(num&Drop(',')&num&Drop(',')&num, (lambda xs: xs[0]+xs[1]+xs[2]))
    >>> a("1,2,3")
    6
    """

    def __init__(self, parser, func):
        Parser.__init__(self)
        self.parser = _p(parser)
        if isinstance(func, str): self.func = lambda _: func
        else: self.func = func

    def parse(self, s, m):
        s1, m = self.skipsep(s, m)
        token, rest, m = self.parser.parse(s1, m)
        if token is None: return None, s, m
        rest, m = self.skipsep(rest, m)
        return self.func(token), rest, m

class ApplyStar(Apply):
    """ applies a function to the result of some parsers

    The function may have several arguments.

    >>> num = Apply(Token(r'\d+'), int)
    >>> a = ApplyStar(num&Drop(',')&num&Drop(',')&num, (lambda x,y,z: x+y+z))
    >>> a("1,2,3")
    6

    >>> a = ApplyStar(num[::','], (lambda *xs: sum(xs)))
    >>> a("1,2,3")
    6
    """

    def parse(self, s, m):
        s1, m = self.skipsep(s, m)
        token, rest, m = self.parser.parse(s1, m)
        if token is None: return None, s, m
        rest, m = self.skipsep(rest, m)
        return self.func(*token), rest, m

if __name__ == '__main__':
    import doctest
    doctest.testmod()
