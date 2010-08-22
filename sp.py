#!/usr/bin/env python

# Simple Parser
# Copyright (C) 2009-2010 Christophe Delord
# http://cdsoft.fr/sp

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
...     num = R(r'\d+') / int
...     with Separator(r'\s+'):
...         expr = Rule()
...         atom = num | '(' & expr & ')'
...         fact = Rule()
...         fact |= atom
...         fact |= ('+' & fact) / (lambda x: +x)
...         fact |= ('-' & fact) / (lambda x: -x)
...         term = ( fact & ( ('*' & fact) / (lambda y: lambda x: x*y)
...                         | ('/' & fact) / (lambda y: lambda x: x/y)
...                         )[:]
...                ) * applyall
...         expr |= ( term & ( ('+' & term) / (lambda y: lambda x: x+y)
...                          | ('-' & term) / (lambda y: lambda x: x-y)
...                          )[:]
...                 ) * applyall
...     return expr
>>> calc1 = calc_parser()
>>> calc1("1 + 2+3")
6
>>> calc1("1 + (2*3)")
7
>>> calc1("1 - (2*3)")
-5

Example: another calculator using the SP language
-------------------------------------------------

>>> from operator import add, sub, mul, truediv as div, mod
>>> op2 = lambda f, y: lambda x: f(x, y)
>>> op1 = lambda f, x: f(0, x)
>>> def red(x, fs):
...     for f in fs: x = f(x)
...     return x
>>> calc2 = compile(r'''
...         number = number.r'\d+' : `int`;
...         addop = '+' `add` | '-' `sub` ;
...         mulop = '*' `mul` | '/' `div` | '%' `mod`;
...
...         separator: r'\s+';
...
...         !expr = term (addop term :: `op2`)* :: `red`;
...         term = fact (mulop fact :: `op2`)* :: `red`;
...         fact = addop fact :: `op1` | '(' expr ')' | number;
...     ''')
>>> calc2("1 + 2+3")
6
>>> calc2("1 + (2*3)")
7
>>> calc2("1 - (2*3)")
-5
"""

__license__ = """
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

import re
import sys

# "from sp import *" only imports objects for hand written parsers
__all__ = ['R', 'K', 'C', 'At', 'D', 'Rule', 'Separator']

class _Caches(list):
    def add(self, cache):
        self.append(cache)
    def clear(self):
        for cache in self:
            cache.clear()
_caches = _Caches()

def clean():
    """ clears the SP internal caches

    >>> clean(); sum(len(cache) for cache in _caches) == 0
    True
    >>> with Separator(' '): p = K('A') | 'B'
    >>> p('A')
    nil
    >>> p('B')
    nil
    >>> sum(len(cache) for cache in _caches) > 0
    True
    >>> clean(); sum(len(cache) for cache in _caches) == 0
    True
    """
    _caches.clear()

def _memoize_self_s_i(f):
    """ creates a memoized parser method

        arguments self, s and i are memoized.
    """
    cache = {}
    def _f(self, s, i):
        try:
            r = cache[self, s, i]
        except KeyError:
            r = cache[self, s, i] = f(self, s, i)
        return r
    _f.__doc__ = f.__doc__
    _f.__name__ = f.__name__
    _caches.add(cache)
    return _f

def _memoize_self_s_i_e(f):
    """ creates a memoized parser method

        arguments self, s and i are memoized.
        The error argument (e) is not indexed.
    """
    cache = {}
    def _f(self, s, i, e):
        try:
            r = cache[self, s, i]
        except KeyError:
            r = cache[self, s, i] = f(self, s, i, e)
        return r
    _f.__doc__ = f.__doc__
    _f.__name__ = f.__name__
    _caches.add(cache)
    return _f

class _pos:
    """ computes the position in a string
    """
    def __init__(self, s, i):
        self.index = i
        self.line = s.count('\n', 0, i) + 1
        self.column = i - s.rfind('\n', 0, i)
    def __str__(self): return "[%d:%d]"%(self.line, self.column)

class _err:
    """ stores the maximal position of the detected errors
    """

    def __init__(self, i, *ts):
        self.i = i
        self.ts = tuple(ts)

    def max(self, other):
        if self.i > other.i:
            return self
        elif self.i < other.i:
            return other
        else:
            return _err(self.i, *(self.ts + tuple(t for t in other.ts if t not in self.ts)))

    def msg(self, s):
        """ returns a message with the location of the error

        >>> num = R('\d+')
        >>> ident = R('\w+')
        >>> begin = 'begin'
        >>> with Separator('\s+'):
        ...     s = begin & ( num | ident )
        >>> s('18')
        Traceback (most recent call last):
            ....
        SyntaxError: [1:1] expected: begin...
        >>> s('begin +18')
        Traceback (most recent call last):
            ....
        SyntaxError: [1:7] expected: \d+ \w+...
        """
        p = _pos(s, self.i)
        msg = "[%d:%d] expected:"%(p.line, p.column)
        for t in self.ts:
            if t.startswith(r'\b'): t = t[2:]
            if t.endswith(r'\b'): t = t[:-2]
            msg += " "+t
        err = SyntaxError(msg)
        err.lineno = p.line
        return err

def _p(obj):
    """ converts 'obj' to a parser object

    A parser is not changed:
    >>> word = K('Ham') | K('Spam')
    >>> id(_p(word)) == id(word)
    True

    A string is translated into a single Keyword parser.
    >>> num = _p(r'\d+')
    >>> num.parse("42 43", 0, _err(0))[:2]
    (fail, 0)
    >>> num.parse(r"\d+ 42 43", 0, _err(0))[:2]
    (nil, 3)
    >>> kw = _p('foo')
    >>> kw.parse("foo bar", 0, _err(0))[:2]
    (nil, 3)
    >>> kw.parse("foobar", 0, _err(0))[:2]
    (fail, 0)

    Other types raise an exception:
    >>> _p(None)
    Traceback (most recent call last):
        ...
    TypeError: None is not a valid parser
    """
    if isinstance(obj, Parser): return obj
    if isinstance(obj, str): return K(obj)
    raise TypeError("%s is not a valid parser"%obj)

class Parser:
    """ A parser shall have a parse method that:
        - trims separator before and after parsing (to allow different
            separators in a single parser)
        - recursively calls the child parsers
        - returns a tuple (value, index, error) in case of success
            if value is nil, it will be later ignored
        - returns a tuple (fail, "initial index", error) in case of failure
        error is the latest error detected.
    """

    def __init__(self):
        global _separator
        self.separator = _separator

    def __call__(self, s):
        """ removes separators before and after parsing and returns the object parsed

        >>> with Separator(r'\s+'):
        ...     num = R('\d+') / int
        ...     num[:]("   42  43     ")
        [42, 43]

        raises a SyntaxError exception when the string can not be parsed:
        >>> num("\\nSpam")
        Traceback (most recent call last):
            ...
        SyntaxError: [2:1] expected: \d+...

        or when their are remaining stuff not parseable:
        >>> num("\\n\\n42\\n...")
        Traceback (most recent call last):
            ...
        SyntaxError: [4:1] expected:...
        """
        i = self.skipsep(s, 0)
        x, i, e = self.parse(s, i, _err(i))
        i = self.skipsep(s, i)
        if x is fail or i < len(s):
            raise e.msg(s)
        return x

    @_memoize_self_s_i
    def skipsep(self, s, i):
        """ removes separators from a string

        >>> with Separator(r'\s'):
        ...     p = Parser()
        >>> p.skipsep("   spam   ", 0)
        3
        """
        if self.separator is None: return i
        while True:
            sep, i, e = self.separator.parse(s, i, _err(i))
            if sep is fail: return i

    def __and__(self, other):
        """ returns a sequence parser

        >>> with Separator(" "): ab = R("a") & "b"
        >>> ab.parse("a b c", 0, _err(0))[:2]
        ('a', 4)
        >>> ab.parse("b a c", 0, _err(0))[:2]
        (fail, 0)
        """
        return And(self, other)

    def __rand__(self, other):
        """ returns a sequence parser

        >>> with Separator(" "): ab = "a" & R("b")
        >>> ab.parse("a b c", 0, _err(0))[:2]
        ('b', 4)
        >>> ab.parse("b a c", 0, _err(0))[:2]
        (fail, 0)
        """
        return And(other, self)

    def __or__(self, other):
        """ returns an alternative parser

        >>> with Separator(" "): ab = R("a") | "b"
        >>> ab.parse("a b c", 0, _err(0))[:2]
        ('a', 2)
        >>> ab.parse("b a c", 0, _err(0))[:2]
        (nil, 2)
        >>> ab.parse("c a b", 0, _err(0))[:2]
        (fail, 0)
        """
        return Or(self, other)

    def __ror__(self, other):
        """ returns an alternative parser

        >>> with Separator(" "): ab = "a" | R("b")
        >>> ab.parse("a b c", 0, _err(0))[:2]
        (nil, 2)
        >>> ab.parse("b a c", 0, _err(0))[:2]
        ('b', 2)
        >>> ab.parse("c a b", 0, _err(0))[:2]
        (fail, 0)
        """
        return Or(other, self)

    def __getitem__(self, slice):
        """ returns a repetition parser

        Repetition is a slice:
            - start is the minimal occurrence number (default is 0)
            - stop is the maximal occurrence number (default is infinite)
            - step is the separator between each object (default is no separator)

        >>> with Separator('\s'):
        ...     item = R(r'\d+') / int
        >>> item[:].parse("1 2 3 4", 0, _err(0))[:2]
        ([1, 2, 3, 4], 7)
        >>> item[:2].parse("1 2 3 4", 0, _err(0))[:2]
        ([1, 2], 4)
        >>> item[::","].parse("1, 2, 3, 4", 0, _err(0))[:2]
        ([1, 2, 3, 4], 10)
        """
        return Rep(self, slice.start, slice.stop, slice.step)

    def __truediv__(self, func):
        """ returns a parser that applies a function to the result of another parser

        The argument of the function is a single object.

        >>> sum = lambda xy: xy[0]+xy[1]
        >>> num = R(r'\d+') / int               # parses an integer
        >>> add = (num & '+' & num) / sum       # returns the sum of two integers
        >>> add.parse("1+2+4", 0, _err(0))[:2]
        (3, 3)
        """
        return Apply(self, func)

    # Python 2 fallback
    if sys.version_info[0] < 3: __div__ = __truediv__

    def __mul__(self, func):
        """ returns a parser that applies a function to the result of another parser

        The arguments of the function are in a list or a tuple,
        each item being given to the function as a separate argument.

        >>> sum = lambda x,y: x+y
        >>> num = R(r'\d+') / int               # parses an integer
        >>> add = (num & '+' & num) * sum       # returns the sum of two integers
        >>> add.parse("1+2+4", 0, _err(0))[:2]
        (3, 3)
        """
        return ApplyStar(self, func)

class Separator:
    """ defines the current separator

    A classical usage is to define tokens outside a separator block
    and rules inside a separator block (for instance to discard spaces and comments).
    The separator is defined by a parser or a single string containing a regular expression.

    It is designed to be used within a 'with' block:

    >>> num = R(r'\d+')
    >>> nums = num[:]
    >>> nums.parse(" 42 43 ", 0, _err(0))[:2] # matches nothing at the very beginning of the input
    ([], 0)
    >>> with Separator(r'\s'):
    ...     nums = num[:]
    >>> nums.parse(" 42 43 ", 0, _err(0))[:2] # can match numbers after discarding spaces
    (['42', '43'], 7)
    """

    def __init__(self, parser=None):
        if isinstance(parser, str): parser = R(parser)
        elif parser is not None: parser = _p(parser)
        self.parser = parser

    def __enter__(self):
        global _separator
        self.previous_parser = _separator
        _separator = self.parser

    def __exit__(self, type=None, value=None, traceback=None):
        global _separator
        _separator = self.previous_parser

_separator = None

class R(Parser):
    """ is a single token parser

    The token is defined by a regular expression.
    The token returns the string matched by the regular expression.

    >>> t = R(r'ham|spam')
    >>> with Separator(r'\s'): t[:]("ham ham spam")
    ['ham', 'ham', 'spam']

    If the regular expression defines groups, the parser returns
    a tuple of these groups.

    >>> t = R('<(\d+)-(\d+)>')
    >>> t("<42-43>")
    ('42', '43')

    If the regular expression defines only one group, the parser
    returns the value of this group.

    >>> t = R('<(\d+)-\d+>')
    >>> t("<42-43>")
    '42'
    """

    def __init__(self, pattern, flags=0, name=None):
        Parser.__init__(self)
        self.pattern = name or pattern
        self.re = re.compile(pattern, flags)

    def parse(self, s, i, e):
        i1 = self.skipsep(s, i)
        token = self.re.match(s, i1)
        if not token: return fail, i, e.max(_err(i1, self.pattern))
        matched = token.group(0)
        if token.lastindex:
            value = token.groups()
            if len(value) == 1: value = value[0]
        else:
            value = matched
        rest = self.skipsep(s, i1 + len(matched))
        return value, rest, e.max(_err(rest))

class K(R):
    """ is a keyword parser

    Works a bit as R.

    If the pattern is a keyword (\w+)
    word boundaries are expected around the token.
    >>> t = K('ham') & C('ok')
    >>> with Separator(r'\s'): t[:].parse("ham ham hammm", 0, _err(0))[:2]
    (['ok', 'ok'], 8)

    Otherwise the pattern is escaped.
    >>> t = K('++') & C('pp')
    >>> with Separator(r'\s'): t[:].parse("++ ++ +++", 0, _err(0))[:2]
    (['pp', 'pp', 'pp'], 8)
    """

    def __init__(self, pattern, flags=0, name=None):
        Parser.__init__(self)
        self.pattern = name or pattern
        if pattern.isalnum(): pattern = r"\b%s\b"%pattern
        else: pattern = re.escape(pattern)
        self.re = re.compile(pattern, flags)

    def parse(self, s, i, e):
        obj, rest, e = R.parse(self, s, i, e)
        if obj is fail: return fail, rest, e
        else: return nil, rest, e

class C(Parser):
    """ is a constant parser

    C parses nothing, simply returns a constant.

    >>> c = C('foo')
    >>> c.parse("spam", 0, _err(0))[:2]
    ('foo', 0)
    """

    def __init__(self, val):
        Parser.__init__(self)
        self.val = val

    def parse(self, s, i, e):
        i = self.skipsep(s, i)
        return self.val, i, e.max(_err(i))

class At(Parser):
    r""" returns the current position

    >>> with Separator('\s'): p = K('a')[:] & At() & 'b'
    >>> p = p * (lambda a, p: (p.index, p.line, p.column))
    >>> p('b')
    (0, 1, 1)
    >>> p('\nb')
    (1, 2, 1)
    >>> p('a b')
    (2, 1, 3)
    >>> p('a\nb')
    (2, 2, 1)
    >>> p('a a b')
    (4, 1, 5)
    >>> p('a\na\nb')
    (4, 3, 1)
    """

    def parse(self, s, i, e):
        i = self.skipsep(s, i)
        return _pos(s, i), i, e.max(_err(i))

class D(Parser):
    """ parses something and replaces the value by 'nil'

    Used to discard some items returned by a sequence parser.
    The sequence parser will ignore 'nil' items.

    >>> num = R(r'\d+') / int
    >>> expr = R(r'\(') & num & R(r'\)')
    >>> expr("(42)")
    ('(', 42, ')')

    >>> expr = D(R(r'\(')) & num & D(R(r'\)'))
    >>> expr("(42)")
    42
    """

    def __init__(self, parser):
        Parser.__init__(self)
        self.parser = _p(parser)

    def parse(self, s, i, e):
        rest = self.skipsep(s, i)
        x, rest, e = self.parser.parse(s, rest, e)
        if x is fail: return fail, i, e.max(_err(rest))
        rest = self.skipsep(s, rest)
        return nil, rest, e.max(_err(rest))

class And(Parser):
    """ parses a sequence.

    Takes parsers as arguments.
    If these arguments are also sequences, the sequence is flatten.
    And(And(x,y),z) <=> And(x,And(y,z)) <=> And(x,y,z)
    or (x & y) & z <=> x & (y & z) <=> x & y & z

    The sequence returns a tuple containing the values returned by the subparsers.
    >>> s = R('a') & R('b') & R('c')
    >>> s("abc")
    ('a', 'b', 'c')
    >>> s = (R('a') & R('b')) & R('c')
    >>> s("abc")
    ('a', 'b', 'c')
    >>> s = R('a') & (R('b') & R('c'))
    >>> s("abc")
    ('a', 'b', 'c')

    Arguments may return 'nil'. Their values will be ignored.
    >>> s = R('a') & D(R('b')) & R('c')
    >>> s("abc")
    ('a', 'c')

    If the sequence returns only one argument, it is not returned in a tuple.
    >>> s = D(R('a')) & D(R('b')) & R('c')
    >>> s("abc")
    'c'
    """

    def __init__(self, *parsers):
        Parser.__init__(self)
        self.items = []
        for parser in parsers:
            if isinstance(parser, And): self.items.extend(parser.items)
            else: self.items.append(_p(parser))

    @_memoize_self_s_i_e
    def parse(self, s, i, e):
        tokens = []
        rest = self.skipsep(s, i)
        for item in self.items:
            token, rest, e = item.parse(s, rest, e)
            if token is fail: return fail, i, e.max(_err(rest))
            if token is not nil: tokens.append(token)
            rest = self.skipsep(s, rest)
        if len(tokens) == 1: return tokens[0], rest, e.max(_err(rest))
        return tuple(tokens), rest, e.max(_err(rest))

class _Singleton:
    def __init__(self, name): self.name = name
    def __repr__(self): return self.name
nil = _Singleton("nil")
fail = _Singleton("fail")

class Or(Parser):
    """ parses an alternative.

    Takes parsers as arguments.
    If these arguments are also alternatives, the alternative is flatten.
    Or(Or(x,y),z) <=> Or(x,Or(y,z)) <=> Or(x,y,z)
    or (x | y) | z <=> x | (y | z) <=> x | y | z

    The alternative returns the value returns by the first longest matching subparser.
    >>> s = R('a') | R('b') | R('c')
    >>> s("b")
    'b'
    >>> s = (R('a') | R('b')) | R('c')
    >>> s("b")
    'b'
    >>> s = R('a') | (R('b') | R('c'))
    >>> s("b")
    'b'
    >>> s = R('a')[:] | R('b')[:]
    >>> s('aa')
    ['a', 'a']
    >>> s('bb')
    ['b', 'b']
    >>> s('')
    []
    >>> s('ba')
    Traceback (most recent call last):
        ...
    SyntaxError: [1:2] expected: b...
    >>> s = R('a')[:] & C('branch 1') | R('a')[:] & R('b')[:] & C('branch 2')
    >>> s('aa')
    (['a', 'a'], 'branch 1')
    >>> s('aab')
    (['a', 'a'], ['b'], 'branch 2')
    """

    def __init__(self, *parsers):
        Parser.__init__(self)
        self.items = []
        for parser in parsers:
            if isinstance(parser, Or): self.items.extend(parser.items)
            else: self.items.append(_p(parser))

    @_memoize_self_s_i_e
    def parse(self, s, i, e):
        i1 = self.skipsep(s, i)
        e = e.max(_err(i1))
        longest = (None, -1)
        for item in self.items:
            token, rest, e = item.parse(s, i1, e)
            if token is not fail:
                rest = self.skipsep(s, rest)
                e = e.max(_err(rest))
                if rest > longest[1]:
                    longest = (token, rest)
        if longest[1] > -1:
            # Returns the longest match
            return longest + (e,)
        else:
            return fail, i, e

class Rule(Parser):
    """ returns an empty parser that can be later enriched.

    Used to define recursive rules.
    The symbol can be used even before the rule is defined.
    Alternatives are added by the |= operator.

    >>> As = Rule()
    >>> A = R('A')
    >>> As |= A & As
    >>> As |= C(())
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

    def parse(self, s, i, e):
        i1 = self.skipsep(s, i)
        x, rest, e = self.parser.parse(s, i1, e)
        if x is fail: return fail, i, e.max(_err(rest))
        rest = self.skipsep(s, rest)
        return x, rest, e.max(_err(rest))

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

    >>> A = R('A')
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
    SyntaxError: [1:1] expected: A...
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
    SyntaxError: [1:2] expected:...

    The separator provides a convenient way to parse comma separated lists for instance.
    >>> t = R(r'\w')
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

    def _parse_no_sep(self, s, i, e):
        items = []
        n = 0
        rest = self.skipsep(s, i)
        while n != self.max:
            n += 1
            item, rest, e = self.parser.parse(s, rest, e)
            if item is fail:
                if n <= self.min: return fail, i, e.max(_err(rest))
                return items, rest, e.max(_err(rest))
            items.append(item)
            rest = self.skipsep(s, rest)
        return items, rest, e.max(_err(rest))

    def _parse_with_sep(self, s, i, e):
        rest = self.skipsep(s, i)
        item, rest, e = self.parser.parse(s, rest, e)
        if item is fail:
            if 1 <= self.min: return fail, i, e.max(_err(rest))
            rest = self.skipsep(s, rest)
            return [], rest, e.max(_err(rest))
        items = [item]
        n = 1
        rest = self.skipsep(s, rest)
        while n != self.max:
            n += 1
            sep, rest, e = self.sep.parse(s, rest, e)
            if sep is fail:
                if n <= self.min: return fail, i, e.max(_err(rest))
                return items, rest, e.max(_err(rest))
            rest = self.skipsep(s, rest)
            item, rest, e = self.parser.parse(s, rest, e)
            if item is fail:
                if n <= self.min: return fail, i, e.max(_err(rest))
                return items, rest, e.max(_err(rest))
            items.append(item)
            rest = self.skipsep(s, rest)
        return items, rest, e.max(_err(rest))

class Apply(Parser):
    """ applies a function to the result of a parser

    The function has one argument.

    >>> num = Apply(R(r'\d+'), int)
    >>> a = Apply(num, int)
    >>> a("42")
    42

    >>> a = Apply(num&','&num&','&num, (lambda xs: xs[0]+xs[1]+xs[2]))
    >>> a("1,2,3")
    6
    """

    def __init__(self, parser, func):
        Parser.__init__(self)
        self.parser = _p(parser)
        self.func = func

    def parse(self, s, i, e):
        i1 = self.skipsep(s, i)
        token, rest, e = self.parser.parse(s, i1, e)
        if token is fail: return fail, i, e.max(_err(rest))
        rest = self.skipsep(s, rest)
        return self.func(token), rest, e.max(_err(rest))

class ApplyStar(Apply):
    """ applies a function to the result of some parsers

    The function may have several arguments.

    >>> num = Apply(R(r'\d+'), int)
    >>> a = ApplyStar(num&','&num&','&num, (lambda x,y,z: x+y+z))
    >>> a("1,2,3")
    6

    >>> a = ApplyStar(num[::','], (lambda *xs: sum(xs)))
    >>> a("1,2,3")
    6
    """

    def parse(self, s, i, e):
        i1 = self.skipsep(s, i)
        token, rest, e = self.parser.parse(s, i1, e)
        if token is fail: return fail, i, e.max(_err(rest))
        rest = self.skipsep(s, rest)
        return self.func(*token), rest, e.max(_err(rest))

def _compile_string(source, frame):
    r""" defines a parser from a grammar

    Token definition
    ----------------

    >>> test = compile('''
    ...     name = r'\w+' ;
    ...     !S = name ;
    ... ''')
    >>> test('foo')
    'foo'
    >>> test(':foo')
    Traceback (most recent call last):
        ...
    SyntaxError: [1:1] expected: \w+...

    With specific lexer options:
    >>> test = compile('''
    ...     lexer: VERBOSE, IGNORECASE;
    ...     string = r" ' ( [^']* ) ' ";
    ...     lexer: IGNORECASE;
    ...     begin = "begin";
    ...     !S = string | begin;
    ... ''')
    >>> test("'this is a string'")
    'this is a string'
    >>> test("BeGiN")
    nil

    Keyword definition
    ------------------

    >>> test = compile('''
    ...     kw = 'begin' ;
    ...     !S = kw ;
    ... ''')
    >>> test('begin')
    nil
    >>> test('end')
    Traceback (most recent call last):
        ...
    SyntaxError: [1:1] expected: begin...

    Position computation
    --------------------

    >>> test = compile('''
    ...     separator: r'\s+';
    ...     !S = 'a'* @ 'b' :: `lambda a, p: (p.index, p.line, p.column)`;
    ... ''')
    >>> test('b')
    (0, 1, 1)
    >>> test('\nb')
    (1, 2, 1)
    >>> test('a b')
    (2, 1, 3)
    >>> test('a\nb')
    (2, 2, 1)
    >>> test('a a b')
    (4, 1, 5)
    >>> test('a\na\nb')
    (4, 3, 1)

    Repetitions
    -----------

    >>> test = compile('''
    ...     separator: ' ' ;
    ...     item = r'\w+' ;
    ...     !S = item* ;
    ... ''')
    >>> test(' ')
    []
    >>> test(' abc ')
    ['abc']
    >>> test(' abc def ')
    ['abc', 'def']
    >>> test(' abc def ghi')
    ['abc', 'def', 'ghi']

    >>> test = compile('''
    ...     separator: ' ' ;
    ...     item = r'\w+' ;
    ...     !S = item+ ;
    ... ''')
    >>> test(' ')
    Traceback (most recent call last):
        ...
    SyntaxError: [1:2] expected: \w+...
    >>> test(' abc ')
    ['abc']
    >>> test(' abc def ')
    ['abc', 'def']
    >>> test(' abc def ghi')
    ['abc', 'def', 'ghi']

    >>> test = compile('''
    ...     separator: ' ' ;
    ...     item = r'\w+' ;
    ...     !S = item? ;
    ... ''')
    >>> test(' ')
    []
    >>> test(' abc ')
    ['abc']
    >>> test(' abc def ')
    Traceback (most recent call last):
        ...
    SyntaxError: [1:6] expected:...

    >>> test = compile('''
    ...     separator: ' ';
    ...     item = r'\w+';
    ...     !S = [item / ',']*;
    ... ''')
    >>> test(' ')
    []
    >>> test('a')
    ['a']
    >>> test('a, b')
    ['a', 'b']
    >>> test('a, b, c')
    ['a', 'b', 'c']

    >>> test = compile('''
    ...     separator: ' ';
    ...     item = r'\w+';
    ...     !S = [item / ',']+;
    ... ''')
    >>> test(' ')
    Traceback (most recent call last):
        ...
    SyntaxError: [1:2] expected: \w+...
    >>> test('a')
    ['a']
    >>> test('a, b')
    ['a', 'b']
    >>> test('a, b, c')
    ['a', 'b', 'c']

    Sequences and Alternatives
    --------------------------

    >>> test = compile('''
    ...     separator: ' ' ;
    ...     A = 'A' 'A' `1` | 'A' 'B' `2` ;
    ...     A = 'A' 'C' `3` ;
    ...     !S = A ;
    ... ''')
    >>> test('A A')
    1
    >>> test('A B')
    2
    >>> test('A C')
    3
    >>> test('A D')
    Traceback (most recent call last):
        ...
    SyntaxError: [1:3] expected: A B C...

    Inline Python functions or objects
    ----------------------------------

    >>> test = compile('''
    ...     separator: ' ';
    ...     number = r'\d+' : `int` ;
    ...     couple = '(' number ',' number ')' `"a string"`
    ...              :: `lambda x,y,s: "%s: <%d,%d>"%(s,x,y)` ;
    ...     !S = couple ;
    ... ''')
    >>> test('(18, 42)')
    'a string: <18,42>'

    Rules and axiom
    ---------------

    >>> test = compile('''
    ...     separator: ' ';
    ...     !axiom = rule1 | rule2 ;
    ...     rule1 = 'A' `"rule 1 - branch 1"` ;
    ...     rule1 = 'B' `"rule 1 - branch 2"` ;
    ...     rule2 = 'A' 'A' `"rule 2 - branch 1"` ;
    ...     rule2 = 'A' 'B' `"rule 2 - branch 2"` ;
    ... ''')
    >>> test('A')
    'rule 1 - branch 1'
    >>> test('B')
    'rule 1 - branch 2'
    >>> test('A A')
    'rule 2 - branch 1'
    >>> test('A B')
    'rule 2 - branch 2'
    >>> test('A C')
    Traceback (most recent call last):
        ...
    SyntaxError: [1:3] expected: A B...

    """

    class _Ident:
        def __init__(self, name): self.name = name
        def gen(self, symbs): return symbs[self.name]

    class _Re:
        def __init__(self, name, expr): self.name, self.expr = name, expr[1:-1]
        def gen(self, symbs): return R(self.expr, flags=symbs.lexer, name=self.name)

    class _Kw:
        def __init__(self, name, val): self.name, self.val = name, val[1:-1]
        def gen(self, symbs): return K(self.val, flags=symbs.lexer, name=self.name)

    class _At:
        def gen(self, symbs): return At()

    class _Rep0N:
        def __init__(self, expr): self.expr = expr
        def gen(self, symbs): return self.expr.gen(symbs)[:]

    class _Rep1N:
        def __init__(self, expr): self.expr = expr
        def gen(self, symbs): return self.expr.gen(symbs)[1:]

    class _Rep01:
        def __init__(self, expr): self.expr = expr
        def gen(self, symbs): return self.expr.gen(symbs)[:1]

    class _RepSep0N:
        def __init__(self, expr, sep): self.expr, self.sep = expr, sep
        def gen(self, symbs): return self.expr.gen(symbs)[::self.sep.gen(symbs)]

    class _RepSep1N:
        def __init__(self, expr, sep): self.expr, self.sep = expr, sep
        def gen(self, symbs): return self.expr.gen(symbs)[1::self.sep.gen(symbs)]

    class _And:
        def __init__(self, A, B): self.A, self.B = A, B
        def gen(self, symbs): return self.A.gen(symbs) & self.B.gen(symbs)

    class _Or:
        def __init__(self, A, B): self.A, self.B = A, B
        def gen(self, symbs): return self.A.gen(symbs) | self.B.gen(symbs)

    class _Func:
        def __init__(self, expr): self.expr = expr[1:-1]
        def gen(self, symbs): return C(self.genpy(symbs))
        def genpy(self, symbs): return symbs.eval(self.expr)

    class _Apply:
        def __init__(self, expr, func): self.expr, self.func = expr, func
        def gen(self, symbs):
            expr = self.expr.gen(symbs)
            func = self.func.genpy(symbs)
            return expr / func

    class _ApplyAll:
        def __init__(self, expr, func): self.expr, self.func = expr, func
        def gen(self, symbs):
            expr = self.expr.gen(symbs)
            func = self.func.genpy(symbs)
            return expr * func

    class _Rule:
        isaxiom = False
        def __init__(self, ident, expr): self.ident, self.expr = ident, expr
        def gen(self, symbs):
            rule = symbs[self.ident.name]
            rule |= self.expr.gen(symbs)

    class _Axiom(_Rule):
        isaxiom = True

    class _Separator(_Rule):
        def __init__(self, expr): self.expr = expr
        def gen(self, symbs):
            expr = self.expr.gen(symbs)
            Separator(expr).__enter__()

    class _Lexer(_Rule):
        def __init__(self, opts): self.opts = opts
        def gen(self, symbs):
            symbs.lexer = 0
            for opt in self.opts:
                val = getattr(re, opt.name)
                if not isinstance(val, int):
                    raise TypeError("re.%s is not an integer"%opt)
                symbs.lexer |= val

    class _Symbs:
        def __init__(self, frame):
            self.symbs = {}
            self.globals = frame.f_globals
            self.locals = frame.f_locals
            self.lexer = 0
        def __iter__(self): return iter(self.symbs)
        def __getitem__(self, name):
            try:
                s = self.symbs[name]
            except KeyError:
                s = self.symbs[name] = Rule()
            return s
        def eval(self, expr):
            return eval(expr, self.globals, self.locals)

    class _Grammar:
        def __init__(self, rules): self.rules = rules
        def gen(self, frame):
            symbs = _Symbs(frame)
            separator = Separator(None)
            separator.__enter__()
            try:
                for rule in self.rules:
                    rule.gen(symbs)
            finally:
                separator.__exit__()
            empty = set(name for name in symbs if symbs[name].parser is None)
            if empty: raise NameError("Undefined symbols: %s"%(", ".join(sorted(empty))))
            axioms = set(rule.ident.name for rule in self.rules if rule.isaxiom)
            if len(axioms) == 0: raise NameError("No axiom")
            if len(axioms) > 1: raise NameError("Too many axioms: %s"%(", ".join(axioms)))
            return symbs[axioms.pop()]

    ident = R(r'[a-z_]\w*', re.I, name='ident') / _Ident
    _string = """
        " [^"\\\n]* (?: \\. [^"\\\n]* )* "
    |   ' [^'\\\n]* (?: \\. [^'\\\n]* )* '
    """
    regexpr = R(r'(?:(\w+)\.)?r(%s)'%_string, re.VERBOSE, name='regexpr') * _Re
    string = R(r'(?:(\w+)\.)?(%s)'%_string, re.VERBOSE, name='string') * _Kw
    func = R(r'''`[^`]+`''', name='func') / _Func

    separator = Separator(r"\s+|#.*")
    separator.__enter__()
    try:
        expr_or = Rule()
        expr_and = Rule()
        expr_rep = Rule()
        expr_func = Rule()
        option = Rule()
        item = ident | regexpr | string | func | '(' & expr_or & ')' | '@' & C(_At())
        expr_rep |= (item & '*') / _Rep0N
        expr_rep |= (item & '+') / _Rep1N
        expr_rep |= (item & '?') / _Rep01
        expr_rep |= ('[' & expr_or & '/' & expr_or & ']' & '*') * _RepSep0N
        expr_rep |= ('[' & expr_or & '/' & expr_or & ']' & '+') * _RepSep1N
        expr_rep |= item
        expr_and |= (expr_rep & expr_and) * _And | expr_rep
        expr_func |= (expr_and & '::' & func) * _ApplyAll | expr_and
        expr_func |= (expr_and & ':' & func) * _Apply | expr_and
        expr_func |= expr_and
        expr_or |= (expr_func & '|' & expr_or) * _Or | expr_func
        rule = (ident & '=' & expr_or & ';') * _Rule
        axiom = '!' & (ident & '=' & expr_or & ';') * _Axiom
        option |= (K('separator') & ':' & expr_or & ';') / _Separator
        lexsep = K('') | ',' | '|' | '+'
        option |= (K('lexer') & ':' & ident[::lexsep] & ';') / _Lexer
        grammar = (axiom|rule|option)[:] / _Grammar
    finally:
        separator.__exit__()

    return grammar(source).gen(frame)

def _exc():
    exc = getattr(sys, 'exc_value', None)       # for Python 2.6
    if exc is None:
        info = getattr(sys, 'exc_info', None)   # for Python 3.1
        if info is not None: exc = info()[1]
    if exc is None:
        class FakeExc:
            filename = ""
            lineno = 0
        exc = FakeExc()
    return exc

def compile(source):
    frame = sys._getframe(1)
    try:
        return _compile_string(source, frame)
    except SyntaxError:
        import os
        filename = frame.f_code.co_filename
        if os.path.isfile(filename):
            # Locate the grammar in the source
            python_source = open(filename).read()
            index = python_source.find(source)
            if index >= 0:
                # if found, update the filename and the error line number
                err = _exc()
                err.filename = filename
                err.lineno += python_source[:index].count('\n')
        raise

def compile_file(filename):
    frame = sys._getframe(1)
    try:
        return _compile_string(open(filename).read(), frame)
    except SyntaxError:
        _exc().filename = filename
        raise

if __name__ == '__main__':
    import doctest
    print(__license__.strip())
    failure_count, test_count = doctest.testmod(optionflags=doctest.ELLIPSIS)
    if failure_count == 0:
        print("*"*70)
        print("All %d tests succeeded"%test_count)
