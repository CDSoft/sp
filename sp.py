#!/usr/bin/env python3

# Simple Parser
# Copyright (C) 2009 Christophe Delord
# http://christophe.delord.free.fr/sp

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
>>> calc = calc_parser()
>>> calc("1+2+3")
6
>>> calc("1+(2*3)")
7
>>> calc("1-(2*3)")
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

def _memoize_self_s(f):
    """ creates a memoized parser method

        arguments self (object) and s (string to parse) are memoized.
    """
    cache = {}
    def _f(self, s):
        try:
            return cache[self, s]
        except KeyError:
            r = f(self, s)
            cache[self, s] = r
            return r
    _f.__doc__ = f.__doc__
    _f.__name__ = f.__name__
    return _f

def _memoize_self_s_e(f):
    """ creates a memoized parser method

        arguments self (object) and s (string to parse) are memoized.
        The error argument (e) is not indexed.
    """
    cache = {}
    def _f(self, s, e):
        try:
            return cache[self, s]
        except KeyError:
            r = f(self, s, e)
            cache[self, s] = r
            return r
    _f.__doc__ = f.__doc__
    _f.__name__ = f.__name__
    return _f

class _err:
    """ stores the maximal position of the detected errors
    """

    def __init__(self, s, *ts):
        self.s = s
        self.ts = tuple(ts)

    def max(self, other):
        if len(self.s) < len(other.s):
            return self
        elif len(self.s) > len(other.s):
            return other
        else:
            return _err(self.s, *(self.ts + tuple(t for t in other.ts if t not in self.ts)))

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
        SyntaxError: [1:1] expected: begin
        >>> s('begin +18')
        Traceback (most recent call last):
            ....
        SyntaxError: [1:7] expected: \d+ \w+
        """
        s = s[:-len(self.s)]
        line = s.count('\n') + 1
        col = len(s[s.rfind('\n')+1:]) + 1
        msg = "[%d:%d] expected:"%(line, col)
        for t in self.ts:
            if t.startswith(r'\b'): t = t[2:]
            if t.endswith(r'\b'): t = t[:-2]
            msg += " "+t
        return SyntaxError(msg)

def _p(obj):
    """ converts 'obj' to a parser object

    A parser is not changed:
    >>> word = K('Ham') | K('Spam')
    >>> id(_p(word)) == id(word)
    True

    A string is translated into a single Keyword parser.
    >>> num = _p(r'\d+')
    >>> num.parse("42 43", _err(""))[:2]
    (fail, '42 43')
    >>> num.parse(r"\d+ 42 43", _err(""))[:2]
    (nil, ' 42 43')
    >>> kw = _p('foo')
    >>> kw.parse("foo bar", _err(""))[:2]
    (nil, ' bar')
    >>> kw.parse("foobar", _err(""))[:2]
    (fail, 'foobar')

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
        - returns a tuple (value, "rest to parse", error) in case of success
            if value is nil, it will be later ignored
        - returns a tuple (fail, "initial value", error) in case of failure
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
        SyntaxError: [2:1] expected: \d+

        or when their are remaining stuff not parseable:
        >>> num("\\n\\n42\\n...")
        Traceback (most recent call last):
            ...
        SyntaxError: [4:1] expected:
        """
        s1 = self.skipsep(s)
        x, s1, e = self.parse(s1, _err(s))
        s1 = self.skipsep(s1)
        if x is fail or s1 != "":
            raise e.msg(s)
        return x

    @_memoize_self_s
    def skipsep(self, s):
        """ removes separators from a string

        >>> with Separator(r'\s'):
        ...     p = Parser()
        >>> p.skipsep("   spam   ")
        'spam   '
        """
        if self.separator is None: return s
        while True:
            sep, s, e = self.separator.parse(s, _err(s))
            if sep is fail: return s

    def __and__(self, other):
        """ returns a sequence parser

        >>> with Separator(" "): ab = R("a") & "b"
        >>> ab.parse("a b c", _err(""))[:2]
        ('a', 'c')
        >>> ab.parse("b a c", _err(""))[:2]
        (fail, 'b a c')
        """
        return And(self, other)

    def __rand__(self, other):
        """ returns a sequence parser

        >>> with Separator(" "): ab = "a" & R("b")
        >>> ab.parse("a b c", _err(""))[:2]
        ('b', 'c')
        >>> ab.parse("b a c", _err(""))[:2]
        (fail, 'b a c')
        """
        return And(other, self)

    def __or__(self, other):
        """ returns an alternative parser

        >>> with Separator(" "): ab = R("a") | "b"
        >>> ab.parse("a b c", _err(""))[:2]
        ('a', 'b c')
        >>> ab.parse("b a c", _err(""))[:2]
        (nil, 'a c')
        >>> ab.parse("c a b", _err(""))[:2]
        (fail, 'c a b')
        """
        return Or(self, other)

    def __ror__(self, other):
        """ returns an alternative parser

        >>> with Separator(" "): ab = "a" | R("b")
        >>> ab.parse("a b c", _err(""))[:2]
        (nil, 'b c')
        >>> ab.parse("b a c", _err(""))[:2]
        ('b', 'a c')
        >>> ab.parse("c a b", _err(""))[:2]
        (fail, 'c a b')
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
        >>> item[:].parse("1 2 3 4", _err(""))[:2]
        ([1, 2, 3, 4], '')
        >>> item[:2].parse("1 2 3 4", _err(""))[:2]
        ([1, 2], '3 4')
        >>> item[::","].parse("1, 2, 3, 4", _err(""))[:2]
        ([1, 2, 3, 4], '')
        """
        return Rep(self, slice.start, slice.stop, slice.step)

    def __truediv__(self, func):
        """ returns a parser that applies a function to the result of another parser

        The argument of the function is a single object.

        >>> sum = lambda xy: xy[0]+xy[1]
        >>> num = R(r'\d+') / int               # parses an integer
        >>> add = (num & '+' & num) / sum       # returns the sum of two integers
        >>> add.parse("1+2+4", _err(""))[:2]
        (3, '+4')
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
        >>> add.parse("1+2+4", _err(""))[:2]
        (3, '+4')
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
    >>> nums.parse(" 42 43 ", _err(""))[:2] # matches nothing at the very beginning of the input
    ([], ' 42 43 ')
    >>> with Separator(r'\s'):
    ...     nums = num[:]
    >>> nums.parse(" 42 43 ", _err(""))[:2] # can match numbers after discarding spaces
    (['42', '43'], '')
    """

    def __init__(self, parser):
        if isinstance(parser, str): parser = R(parser)
        self.parser = _p(parser)

    def __enter__(self):
        global _separator
        self.previous_parser = _separator
        _separator = self.parser

    def __exit__(self, type, value, traceback):
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
    """

    def __init__(self, pattern, flags=0):
        Parser.__init__(self)
        self.pattern = re.compile(pattern, flags)

    def parse(self, s, e):
        s1 = self.skipsep(s)
        token = self.pattern.match(s1)
        if not token: return fail, s, e.max(_err(s1, self.pattern.pattern))
        token = token.group(0)
        rest = self.skipsep(s1[len(token):])
        return token, rest, e.max(_err(rest))

class K(R):
    """ is a keyword parser

    Works a bit as R.
    
    If the pattern is a keyword (\w+)
    word boundaries are expected around the token.
    >>> t = K('ham') & C('ok')
    >>> with Separator(r'\s'): t[:].parse("ham ham hammm", _err(""))[:2]
    (['ok', 'ok'], 'hammm')

    Otherwise the pattern is escaped.
    >>> t = K('++') & C('pp')
    >>> with Separator(r'\s'): t[:].parse("++ ++ +++", _err(""))[:2]
    (['pp', 'pp', 'pp'], '+')
    """

    def __init__(self, pattern, flags=0):
        if pattern.isalnum(): pattern = r"\b%s\b"%pattern
        else: pattern = re.escape(pattern)
        R.__init__(self, pattern, flags)

    def parse(self, s, e):
        obj, rest, e = R.parse(self, s, e)
        if obj is fail: return fail, rest, e
        else: return nil, rest, e

class C(Parser):
    """ is a constant parser

    C parses nothing, simply returns a constant.

    >>> c = C('foo')
    >>> c.parse("spam", _err(""))[:2]
    ('foo', 'spam')
    """

    def __init__(self, val):
        Parser.__init__(self)
        self.val = val

    def parse(self, s, e):
        s = self.skipsep(s)
        return self.val, s, e.max(_err(s))

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

    def parse(self, s, e):
        rest = self.skipsep(s)
        x, rest, e = self.parser.parse(rest, e)
        if x is fail: return fail, s, e.max(_err(rest))
        rest = self.skipsep(rest)
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

    @_memoize_self_s_e
    def parse(self, s, e):
        tokens = []
        rest = self.skipsep(s)
        for item in self.items:
            token, rest, e = item.parse(rest, e)
            if token is fail: return fail, s, e.max(_err(rest))
            if token is not nil: tokens.append(token)
            rest = self.skipsep(rest)
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
    SyntaxError: [1:2] expected: b
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

    @_memoize_self_s_e
    def parse(self, s, e):
        s1 = self.skipsep(s)
        e = e.max(_err(s1))
        matches = []
        for item in self.items:
            token, rest, e = item.parse(s1, e)
            if token is not fail:
                rest = self.skipsep(rest)
                e = e.max(_err(rest))
                matches.append((token, rest))
        if matches:
            # Returns the match that leaves the shortest rest
            return min(matches, key=(lambda t: len(t[1]))) + (e,)
        else:
            return fail, s, e

# Python 2.4 fallback
if sys.version_info[:2] < (2,5):
    def min(xs, key=(lambda x:x), min=min):
        return min((key(x), x) for x in xs)[1]

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

    def parse(self, s, e):
        s1 = self.skipsep(s)
        x, rest, e = self.parser.parse(s1, e)
        if x is fail: return fail, s, e.max(_err(rest))
        rest = self.skipsep(rest)
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
    SyntaxError: [1:1] expected: A
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
    SyntaxError: [1:2] expected:

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

    def _parse_no_sep(self, s, e):
        items = []
        i = 0
        rest = self.skipsep(s)
        while i != self.max:
            i += 1
            item, rest, e = self.parser.parse(rest, e)
            if item is fail:
                if i <= self.min: return fail, s, e.max(_err(rest))
                return items, rest, e.max(_err(rest))
            items.append(item)
            rest = self.skipsep(rest)
        return items, rest, e.max(_err(rest))

    def _parse_with_sep(self, s, e):
        rest = self.skipsep(s)
        item, rest, e = self.parser.parse(rest, e)
        if item is fail:
            if 1 <= self.min: return fail, s, e.max(_err(rest))
            rest = self.skipsep(rest)
            return [], rest, e.max(_err(rest))
        items = [item]
        i = 1
        rest = self.skipsep(rest)
        while i != self.max:
            i += 1
            sep, rest, e = self.sep.parse(rest, e)
            if sep is fail:
                if i <= self.min: return fail, s, e.max(_err(rest))
                return items, rest, e.max(_err(rest))
            rest = self.skipsep(rest)
            item, rest, e = self.parser.parse(rest, e)
            if item is fail:
                if i <= self.min: return fail, s, e.max(_err(rest))
                return items, rest, e.max(_err(rest))
            items.append(item)
            rest = self.skipsep(rest)
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

    def parse(self, s, e):
        s1 = self.skipsep(s)
        token, rest, e = self.parser.parse(s1, e)
        if token is fail: return fail, s, e.max(_err(rest))
        rest = self.skipsep(rest)
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

    def parse(self, s, e):
        s1 = self.skipsep(s)
        token, rest, e = self.parser.parse(s1, e)
        if token is fail: return fail, s, e.max(_err(rest))
        rest = self.skipsep(rest)
        return self.func(*token), rest, e.max(_err(rest))

if __name__ == '__main__':
    import doctest
    print(__license__.strip())
    failure_count, test_count = doctest.testmod()
    if failure_count == 0:
        print("*"*70)
        print("All %d tests succeeded"%test_count)

