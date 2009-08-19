..  Simple Parser
    Copyright (C) 2009 Christophe Delord
    http://christophe.delord.free.fr/sp

..  This file is part of Simple Parser.

..  Simple Parser is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published
    by the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

..  Simple Parser is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

..  You should have received a copy of the GNU Lesser General Public License
    along with Simple Parser.  If not, see <http://www.gnu.org/licenses/>.

===============
 Simple Parser
===============
---------------------------------------
 How to easily write parsers in Python
---------------------------------------

:Author:    Christophe Delord
:Contact:   christophe.delord@free.fr
:Web site:  http://christophe.delord.free.fr/sp
:Date:      |date|
:License:   This software is released under the LGPL license.

.. |date| date:: %A %d %B %Y

.. contents:: Table of Contents
    :depth: 2

.. sectnum::
    :depth: 2

Introduction and tutorial
=========================

Introduction
------------

SP (Simple Parser) is a Python [#]_ parser generator.
It is aimed at easy usage rather than performance.

.. [#] Python is a wonderful object oriented programming language available at http://www.python.org

License
~~~~~~~

SP is available under the GNU Lesser General Public::

    Simple Parser: A Python parser generator

    Copyright (C) 2009 Christophe Delord

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


Structure of the document
~~~~~~~~~~~~~~~~~~~~~~~~~

`Introduction and tutorial`_
    starts smoothly with a gentle tutorial as an introduction.
    I think this tutorial may be sufficent to start with SP.
`SP reference`_
    is a reference documentation. It will detail SP as much as possible.
`Some examples to illustrate SP`_
    gives the reader some examples to illustrate SP.

Installation
------------

Getting SP
~~~~~~~~~~

SP is freely available on its web page (http://christophe.delord.free.fr/sp).

Requirements
~~~~~~~~~~~~

SP is a *pure Python* package.
It may run on *any platform* supported by Python.
The only requirement of SP is *Python 2.6* or newer [#]_.
Python can be downloaded at http://www.python.org.

.. [#] Older *Python* versions may work (tested with Python 2.4 and 2.5). See the `Older Python versions`_ chapter.

Tutorial
--------

Introduction
~~~~~~~~~~~~

This short tutorial presents how to make a simple calculator.
The calculator will compute basic mathematical expressions (``+``, ``-``, ``*``, ``|``) possibly nested in parenthesis.
We assume the reader is familiar with regular expressions.

Defining the grammar
~~~~~~~~~~~~~~~~~~~~

Expressions are defined with a grammar.
For example an expression is a sum of terms and a term is a product of factors.
A factor is either a number or a complete expression in parenthesis.

We describe such grammars with rules.
A rule describes the composition of an item of the language.
In our grammar we have 3 items (expr, term, factor).
We will call these items *symbols* or *non terminal symbols*.
The decomposition of a symbol is symbolized with ``->``.

Grammar for expressions:

+----------------------------------------------------+---------------------------------------------------------------+
| Grammar rule                                       | Description                                                   |
+====================================================+===============================================================+
| ``expr -> term (('+'|'-') term)*``                 | An expression is a term eventually followed                   |
|                                                    | with a plus (``+``) or a minus (``-``) sign                   |
|                                                    | and an other term any number of times                         |
|                                                    | (``*`` is a repetition of an expression 0 or more times).     |
+----------------------------------------------------+---------------------------------------------------------------+
| ``term -> fact (('*'|'/') fact)*``                 | A term is a factor eventually followed                        |
|                                                    | with a ``*`` or ``/`` sign                                    |
|                                                    | and an other factor any number of times.                      |
+----------------------------------------------------+---------------------------------------------------------------+
| ``fact -> ('+'|'-') fact | number | '(' expr ')'`` | A factor is either a factor precedeed by a sign, a number     |
|                                                    | or an expression in parenthesis.                              |
+----------------------------------------------------+---------------------------------------------------------------+

We have defined here the grammar rules (i.e. the sentences of the language).
We now need to describe the lexical items (i.e. the words of the language).
These words - also called *terminal symbols* - are described using regular expressions.
In the rules we have written some of these terminal symbols (``+``, ``-``, ``*``, ``/``, ``(``, ``)``).
We have to define ``number``.
For sake of simplicity numbers are integers composed of digits (the corresponding regular expression can be ``[0-9]+``).
To simplify the grammar and then the Python script we define two terminal symbols to group the operators (additive and multiplicative operators).
We can also define a special symbol that is ignored by SP.
This symbol is used as a separator.
This is generaly useful for white spaces and comments.

Terminal symbol definition for expressions:

+-------------------+-----------------------+--------------------+
| Terminal symbol   | Regular expression    | Comment            |
+===================+=======================+====================+
| ``number``        | ``[0-9]+ or \d+``     | One or more digits |
+-------------------+-----------------------+--------------------+
| ``addop``         | ``[+-]``              | a ``+`` or a ``-`` |
+-------------------+-----------------------+--------------------+
| ``mulop``         | ``[*/]``              | a ``*`` or a ``/`` |
+-------------------+-----------------------+--------------------+
| ``spaces``        | ``\s+``               | One or more spaces |
+-------------------+-----------------------+--------------------+

This is sufficient to define our parser with SP.

Grammar of the expression recognizer::

    def Calc():

        number = R(r'[0-9]+')
        addop = R('[+-]')
        mulop = R('[*/]')

        with Separator(r'\s+'):

            expr = Rule()
            fact = Rule()
            fact |= addop & fact
            fact |= '(' & expr & ')'
            fact |= number
            term = fact & ( mulop & fact )[:]
            expr |= term & ( addop & term )[:]

        return expr

``Calc`` is the name of the Python function that returns a parser.
This function returns ``expr`` which is the *axiom* [#]_ of the grammer.

``expr`` and ``fact`` are recursive rules.
They are first declared as empty rules (``expr = Rule()``) and alternatives are later added (``expr |= ...``).

Slices are used to implement repetitions.
``foo[:]`` parses ``foo`` zero or more times, which is equivalent to ``foo*`` is a classical grammar notation.

With this small grammar we can only recognize a correct expression.
We will see in the next sections how to read the actual expression and to compute its value.

.. [#] The axiom is the symbol from which the parsing starts

Reading the input and returning values
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The input of the grammar is a string.
To do something useful we need to read this string in order to transform it into an expected result.

This string can be read by catching the return value of terminal symbols.
By default any terminal symbol returns a string containing the current token.
So the token ``'('`` always returns the string ``'('``.
For some tokens it may be useful to compute a Python object from the token.
For example ``number`` should return an integer instead of a string,
``addop`` and ``mulop``, followed by a number, should return a function corresponding to the operator.
That's why we will add a function to the token and rule definitions.
So we associate ``int`` to ``number`` and ``op1`` and ``op2`` to unary and binary operators.

``int`` is a Python function converting objects to integers and ``op1`` and ``op2`` are user defined functions.

``op1`` and ``op2`` functions::

    op1 = lambda f,x: {'+':pos, '-':neg}[f](x)
    op2 = lambda f,y: lambda x: {'+': add, '-': sub, '*': mul, '/': div}[f](x,y)

    # red applyies functions to a number
    def red(x, fs):
        for f in fs: x = f(x)
        return x

To associate a function to a token or a rule it must be applyed using ``/`` or ``*`` operators:
    * ``/`` applyies a function to an object returned by a (sub)parser.
    * ``*`` applyies a function to an tuple of objects returned by a sequence of (sub) parsers.

Token and rule definitions with functions::

    number = R(r'[0-9]+') / int

    fact |= (addop & fact) * op1
    term = (fact & ( (mulop & fact) * op2 )[:]) * red

    # R(r'[0-9]+') applyed on "42" will return "42".
    # R(r'[0-9]+') / int will return int("42")

    # addop & fact applyied on "+ 42" will return ('+', 42)
    # (addop & fact) * op1 will return op1(*('+', 42)), i.e. op1('+', 42)
    # so (addop & fact) * op1 returns +42

    # (addop & fact) * op2 will return op2(*('+', 42)), i.e. op2('+', 42)
    # so (addop & fact) * op2 returns lambda x: add(x, 42)

    # fact & ( (mulop & fact) * op2 )[:] returns a number and a list of functions
    # for instance (42, [(lambda x:mul(x, 43)), (lambda x:mul(x, 44))])
    # so (fact & ( (mulop & fact) * op2 )[:]) * red applyied on "42*43*44"
    # will return red(42, [(lambda x:mul(x, 43)), (lambda x:mul(x, 44))])
    # i.e. 42*43*44

Here is finally the complete parser.

Expression recognizer and evaluator::

    from sp import *

    def Calc():

        from operator import pos, neg, add, sub, mul, truediv as div

        op1 = lambda f,x: {'+':pos, '-':neg}[f](x)
        op2 = lambda f,y: lambda x: {'+': add, '-': sub, '*': mul, '/': div}[f](x,y)

        def red(x, fs):
            for f in fs: x = f(x)
            return x

        number = R(r'[0-9]+') / int
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

        return expr

Embeding the parser in a script
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A parser is a simple Python object.
This example show how to write a function that returns a parser.
The parser can be applyied to strings by simply calling the parser.

Writting SP grammars in Python::

    from sp import *

    def MyParser():

        parser = ...

        return parser

    # You can instanciate your parser here
    my_parser = MyParser()

    # and use it
    parsed_object = my_parser(string_to_be_parsed)

To use this parser you now just need to instanciate an object.

Complete Python script with expression parser::

    from sp import *

    def Calc():

        from operator import pos, neg, add, sub, mul, truediv as div

        op1 = lambda f,x: {'+':pos, '-':neg}[f](x)
        op2 = lambda f,y: lambda x: {'+': add, '-': sub, '*': mul, '/': div}[f](x,y)

        def red(x, fs):
            for f in fs: x = f(x)
            return x

        number = R(r'[0-9]+') / int
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

        return expr

    calc = Calc()
    while True:
        expr = input('Enter an expression: ')
        try: print(expr, '=', calc(expr))
        except Exception as e: print("%s:"%e.__class__.__name__, e)

Conclusion
~~~~~~~~~~

This tutorial shows some of the possibilities of SP.
If you have read it carefully you may be able to start with SP.
The next chapters present SP more precisely.
They contain more examples to illustrate all the features of SP.

Happy SP'ing!

SP reference
============

Usage
-----

SP is a package which main function is to provide basic objects to build a complete parser.

The grammar is a Python object.

Grammar embeding example::

    def Foo():
        bar = R('bar')
        return bar

Then you can use the new generated parser.
The parser is simply a Python object.

Parser usage example::

    test = "bar"
    my_parser = Foo()
    x = my_parser(test)               # Parses "bar"
    print x

Grammar structure
-----------------

SP grammars are Python objects.
SP grammars may contain two parts:

Tokens
    are built by the ``R`` or ``K`` keywords.
Rules
    are described after tokens in a ``Separator`` context.

Example of SP grammar structure::

    def Foo():

        # Tokens
        number = R(r'\d+') / int

        # Rules
        with Separator(r'\s+'):
            S = number[:]

        return S

    foo = Foo()
    result = foo("42 43 44") # return [42, 43, 44]

Lexer
-----

Regular expression syntax
~~~~~~~~~~~~~~~~~~~~~~~~~

The lexer is based on the *re* [#]_ module.
SP profits from the power of Python regular expressions.
This document assumes the reader is familiar with regular expressions.

You can use the syntax of regular expressions as expected by the *re* [#]_ module.

.. [#] *re* is a standard Python module.
       It handles regular expressions.
       For further information about *re* you can read http://docs.python.org/lib/module-re.html

.. [#] Read the Python documentation for further information: http://docs.python.org/lib/re-syntax.html

Predefined tokens
~~~~~~~~~~~~~~~~~

Tokens can be explicitely defined by the ``R``, ``K`` and ``Separator`` keywords.

+---------------+---------------------------------------------------------------------------+
| Expression    | Usage                                                                     |
+===============+===========================================================================+
| ``R``         | defines a regular token.                                                  |
|               | The token is defined with a regular expression and returns a string.      |
+---------------+---------------------------------------------------------------------------+
| ``K``         | defines a token that returns nothing (useful for keywords for instance).  |
|               | The keyword is defined by an identifier (in this case word boundaries     |
|               | are expected around the keyword) or another string (in this case the      |
|               | pattern is not considered as a regular expression).                       |
|               | The token just recognizes a keyword and returns nothing.                  |
+---------------+---------------------------------------------------------------------------+
| ``Separator`` | if a context manager used to define separators for the rules defined      |
|               | in the context.                                                           |
|               | The token is defined with a regular expression and returns nothing.       |
+---------------+---------------------------------------------------------------------------+

A token can be defined by:

a name
    which identifies the token.
    This name is used by the parser.
a regular expression
    which describes what to match to recognize the token.
an action
    which can translate the matched text into a Python object.
    It can be a function of one argument or a non callable object.
    If it is not callable, it will be returned for each token
    otherwise it will be applied to the text of the token and the result will be returned.
    This action is optional. By default the token text is returned.

Token definition examples::

    integer = R(r'\d+') / int
    identifier = R(r'[a-zA-Z]\w*\b')
    boolean = R(r'(True|False)\b') / (lambda b: b=='True')

    spaces = K(r'\s+')
    comments = K(r'#.*')

    with Separator(spaces|comments):
        # rules defined here will use spaces and comments as separators
        atom = '(' & expr & ')'

There are two kinds of tokens.
Tokens defined by the ``R`` or ``K`` keywords are parsed by the parser
and tokens defined by the ``Separator`` keyword are considered as separators
(white spaces or comments for example) and are wiped out by the lexer.

The word boundary ``\b`` can be used to avoid recognizing "True" at the beginning of "Truexyz".

Inline tokens
~~~~~~~~~~~~~

Tokens can also be defined on the fly.
Their definition are then inlined in the grammar rules.
This feature may be useful for keywords or punctuation signs.

In this case tokens can be written without the ``R`` or ``K`` keywords.
They are considered as keywords (as defined by ``K``).

Inline token definition examples::

    IfThenElse = 'if' & Cond &
                 'then' & Statement &
                 'else' & Statement

Parser
------

Declaration
~~~~~~~~~~~

A parser is declared as a Python object.

Grammar rules
~~~~~~~~~~~~~

Rule declarations have two parts.
The left side declares the symbol associated to the rule.
The right side describes the decomposition of the rule.
Both parts of the declaration are separated with an equal sign (``=``).

Rule declaration example::

    SYMBOL = (A & B) * (lambda a, b: f(a, b))

Sequences
~~~~~~~~~

Sequences in grammar rules describe in which order symbols should appear in the input string.
For example the sequence ``A & B`` recognizes an ``A`` followed by a ``B``.

For example to say that a ``sum`` is a ``term`` plus another ``term`` you can write::

    Sum = Term & '+' & Term

Alternatives
~~~~~~~~~~~~

Alternatives in grammar rules describe several possible decompositions of a symbol.
The infix pipe operator (``|``) is used to separate alternatives.
``A | B`` recognizes either an ``A`` or a ``B``.
If both ``A`` and ``B`` can be matched only the first longest match is considered.
So the order of alternatives may be very important
when two alternatives can match texts of the same size.

For example to say that an ``atom`` is an *integer* or an *expression in paranthesis*
you can write::

    Atom = integer | '(' & Expr & ')'

Repetitions
~~~~~~~~~~~

Repetitions in grammar rules describe how many times an expression should be matched.

+---------------+---------------------------------------------------------------------------+
| Expression    | Usage                                                                     |
+===============+===========================================================================+
| ``A[:1]``     | recognizes zero or one ``A``.                                             |
+---------------+---------------------------------------------------------------------------+
| ``A[:]``      | recognizes zero or more ``A``.                                            |
+---------------+---------------------------------------------------------------------------+
| ``A[1:]``     | recognizes one or more ``A``.                                             |
+---------------+---------------------------------------------------------------------------+
| ``A[m:n]``    | recognizes at least m and at most n ``A``.                                |
+---------------+---------------------------------------------------------------------------+

Repetitions are greedy.
Repetitions are implemented as Python loops.
Thus whatever the length of the repetitions, the Python stack will not overflow.

Precedence and grouping
~~~~~~~~~~~~~~~~~~~~~~~

The following table lists the different structures in increasing precedence order.
To override the default precedence you can group expressions with parenthesis.

Precedence in SP expressions:

+-----------------------+-----------------------+
| Structure             | Example               |
+=======================+=======================+
+ Alternative           | ``A | B``             |
+-----------------------+-----------------------+
+ Sequence              | ``A & B``             |
+-----------------------+-----------------------+
+ Repetitions           | ``A[x:y]``            |
+-----------------------+-----------------------+
+ Symbol and grouping   | ``A`` and ``( ... )`` |
+-----------------------+-----------------------+

Actions
~~~~~~~

Grammar rules can contain actions as Python functions.

Functions are applyied to parsed objects using ``/`` or ``*``.

+-----------------------+---------------------------------------------------------------------------+
| Expression            | Value                                                                     |
+=======================+===========================================================================+
| ``parser / function`` | returns *function(result of parser)*.                                     |
+-----------------------+---------------------------------------------------------------------------+
| ``parser * function`` | returns *function(\*result of parser)*.                                   |
+-----------------------+---------------------------------------------------------------------------+

``*`` can be used to analyse the result of a sequence.

Abstract syntax trees
~~~~~~~~~~~~~~~~~~~~~

An abstract syntax tree (AST) is an abstract representation of the structure of the input.
A node of an AST is a Python object (there is no constraint about its class).
AST nodes are completely defined by the user.

AST example (parsing a couple)::

    class Couple:
        def __init__(self, a, b):
            self.a = a
            self.b = b

    def Foo():
        couple = ('(' & item & ',' & item & ')') * Couple
        return couple

Constants
~~~~~~~~~

It is sometimes useful to return a constant.
``C`` defines a parser that matches an empty input and returns a constant.

Constant example::

    number = (  '1' & C("one")
             |  '2' & C("two")
             |  '3' & C("three")
             )

Older Python versions
=====================

This document describes the usage of SP with Python 2.6.
Grammars need some adaptations to work with Python 2.5. or older.

Separators
----------

Separators use context managers which don't exist in Python 2.4.
Context managers have been introduced in Python 2.5
(``from __future__ import with_statement``)
and in Python 2.6 (as a standard feature).
When the context managers are not available, it may be possible
to call the ``__enter__`` and ``__exit__`` method explicitly
(tested for Python 2.4).

Python 2.6 and later::

    number = R(r'\d+') / int
    with Separator('\s+'):
        coord = number & ',' & number

Python 2.5 with ``with_statement``::

    from __future__ import with_statement

    number = R(r'\d+') / int
    with Separator('\s+'):
        coord = number & ',' & number

Python 2.5 or 2.4 (or older but not tested) without ``with_statement``::

    sep = Separator('\s+')

    number = R(r'\d+') / int
    sep.__enter__()
    coord = number & ',' & number
    sep.__exit__()

Some examples to illustrate SP
==============================

Complete interactive calculator
-------------------------------

This chapter presents an extention of the calculator described in the `tutorial`_.
This calculator has more functions and a memory.

New functions
~~~~~~~~~~~~~

The calculator has memories.
A memory cell is identified by a name.
For example, if the user types ``pi = 3.14``,
the memory cell named ``pi`` will contain the value of ``pi``
and ``2*pi`` will return ``6.28``.

The variables are saved in a dictionnary.

Source code

Here is the complete source code (*calc.py*):

.. include:: calc.py
    :literal:
