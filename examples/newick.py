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

# from http://en.wikipedia.org/wiki/Newick_format

import sp

EXAMPLES = """\
(,,(,));                               no nodes are named
(A,B,(C,D));                           leaf nodes are named
(A,B,(C,D)E)F;                         all nodes are named
(:0.1,:0.2,(:0.3,:0.4):0.5);           all but root node have a distance to parent
(:0.1,:0.2,(:0.3,:0.4):0.5):0.0;       all have a distance to parent
(A:0.1,B:0.2,(C:0.3,D:0.4):0.5);       distances and leaf names (popular)
(A:0.1,B:0.2,(C:0.3,D:0.4)E:0.5)F;     distances and all names
((B:0.2,(C:0.3,D:0.4)E:0.5)F:0.1)A;    a tree rooted on a leaf node (rare)
"""

class Leaf:
    def __init__(self, name): self.name = name
    def __str__(self): return self.name
    def nb_leaves(self): return 1

class Internal:
    def __init__(self, subtrees, name):
        self.subtrees, self.name = subtrees, name
    def __str__(self):
        return "(%s)%s"%(
            ','.join(str(st) for st in self.subtrees),
            self.name
        )
    def nb_leaves(self):
        return sum(st.nb_leaves() for st in self.subtrees)

class Branch:
    def __init__(self, subtree, length):
        self.subtree, self.length = subtree, length
    def __str__(self):
        return "%s:%s"%(self.subtree, self.length)
    def nb_leaves(self):
        return self.subtree.nb_leaves()

parser = sp.compile(r"""
    !Tree = Subtree ';' | Branch ';' ;
    Subtree = Leaf | Internal ;
    Leaf = Name : `Leaf` ;
    Internal = '(' [Branch/',']+ ')' Name :: `Internal` ;
    Branch = Subtree Length :: `Branch` ;
    Name = r'[^;:,()]*';
    Length = ':' r'[0-9.]+' : `float` | `0.0` ;
""")

for example in EXAMPLES.splitlines():
    example, description = example.split(' ', 1)
    description = description.strip()
    tree = parser(example)
    print("%s:"%description)
    print("-"*len(description))
    print("    Input : %s"%example)
    print("    Parsed: %s"%tree)
    print("    Leaves: %s"%tree.nb_leaves())
    print("")
