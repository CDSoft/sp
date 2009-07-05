#!/usr/bin/env python3

import sp

def tag(name, text):
    print("[%s %s]"%(name, text.strip()))

title = lambda t: tag("title", t)
keywords = lambda t: tag("keywords", t)
section = lambda t: tag("section", t)

def text(t):
    print("<pre>")
    print(t.strip())
    print("</pre>")

def esc(s):
    s = s.replace("[", "&#91;")
    s = s.replace("]", "&#93;")
    s = s.replace("$", "&#36;")
    return s


#####################################################################

title(sp.__doc__.splitlines()[0])

keywords("SP, Simple Parser, Parser, Generator, lexical, syntactic, parser, analyseur, syntaxique, Python, grammar, grammaire, récursive, descendante, recursive, descendant, arbre, abstrait, tree, abstract, download, téléchargement, license, rule, règle, parsing, analyse, scanning")

section("Introduction")

text(esc('\n'.join(sp.__doc__.splitlines()[1:])))

section("License")

text(sp.__license__)

section("Download")

text("Archive with sp.py, documentation and examples: [link sp.tgz]")

section("Under construction")

text("""The documentation is under construction.
You will find more information in the doc strings of the script.
""")
