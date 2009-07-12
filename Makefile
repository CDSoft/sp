all: sp.tgz

sp.tgz: sp.py COPYING COPYING.LESSER $(wildcard doc/*.tex) doc/tut_calc.py examples/calc.py sp.pdf Makefile
	tar czvf $@ $^

sp.pdf: doc/*.tex doc/*.py
	(cd doc; pdflatex sp && pdflatex sp && pdflatex sp)
	cp doc/$@ $@

