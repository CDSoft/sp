all: sp.tgz

sp.tgz: sp.py examples/calc.py examples/notation.py sp.pdf
	tar czvf $@ $^

sp.pdf: doc/*.tex doc/*.py
	(cd doc; pdflatex sp)
	cp doc/$@ $@

