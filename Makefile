all: sp.tgz

sp.pdf: doc/*.tex doc/*.py
	(cd doc; pdflatex sp && pdflatex sp && pdflatex sp)
	cp doc/$@ $@

