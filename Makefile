# Simple Parser
# Copyright (C) 2009 Christophe Delord
# http://christophe.delord.free.fr/sp

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

all: doc/sp.html doc/sp.pdf

# Only the documentation is generated.
# The archive is built by the release script.

doc/sp.html: doc/sp.rst doc/*.py doc/style.css
	LANG=en rst2html --stylesheet-path=doc/style.css --section-numbering --language=en --cloak-email-addresses $< > $@

doc/sp.pdf: doc/sp.rst doc/*.py
	LANG=en rst2latex --section-numbering --language=en $< $(@:.pdf=.tex)
	pdflatex -output-directory doc $(@:.pdf=.tex)
	pdflatex -output-directory doc $(@:.pdf=.tex)
	pdflatex -output-directory doc $(@:.pdf=.tex)
	rm -f doc/sp.aux
	rm -f doc/sp.log
	rm -f doc/sp.out
	rm -f doc/sp.tex
