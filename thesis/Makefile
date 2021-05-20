
 # This is a GNU makefile.

all:	paper


clean:	
	rm -rf work.bib *.aux *.bbl *.log *.blg *.dvi *.ps *.pdf *.toc

quick:  notations.tex psbq3.tex
	pdflatex psbq3.tex

paper:  notations.tex psbq3.tex
	source updatebib.sh; bibtex psbq3; pdflatex psbq3.tex; pdflatex psbq3.tex
