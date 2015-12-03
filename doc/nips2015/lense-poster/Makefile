LATEX = pdflatex
#LATEX = pdflatex -interaction=nonstopmode
OUTPUT=$(shell basename `pwd`)
SECTIONS = macros.tex column1.tex column2.tex column3.tex
FIGURES = 

all: $(OUTPUT).pdf

$(OUTPUT).pdf: poster.tex $(SECTIONS) $(FIGURES)
	$(LATEX) $<
	mv $(subst .tex,.pdf,$<) $@

final: poster.tex $(SECTIONS)
	./texify < $< > $(subst .tex,.tmp.tex,$<)
	$(LATEX) $(subst .tex,.tmp.tex,$<)
	mv $(subst .tex,.tmp.pdf,$<) $(OUTPUT).pdf

bib: poster.tex $(SECTIONS)
	$(LATEX) $< 
	bibtex $(subst .tex,,$<)
	$(LATEX) $< 
	$(LATEX) $< 

clean:
	rm -rf *.{aux,toc,bbl,blg,log,out,nav,snm}
	rm -rf $(OUTPUT).pdf


.PHONY: clean bib final
