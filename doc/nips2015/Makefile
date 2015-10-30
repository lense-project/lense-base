LATEX = pdflatex
#LATEX = pdflatex -interaction=nonstopmode
OUTPUT=$(shell basename `pwd`)
SECTIONS = $(wildcard *.tex)
FIGURES = $(wildcard figures/*.tikz)

all: $(OUTPUT).pdf

$(OUTPUT).pdf: paper.tex $(SECTIONS) $(FIGURES)
	make -C figures
	$(LATEX) $<
	cp $(subst .tex,.pdf,$<) $@

final: paper.tex $(SECTIONS)
	texify < $< > $(subst .tex,.tmp.tex,$<)
	$(LATEX) $(subst .tex,.tmp.tex,$<)
	bibtex $(subst .tex,,$<)
	$(LATEX)  $(subst .tex,.tmp.tex,$<)
	$(LATEX)  $(subst .tex,.tmp.tex,$<)
	cp $(subst .tex,.tmp.pdf,$<) $(OUTPUT).pdf

bib: paper.tex all.bib $(SECTIONS)
	$(LATEX) $< 
	bibtex $(subst .tex,,$<)
	$(LATEX) $< 
	$(LATEX) $< 
	cp $(subst .tex,.pdf,$<) $(OUTPUT).pdf

# Simple thing that just works
default: all.bib
	pdflatex paper
	bibtex paper
run:
	go paper.pdf
deploy:
	scp paper.pdf jamie:/u/apache/htdocs/pliang/lense.pdf

split: $(OUTPUT).pdf
	pdfjam --paper letterpaper --outfile main.pdf $(OUTPUT).pdf 1-9
	pdfjam --paper letterpaper --outfile appendix.pdf $(OUTPUT).pdf 10-12

clean:
	rm -rf *.{aux,bbl,blg,log,out}
	rm -rf $(OUTPUT).pdf

refdb:
	git clone https://github.com/percyliang/refdb

all.bib: refdb refdb/data/entries.rb
	cd refdb; git pull
	refdb/generate.rb bib out=all.bib

.PHONY: clean bib final
