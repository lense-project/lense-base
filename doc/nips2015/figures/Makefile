LATEX = pdflatex
#LATEX = pdflatex -interaction=nonstopmode
FIGURES = $(wildcard *.tikz)
PDFS = $(patsubst %.tikz,%.pdf,$(FIGURES))

all: $(PDFS)

%.pdf: %.tikz
	$(LATEX) $<

clean:
	rm -rf *.{aux,bbl,blg,log,out}
	rm -rf $(OUTPUT).pdf

.PHONY: clean bib final
