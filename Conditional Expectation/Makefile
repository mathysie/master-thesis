all: clean
	latexmk --xelatex

clean:
	rm -f $(filter-out $(wildcard *.tex) $(wildcard *.pdf) Makefile, $(wildcard *.*))

